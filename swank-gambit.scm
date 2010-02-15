;;;============================================================================

;;; File: "swank-gambit.scm", Time-stamp: <2010-02-15 19:08:52 james>

;;; Copyright (c) 2009-2010 by Marc Feeley and James Long, All Rights Reserved.

;;; This software is released under a dual LGPL and Apache 2 license.
;;; These licenses are included with this software in LGPL.txt and
;;; LICENSE-2.0.txt.

;;;============================================================================

(include "~~lib/_gambit#.scm")
(include "Sort.scm")

(macro-readtable-escape-ctrl-chars?-set! ##main-readtable #f)

(define SWANK-DEBUG #f)

(define (debug msg)
  (if SWANK-DEBUG
      (pp msg)))

;;; ===========================================================================

(define (unfold p f g seed . maybe-tail-gen)
  (if (pair? maybe-tail-gen)

      (let ((tail-gen (car maybe-tail-gen)))
	(if (pair? (cdr maybe-tail-gen))
	    (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)

	    (let recur ((seed seed))
	      (if (p seed) (tail-gen seed)
		  (cons (f seed) (recur (g seed)))))))

      (let recur ((seed seed))
	(if (p seed) '()
	    (cons (f seed) (recur (g seed)))))))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (drop-right lis k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;;;============================================================================

(define swank-server-address ":4005")

(define swank-server-thread-group (make-thread-group "swank-tcp-handlers"))

(define (swank-server-register! #!key (server-address swank-server-address))
  (tcp-service-register! server-address
                         swank-server-handle
                         swank-server-thread-group))

(define (swank-server-unregister! #!key (server-address swank-server-address))
  (tcp-service-unregister! server-address))

(define (swank-server-handle)
  (let loop ()
    (debug '------------------------------------------)
    (let ((req (swank-read)))
      (if (not (eof-object? req))
          (begin
            (swank-process-request req)
            (loop))))))

(define swank-length-len 6) ;; length of request length

(define (swank-read)
  (let* ((length-str (make-string swank-length-len))
         (n (read-substring length-str 0 swank-length-len)))
    (if (not (equal? n swank-length-len))
        '#!eof
        (let ((length (string->number length-str 16)))
          (and length
               (let* ((obj-str (make-string length))
                      (n (read-substring obj-str 0 length)))
                 (if (not (equal? n length))
                     #f
                     (with-input-from-string
                         obj-str
                       read))))))))

(define (swank-write obj)
  (debug (list 'emacs<== obj))
  (let* ((obj-str
          (with-output-to-string
            ""
            (lambda () (write obj) (newline))))
         (obj-str-len
          (string-length obj-str))
         (length-str-unpadded
          (number->string obj-str-len 16))
         (length-str
          (string-append
           (make-string (- swank-length-len
                           (string-length length-str-unpadded))
                        #\0)
           length-str-unpadded)))
    #;
    (begin
      (write-substring length-str 0 swank-length-len ##stdout-port)
      (write-substring obj-str 0 (string-length obj-str) ##stdout-port)
      (force-output ##stdout-port))
    (write-substring length-str 0 swank-length-len)
    (write-substring obj-str 0 (string-length obj-str))
    (force-output)))

(define swank-wire-protocol-version 'nil)

(define (swank-process-request req)
  (debug (list 'emacs==> req))
  (if (pair? req)
      (let ((op (car req)))
        (case op

          ((:emacs-rex)
           (let ((args (cdr req)))
             (if (= 4 (length args))
                 (apply
                  (lambda (msg package-name thread-id seqnum)
                    (current-seqnum seqnum)
                    (if (pair? msg)
                        (let ((args (cdr msg)))
                          (let ((op (table-ref swank-op-table (car msg) #f)))
                            (if op
                                (with-exception-catcher
                                 (lambda (exc)
                                   (if (swank-abort? exc)
                                       (swank-write
                                        (list ':return
                                              '(:abort)
                                              seqnum))
                                       (raise exc)))
                                 (lambda ()
                                   (let ((result (apply op args)))
                                   (if result
                                       (swank-write
                                        (list ':return
                                              (list ':ok result)
                                              seqnum))))))
                                (begin
                                  (debug (list '***unhandled*** req))
                                  (swank-write
                                   (list ':return
                                         '(:abort)
                                         seqnum))))))))
                  args))))

          (else
           (debug (list '***unhandled*** req)))))
      #f))

;; force `repl-input-port` and `repl-output-port` to be the
;; same as `current-input-port` and `current-output-port`

(define (thread-make-swank-repl-channel thread)
  (##make-repl-channel-ports (current-input-port)
                             (current-output-port)))


(if (not SWANK-DEBUG)
    (set! ##thread-make-repl-channel
          thread-make-swank-repl-channel))

;;;============================================================================

(define swank-modules '())

(define (swank:connection-info)

  #;
  (swank-write
   `(:indentation-update
     (("with-timeout" . 1)
      ("without-package-locks" . 0)
      ("define-source-context" . 2))))

  `(:pid ,(##os-getpid)
    :style :spawn
    :lisp-implementation (:name "gambit" :type "Gambit" :version ,(system-version-string))
    :machine (:instance ,(host-name) :type ,(system-type-string))
    :features ()
    :modules swank-modules
    :package (:name "#package-name#" :prompt "")
    :version ,swank-wire-protocol-version))

(define (swank:swank-require modules)

  (let loop ([modules (if (list? modules) modules (list modules))])
    (if (car modules)
        (if (not (member (car modules) swank-modules))
            (let ((colon-filename (object->string (car modules))))
              (load (append-strings
                     (list (substring
                            colon-filename
                            1
                            (string-length colon-filename))
                           ".scm")))))
        (loop (cdr modules))))

  swank-modules)

(define (swank:create-repl arg)
  ;; fake it
  `("???" ""))

(define (swank:arglist-for-echo-area . rest)
  ;; fake it
  'nil)

(define (swank:do-with-result thunk)
  (let ((result #f))
    (let ((output
           (with-output-to-string ""
             (lambda ()
               (set! result (do-with-sldb-handler thunk))))))
      (if (not (equal? output ""))
          (swank-write `(:write-string ,output :repl-result)))
      (if (exception-result? result)
          (debug-exception-result result))
      result)))

(define (send-repl-results values)
  (define (f value)
    (let ((result-str (object->string value)))
      (swank-write `(:write-string ,result-str :repl-result))
      (swank-write `(:write-string "\n" :repl-result))))

  (apply f values))

;; Hook for serializing results for use by presentations
;; Overridden by swank-gambit-presentations.scm
(define *send-repl-results-function* send-repl-results)

;; Hook for workaround for lack of #. syntax in scheme
;; Overridden by swank-gambit-presentations.scm
(define *preprocessing-eval* (lambda (expr) (eval expr)))

(define (swank:listener-eval expr-str)
  (let ((result
         (swank:do-with-result
          (lambda () (*preprocessing-eval*
                      (with-input-from-string expr-str read))))))
    (cond
     ((exception-result? result) #f)
     ((eq? result '#!void) 'nil)
     (else
      (*send-repl-results-function* (list result))
      'nil))))

(define (swank:interactive-eval expr-str)
  (swank-do-interactive
   write
   (lambda () (*preprocessing-eval*
               (with-input-from-string expr-str read)))))

(define (swank:interactive-eval-region expr-str)
  (swank-do-interactive
   write
   (lambda () (*preprocessing-eval*
               (cons
                'begin
                (with-input-from-string expr-str read-all))))))

(define (swank:pprint-eval expr-str)
  (swank-do-interactive
   pretty-print
   (lambda () (*preprocessing-eval*
               (with-input-from-string expr-str read)))))

(define (swank-current-continuation frame)
  (list-ref (current-backtrace) frame))

(define (eval-in-frame expr-str frame)
  (let ((expr (with-input-from-string expr-str read)))
    ;; Modelled after _repl.scm's eval-print
    (##continuation-capture
     (lambda (return)
       (##eval-within
        expr
        (swank-current-continuation frame)
        (macro-current-repl-context)
        (lambda (results)
          (##continuation-return return results)))))))

(define (swank:pprint-eval-string-in-frame expr-str frame)
  (swank-do-interactive
   pretty-print
   (lambda ()
     (eval-in-frame expr-str frame))))

(define (swank:inspect-in-frame expr-str frame)
  (let ((result
         (swank:do-with-result
          (lambda ()
            (eval-in-frame expr-str frame)))))
    (reset-inspector)
    (inspect-object result)))

(define (swank-do-interactive wr thunk)
  (let ((result (swank:do-with-result thunk)))
    (cond
     ((exception-result? result) #f)
     ((eq? result '#!void) "")
     (else 
      (with-output-to-string "" (lambda () (wr result)))))))

(define (swank:load-file filename)
  (load filename))

(define swank-server-portnum 4005)

(define (swank:start-swank-server-in-thread id filename)
  (let ((portnum (+ swank-server-portnum 1)))
    (set! swank-server-portnum portnum)
    (swank-server-register!
     server-address: (string-append ":" (number->string portnum)))
    (thread-sleep! 0.1) ;; wait for server to start
    (with-output-to-file
        filename
      (lambda () (pretty-print portnum)))
    'nil))

(define swank-threads (make-table test: eq? weak-keys: #t))

(define (swank-get-nth-thread n)
  (let ((threads (table-ref swank-threads (current-input-port) #f)))
    (and threads
         (< n (length threads))
         (list-ref threads n))))

(define (swank:list-threads)

  (define (all-threads exclude-thread-groups)
    (define (recurse thread-group threads)
      (let loop ((children (thread-group->thread-group-list
                            thread-group))
                 (threads (append
                           threads
                           (if (memq thread-group
                                     exclude-thread-groups)
                               '()
                               (thread-group->thread-list
                                thread-group)))))
        (if (pair? children)
            (loop (cdr children) (recurse (car children) threads))
            threads)))

    (##declare (not interrupts-enabled))
    (recurse (thread-thread-group (macro-primordial-thread)) '()))

  (let ((threads (all-threads (list swank-server-thread-group))))
    (table-set! swank-threads (current-input-port) threads)
    (cons '(:id :name :status)
          (map (lambda (t)
                 (list (object->string (object->serial-number t))
                       (object->string (thread-name t))
                       (object->string (list (thread-state t)
                                             (thread-thread-group t)
                                             (thread-specific t)))))
               threads))))

(define (swank:quit-thread-browser)
  (set! swank-threads (make-table test: eq? weak-keys: #t))
  'nil)

(define (swank:quit-thread-browser)
  (set! swank-threads (make-table test: eq? weak-keys: #t))
  'nil)

(define (swank:debug-nth-thread n)
  (let ((t (swank-get-nth-thread n))
        (cont-mutex (make-mutex 'swank-get-thread-capture))
        (wakeup-mutex (make-mutex 'swank-wake-thread)))
    (if t
        (begin
          (mutex-lock! cont-mutex #f t)
          (mutex-lock! wakeup-mutex #f)
          (##thread-interrupt!
           t
           (lambda ()
             (mutex-specific-set! cont-mutex (##continuation-capture (lambda (k) k)))
             (mutex-unlock! cont-mutex)
             (mutex-lock! wakeup-mutex)
             '#!void))
          (mutex-lock! cont-mutex #f (current-thread))
          (let* ((cont (mutex-specific cont-mutex))
                 (backtrace (cdr (continuation-interesting-frames cont))))
            (current-backtrace backtrace)
            (current-toplevel-cont cont)
            (current-restarts '(CONTINUE))
            (current-debug-thread t)
            (current-wakeup-mutex wakeup-mutex)
            (swank-write
             `(:debug ,(object->serial-number t)
                      1
                      (,(append-strings (list "Thread "
                                              (object->string t)
                                              " interrupt")) "  [N/A]" nil)
                      (("CONTINUE" "Continue thread execution."))
                      ,(let ((backtrace (if (>= (length backtrace) 10)
                                            (take backtrace 10)
                                            backtrace)))
                         (swank:make-frames backtrace))
                      ())))
          (swank-write
           `(:debug-activate ,(object->serial-number t) 1 true))
          (##thread-yield!))))
  'nil)

(define (swank:kill-nth-thread n)
  (let ((t (swank-get-nth-thread n)))
    (if t (thread-terminate! t))
    'nil))

(define (swank:quit-lisp)
  (exit))

(define (swank:completions prefix package)
  ;; fake it (should use the all-symbols procedure)
  (cond ((string=? prefix "app")
         '(("append" "apply") "app"))
        ((string=? prefix "appe")
         '(("append") "append"))
        (else
         (list (list (string-append prefix "1")
                     (string-append prefix "2")
                     (string-append prefix "3"))
               prefix))))

(define (all-symbols)

  (define (symbol-next x)
    (##vector-ref x 2))

  (define (f x)
    (if (symbol? x)
        (cons x
              (f (symbol-next x)))
        '()))

  (apply append (map f (cdr (vector->list (##symbol-table))))))


(define (swank:simple-completions prefix package)
  (define (f sym)
    (let ((name (symbol->string sym))
          (prefix-len (string-length prefix)))
      (if (and (<= prefix-len (string-length name))
               (string=? (substring name 0 prefix-len) prefix))
          (list name)
          '())))

  (let ((strings (apply append (map f (all-symbols)))))
    (list (sort strings string<?)
	  (longest-common-prefix strings))))

(define (longest-common-prefix strings)
  (define (common-prefix s1 s2)
    (let loop ([i 0])
      (if (and (< i (string-length s1))
               (< i (string-length s2))
               (char=? (string-ref s1 i)
                       (string-ref s2 i)))
          (loop (+ i 1))
          (substring s1 0 i))))

  (define (f prefix strings)
    (if (null? strings)
        prefix
        (f (common-prefix prefix (car strings))
           (cdr strings))))

  (if (null? strings)
      ""
      (f (car strings) (cdr strings))))

(define (swank:frame-locals-and-catch-tags frame)

  (define (get-continuation-environment cont)

    (define (get-var-val var val-or-box cte)
      ;; Modelled after _repl.scm's ##display-var-val
      (cond ((##var-i? var)
             (get-var-val-aux (##var-i-name var)
                              val-or-box
                              cte))
            ((##var-c-boxed? var)
             (get-var-val-aux (##var-c-name var)
                              (##unbox val-or-box)
                              cte))
            (else
             (get-var-val-aux (##var-c-name var)
                              val-or-box
                              cte))))

    (define (get-var-val-aux var val cte)
      ;; Modelled after _repl.scm's ##display-var-val-aux
      (list
       ':name (##object->string var)
       ':value (##object->string
                (if (##cte-top? cte)
                    (##inverse-eval-in-env val
                                           cte)
                    (##inverse-eval-in-env val
                                           (##cte-parent-cte cte))))
       ':id 0))

    (define (get-vars lst cte)
      ;; Modelled after _repl.scm's ##display-vars
      (let loop ((lst lst) (result '()))
        (if (##pair? lst)
            (let* ((loc (##car lst))
                   (var (##car loc))
                   (val (##cdr loc)))
              (loop (##cdr lst)
                    (cons result (get-var-val var val cte))))
            result)))

    (define (get-parameters lst cte)
      ;; Modelled after _repl.scm's ##display-parameters
      (let loop ((lst lst) (result '()))
        (if (##pair? lst)
            (let* ((param-val (##car lst))
                   (param (##car param-val))
                   (val (##cdr param-val)))
              (if (##not (##hidden-parameter? param))
                  (let ((x (##inverse-eval-in-env param cte)))
                    (loop (##cdr lst)
                          (cons result (get-var-val-aux (##list x) val cte))))
                  (loop (##cdr lst) result)))
            result)))

    (define (get-rte cte rte)
      ;; Modelled after _repl.scm's ##display-rte
      (let loop1 ((c cte)
                  (r rte)
                  (result '()))
        (cond ((##cte-top? c) result)
              ((##cte-frame? c)
               (let loop2 ((vars (##cte-frame-vars c))
                           (vals (##cdr (##vector->list r)))
                           (result result))
                 (if (##pair? vars)
                     (let ((var (##car vars)))
                       (loop2 (##cdr vars)
                              (##cdr vals)
                              (if (##not (##hidden-local-var? var))
                                  (let ((val-or-box (##car vals)))
                                    (cons (get-var-val var val-or-box c) result))
                                  result)))
                     (loop1 (##cte-parent-cte c)
                            (macro-rte-up r)
                            result))))
              (else
               (loop1 (##cte-parent-cte c)
                      r
                      result)))))

    ;; Modelled after _repl.scm's ##display-environment
    (if (##interp-continuation? cont)
        (let (($code (##interp-continuation-code cont))
              (rte (##interp-continuation-rte cont)))
          (get-rte (macro-code-cte $code) rte))
        (get-vars (##continuation-locals cont)
                  ##interaction-cte)))

  (list
   (get-continuation-environment (swank-current-continuation frame))
   '()))

(define (swank:frame-source-location frame)
  ;; Modelled after _repl.scm's display-locat
  (let ((locat (##continuation-locat (swank-current-continuation frame))))
    (if locat
        (let* ((container (##locat-container locat))
               (path (##container->path container))
               (filepos (##position->filepos (##locat-position locat)))
               (line (##fixnum.+ (##filepos-line filepos) 1))
               (col (##fixnum.+ (##filepos-col filepos) 1)))
          (if path
              (list ':location
                    (list ':file path)
                    (list ':line line col)
                    'nil) ; hints
              'nil)) ; path not available
        'nil))) ; location not available

;;;============================================================================

;;;; SLDB

(include "~~lib/_gambit#.scm")

(define (exception-message exc)
  (let ((msg (call-with-output-string
              ""
              (lambda (p)
                (##display-exception exc p)))))
    (list->string
     (reverse
      (cdr
       (reverse (string->list msg)))))))

(define (continuation-interesting-frames cont)
  (unfold (lambda (cont) (not cont))
          values
          (lambda (cont) (##continuation-next-frame cont #f))
          cont))

(define (continuation-code cont)
  (if (##interp-continuation? cont)
      (let* (($code (##interp-continuation-code cont))
             (cprc (macro-code-cprc $code)))
        (if (##eq? cprc ##interp-procedure-wrapper)
            #f
            (##decomp $code)))
      (let* ((ret (##continuation-ret cont))
             (call (##decompile ret)))
        (if (##eq? call ret)
            #f
            call))))

(define-type swank-abort)

(define-type exception-result
  exc
  cont)

(define (debug-exception-result exc-result)
  (swank:invoke-debugger (exception-result-exc exc-result)
                         (exception-result-cont exc-result)))

;;; Evaluate expressions and invoke sldb when an exception occurs.
;;; Only do this for exceptions that occur from the eval'ed
;;; expression, not any exceptions thrown by swank-gambit code.
(define (do-with-sldb-handler thunk)
  ;; Capture the top-level continuation so that we can break out of
  ;; the debugger without subsequent errors being raised.
  (##continuation-capture
   (lambda (toplevel-cont)
     (current-toplevel-cont toplevel-cont)
     ;; Capture the outer continuation so that we can exit from
     ;; the context of where the exception happens.
     (call/cc
      (lambda (outer-cont)
        (with-exception-handler
         (lambda (exc)
           (##continuation-capture
            (lambda (cont)
              (outer-cont (make-exception-result exc cont)))))
         thunk))))))

(define (swank:invoke-debugger exc cont)
  (let ((thread-id (object->serial-number (current-thread)))
        (backtrace (continuation-interesting-frames cont)))
    (current-backtrace backtrace)
    (current-restarts '(ABORT))
    (current-debug-thread (current-thread))    (swank-write
     `(:debug ,thread-id
              1
              (,(exception-message exc)
               ,(string-append "  [Object: "
                               (object->string exc)
                               "]")
               nil)
              (("ABORT" "Return to SLIME's top level."))
              ,(let ((backtrace (if (>= (length backtrace) 10)
                                    (take backtrace 10)
                                    backtrace)))
                 (swank:make-frames backtrace))
              ()))
    (swank-write
     `(:debug-activate ,thread-id 1 true))))

(define (swank:invoke-nth-restart-for-emacs level restart)
  ;; FIXME: this currently assumes that ABORT is sent for the current
  ;; thread and CONTINUE is sent for a suspended thread

  (swank-write
   `(:debug-return ,(object->serial-number (current-debug-thread)) 1 nil))
  ;; ##continuation-return means emacs-rex won't get a chance to send
  ;; a reply for this seqnum, so send it now.  Sending :abort because
  ;; otherwise SLIME prints spam in the echo area.
  (let ((restart (list-ref (current-restarts) restart)))
    (cond
     ((eq? restart 'ABORT)
      (swank-write
       (list ':return '(:abort) (current-seqnum)))
      (##continuation-graft (current-toplevel-cont)
                            (lambda () (raise (make-swank-abort)))))
     ((eq? restart 'CONTINUE)
      (mutex-specific-set! (current-wakeup-mutex) '(CONTINUE))
      (mutex-unlock! (current-wakeup-mutex))
      'nil))))

(define (swank:make-frames frames)
  (let loop ((acc '())
             (i 0)
             (tail frames))
    (if (null? tail)
        (reverse acc)
        (loop (cons (swank:make-frame i (car tail))
                    acc)
              (+ i 1)
              (cdr tail)))))

(define (swank:make-frame i cont)
  `(,i
    ,(string-append
      (call-with-output-string
       ""
       (lambda (p)
         (##display-locat (##continuation-locat cont)
                          #f
                          p)))
      " "
      (object->string
       (or (##procedure-friendly-name (##continuation-creator cont))
           "(global)"))
      " "
      (object->string (continuation-code cont) 40))
    (:restartable nil)))

(define (swank:sldb-return-from-frame frame expr-str)
  (swank-write
   `(:debug-return ,(object->serial-number (current-thread)) 1 nil))
  ;; ##continuation-return means emacs-rex won't get a chance to send
  ;; a reply for this seqnum, so send it now.  Sending :abort because
  ;; otherwise SLIME prints spam in the echo area.
  (swank-write
   (list ':return '(:abort) (current-seqnum)))
  (let ((result
         (swank:do-with-result
          (lambda () (*preprocessing-eval*
                      (with-input-from-string expr-str read))))))
    (##continuation-return
     (swank-current-continuation frame)
     result)))

(define (swank:sldb-disassemble frame)
  ;; Using ##cmd-i for now, but really should be custom code that
  ;; highlights the instruction pointer
  (call-with-output-string
   "" (lambda (p) (##cmd-i (swank-current-continuation frame) p))))

(define current-backtrace (make-parameter #f))
(define current-toplevel-cont (make-parameter #f))
(define current-seqnum (make-parameter #f))
(define current-restarts (make-parameter #f))
(define current-debug-thread (make-parameter #f))
(define current-wakeup-mutex (make-parameter #f))

(define (swank:backtrace start stop)
  (let ((backtrace (current-backtrace)))
    (swank:make-frames
     (if backtrace
         (let ((left (drop backtrace start)))
           (if (null? left)
               left
               (drop-right
                left
                (- (length backtrace) 1 stop))))
         '()))))

;;;============================================================================

;;;; Inspector

;; Notes:
;;
;; - the plumbing is adapted from the MIT Scheme swank server that can
;;   be found in SLIME's contrib/ directory.  Permission to adapt the
;;   code kindly granted by Helmut Eller.
;;
;; - it would be better if this would use SRFI-40 for streaming like
;;   swank-mit-scheme.scm does, however this would introduce a
;;   dependency on Snow.  It would be nice to rewrite this code so
;;   that it uses streams when available, lists otherwise.

(define-structure inspector-state
  object parts next previous content verbose)

(define istate #f)

(define (reset-inspector)
  (set! istate #f))

(define (swank:init-inspector string)
  (reset-inspector)
  (inspect-object (eval (with-input-from-string string read))))

(define (>line o)
  (with-output-to-string '() (lambda () (pretty-print o))))

(define (iline label value) `(line ,label ,value))

(define (inspect-object o)
  (let ((previous istate)
	(content (append (list
                          (iline "Serial Number"
                                 (object->serial-number o)))
                         (inspect o)))
	(parts (make-table)))
    (set! istate (make-inspector-state
                  o parts #f previous content
                  (and previous (inspector-state-verbose previous))))
    (if previous (inspector-state-next-set! previous istate))
    (istate>elisp istate)))

(define (inspect o)
  (cond
   ;; keyword?
   ;; uninterned-symbol?
   ;; uninterned-keyword?
   ;; continuation?
   ;; thread-group?
   ;; foreign?
   ;; socket-info?
   ;; tty?
   ;; fixnum?
   ;; flonum?
   ;; random-source?
   ;; exception?
   ;; will?
   ;; thread?
   ;; mutex?
   ;; condition-variable?
   ;; string?
   ;; symbol?
   ;; time?
   ;; file-info?
   ;; group-info?
   ;; user-info?
   ;; host-info?
   ;; address-info?
   ;; service-info?
   ;; protocol-info?
   ;; network-info?
   ;; input-port?
   ;; output-port?
   ;; port?
   ;; readtable?
   ;; procedure?
   ;; probably-scode?
   ((s8vector? o) (inspect-s8vector o))
   ((u8vector? o) (inspect-u8vector o))
   ((s16vector? o) (inspect-s16vector o))
   ((u16vector? o) (inspect-u16vector o))
   ((s32vector? o) (inspect-s32vector o))
   ((u32vector? o) (inspect-u32vector o))
   ((s64vector? o) (inspect-s64vector o))
   ((u64vector? o) (inspect-u64vector o))
   ((f32vector? o) (inspect-f32vector o))
   ((f64vector? o) (inspect-f64vector o))
   ((table? o) (inspect-table o))
   ((box? o) (inspect-box o))
   (else (list (iline "Contents" o)))))

(define (inspect-s8vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (s8vector-length o))
         (loop (+ i 1) (cons (iline i (s8vector-ref o i)) result))
         result))))

(define (inspect-u8vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (u8vector-length o))
         (loop (+ i 1) (cons (iline i (u8vector-ref o i)) result))
         result))))

(define (inspect-s16vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (s16vector-length o))
         (loop (+ i 1) (cons (iline i (s16vector-ref o i)) result))
         result))))

(define (inspect-u16vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (u16vector-length o))
         (loop (+ i 1) (cons (iline i (u16vector-ref o i)) result))
         result))))

(define (inspect-s32vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (s32vector-length o))
         (loop (+ i 1) (cons (iline i (s32vector-ref o i)) result))
         result))))

(define (inspect-u32vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (u32vector-length o))
         (loop (+ i 1) (cons (iline i (u32vector-ref o i)) result))
         result))))

(define (inspect-s64vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (s64vector-length o))
         (loop (+ i 1) (cons (iline i (s64vector-ref o i)) result))
         result))))

(define (inspect-u64vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (u64vector-length o))
         (loop (+ i 1) (cons (iline i (u64vector-ref o i)) result))
         result))))

(define (inspect-f32vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (f32vector-length o))
         (loop (+ i 1) (cons (iline i (f32vector-ref o i)) result))
         result))))

(define (inspect-f64vector o)
  (reverse
   (let loop ((i 0) (result '()))
     (if (< i (f64vector-length o))
         (loop (+ i 1) (cons (iline i (f64vector-ref o i)) result))
         result))))

(define (inspect-table o)
  (let ((result '()))
    (table-for-each
     (lambda (key value)
       (set! result
             (cons (iline "Key" key)
                   (cons (iline "Value" value)
                         result))))
     o)
    (reverse result)))

(define (inspect-box o)
  (list (iline "Contents" (unbox o))))

(define (swank:inspect-nth-part index)
  (inspect-object (swank:inspector-nth-part index)))

(define (istate>elisp istate)
  (list ':title (>line (inspector-state-object istate))
	':id (assign-index (inspector-state-object istate)
                           (inspector-state-parts istate))
	':content (prepare-range (inspector-state-parts istate)
				 (inspector-state-content istate)
				 0 500)))

(define (assign-index o parts)
  (let ((i (table-length parts)))
    (table-set! parts i o)
    i))

(define (substream s from to)
  (let loop ((i 0) (l '()) (s s))
    (cond ((or (= i to) (null? s)) (reverse l))
	  ((< i from) (loop (1+ i) l (cdr s)))
	  (else (loop (+ 1 i) (cons (car s) l) (cdr s))))))

(define (prepare-range parts content from to)
  (let* ((cs (substream content from to))
	 (ps (prepare-parts cs parts)))
    (list ps
	  (if (< (length cs) (- to from))
	      (+ from (length cs))
	      (+ to 1000))
	  from to)))

(define (prepare-parts ps parts)
  (define (line label value)
    `(,(append-strings (list (object->string label) ": "))
      (:value ,(>line value) ,(assign-index value parts))
      "\n"))
  (apply append (map (lambda (p)
                       (cond ((string? p) (list p))
                             ((symbol? p) (list (symbol->string p)))
                             (#t
                              (case (car p)
                                ((line) (apply line (cdr p)))
                                (else (error "Invalid part:" p))))))
                     ps)))

(define (swank:quit-inspector)
  (reset-inspector)
  'nil)

(define (swank:inspector-pop)
  (cond ((inspector-state-previous istate)
	 (set! istate (inspector-state-previous istate))
	 (istate>elisp istate))
	(else 'nil)))

(define (swank:inspector-next)
  (cond ((inspector-state-next istate)
	 (set! istate (inspector-state-next istate))
	 (istate>elisp istate))
	(else 'nil)))

(define (swank:inspector-range from to)
  (prepare-range (inspector-state-parts istate)
		 (inspector-state-content istate)
		 from to))

(define (swank:inspector-toggle-verbose)
  (inspector-state-verbose-set! istate (not (inspector-state-verbose istate)))
  (istate>elisp istate))

(define (swank:describe-inspectee)
  "dummy")

(define (swank:inspector-reinspect)
  (istate>elisp istate))

(define (swank:pprint-inspector-part index)
  (with-output-to-string
    '()
    (lambda ()
      (pretty-print (swank:inspector-nth-part index)))))

(define (swank:inspector-nth-part index)
  (table-ref (inspector-state-parts istate) index 'no-such-part))

(define (swank:find-source-location-for-emacs spec)
  (find-source-location (value-spec-ref spec)))

(define (find-source-location obj)
  '(:error "Not implemented: find-source-location"))

(define (value-spec-ref spec)
  (cond
   ((eq? ':string (car spec))
    (eval (read-from-string (nth spec 1))))
   ((eq? ':inspector (car spec))
    (inspector-nth-part (nth spec 1)))
   ((eq? ':sldb (car spec))
    (frame-var-value (nth spec 1) (nth spec 2)))))

(define (frame-var-value frame var)
  'nil)

;;;============================================================================

(define-macro (swank-define-op proc-name)
  `(table-set! swank-op-table ',proc-name ,proc-name))

(define swank-op-table (make-table))

(swank-define-op swank:connection-info)
(swank-define-op swank:swank-require)
(swank-define-op swank:create-repl)
(swank-define-op swank:arglist-for-echo-area)
(swank-define-op swank:listener-eval)
(swank-define-op swank:interactive-eval)
(swank-define-op swank:interactive-eval-region)
(swank-define-op swank:pprint-eval)
(swank-define-op swank:invoke-nth-restart-for-emacs) 
(swank-define-op swank:load-file)
(swank-define-op swank:start-swank-server-in-thread)
(swank-define-op swank:list-threads)
(swank-define-op swank:debug-nth-thread)
(swank-define-op swank:kill-nth-thread)
(swank-define-op swank:quit-lisp)
(swank-define-op swank:completions)
(swank-define-op swank:backtrace)
(swank-define-op swank:simple-completions)
(swank-define-op swank:pprint-eval-string-in-frame)
(swank-define-op swank:frame-locals-and-catch-tags)
(swank-define-op swank:frame-source-location)
(swank-define-op swank:sldb-return-from-frame)
(swank-define-op swank:sldb-disassemble)
(swank-define-op swank:quit-thread-browser)
(swank-define-op swank:init-inspector)
(swank-define-op swank:quit-inspector)
(swank-define-op swank:inspect-nth-part)
(swank-define-op swank:inspector-pop)
(swank-define-op swank:inspector-next)
(swank-define-op swank:inspect-in-frame)
(swank-define-op swank:inspector-toggle-verbose)
(swank-define-op swank:describe-inspectee)
(swank-define-op swank:inspector-reinspect)
(swank-define-op swank:pprint-inspector-part)
(swank-define-op swank:find-source-location-for-emacs)

;; Not yet implemented
;;
;; (this list is incomplete)

;(swank-define-op swank:apropos-list-for-emacs)
;(swank-define-op swank:buffer-first-change)
;(swank-define-op swank:compile-file-for-emacs)
;(swank-define-op swank:compile-string-for-emacs)
;(swank-define-op swank:debugger-info-for-emacs)
;(swank-define-op swank:disassemble-symbol)
;(swank-define-op swank:filename-to-modulename)
;(swank-define-op swank:find-definitions-for-emacs)
;(swank-define-op swank:inspect-frame-var)
;(swank-define-op swank:list-all-package-names)
;(swank-define-op swank:operator-arglist)
;(swank-define-op swank:set-package)
;(swank-define-op swank:sldb-abort)
;(swank-define-op swank:sldb-continue)
;(swank-define-op swank:swank-macroexpand)
;(swank-define-op swank:swank-macroexpand-1)
;(swank-define-op swank:swank-macroexpand-all)
;(swank-define-op swank:swank-require)
;(swank-define-op swank:throw-to-toplevel)

;;;============================================================================

(swank-server-register!)

;;(##repl-debug-main)

;; Run until interrupt from user
(thread-sleep! +inf.0)

;;;============================================================================

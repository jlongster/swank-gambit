;;;============================================================================

;;; File: "swank-gambit.scm", Time-stamp: <2010-02-15 19:08:52 james>

;;; Copyright (c) 2009-2010 by Marc Feeley and James Long, All Rights Reserved.

;;; This software is released under a dual LGPL and Apache 2 license.
;;; These licenses are included with this software in LGPL.txt and
;;; LICENSE-2.0.txt.

;;;============================================================================

(include "~~lib/_gambit#.scm")
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

(define (swank-server-register! #!key (server-address swank-server-address))
  (tcp-service-register! server-address
                         swank-server-handle
                         (thread-thread-group (current-thread))))

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
                    (if (pair? msg)
                        (let ((args (cdr msg)))
                          (let ((op (table-ref swank-op-table (car msg) #f)))
                            (if op
                                (let ((result (apply op args)))
                                  (cond
                                   ((eq? result 'abort)
                                    (swank-write
                                     (list ':return
                                           '(:abort)
                                           seqnum)))
                                   (result
                                    (swank-write
                                     (list ':return
                                           (list ':ok result)
                                           seqnum)))))
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
    :modules ()
    :package (:name "#package-name#" :prompt "")
    :version ,swank-wire-protocol-version))

(define (swank:swank-require modules)

  '(
    "SWANK-PRESENTATIONS"
    "SWANK-ARGLISTS"
    "SWANK-FANCY-INSPECTOR"
    "SWANK-FUZZY"
    "SWANK-C-P-C"
    "SWANK-PACKAGE-FU"
    "SB-CLTL2"
    "SB-POSIX"
    "SB-INTROSPECT"
    "SB-BSD-SOCKETS"
    "SB-GROVEL"
    "ASDF"
    )

  '())

(define (swank:create-repl arg)
  ;; fake it
  `("???" ""))

(define (swank:arglist-for-echo-area . rest)
  ;; fake it
  'nil)

(define (swank:eval expr)
  (let ((result #f))
    (let ((output
           (with-output-to-string ""
             (lambda ()
               (set! result (eval-with-sldb-handler expr))))))
      (if (not (equal? output ""))
          (swank-write `(:write-string ,output :repl-result)))
      (if (exception-result? result)
          (debug-exception-result result))
      result)))

(define (swank:listener-eval expr-str)
  (let* ((expr (with-input-from-string expr-str read))
         (result (swank:eval expr)))
    (cond
     ((exception-result? result) 'abort)
     ((eq? result '#!void) 'nil)
     (else
      (let ((result-str (object->string result)))
        (swank-write `(:write-string ,result-str :repl-result))
        (swank-write `(:write-string "\n" :repl-result))
        'nil)))))

(define (swank:interactive-eval expr-str)
  (let ((expr (with-input-from-string expr-str read)))
    (swank-interactive-evaluation expr write)))

(define (swank:interactive-eval-region expr-str)
  (let ((expr (cons 'begin (with-input-from-string expr-str read-all))))
    (swank-interactive-evaluation expr write)))

(define (swank:pprint-eval expr-str)
  (let ((expr (with-input-from-string expr-str read)))
    (swank-interactive-evaluation expr pretty-print)))

(define (swank-interactive-evaluation expr wr)
  (let ((result (swank:eval expr)))
    (cond
     ((exception-result? result) 'abort)
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
         (< n (vector-length threads))
         (vector-ref threads n))))

(define (swank:list-threads)
  (let ((threads
         (thread-group->thread-vector
          (thread-thread-group (current-thread)))))
    (table-set! swank-threads (current-input-port) threads)
    (cons '("a" "b" "c" "d") ;; don't know why this is needed...
          (map (lambda (t)
                 (list "id"
                       (object->string t)
                       "status"
                       "description"))
               (vector->list threads)))))

(define (swank:debug-nth-thread n)
  (let ((t (swank-get-nth-thread n)))
    (if t (thread-interrupt! t)) ;; not quite sufficient...
    'nil))

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

;; Is this is the right way to do this?
(define (get-thread-id thread)
  (let* ((threads
          (thread-group->thread-vector
           (thread-thread-group (current-thread))))
         (len (vector-length threads)))
    (let loop ((i 0))
      (if (>= i len)
          #f
          (if (eq? (vector-ref threads i) thread)
              i
              (loop (+ i 1)))))))

(define-type exception-result
  exc
  cont)

(define (debug-exception-result exc-result)
  (swank:invoke-debugger (exception-result-exc exc-result)
                         (exception-result-cont exc-result)))

;;; Evaluate expressions and invoke sldb when an exception occurs.
;;; Only do this for exceptions that occur from the eval'ed
;;; expression, not any exceptions thrown by swank-gambit code.
(define (eval-with-sldb-handler expr)
  (let ((result
         
         ;; Capture the outer continuation so that we can exit from
         ;; the context of where the exception happens.
         (call/cc
          (lambda (outer-cont)
            (with-exception-handler
             (lambda (exc)
               (##continuation-capture
                (lambda (cont)
                  (outer-cont (make-exception-result exc cont)))))
             (lambda ()
               (eval expr)))))))
    result))

(define (swank:invoke-debugger exc cont)
  (let ((thread-id (get-thread-id (current-thread)))
        (backtrace (continuation-interesting-frames cont)))
    (current-backtrace backtrace)
    (swank-write
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
  (swank-write
   `(:debug-return ,(get-thread-id (current-thread)) 1 nil))
  'exited)

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

(define current-backtrace (make-parameter #f))

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

;(swank-define-op swank:connection-info)
;(swank-define-op swank:interactive-eval)
;(swank-define-op swank:interactive-eval-region)
;(swank-define-op swank:listener-eval)
;(swank-define-op swank:pprint-eval)
;(swank-define-op swank:compile-file-for-emacs)
;(swank-define-op swank:compile-string-for-emacs)
;(swank-define-op swank:load-file)
;(swank-define-op swank:simple-completions)
;(swank-define-op swank:quit-lisp)
;(swank-define-op swank:operator-arglist)
;(swank-define-op swank:buffer-first-change)
;(swank-define-op swank:swank-require)
;(swank-define-op swank:find-definitions-for-emacs)
;(swank-define-op swank:disassemble-symbol)
;(swank-define-op swank:swank-macroexpand-1)
;(swank-define-op swank:swank-macroexpand)
;(swank-define-op swank:swank-macroexpand-all)
;
;swank:connection-info
;swank:quit-lisp
;swank:listener-eval
;swank:interactive-eval
;swank:pprint-eval
;swank:interactive-eval-region
;swank:set-package
;swank:compile-string-for-emacs
;swank:compiler-notes-for-emacs
;swank:compile-file-for-emacs
;swank:load-file
;swank:disassemble-symbol
;swank:swank-macroexpand-all
;swank:swank-macroexpand-1
;swank:swank-macroexpand
;swank:operator-arglist
;swank:buffer-first-change
;swank:filename-to-modulename
;swank:find-definitions-for-emacs
;swank:throw-to-toplevel
;swank:sldb-abort
;swank:sldb-continue
;swank:invoke-nth-restart-for-emacs
;swank:debugger-info-for-emacs
;swank:backtrace
;swank:frame-locals-and-catch-tags
;swank:inspect-frame-var
;swank:simple-completions
;swank:apropos-list-for-emacs
;swank:list-all-package-names
;swank:init-inspector
;swank:inspect-nth-part
;swank:quit-inspector
;swank:inspector-pop
;swank:inspector-next
;swank:inspector-range

;;;============================================================================

(swank-server-register!)

;;(##repl-debug-main)

;; Run until interrupt from user
(thread-sleep! +inf.0)

;;;============================================================================

;;;============================================================================

;;; File: "swank-presentations.scm"

;;; Copyright (c) 2010 by Julian Scheid, All Rights Reserved.

;;; This software is released under a dual LGPL and Apache 2 license.
;;; These licenses are included with this software in LGPL.txt and
;;; LICENSE-2.0.txt.

;;;============================================================================

;; This implementation of presentations for Gambit Scheme is
;; experimental, incomplete, and currently requires patching
;; slime-presentation.el with slime-presentations-gambit.patch - which
;; in turn is not a proper patch but rather a temporary workaround.

;; The patch is required because slime-presentations.el currently
;; assumes that the backend is running Common Lisp.

;; Missing features:
;; - menu-choices
;; - inspect-presentation

;;;============================================================================

;; For compatibility only, ignored because implicit serial numbers are
;; used instead of a hash table.")
(define *record-repl-results* #t)

(define (present-repl-results values)
  (define (f value)
    (let ((result-str (object->string value))
          (id (object->serial-number value)))
      (swank-write `(:presentation-start ,id :repl-result))
      (swank-write `(:write-string ,result-str :repl-result))
      (swank-write `(:presentation-end ,id :repl-result))
      (swank-write `(:write-string "\n" :repl-result))))

  (apply f values))

(define (preprocessing-eval expr)
  ;; Scheme lacks the #. operator which is used in CL presentations to
  ;; allow substitution within quote.  As a workaround, preprocess the
  ;; S-Exp before it is being evaluated, replacing
  ;; (swank:get-repl-result SERIAL-NUMBER) by the corresponding
  ;; object.
  (define (f elt)
    (cond
     ((and (list? elt)
           (eq? 'swank:get-repl-result (car elt)))
      (apply swank:get-repl-result (cdr elt)))
     ((list? elt)
      (map f elt))
     (else
      elt)))

  (eval (f expr)))

(define (swank:get-repl-result id)
  ;; Don't bother catching unbound-serial-number-exception, this being
  ;; thrown is as informative as it gets.
  (serial-number->object id))

(define (swank:inspect-presentation id reset-p)
  (if reset-p
      (reset-inspector))
  (with-exception-catcher
   (lambda (exc)
     (if (unbound-serial-number-exception? exc)
         (inspect-object '()) ;; FIXME
         (raise exc)))
   (lambda ()
     (let ((what (serial-number->object id)))
       (inspect-object what)))))

(table-set! swank-op-table 'swank:inspect-presentation swank:inspect-presentation)

(set! *send-repl-results-function* present-repl-results)
(set! *preprocessing-eval* preprocessing-eval)

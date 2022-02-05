(in-package #:schemeish.backend)

(install-syntax!)

;;; SYMBOLS

(export
 (define (make-keyword symbol)
   (intern (symbol-name symbol) :keyword)))
(export
 (define symbol->string #'symbol-name))
(export
 (define symbol? #'symbolp))

(export
 (define (parameter? symbol (environment))
   "Returns true if symbol is a parameter i.e. dynamically scoped."
   (eq? :special (trivial-cltl2:variable-information symbol environment))

   #+slow-check-for-parameter
   (and (not (constantp symbol))
	;; If we have an error, its because the parameter has a type
	;; associated with it. Therefore we know its a parameter.
	(eval `(not (ignore-errors
		     (let (,symbol)
		       (let ((f (lambda () ,symbol)))
			 (let ((,symbol t))
			   (not (eq? [f] t)))))))))))


(export
 (define (symbolicate . things)
   (intern (apply #'concatenate 'string (map #'string things)))))


(uninstall-syntax!)

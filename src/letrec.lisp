(in-package #:schemeish.backend)

(install-syntax!)

;;; LET-REC

(export-definition
  (defmacro letrec (bindings &body body)
    "Establish lexical bindings. All lexical variables are in scope for the binding values.
Values are bound sequentially. Bindings are established for body.
Body is (declarations... forms...)"
    (let ((declare? (lambda (form) (and (pair? form) (eq? 'cl:declare (first form))))))
      (let ((declarations (takef body declare?))
	    (forms (dropf body declare?)))
	`(let ,(map #'first bindings)
	   ,@declarations
	   ,@(map (lambda (binding)
		    `(setq ,(first binding) ,(second binding)))
		  bindings)
	   ,@forms)))))

(uninstall-syntax!)

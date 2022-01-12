(in-package #:schemeish.lexically)

(install-syntax!)


(for-macros
  (define (lexical-name->parameter-name symbol)
    (intern (string-append "*" (symbol->string symbol) "*"))))

(defmacro lexically (&body lexical-body)
  "Evaluate lexical-body. Expanding defines as if inside of a define or lambda.
See documentation of DEFINE.
Use in conjunction with EXPOSE to expose lexical definitions to the package."
  `(let ()
     ,@(expand-defines-in-lexical-body lexical-body)))

(defmacro expose ((&rest fn-specs) &rest var-specs)
  "Define var-specs as parameters in the global scope.
Define fn-specs as functions in the global scope.

Fn-specs are one of the following forms:
  fn-name: (fdefinition 'fn-name) is set in the global environment to the value of fn-name
  (global-fn-name fn-name): (fdefinition 'global-fn-name) is set in the the global environment to the value of fn-name

Var-specs are of the following forms:
  VAR-NAME: *VAR-NAME* is defined/set as a special variable in the global environment with its initial value as VAR-NAME
  (global-special-name var-name): GLOBAL-SPECIAL-NAME is defined/set as a special variable in the global environment with its initial value as VAR-NAME
     It is STRONGLY recommended that you use *'s to wrap global-special-name.

The return value is a list of '(PARAMETER-NAMES... GLOBAL-FN-NAMES ...)
Used in conjunction with LEXICALLY you can do something like:
  (export (lexically () ... (expose ...)))"
  (let ((var-names (map (lambda (spec)
			  (cond ((pair? spec) (second spec))
				(t spec)))
			var-specs))
	(parameter-names (map (lambda (spec)
				(cond ((pair? spec) (first spec))
				      (t (lexical-name->parameter-name spec))))
			      var-specs))
	(fn-names (map (lambda (spec)
			 (cond ((pair? spec) (second spec))
			       (t spec)))
		       fn-specs))
	(global-fn-names (map (lambda (spec)
				(cond ((pair? spec) (first spec))
				      (t spec)))
			      fn-specs)))
    `(progn
       ,@(map (lambda (parameter-name var-name) `(defparameter ,parameter-name ,var-name))
	      parameter-names var-names)
       ,@(map (lambda (fn-name global-fn-name) `(setf (fdefinition ',global-fn-name) ,fn-name))
	      fn-names global-fn-names)
       ',(append parameter-names global-fn-names))))

(progn
  (assert (equal? (lexically
		    (define test-x 1)
		    (define (test-y) "test-y" (+ test-x 2))
		    (define (lexical-test-z) "tests z" (+ [test-y] test-x))
		    (define lexical-test-w 1)
		    (expose ((lexical-test-y test-y)
			     lexical-test-z)
			    (*lexical-test-x* test-x)
			    lexical-test-w))
		  '(*LEXICAL-TEST-X* *lexical-test-w* LEXICAL-TEST-Y LEXICAL-TEST-Z))))

(uninstall-syntax!)

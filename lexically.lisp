(in-package #:schemeish.lexically)


(for-macros (install-syntax!))

(defmacro lexically ((&key (parameter-wrap-sym '/) (special-fn-append-sym 'f)) &body body)
  "Evaluate body in a lexical scope, expanding defines as if inside of a define or lambda.
Establishes lexical-bindings for all normal functions and parameters in the current package.
Effectively all functions can be called with [], while all special-forms and macros would use (). 
Use in conjunction with EXPOSE.

Lexical bindings for the current package are of the form (fn #'fn) if fn is not any of:
  - a special form
  - a macro-function
  - a special variable (such as /)
If fn is a special variable it is appended with special-fn-append. 
E.g. since / is a special variable, the binding is (/f #'/). 

or (/parameter/ *parameter*) if parameter is all of
  - a special variable?
  - named the special variable naming convention *NAME*
Parameter-wrap-string is used to determine the string that wraps the lexical parameter name.

Note: Since the parameter name is declared special, the lexical name must be different.
There are cases where there are functions named SAME-NAME and parameters named *SAME-NAME*,
so we can't simply omit the *'s."
  (let ((lexical-bindings (lexical-bindings (symbol->string parameter-wrap-sym) (symbol->string special-fn-append-sym))))
    `(let ,lexical-bindings
       (declare (ignorable ,@(map #'first lexical-bindings)))
       ,@(expand-function-body body))))

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
  (assert (equal? (lexically (:parameter-wrap-sym / :special-fn-append-sym f)
		    (define test-x 1 "test-x")
		    (define (test-y) "test-y" [+f test-x 2])
		    (define (lexical-test-z) "tests z" [+f [test-y] test-x])
		    (define lexical-test-w 1)
		    (print [test-y])
		    (expose ((lexical-test-y test-y)
			     lexical-test-z)
			    (*lexical-test-x* test-x)
			    lexical-test-w))
		  '(*LEXICAL-TEST-X* *lexical-test-w* LEXICAL-TEST-Y LEXICAL-TEST-Z)))

  (assert (string= (documentation 'lexical-test-y 'function)
		   "test-y"))

  (assert (= 1 *lexical-test-x*))
  (assert (= 1 *lexical-test-w*))
  (assert (= 3 (lexical-test-y)))
  (assert (= 4 (lexical-test-z))))

(for-macros (uninstall-syntax!))

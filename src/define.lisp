(in-package #:schemeish.internals)

(install-syntax!)

(export
 (defmacro define (name-field &body body)
   "Essentially expands to (lexically (define name-field body...) (expose-functions ,name)).
Returns the name of the defined function.

For more information about lexically, see LEXICALLY.
For more information about expose, see EXPOSE.
For more information about the lisp-2 style lexical-body definition DEFINE, see TRANSFORM-LEXICAL-BODY2-DEFINE-SYMBOL-OR-PAIR."
   (let ((name (definition-name-field->name name-field)))
     `(for-macros
	(fmakunbound ',name)
	(lexically (define ,name-field ,@body) (expose-functions ,name))
	',name))))

(export
 (defmacro undefine (name-field &body ignored-body)
   "Expands to (fmakunbound name). Mimics the form of DEFINE."
   (declare (ignore ignored-body))
   `(for-macros (fmakunbound ',(definition-name-field->name name-field)))))

#;
(progn
  (define TEST-2+ "Adds 2." (cl:lambda (&rest numbers) (apply #'+ 2 numbers)))
  (documentation #'test-2+ t)
  ;; => "Adds 2."
  (assert (= (test-2+ 1 2 3) 8))

  (define (test)
    #d"Documentation"
    #g(*guard-clauses-enabled?*)
    (define a 1)
    (define-values (b c d) (values 2 3 4))
    (define (e x)
      #d"Returns the number x."
      #g((numberp x))
      x)
    (define (((f y) z) w)
      #g((numberp y)
	 (numberp z)
	 (numberp w))
      (list y z w))
    (list* a b c d (e 5) [[(f 6) 7] 8]))
  (assert (equal (test) '(1 2 3 4 5 6 7 8)))
  (documentation #'test t)
  "Documentation

Parameters: NIL
Definition Form: (TEST)

TEST has the following guard clauses:
(*GUARD-CLAUSES-ENABLED?*)"

  (progn
    (assert (equal (lexically
		     (define test-x 1)
		     (define (test-y) "test-y" (+ test-x 2))
		     (define (lexical-test-z) "tests z" (+ [test-y] test-x))
		     (define lexical-test-w 1)
		     (expose ((lexical-test-y test-y)
			      lexical-test-z)
			     ((*lexical-test-x* test-x)
			      lexical-test-w)))

		   '(*LEXICAL-TEST-X* *lexical-test-w* LEXICAL-TEST-Y LEXICAL-TEST-Z)))

    (assert (= *lexical-test-w* 1))
    (assert (= *lexical-test-x* 1))
    (assert (= (lexical-test-y) 3))
    (assert (= (lexical-test-z) 4))))

(uninstall-syntax!)

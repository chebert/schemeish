(in-package #:schemeish.define)

(install-syntax!)

(for-macros
  (defun flatten (v)
    "Flatten tree v into a list."
    (cond
      ((consp v) (append (flatten (car v))
			 (flatten (cdr v))))
      ((null v) ())
      (t (list v)))))

(assert (equal (flatten '((a) b (c (d) . e) ()))
	       '(a b c d e)))

(for-macros
  (defun define? (form) (and (consp form) (eq (first form) 'schemeish.define:define))))
(assert (define? '(define name var)))

(for-macros
  (defun define-name (form)
    (let ((name-form (second form)))
      (first (flatten name-form)))))

(assert (equal (define-name '(define (((nested-fn) x) y) z))
	       'nested-fn))

(for-macros (defun append* (lists)
	      "Append lists."
	      (apply #'append lists)))
(assert (equal
	 (append* '((1 2 3) (4 5 6)))
	 '(1 2 3 4 5 6)))

(for-macros (defun declaration? (form) (and (consp form) (eq 'declare (first form)))))
(assert (declaration? '(declare (ignore x))))

(for-macros (defun takef (list predicate)
	      "Takes initial elements of list that satisfy pred."
	      (let rec ((list list)
			(result '()))
		(if (or (null list) (not [predicate (first list)]))
		    (nreverse result)
		    (rec
		     (rest list)
		     (cons (first list) result))))))

(assert (equal (takef '(2 4 5 8) 'evenp)
	       '(2 4)))

(for-macros (defun dropf (list predicate)
	      "Drops initial elements of list that don't satisfy pred."
	      (let rec ((list list))
		(if (or (null list) (not [predicate (first list)]))
		    list
		    (rec (rest list))))))


(assert (equal (dropf '(2 4 5 8) 'evenp)
	       '(5 8)))

(for-macros (defun splitf-at (list predicate)
	      "Returns (list (takef list predicate) (dropf list predicate))"
	      (list (takef list predicate) (dropf list predicate))))


(for-macros (defun split-function-body (body)
	      "Split the function body into '(declarations defines body)"
	      (let* ((doc-list (cond
				 ((stringp (first body))
				  (cond ((null (rest body)) ())
					(t (list (first body)))))
				 (t ())))
		     (body (if doc-list (rest body) body)))
		(destructuring-bind (declarations body) (splitf-at body 'declaration?)
		  (unless (null (remove-if-not 'declaration? body))
		    (error "declarations intermixed with definitions or code."))
		  (destructuring-bind (defines body) (splitf-at body 'define?)
		    (unless body
		      (error "Empty body."))
		    (unless (null (remove-if-not 'define? body))
		      (error "definitions intermixed with code."))
		    (list (append doc-list declarations)
			  defines
			  body))))))

;; if the body is just a string, then it's code.
(assert (equal (split-function-body '("result"))
	       '(() () ("result"))))

;; otherwise the string is a docstring
(assert (equal (split-function-body '("doc" result))
	       '(("doc") () (result))))

(assert (equal (split-function-body '("doc" (declare) "result"))
	       '(("doc" (DECLARE)) NIL ("result"))))

(assert (equal (split-function-body '("doc" (declare) (define) result))
	       '(("doc" (DECLARE)) ((DEFINE)) (RESULT))))


(assert (null (ignore-errors (split-function-body '((declare) (define) (declare) result)))))
(assert (null (ignore-errors (split-function-body '((declare) (define) result (define))))))
(assert (null (ignore-errors (split-function-body '("doc" (declare) result (declare))))))
(assert (null (ignore-errors (split-function-body '("doc" (declare) (define))))))

(for-macros (defun define-function? (form)
	      (and (define? form)
		   (consp (second form)))))

(for-macros (defun expand-define-closure-or-function (name-and-arg-list body expand-define-function)
	      (let ((name (first name-and-arg-list))
		    (arg-list (rest name-and-arg-list)))
		(cond
		  ;; e.g. name-and-arglist is (((nested-fn x) y) z)
		  ((consp name)
		   (expand-define-closure-or-function
		    name
		    (list (expand-define-closure arg-list body))
		    expand-define-function))
		  ;; e.g. name-and-arglist is (fn x y z)
		  ((symbolp name) [expand-define-function name arg-list body])
		  (t (error "Bad thing to define: ~s" name))))))

(for-macros (defun expand-define-function-for-labels (name arg-list body)
	      (list* name (arg-list->lambda-list arg-list) body)))

(for-macros (defun named-let? (form)
	      (and (listp form)
		   (>= (length form) 3)
		   (eq (first form) 'let)
		   (symbolp (second form))
		   (consp (third form)))))
(for-macros (defun let? (form)
	      (and (listp form)
		   (>= (length form) 2)
		   (member (first form) '(cl:let let))
		   (consp (second form)))))
(for-macros (defun let*? (form)
	      (and (listp form)
		   (>= (length form) 2)
		   (eq (first form) 'cl:let*)
		   (consp (second form)))))

(for-macros (defun make-named-let (name bindings body)
	      `(let ,name ,bindings ,@body)))
(for-macros (defun named-let-bindings (form)
	      (third form)))
(for-macros (defun named-let-name (form)
	      (second form)))
(for-macros (defun named-let-body (form)
	      (cdddr form)))
(for-macros (defun let-body (form)
	      (cddr form)))
(for-macros (defun let-bindings (form)
	      (second form)))
(for-macros (defun make-let (bindings body)
	      `(let ,bindings ,@body)))
(for-macros (defun make-let* (bindings body)
	      `(cl:let* ,bindings ,@body)))

(for-macros (defun expand-define-let-or-let* (body)
	      (cond
		((null body) body)
		(t
		 (let ((form (first body)))
		   (cond
		     ((named-let? form)
		      (cons (make-named-let (named-let-name form)
					    (named-let-bindings form)
					    (expand-function-body (named-let-body form)))
			    (rest body)))
		     ((let? form)
		      (cons (make-let (let-bindings form)
				      (expand-function-body (let-body form)))
			    (rest body)))
		     ((let*? form)
		      (cons (make-let* (let-bindings form)
				       (expand-function-body (let-body form)))
			    (rest body)))
		     (t body)))))))

(for-macros (defun expand-function-body-definitions (definitions body)
	      (cond
		((null definitions) body)
		(t (let* ((function-definitions (remove-if-not 'define-function? definitions)))
		     `((let ,(mapcar 'define-name definitions)
			 (labels ,(append
				   (mapcar (cl:lambda (form)
					     (expand-define-closure-or-function (second form)
										(expand-function-body (cddr form))
										'expand-define-function-for-labels))
				     function-definitions)
				   (mapcar (cl:lambda (form)
					     (let ((args (unique-symbol 'args)))
					       `(,(define-name form) (&rest ,args) (apply ,(define-name form) ,args))))
				     (remove-if 'define-function? definitions)))
			   (setq ,@(append* (mapcar (cl:lambda (form)
						      (cond ((define-function? form)
							     (let ((name (define-name form)))
							       `(,name #',name)))
							    ;; Variable definition.
							    (t `(,(second form) ,(third form)))))
						    definitions)))
			   ,@(expand-define-let-or-let* body)))))))))
(for-macros (defun expand-function-body (body)
	      (destructuring-bind (declarations definitions body) (split-function-body body)
		`(,@declarations
		  ,@(expand-function-body-definitions definitions body)))))

(for-macros (defun expand-define-closure (arg-list body)
	      `(lambda ,arg-list
		 ,@(expand-function-body body))))

(assert (equal (expand-define-closure 'args '((print args)
					      (apply '+ args)))
	       '(lambda args
		 (PRINT args)
		 (apply '+ args))))

(assert (equal (expand-define-closure '(x y z) '((print (list x y z))
						 (+ x y z)))
	       '(lambda (X Y Z)
		 (PRINT (LIST X Y Z))
		 (+ X Y Z))))

(assert (equal (expand-function-body-definitions '() '(body))
	       '(body)))

(assert (equal (with-readable-symbols (expand-function-body-definitions '((define x 1) (define (f x) (+ x 1))) '(body)))
	       '((LET (X F)
		   (LABELS ((F (X)
			      (+ X 1))
			    (X (&REST ARGS)
			      (APPLY X ARGS)))
		     (SETQ X 1
			   F #'F)
		     BODY)))))

(assert (equal (expand-function-body-definitions '((define ((f x) y)
						     (list x y)))
						 '([(f 1) 2]))
	       '((LET (F)
		   (LABELS ((F (X)
			      (LAMBDA (Y)
				(LIST X Y))))
		     (SETQ F #'F)
		     (FUNCALL (F 1) 2))))))


(for-macros (defun expand-top-level-define-function (name arg-list body)
	      `(defun ,name ,(arg-list->lambda-list arg-list)
		 ,@(expand-function-body body))))

(assert (equal (expand-top-level-define-function 'fn-name '(x y z) '(body))
	       '(DEFUN FN-NAME (X Y Z) BODY)))

(for-macros (defun expand-top-level-define-closure-or-function (name-and-arg-list body)
	      (expand-define-closure-or-function name-and-arg-list body 'expand-top-level-define-function)))

(assert (equal (expand-top-level-define-closure-or-function '(((nested-fn x) y) z) '(body))
	       '(DEFUN NESTED-FN (X)
		 (lambda (Y)
		   (lambda (Z)
		     BODY)))))
(for-macros (defun expand-top-level-define-parameter (name body)
	      `(defparameter ,name ,@body)))

(assert (equal (expand-top-level-define-parameter '*name* '(value "docs"))
	       '(DEFPARAMETER *NAME* VALUE "docs")))

(for-macros
  (defun expand-top-level-define-setf-fdefinition (name body)
    (let ((function-name (unique-symbol 'fn)))
      `(let ((,function-name (progn ,@body)))
	 (assert (functionp ,function-name))
	 ,@(when (stringp (first body))
	     `((setf (documentation ',name 'function) ,(first body))))
	 (setf (fdefinition ',name) ,function-name)
	 ',name))))

(assert (equal (with-readable-symbols
		 (expand-top-level-define-setf-fdefinition '5+ '("returns 5+ a number" (compose 'lcurry + 5))))
	       '(LET ((FN (PROGN "returns 5+ a number" (COMPOSE 'LCURRY + 5))))
		 (ASSERT (FUNCTIONP FN))
		 (SETF (DOCUMENTATION '5+ 'FUNCTION) "returns 5+ a number")
		 (SETF (FDEFINITION '5+) FN) '5+)))

(for-macros (defun expand-top-level-define (name body)
	      (cond
		;; name is (name . args)
		((consp name) (expand-top-level-define-closure-or-function name body))
		((symbolp name) (expand-top-level-define-setf-fdefinition name body))
		(t (error "Bad thing to define: ~S" name)))))


(assert (equal (with-readable-symbols
		 (expand-top-level-define 'add '('+)))
	       '(LET ((FN (PROGN '+))) (ASSERT (FUNCTIONP FN)) (SETF (FDEFINITION 'ADD) FN) 'add)))

(assert (equal (expand-top-level-define '(test-inner-nested-defines)
					'("Also returns a thing"
					  (define ((inner-nested x) y)
					    (list x y))
					  inner-nested))
	       '(DEFUN TEST-INNER-NESTED-DEFINES ()
		 "Also returns a thing"
		 (LET (INNER-NESTED)
		   (LABELS ((INNER-NESTED (X)
			      (LAMBDA (Y) (LIST X Y))))
		     (SETQ INNER-NESTED #'INNER-NESTED)
		     INNER-NESTED)))))


(defmacro lambda (arg-list &body body)
  "A lambda with scheme style argument lists. Some examples:
  (lambda (arg1 arg2 arg3) (list arg1 arg2 arg3)) ; Arity: 3
  (lambda (arg1 . args) (list* arg1 args)) ; Arity: at least 1
  (lambda args args) ; Arity: at least 0

  (lambda args args) ; all arguments stored in args
  (lambda (p1 p2 . args) (list* p1 p2 args)) ; Rest arg
  (lambda (p1 p2 (o1 \"default\") (o2 \"default\")) (list p1 p2 o1 o2)) ; Optional args
  (lambda (p1 p2 :k1 (:k2 \"default\")) (list p1 p2 k1 k2)) ; keyword args
  Rest/Optional/Keyword arguments are not compatible with each other.
  See DEFINE for more information on argument lists.
"
  `(cl:lambda ,(arg-list->lambda-list arg-list) ,@(expand-function-body body)))

(defmacro define (name-or-form &body body)
  "Definition form.
  (define new-function-name #'function-name) ;; Sets the fdefinition of new-function-name
  (define (function-name arg1 arg2 . args) 
    body...) ;; Expands to
  (defun function-name (arg1 arg2 &rest args)
    body...)

  ;; Optional arguments are specified as (arg-name default-value-form) or (arg-name) which defaults to nil.
  (define (fn pos1 pos2 (opt1) (opt2 \"default\"))
    (list pos1 pos2 opt1 opt2))

  ;; Keyword arguments are specified as :arg-name, (:arg-name) both of which default to nil, or (:arg-name default-value-form).
  (define (fn pos1 pos2 (:k1 \"default\") :k2)
    (list pos1 pos2 k1 k2))

  ;; Rest/Optional/Keyword arguments are not compatible with each other.
    
  ;; Defines can be nested like in scheme.
  (define (outer-function-name oarg1 oarg2 . oargs)
    (define (inner-function-name a1 a2) (list a1 a2))

    ;; Definitions can be mutually recursive.
    (define (mutually-recursive-function1 a) (mutually-recursive-function2 a))
    (define (mutually-recursive-function2 a) (mutually-recursive-function1 a))

    ;; Nested defines bind both function/lexical variable
    (inner-function-name oarg1 oarg2) ; as a function
    inner-function-name) ; returned as a value
  
  ;; Define can define functions which return closures:
  (define ((left-curry . args) f)
    ...)
  ;; Expands to
  (defun left-curry (&rest args)
    (lambda (f)
       ...))
  
  It is an error to mix defines with expressions.
  Special case: if the first form of a post-define body is a let or a let* you can place defines in that form.
  Example:
  (define (outer)
    (let ((x :x) (y :y))
      (define (inner1) (cons x y))
      (let ((z :z))
        (define (inner2) (list x y z))
        (inner2))
      (inner1)))"
  `(for-macros
     (macrolet ((define (&whole inner-whole &body ignored)
		  (declare (ignore ignored))
		  (error "Improperly nested define: ~S in expansion for ~S" inner-whole ',name-or-form)))
       ,(expand-top-level-define name-or-form body))))

(define (((test-nested-defines x) y . yargs) . zargs)
  "Returns a thing"
  `(,x ,y ,@yargs ,@zargs))

(assert (equal [[(test-nested-defines :x) :y :z] :a :b :c]
	       '(:x :y :z :a :b :c)))
(define (test-inner-nested-defines)
  "Also returns a thing"
  (define ((inner-nested x) y)
    (list x y))
  inner-nested)

(assert (equal [[(test-inner-nested-defines) :x] :y]
	       '(:x :y)))

(define (definition-with-definitions-nested-inside-let)
  (define a 1)
  (define steak :sauce)
  (let ((here :marker))
    (define (okay? var)
      (if (eq var :sauce)
	  :yeah-its-kay))
    (let* ((uh-oh :hi))
      (define (steak-sauce) steak)
      (okay? (and here uh-oh (steak-sauce))))))

(assert (eq :yeah-its-kay (definition-with-definitions-nested-inside-let)))

(defmacro undefine (name &rest ignored)
  "Undefines a globally defined function using fmakunbound."
  (declare (ignore ignored))
  `(fmakunbound ',name))

(uninstall-syntax!)

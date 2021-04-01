;;;; schemeish.lisp

(in-package #:schemeish)

(defmacro for-macros (&body body)
  "Evaluates to `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body).
Used to annotate functions that are used in macros."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(for-macros
  (defun map (function list &rest more-lists)
    (apply #'mapcar function list more-lists)))

;; Named let.
(defmacro let (&whole whole &rest rest)
  "Extends CL:let by allowing for named let recursive forms. E.g.
  (let rec ((n 10)
            (result '()))
    (if (= n 0)
        result
        (rec (1- n) (cons n result))))
  ;; => '(1 2 3 4 5 6 7 8 9 10)"
  (declare (ignore rest))
  (if (symbolp (second whole))
      (destructuring-bind (name bindings &rest body) (rest whole)
	`(labels ((,name ,(map #'car bindings)
		    ,@body))
	   (,name ,@(map #'second bindings))))
      `(cl:let ,@(rest whole))))

(for-macros
  (defun read-left-bracket (stream char)
    (declare (ignore char))
    (let ((form (read-delimited-list #\] stream t)))
      (if (null form)
	  '()
	  `(funcall ,@form))))
  (defun read-right-bracket (stream char)
    (declare (ignore stream char))
    (error "read: unmatched ]"))

  (defun install-syntax! ()
    "Installs [] reader syntax. 
    [function-name arg1 ...] => (funcall function-name arg1 ...)"
    (set-macro-character #\[ #'read-left-bracket)
    (set-macro-character #\] #'read-right-bracket))

  (defun uninstall-syntax! ()
    "Uninstalls [] reader syntax if it was installed using INSTALL-SYNTAX!."
    (when (eq (get-macro-character #\[) #'read-left-bracket)
      (set-macro-character #\[ nil))
    (when (eq (get-macro-character #\]) #'read-right-bracket)
      (set-macro-character #\] nil))))

(for-macros
  (install-syntax!))

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
  (defun optional-arg-list->lambda-list (args)
    (cond
      ((consp args)
       (let ((arg (first args)))
	 (cond
	   ((consp arg) `(,arg ,@(optional-arg-list->lambda-list (rest args))))
	   (t (error "Expected optional argument ~S to be of the form (name default-value-form) or (name)." arg)))))
      ((null args) ())
      ((symbolp args) (error "Optional argument list cannot have a rest argument.")))))


(assert (equal (optional-arg-list->lambda-list '((opt "option") (option)))
	       '((OPT "option") (OPTION))))
(assert (null (ignore-errors (optional-arg-list->lambda-list '(bad-argument)))))
(assert (null (ignore-errors (optional-arg-list->lambda-list 'bad-rest-argument))))

(for-macros
  (defun keyword-arg->lambda-list-arg (arg)
    (if (consp arg)
	(cons (intern (symbol-name (first arg))) (rest arg))
	(intern (symbol-name arg)))))

(assert (equal (keyword-arg->lambda-list-arg :keyword)
	       'keyword))
(assert (equal (keyword-arg->lambda-list-arg '(:keyword "default"))
	       '(keyword "default")))

(for-macros
  (defun keyword-arg-list->lambda-list (args)
    (cond
      ((consp args)
       (let ((arg (first args)))
	 (cond
	   ((consp arg)
	    (let ((arg-name (first arg)))
	      (cond
		((keywordp arg-name) `(,(keyword-arg->lambda-list-arg arg) ,@(keyword-arg-list->lambda-list (rest args))))
		(t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))
	   ((keywordp arg)
	    `(,(keyword-arg->lambda-list-arg arg) ,@(keyword-arg-list->lambda-list (rest args))))
	   (t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))
      ((null args) ())
      ((symbolp args) (error "Keyword argument list not compatible with rest argument.")))))

(assert (null (keyword-arg-list->lambda-list ())))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(bad-argument)))))
(assert (equal (keyword-arg-list->lambda-list '(:k1))
	       '(k1)))
(assert (equal (keyword-arg-list->lambda-list '((:k1 "default")))
	       '((k1 "default"))))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(((:bad-keyword) "default"))))))

(for-macros
  (defun arg-list->lambda-list (args)
    (cond
      ;; (arg . rest-args)
      ((consp args)
       (let ((arg (first args)))
	 (cond
	   ;; arg is (optional default) or (:keyword default)
	   ((consp arg)
	    (let ((arg-name (first arg)))
	      (cond
		((keywordp arg-name) `(&key ,@(keyword-arg-list->lambda-list args)))
		((symbolp arg-name) `(&optional ,@(optional-arg-list->lambda-list args)))
		(t (error "bad thing to be an arg-name: ~S" arg-name)))))
	   ;; arg is :keyword
	   ((keywordp arg) `(&key ,@(keyword-arg-list->lambda-list args)))
	   ;; arg is positional
	   (t (cons arg (arg-list->lambda-list (rest args)))))))
      ;; args is empty
      ((null args) '())
      ;; args is a rest parameter
      ((symbolp args) `(&rest ,args))
      (t (error "bad thing to be in an arglist: ~S" args)))))

;; Null arguments
(assert (null (arg-list->lambda-list ())))
;; position + rest
(assert (equal (arg-list->lambda-list '(arg1 arg2 . args))
	       '(arg1 arg2 &rest args)))
;; positional arguments
(assert (equal (arg-list->lambda-list '(arg1 arg2 arg3))
	       '(arg1 arg2 arg3)))
;; optional arugments
(assert (equal (arg-list->lambda-list '(arg1 (opt-arg "optional") (opt-arg2 "optional2") (null-arg)))
	       '(arg1 &optional (opt-arg "optional") (opt-arg2 "optional2") (null-arg))))

;; Can't mix positional and optional
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 (opt-arg "optional") bad-arg (opt-arg2 "optional2"))))))
;; Can't mix optional and rest (...well you *could*)
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 (opt-arg "optional") . bad-args)))))
;; Bad arg-name for optional args
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 ((oops-arg) "form"))))))

;; Invalid argument lists
(assert (equal (arg-list->lambda-list '(a1 a2 :k1 :k2))
	       '(A1 A2 &KEY K1 K2)))
(assert (equal (arg-list->lambda-list '(:k1))
	       '(&key k1)))
(assert (equal (arg-list->lambda-list '(a1 a2 (:k1 "default") :k2 (:k3 "default2")))
	       '(A1 A2 &KEY (K1 "default") K2 (K3 "default2"))))
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") bad-positional)))))
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") (bad-optional))))))

(for-macros
  (defun define? (form) (and (consp form) (eq (first form) 'define)))
  (assert (define? '(define name var)))

  (defun define-name (form)
    (let ((name-form (second form)))
      (first (flatten name-form))))

  (assert (equal (define-name '(define (((nested-fn) x) y) z))
		 'nested-fn)))

(for-macros
  (defun append* (lists)
    "Append lists."
    (apply #'append lists)))
(assert (equal
	 (append* '((1 2 3) (4 5 6)))
	 '(1 2 3 4 5 6)))

(for-macros
  (defun declaration? (form) (and (consp form) (eq 'declare (first form)))))
(assert (declaration? '(declare (ignore x))))

(for-macros
  (defun takef (list predicate)
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

(for-macros
  (defun dropf (list predicate)
    "Drops initial elements of list that don't satisfy pred."
    (let rec ((list list))
      (if (or (null list) (not [predicate (first list)]))
	  list
	  (rec (rest list))))))


(assert (equal (dropf '(2 4 5 8) 'evenp)
	       '(5 8)))

(for-macros
  (defun splitf-at (list predicate)
    "Returns (list (takef list predicate) (dropf list predicate))"
    (list (takef list predicate) (dropf list predicate))))


(for-macros
  (defun split-function-body (body)
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

(for-macros
  (defun define-function? (form)
    (and (define? form)
	 (consp (second form)))))

(for-macros
  (defun expand-define-closure-or-function (name-and-arg-list body expand-define-function)
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

(for-macros
  (defun expand-define-function-for-labels (name arg-list body)
    (list* name (arg-list->lambda-list arg-list) body)))

(for-macros
  (defun expand-function-body-definitions (definitions body)
    (cond
      ((null definitions) body)
      (t (let* ((function-definitions (remove-if-not 'define-function? definitions)))
	   `((let ,(map 'define-name definitions)
	       (labels ,(map (cl:lambda (form)
			       (expand-define-closure-or-function (second form)
								  (expand-function-body (cddr form))
								  'expand-define-function-for-labels))
			 function-definitions)
		 (setq ,@(append* (map (cl:lambda (form)
					 (cond ((define-function? form)
						(let ((name (define-name form)))
						  `(,name #',name)))
					       ;; Variable definition.
					       (t `(,(second form) ,(third form)))))
				       definitions)))
		 ,@body))))))))

(assert (equal (expand-function-body-definitions '() '(body))
	       '(body)))

(assert (equal (expand-function-body-definitions '((define x 1) (define (f x) (+ x 1))) '(body))
	       '((LET (X F)
		   (LABELS ((F (X) (+ X 1)))
		     (SETQ X 1 F #'F)
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

(for-macros
  (defun expand-function-body (body)
    (destructuring-bind (declarations definitions body) (split-function-body body)
      `(,@declarations
	,@(expand-function-body-definitions definitions body)))))

(defmacro lambda (arg-list &body body)
  "A lambda with scheme style argument lists. Some examples:
  (λ (arg1 arg2 arg3) (list arg1 arg2 arg3)) ; Arity: 3
  (λ (arg1 . args) (list* arg1 args)) ; Arity: at least 1
  (λ args args) ; Arity: at least 0

  (λ args args) ; all arguments stored in args
  (λ (p1 p2 . args) (list* p1 p2 args)) ; Rest arg
  (λ (p1 p2 (o1 \"default\") (o2 \"default\")) (list p1 p2 o1 o2)) ; Optional args
  (λ (p1 p2 :k1 (:k2 \"default\")) (list p1 p2 k1 k2)) ; keyword args
  Rest/Optional/Keyword arguments are not compatible with each other.
  See DEFINE for more information on argument lists.
"
  `(cl:lambda ,(arg-list->lambda-list arg-list) ,@(expand-function-body body)))
(defmacro λ (arg-list &body body)
  "Alias for schemeish:lambda"
  `(lambda ,arg-list ,@body))

(for-macros
  (defun expand-top-level-define-function (name arg-list body)
    `(defun ,name ,(arg-list->lambda-list arg-list)
       ,@(expand-function-body body))))

(assert (equal (expand-top-level-define-function 'fn-name '(x y z) '(body))
	       '(DEFUN FN-NAME (X Y Z) BODY)))

(for-macros
  (defun expand-define-closure (arg-list body)
    `(lambda ,arg-list
       ,@(expand-function-body body))))

(assert (equal (expand-define-closure '(x y z) '((print (list x y z))
						 (+ x y z)))
	       '(lambda (X Y Z)
		 (PRINT (LIST X Y Z))
		 (+ X Y Z))))

(for-macros
  (defun expand-top-level-define-closure-or-function (name-and-arg-list body)
    (expand-define-closure-or-function name-and-arg-list body 'expand-top-level-define-function)))

(assert (equal (expand-top-level-define-closure-or-function '(((nested-fn x) y) z) '(body))
	       '(DEFUN NESTED-FN (X)
		 (lambda (Y)
		   (lambda (Z)
		     BODY)))))
(for-macros
  (defun expand-top-level-define-parameter (name body)
    `(defparameter ,name ,@body)))

(assert (equal (expand-top-level-define-parameter '*name* '(value "docs"))
	       '(DEFPARAMETER *NAME* VALUE "docs")))

(for-macros
  (defun expand-top-level-define (name body)
    (cond
      ;; name is (name . args)
      ((consp name) (expand-top-level-define-closure-or-function name body))
      ((symbolp name) (expand-top-level-define-parameter name body))
      (t (error "Bad thing to define: ~S" name)))))

(assert (equal (expand-top-level-define '*name* '(value))
	       '(DEFPARAMETER *NAME* VALUE)))

(defmacro define (name-or-form &body body)
  "Definition form.
  (define *variable-name* value) ;; Expands to (defparameter *variable-name* value)
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
    (defien (mutually-recursive-function2 a) (mutually-recursive-function1 a))

    ;; Nested defines bind both function/lexical variable
    (inner-function-name oarg1 oarg2) ; as a function
    inner-function-name) ; returned as a value
  
  ;; Define can define functions which return closures:
  (define ((left-curry . args) f)
    ...)
  ;; Expands to
  (defun left-curry (&rest args)
    (λ (f)
       ...))"
  (expand-top-level-define name-or-form body))

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

(assert (equal
	 (macroexpand-1 '(define *hello* 'algebra))
	 '(defparameter *hello* 'algebra)))

(assert (equal
	 (macroexpand-1
	  '(define (name arg1 . args)
	    (print arg1)
	    (print args)))
	 '(defun name (arg1 &rest args)
	   (print arg1)
	   (print args))))
;; (define (name arg1 . args) body...) =>
;; (defun name (arg1 &rest args) body...)

(assert (equal
	 (expand-top-level-define '(top arg1 . args)
				  '((define local-var 1)
				    (define (local-fn arg1 . args)
				      (define (local-local-fn arg)
					'body)
				      'body)
				    local-var))
	 '(DEFUN TOP (ARG1 &REST ARGS)
	   (LET (LOCAL-VAR LOCAL-FN)
	     (LABELS ((LOCAL-FN (ARG1 &REST ARGS)
			(LET (LOCAL-LOCAL-FN)
			  (LABELS ((LOCAL-LOCAL-FN (ARG)
				     'BODY))
			    (SETQ LOCAL-LOCAL-FN #'LOCAL-LOCAL-FN)
			    'BODY))))
	       (SETQ LOCAL-VAR 1
		     LOCAL-FN #'LOCAL-FN)
	       LOCAL-VAR)))))

(define (empty? datum) (null datum))

(define *get-bundle-type-predicate* (gensym))
(define (make-bundle-predicate name)
  "Returns a predicate which, only evaluates to true 
when given a bundle with this type-predicate"
  (let (self)
    (setq self
	  (λ args
	    (if (empty? args)
		`(make-bundle-predicate ,name)
		(let ((data (first args)))
		  (and (functionp data)
		       (eq self [data *get-bundle-type-predicate*]))))))))

(define *name?* (make-bundle-predicate :bundle))
(assert [*name?* (λ (arg)
		   (cond
		     ((eq *get-bundle-type-predicate* arg) *name?*)))])

(define *get-bundle-list* (gensym))
(for-macros
  (define (make-keyword symbol)
    (intern (symbol-name symbol) :keyword)))

(define *get-bundle-permissions* (gensym))

(defmacro bundle (type-predicate bundle-list-form &rest fn-names)
  "Create a bundle of permissions for closure objects.
Type-predicate is nil or a predicate created by make-bundle-predicate.
Example:
    (define *point?* (make-bundle-predicate :point))
    (define (make-point x y)
      (define (get-x) x)
      (define (get-y) y)
      (define (set-x! new-x) (setq x new-x))
      (define (set-y! new-y) (setq y new-y))
    
      (bundle *point?* (make-point x y) get-x get-y set-x! set-y!))
    
    (let ((point (make-point 3 4)))
      [point :get-x] ;; => closure of 0 arguments
      (assert (= 3 [[point :get-x]]))
      [point :set-x!] ;; => closure of 1 argument
      [[point :set-x!] 32]
      (assert (= 32 [[point :get-x]]))
      (assert [*point?* point])
      (bundle-permissions bundle) ; => '(:get-x :get-y :set-x! :set-y!)
      (bundle-list bundle) ; => '(make-point 32 4)
      )"
  `(λ (arg)
     (cond
       ((and ,type-predicate (eq *get-bundle-type-predicate* arg))
	,type-predicate)
       ((eq *get-bundle-list* arg)
	,(if bundle-list-form
	     `(list ',(first bundle-list-form) ,@(rest bundle-list-form))
	     '()))
       ((eq *get-bundle-permissions* arg) ',(map (λ (name) (make-keyword name)) fn-names))
       ,@ (map (λ (name) `((eq ,(make-keyword name) arg) ,name)) fn-names))))

(define (bundle-permissions bundle)
  "Return a list of permissions to the bundle."
  [bundle *get-bundle-permissions*])
(define (bundle-list bundle)
  "Return the bundle as a '(constructor-name arg-values...)"
  [bundle *get-bundle-list*])

(define *point?* (make-bundle-predicate :point))
(define (make-bundle-point x y)
  (define (get-x) x)
  (define (get-y) y)
  (define (set-x! new-x) (setq x new-x))
  (define (set-y! new-y) (setq y new-y))

  (bundle *point?* (make-bundle-point x y) get-x get-y set-x! set-y!))

(let ((point (make-bundle-point 3 4)))
  (assert (= 3 [[point :get-x]]))
  [[point :set-x!] 32]
  (assert (= 32 [[point :get-x]]))
  (assert [*point?* point]))
#+nil
(sb-introspect:function-lambda-list [(make-bundle-point 3 4) :set-x!])
;; => (NEW-X)


(define (filter predicate list)
  "Keep elements of list that satisfy predicate."
  (remove-if-not predicate list))

(for-macros
  (define (pair? datum) "T if datum is a cons." (consp datum))
  (define (null? datum) "T if datum is nil." (null datum))
  (define (list? datum) "Alias for (listp datum)." (listp datum)))

(define (list-ref list pos)
  "Return the value of list at pos."
  (nth pos list))

(define (list-tail list pos)
  "Return the sublist of list starting at pos."
  (nthcdr pos list))

(define (foldl proc init . lists)
  "Fold (proc e1 e2 ... result) across lists starting from the start of the lists."
  (let rec ((result init)
	    (lists lists))
    (if (member nil lists)
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldl 'cons () '(1 2 3 4))
	       '(4 3 2 1)))

(assert (= (foldl (λ (a b result)
		    (* result (- a b)))
		  1
		  '(1 2 3)
		  '(4 5 6))
	   -27))

(define (foldr proc init . lists)
  "Fold (proc e1 e2 ... result) across lists starting from the end of the lists."
  (let rec ((result init)
	    (lists (map 'reverse lists)))
    (if (member nil lists)
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldr 'cons '() '(1 2 3 4))
	       '(1 2 3 4)))
(assert (equal (foldr (λ (v l) (cons (1+ v) l)) '() '(1 2 3 4))
	       '(2 3 4 5)))


(define (negative? num) (minusp num))
(define (positive? num) (plusp num))

(define (andmap proc . lists)
  "Return the last non-nil result of mapping proc across lists, or nil if some result is nil."
  (let rec ((result t)
	    (lists lists))
    (if (or (not result) (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))

(assert (andmap 'positive? '(1 2 3)))
;; (andmap 'positive? '(1 2 a)) => error
(assert (not (andmap 'positive? '(1 -2 a))))
(assert (= 9 (andmap '+ '(1 2 3) '(4 5 6))))

(define (ormap proc . lists)
  "Return the first non-nil result of mapping proc across lists."
  (let rec ((result ())
	    (lists lists))
    (if (or result (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))

(define (eq? obj1 obj2) (eq obj1 obj2))
(defgeneric equal? (object1 object2)
  (:documentation "Provides a generic interface to EQUAL."))
(defmethod equal? (object1 object2) (equal object1 object2))

(assert (ormap 'eq? '(a b c) '(a b c)))
(assert (ormap 'positive? '(1 2 a)))
(assert (= 5 (ormap '+ '(1 2 3) '(4 5 6))))

(define (for-each proc . lists)
  "Apply proc to each element of lists. Arity of proc should match the number of lists."
  (let rec ((lists lists))
    (unless (member nil lists)
      (apply proc (map 'first lists))
      (rec (map 'rest lists)))))

(assert (equal?
	 (with-output-to-string (s)
	   (for-each (λ (x y) (format s "~S" (list x y)))
		     '(a b c)
		     '(1 2 3)))
	 "(A 1)(B 2)(C 3)"))

(define (remq v list)
  "Remove using eq? as a test."
  (remove v list :test #'eq))

(define (remove* v-list list (test #'equal?))
  "Removes all elements in v-list from list."
  (foldl (λ (v result) (remove v result :test test)) list v-list))

(assert (equal
	 (remove* (list 1 2) (list 1 2 3 2 4 5 2))
	 '(3 4 5)))

(define (remq* v-list list) (remove* v-list list #'eq?))

(define (sort list less-than? (:extract-key (λ (x) x)))
  "Returns a sorted list."
  (cl:sort (copy-list list) less-than? :key extract-key))

(assert (equal (let* ((original-list '(1 3 4 2))
		      (sorted-list (sort original-list '<)))
		 (assert (equal '(1 3 4 2) original-list))
		 sorted-list)
	       '(1 2 3 4)))

(define (memf proc list)
  "Returns the first sublist of list whose first element satisfies predicate proc."
  (let rec ((list list))
    (if (or (null? list) [proc (first list)])
	list
	(rec (rest list)))))

(assert (equal (memf (λ (arg) (> arg 9)) '(7 8 9 10 11))
	       '(10 11)))

(define (findf proc list)
  "Finds the first element in list that satisfies predicate proc."
  (let ((found (memf proc list)))
    (if (list? found)
	(first found)
	())))

(assert (= (findf (λ (arg) (> arg 9)) '(7 8 9 10 11))
	   10))

(define (list-update list pos updater)
  "Returns a list with (updater value) to the value at pos in list."
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (cond
      ((null? list) (nreverse result))
      (t
       (rec (rest list)
	    (1+ current-pos)
	    (cons (let ((x (first list)))
		    (if (= current-pos pos) [updater x] x))
		  result))))))

(define (symbol->string symbol) (symbol-name symbol))

(assert (equal (list-update '(zero one two) 1 'symbol->string)
	       '(ZERO "ONE" TWO)))

(define (list-set list pos value)
  "Return a list with the value at pos replaced."
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (cond
      ((null? list) (nreverse result))
      (t
       (rec (rest list)
	    (1+ current-pos)
	    (cons (let ((x (first list)))
		    (if (= current-pos pos) value x))
		  result))))))

(assert (equal (list-set '(zero one two) 2 "two")
	       '(zero one "two")))

(define (take list n)
  "Takes the first n elements from list"
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (if (or (= n current-pos) (empty? list))
	(nreverse result)
	(rec (rest list)
	     (1+ current-pos)
	     (cons (first list) result)))))

(assert (equal (take '(1 2 3 4 5) 2)
	       '(1 2)))

(define (drop list n)
  "Drops the first n elements from list"
  (list-tail list n))

(define (split-at list pos)
  "Returns (list (take list pos) (drop list pos))"
  (list (take list pos) (drop list pos)))

(define (even? x) (evenp x))
(define (odd? x) (oddp x))
(define (zero? x) (zerop x))


(define (compose . procs)
  "Function compositions. Mulitple values of one function are used as arguments to the next."
  (foldr (λ (proc result)
	   (λ args
	     (multiple-value-call proc (apply result args))))
	 (λ args (values-list args))
	 procs))

(assert (equal (multiple-value-list [(compose) :x :y :z])
	       '(:x :y :z)))

(assert (equal [(compose (λ (x y z) (list 'f x y z))) 'x 'y 'z]
	       '(f x y z)))

(assert (equal
	 [(compose (λ (a b c) (list 'f a b c))
		   (λ (x y) (values (list 'g x) (list 'g y) (list 'g 'c))))
	  'x 'y]

	 '(f (g x) (g y) (g c))))

(define (filter-map proc . lists)
  "Remove nil from the result of mapping proc over lists."
  (remove nil (apply 'map proc lists)))

(define (range end (start 0) (step 1))
  "Return a list of elements from [start,end) using step as the stepsize."
  (if (<= start end)
      (loop for i from start below end by step collecting i)
      (loop for i from start downto end by step collecting i)))

(define (append-map proc . lists)
  "Append the results of mapping procedure across lists."
  (append* (apply 'map proc lists)))

(define (filter-not pred list)
  "Returns a list of elements that don't satisfy predicate pred."
  (filter (compose 'not pred) list))

(define (partition pred list)
  "Returns (list elements-satisfying-pred elements-not-satisfying-pred)"
  (list (filter pred list)
	(filter-not pred list)))

(assert (equal (partition 'even? '(1 2 3 4 5 6))
	       '((2 4 6) (1 3 5))))

(define (procedure? datum) (functionp datum))

(define (lcurry proc . left-args)
  "Return a procedure waiting for the right-args."
  (λ right-args
    (apply proc (append left-args right-args))))

(assert (= [(lcurry '- 5 4) 3]
	   (- 5 4 3)))

(define (rcurry proc . right-args)
  "Return a procedure waiting the left-args."
  (λ left-args
    (apply proc (append left-args right-args))))

(assert (= [(rcurry '- 4 3) 5]
	   (- 5 4 3)))

(define (swap-args proc)
  "Swap args of 2-argument procedure proc."
  (λ (x y) [proc y x]))

(assert (equal [(swap-args 'cons) 1 2]
	       (cons 2 1)))

(define (memo-proc proc)
  "Memoize procedure proc of no arguments."
  (let ((run? ())
	(result-values))
    (λ args
      (unless run?
	(setq result-values (multiple-value-list (apply proc args))
	      run? t)
	result-values)
      (values-list result-values))))

(defmacro delay (&body body)
  "Delays body."
  `(memo-proc (λ () ,@body)))
(define (force promise)
  "Evaluates promise."
  [promise])

(define *the-empty-stream* ())
(defmacro stream-cons (first rest)
  "Construct a stream from an element and a delayed stream."
  `(cons ,first (delay ,rest)))
(define (stream-car stream)
  "First element of stream."
  (car stream))
(define (stream-cdr stream)
  "Forces the rest of the stream."
  (force (cdr stream)))

(define (stream-empty? stream)
  "T if the stream is *the-empty-stream*"
  (eq *the-empty-stream* stream))

(define (stream-for-each stream proc)
  "Applies proc to each element of stream, discarding results"
  (let rec ((stream stream))
    (unless (stream-empty? stream)
      [proc (stream-car stream)]
      (rec (stream-cdr stream)))))

(define (stream-length stream)
  "The length of the stream."
  (let ((count 0))
    (stream-for-each stream (λ (x) (declare (ignore x)) (incf count)))
    count))

(define (stream->list stream)
  "A list of all of the elements in stream."
  (let ((xs ()))
    (stream-for-each stream (λ (x) (push x xs)))
    (nreverse xs)))

(define (stream-first stream)
  "The first element of a stream."
  (stream-car stream))
(define (stream-rest stream)
  "Forces the rest of the stream."
  (stream-cdr stream))

(define (stream? datum)
  "True if datum is a stream-like object."
  (or (eq? datum *the-empty-stream*)
      (and (pair? datum)
	   (procedure? (cdr datum)))))

(define (list->stream list)
  "Constructs a stream from a list of values."
  (if (empty? list)
      *the-empty-stream*
      (stream-cons (first list) (list->stream (rest list)))))

(define (stream . list)
  "Constructs a stream from a list of values."
  (list->stream list))

(define *test-stream* (stream 1 2 3))

(assert (equal (stream->list *test-stream*)
	       '(1 2 3)))

(assert (equal (let* ((one 0) (two 1) (three 2)
		      (stream (stream-cons (incf one) (stream-cons (incf two) (stream-cons (incf three) *the-empty-stream*)))))
		 (stream->list stream)
		 (stream->list stream))
	       '(1 2 3)))

(define (stream-map proc stream)
  "A stream which has proc applied to each element."
  (if (stream-empty? stream)
      *the-empty-stream*
      (stream-cons [proc (stream-first stream)] (stream-map proc (stream-rest stream)))))

(assert (equal (stream->list (stream-map (lcurry '* 5) *test-stream*))
	       '(5 10 15)))

(define (stream-fold proc init stream)
  "A stream which applies (proc accumulated-value element) to successive elements of stream."
  (cond
    ((stream-empty? stream) init)
    (t
     (let ((first (stream-first stream))
	   (rest (stream-rest stream)))
       (cond
	 ((stream-empty? rest) [proc init first])
	 (t (stream-fold proc [proc init (stream-first stream)] rest)))))))

(assert (eq :init (stream-fold t :init *the-empty-stream*)))
(assert (equal (stream-fold (swap-args 'cons) () *test-stream*)
	       '(3 2 1)))

(define (stream-filter predicate stream)
  "A stream with only the elements which satisfy predicate."
  (cond
    ((stream-empty? stream) stream)
    (t
     (let ((x (stream-first stream)))
       (if [predicate x]
	   (stream-cons x (stream-filter predicate (stream-rest stream)))
	   (stream-filter predicate (stream-rest stream)))))))

(assert (equal (stream->list (stream-filter 'odd? (stream 1 2 3)))
	       '(1 3)))

(define (stream-drop stream n)
  "A stream without the first n elements of stream."
  (let rec ((stream stream)
	    (n n))
    (if (or (stream-empty? stream) (<= n 0))
	stream
	(rec (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-drop (stream 1 2 3) 2))
	       '(3)))

(define (stream-take stream n)
  "A stream with up to the first n elements of stream."
  (if (or (stream-empty? stream) (<= n 0))
      *the-empty-stream*
      (stream-cons (stream-first stream)
		   (stream-take (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-take (stream 1 2 3) 2))
	       '(1 2)))

(define (stream-ref stream i)
  "Returns the i-th element (0-based indexing) of stream."
  (stream-first (stream-drop stream i)))

(assert (= (stream-ref (stream 0 1 2 3) 1)
	   1))

(define (stream-append . streams)
  "A stream in which combines streams to follow one after the other."
  (cond
    ((null? streams) *the-empty-stream*)
    (t
     (let ((stream (first streams)))
       (cond
	 ((stream-empty? stream)
	  (apply 'stream-append (rest streams)))
	 (t
	  (stream-cons (stream-first stream)
		       (apply 'stream-append
			      (stream-rest stream)
			      (rest streams)))))))))

(assert (equal
	 (stream->list (stream-append (stream 1 2 3) (stream 4 5 6) (stream 7 8 9)))
	 '(1 2 3 4 5 6 7 8 9)))

(define (stream-flatten stream-of-streams)
  "A stream which combines a stream of streams into a single stream using stream-append."
  (stream-fold 'stream-append
	       *the-empty-stream*
	       stream-of-streams))

(assert (equal
	 (stream->list (stream-flatten (stream (stream 1 2 3)
					       (stream 4 5 6)
					       (stream 7 8 9))))
	 '(1 2 3 4 5 6 7 8 9)))


(define (stream-range start end)
  "A stream of integers from start up to (1- end)."
  (cond
    ((> start end) *the-empty-stream*)
    (t
     (stream-cons start
		  (stream-range (1+ start) end)))))

(assert (equal (stream->list (stream-range 4 8))
	       '(4 5 6 7 8)))

(define (stream-flatmap proc s)
  "Stream-flatten the result of mapping proc across stream s."
  (stream-flatten (stream-map proc s)))

(assert (equal (stream->list (stream-flatmap
			      (λ (i)
				(stream-map
				 (λ (j) (list i j))
				 (stream 4 5)))
			      (stream 1 2)))
	       '((1 4) (1 5) (2 4) (2 5))))


(for-macros
  (define (stream-collect-bindings-fn binding-names body)
    (let ((arg-name (gensym)))
      `(λ (,arg-name)
	 (destructuring-bind ,binding-names ,arg-name
	   (declare (ignorable ,@binding-names))
	   ,@body)))))

(for-macros
  (define (stream-collect-filter-form binding-names test-form stream-form)
    `(stream-filter
      ,(stream-collect-bindings-fn binding-names (list test-form))
      ,stream-form)))

(stream-collect-filter-form '(i j) '(even? (+ i j)) :stream)

(for-macros
  (define (stream-collect-inner-map-form binding binding-names)
    `(stream-map (λ (,(first binding))
		   (list ,@binding-names))
		 ,(second binding))))

(stream-collect-inner-map-form '(j (stream-range 1 (1- i)))
			       '(i j))
(for-macros
  (define (stream-collect-flatmap-form binding body)
    `(stream-flatmap (λ (,(first binding))
		       ,@body)
		     ,(second binding))))

(stream-collect-flatmap-form '(i (stream-range 1 n)) '(:body))

(for-macros
  (define (stream-collect-outer-map binding-names form stream)
    `(stream-map
      ,(stream-collect-bindings-fn binding-names (list form))
      ,stream)))

(stream-collect-outer-map '(i j) '(list i j (+ i j)) :stream)

(for-macros
  (define (stream-collect-inner-flatmaps bindings)
    (when (null? bindings)
      (error "stream-collect: requires at least one binding."))
    (let ((binding-names (map 'car bindings))
	  (bindings (reverse bindings)))
      (let rec ((result (stream-collect-inner-map-form (first bindings)
						       binding-names))
		(bindings (rest bindings)))
	(if (null? bindings)
	    result
	    (rec
	     (stream-collect-flatmap-form (first bindings) (list result))
	     (rest bindings)))))))

(stream-collect-inner-flatmaps '((i (stream-range 1 n))
				 (j (stream-range 1 (1- i)))))

(for-macros
  (define (stream-collect-form map-form bindings filter-form)
    (let ((binding-names (map 'car bindings)))
      (stream-collect-outer-map
       binding-names
       map-form
       (stream-collect-filter-form
	binding-names
	filter-form
	(stream-collect-inner-flatmaps bindings))))))


(stream-collect-form '(list i j (+ i j))
		     '((i (stream-range 1 n))
		       (j (stream-range 1 (1- i))))
		     '(even? (+ i j)))

(defmacro stream-collect (map-form bindings filter-form)
  "Given bindings ((b1 stream1)
                   (b2 stream2) ...)
Generates a stream all combinations of b1,b2...,
Applies a filter to the generated stream using filter-form with b1,b2... bound.
Applies a map to the filtered/generated stream using map-form with b1,b2... bound.
Example:
 (define (prime-sum-pairs n)
   (stream-collect
    (list i j (+ i j))
    ((i (stream-range 1 n))
     (j (stream-range 1 (1- i))))
    (prime? (+ i j))))

 (prime-sum-pairs n) results in all of the (list i j (+ i j)) numbers i,j such that 0 < j < i <= n"
  (stream-collect-form map-form bindings filter-form))

(define (prime? num)
  (let ((root (floor (sqrt num))))
    (not (find-if (λ (div) (zerop (rem num div))) (range (1+ root) 2)))))

(define (prime-sum-pairs n)
  (stream-collect
   (list i j (+ i j))
   ((i (stream-range 1 n))
    (j (stream-range 1 (1- i))))
   (prime? (+ i j))))

(assert (equal (stream->list (prime-sum-pairs 6))
	       '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))))

(define (random-stream limit)
  "Return a stream of random numbers below limit.
If limit is an integer, returns integers.
If limit is a float returns floats.
Does not affect the random-state."
  (define (%random-stream rs)
    (stream-cons (random limit rs)
		 (%random-stream rs)))
  (%random-stream (make-random-state)))

;; Random-stream does not affect the random-state
(assert (equal (stream->list (stream-take (random-stream 1.0) 10))
	       (stream->list (stream-take (random-stream 1.0) 10))))

(assert (stream-empty? (stream-filter (λ (x) (not (<= 0.0 x 1.0)))
				      (stream-take (random-stream 1.0) 10))))

(define (alist-ref alist key (failure-result))
  "Rerturns the value associated with key in alist, else the failure-result."
  (let ((pair (assoc key alist :test #'equal?)))
    (if (pair? pair)
	(cdr pair)
	failure-result)))
(define (alist-remove alist key)
  "Returns an alist with key removed."
  (remove key alist :test #'equal? :key #'car))
(define (alist-set alist key value)
  "Returns an alist with key set to value."
  (acons key value (alist-remove alist key)))

(define (alist-update alist key updater (failure-result))
  "Applies updater to the value associated with key and updates the result in alist.
Applies updater to failure-result if key is not present."
  (alist-set alist key [updater (alist-ref alist key failure-result)]))
(define (alist-map alist proc)
  "Alist with proc applied to all values of alist."
  (map (λ (binding) [proc (car binding) (cdr binding)]) alist))

(define (alist-keys alist)
  "A list of all keys in alist."
  (alist-map alist (λ (key value) (declare (ignore value)) key)))
(define (alist-values alist)
  "A list of all of the values in alist."
  (alist-map alist (λ (key value) (declare (ignore key)) value)))

(define (alist-has-key? alist key)
  "T if the key is present in alist"
  (let ((no-key (gensym)))
    (not (eq? no-key (alist-ref alist key no-key)))))

(define (alist-set* alist . keys-and-values)
  "Update all of the values in alist with pairs of key value ..."
  (let rec ((keys-and-values keys-and-values)
	    (alist alist))
    (cond
      ((empty? keys-and-values) alist)
      ((empty? (rest keys-and-values)) (error "badly formed arguments."))
      (t (let ((key (first keys-and-values))
	       (value (second keys-and-values)))
	   (rec
	    (drop keys-and-values 2)
	    (alist-set alist key value)))))))

(define (alist . keys-and-values)
  "Constructs an alist from pairs of key value ..."
  (apply #'alist-set* () keys-and-values))

(define (disjoin* predicates)
  "Return a predicate equivalent to predicates joined together with or."
  (λ (x)
    (let rec ((result nil)
	      (predicates predicates))
      (if (or result (null? predicates))
	  result
	  (rec [(first predicates) x]
	       (rest predicates))))))
(define (disjoin . predicates)
  "Return a predicate equivalent to predicates joined together with or."
  (disjoin* predicates))

(assert (equal (map (disjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(t t nil t)))


(define (conjoin* predicates)
  "Return a predicate equivalent to predicates joined together with and."
  (λ (x)
    (let rec ((result t)
	      (predicates predicates))
      (if (or (not result) (null? predicates))
	  result
	  (rec [(first predicates) x]
	       (rest predicates))))))
(define (conjoin . predicates)
  "Return a predicate equivalent to predicates joined together with and."
  (conjoin* predicates))

(assert (equal (map (conjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(nil t nil nil)))

(define (const v)
  "Return a procedure of 0+ args that always returns v"
  (λ args
    (declare (ignore args))
    v))

(assert (= [(const 3) 1 2 3] 3))

(defmacro nand (&rest expressions)
  "The same as (not (and expressions...))"
  `(not (and ,@expressions)))

(defmacro nor (&rest expressions)
  "The same as (not (or expressions...))"
  `(not (or ,@expressions)))

(define (xor b1 b2)
  "Logical xor of booleans b1 and b2."
  (or (and b1 (not b2))
      (and b2 (not b1))))

(define (quotient n m)
  "Trunacate n/m"
  (truncate n m))

(define (number->string number (radix 10))
  "Convert number to string using radix as the base."
  (let ((*print-base* radix))
    (format nil "~S" number)))

(define (degrees->radians deg)
  "convert degrees to radians"
  (/ (* pi deg) 180))
(define (radians->degrees rads)
  "convert radians to degrees."
  (/ (* 180 rads) pi))

(define (sqr n)
  "n*n"
  (* n n))

(define (sgn x)
  "Return the sign of x: 1,-1, or 0"
  (cond
    ((positive? x) 1)
    ((negative? x) -1)
    ((zero? x) 0)))


(define (set-member? set value)
  "True if value is a member of set."
  (member value set :test #'equal?))
(define (set-add set value)
  "Adds value to set."
  (if (set-member? set value)
      set
      (cons value set)))
(define (set-remove set value)
  "Removes value from set."
  (remove set value :test #'equal?))
(define (set-empty? set)
  "True if set is empty."
  (empty? set))
(define (set-count set)
  "Number of elements in set."
  (length set))
(define (set->stream set)
  "Returns the elements of set as a stream."
  (list->stream set))
(define (set-union . sets)
  "Returns the union of all sets."
  (foldl (λ (set result)
	   (union set result :test #'equal?))
	 ()
	 sets))
(define (set-intersect set . sets)
  "Return a set with all elements in set that are also in all sets."
  (foldl (λ (set result)
	   (intersection result set :test #'equal?))
	 set
	 sets))
(define (set-subtract set . sets)
  "Return a set with all elements in set that are not in any of sets."
  (foldl (λ (set result)
	   (set-difference result set :test #'equal?))
	 set
	 sets))
(define (subset? set1 set2)
  "True if set1 is a subset of set2"
  (set-empty? (set-subtract set1 set2)))
(define (set=? set1 set2)
  "True if set1 = set2"
  (and (subset? set1 set2)
       (subset? set2 set1)))

(for-macros
  (define (symbol? datum) (symbolp datum)))

(for-macros
  (define (and-let*-form clauses body)
    (cond
      ((empty? clauses) `(progn ,@body))
      (t
       (let ((clause (first clauses)))
	 (cond
	   ((list? clause)
	    (cond
	      ((null? (rest clause))
	       `(and ,(first clause) ,(and-let*-form (rest clauses) body)))
	      (t
	       `(let ((,(first clause) ,(second clause)))
		  (and ,(first clause) ,(and-let*-form (rest clauses) body))))))
	   (t (error "invalid clause in and-let*: ~S" clause))))))))

(defmacro and-let* ((&rest clauses) &body body)
  "Evaluate each clause from first to last until one is false. If all are true, evaluate body.
Each clause is one of: identifier, (expression), or (identifier expression).
If the clause is (identifier expression) it creates a binding for the rest of the clauses and the body.
Example (and-let* ((list (compute-list))
                   ((pair? list))
                   (item (car list))
                   ((integer? item)))
          (sqrt item))"
  (and-let*-form clauses body))

(define (make-struct-info type-name super-type-name field-names)
  (alist :type-name type-name
	 :super-type-name super-type-name
	 :field-names field-names))

(define (struct-info-type-name si) (alist-ref si :type-name))
(define (struct-info-super-type-name si) (alist-ref si :super-type-name))
(define (struct-info-field-names si) (alist-ref si :field-names))

(defvar *struct-info-table*
  (make-hash-table :test #'eq)
  "Hash Table from structure type-name->struct-info")

(define (get-struct-info type-name)
  (gethash type-name *struct-info-table* nil))
(define (set-struct-info! info)
  (let* ((type-name (struct-info-type-name info))
	 (existing-info (get-struct-info type-name)))
    (when (and existing-info
	       (or (not (equal? (struct-info-super-type-name info)
				(struct-info-super-type-name existing-info)))
		   (not (equal? (struct-info-field-names info)
				(struct-info-field-names existing-info)))))
      (warn "Modifying structure ~S. Any sub-classed structures need to be recompiled." type-name))
    (setf (gethash type-name *struct-info-table*) info)))

(define (struct-info-ancestor-fields info)
  "Returns an alist of ((ancestor . fields) ... (parent . fields) (me . fields)) From oldest generation to youngest."
  (let ((super-type-name (struct-info-super-type-name info)))
    (cond
      ((null? super-type-name)
       (list (cons (struct-info-type-name info) (struct-info-field-names info))))
      (t
       (let ((super-struct-info (get-struct-info super-type-name)))
	 (cond
	   ((null? super-struct-info)
	    (error "The super type ~S does not exist in the *struct-info-table*" super-type-name))
	   (t  (append
		(struct-info-ancestor-fields super-struct-info)
		(list (cons (struct-info-type-name info) (struct-info-field-names info)))))))))))

(define (ancestor-fields->field-names ancestor-fields)
  (append-map 'cdr ancestor-fields))
(define (ancestor-fields->slot-names ancestor-fields)
  (append* (alist-map ancestor-fields
		      (λ (type-name field-names)
			(struct-defclass-slot-names type-name field-names)))))

(let ((*struct-info-table* (make-hash-table :test #'eq)))
  (set-struct-info! (make-struct-info 'grandpa () '(father)))
  (set-struct-info! (make-struct-info 'father 'grandpa '(son)))
  (set-struct-info! (make-struct-info 'son 'father '(grandpa)))

  (let ((ancestor-fields (struct-info-ancestor-fields (get-struct-info 'son))))
    (assert (equal? (ancestor-fields->field-names ancestor-fields)
		    '(father son grandpa)))
    (assert (equal? (ancestor-fields->slot-names ancestor-fields)
		    '(grandpa-father father-son son-grandpa)))))


(define (parse-struct-field-spec field-spec)
  (cond
    ((symbol? field-spec) (cons field-spec :immutable))
    ((and (pair? field-spec)
	  (symbol? (first field-spec)))
     (cond
       ((equal? (rest field-spec) '(:mutable))
	(cons (first field-spec) :mutable))
       (t (error "Unknown field-option(s): ~S" (rest field-spec)))))
    (t (error "bad thing to be a field-spec: ~S" field-spec))))

(assert (equal (parse-struct-field-spec 'field-name)
	       '(FIELD-NAME . :IMMUTABLE)))
(assert (equal (parse-struct-field-spec '(field-name :mutable))
	       '(FIELD-NAME . :MUTABLE)))

(define (parse-struct-options struct-options)
  (cond
    ((empty? struct-options) ())
    (t
     (let ((opt (first struct-options)))
       (cond
	 ((or (eq? :transparent opt)
	      (eq? :mutable opt))
	  (cons (cons opt ()) (parse-struct-options (rest struct-options))))
	 ((or (eq? :guard opt)
	      (eq? :super opt))
	  (cond
	    ((or (null? (rest struct-options))
		 (keywordp (second struct-options)))
	     (error "Expected form for struct-option ~S" opt))
	    (t
	     (cons (cons opt (eval (second struct-options))) (parse-struct-options (cddr struct-options))))))
	 (t (error "Bad thing to be a struct-option ~S" opt)))))))


#+nil(assert (equal (parse-struct-options '(:transparent :mutable :guard (lambda (x y z) (values x y z)) :super 'point))
		    '((:TRANSPARENT) (:MUTABLE)
		      (:GUARD Λ NIL
		       (LAMBDA (X Y Z)
			 (VALUES X Y Z)))
		      (:SUPER Λ NIL 'POINT))))

(define (string-append . strings)
  (apply 'concatenate 'string strings))

(define (struct-constructor-name type-name)
  (intern (string-append (symbol->string 'make-) (symbol->string type-name))))

(assert (eq? (struct-constructor-name 'point)
	     'make-point))

(define (struct-defclass-slot-names type-name field-names)
  (map (λ (field-name)
	 (intern (string-append (symbol->string type-name) "-" (symbol->string field-name))))
       field-names))

(assert (equal? (struct-defclass-slot-names 'point '(x y))
		'(point-x point-y)))

(defclass struct () ())
(define (struct? datum)
  (typep datum 'struct))
(defgeneric struct-copy (struct)
  (:documentation "Returns a shallow copy of struct."))
(defmethod struct-copy (struct)
  (error "Struct ~S is not a known structure type." struct))
(defgeneric struct->list (transparent-struct)
  (:documentation "Returns a list of the form '(constructor-name field-values) for the transparent structure."))
(defmethod struct->list (struct)
  (error "Struct ~S is not a transparent structure." struct))
(defgeneric struct-accessors (transparent-struct)
  (:documentation "Returns a list of accessor symbols for the transparent structure."))
(defmethod struct-accessors (struct)
  (error "Struct ~S is not a transparent structure." struct))

(define (struct-defclass-form type-name field-names super-type-name)
  (let ((supers (cond ((null? super-type-name) '(struct))
		      (t `(,super-type-name)))))
    `(defclass ,type-name ,supers
       ,(struct-defclass-slot-names type-name field-names))))

(assert (equal? (struct-defclass-form 'point '(x y) ())
		'(DEFCLASS POINT (struct) (point-x point-y))))

(assert (equal? (struct-defclass-form 'point3 '(z) 'point)
		'(DEFCLASS POINT3 (point) (point3-z))))

(define (struct-define-constructor-form type-name constructor-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-field-names (ancestor-fields->field-names ancestor-fields))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(define (,constructor-name ,@(append super-field-names field-names))
       (let ((struct (make-instance ',type-name)))
	 ,@(map (λ (slot-name value-name)
		  `(setf (slot-value struct ',slot-name) ,value-name))
		super-slot-names
		super-field-names)
	 ,@(map (λ (slot-name value-name)
		  `(setf (slot-value struct ',slot-name) ,value-name))
		(struct-defclass-slot-names type-name field-names)
		field-names)
	 struct))))

(assert (equal? (struct-define-constructor-form 'point 'make-point '(x y) '())
		'(DEFINE (MAKE-POINT X Y)
		  (LET ((STRUCT (MAKE-INSTANCE 'POINT)))
		    (SETF (SLOT-VALUE STRUCT 'POINT-X) X)
		    (SETF (SLOT-VALUE STRUCT 'POINT-Y) Y)
		    STRUCT))))

(assert (equal? (struct-define-constructor-form 'point3 'make-point3 '(z) 'point)
		'(DEFINE (MAKE-POINT3 X Y Z)
		  (LET ((STRUCT (MAKE-INSTANCE 'POINT3)))
		    (SETF (SLOT-VALUE STRUCT 'POINT-X) X)
		    (SETF (SLOT-VALUE STRUCT 'POINT-Y) Y)
		    (SETF (SLOT-VALUE STRUCT 'POINT3-Z) Z)
		    STRUCT))))

(define (struct-define-struct-copy-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod struct-copy ((struct ,type-name))
       (let ((copy (make-instance ',type-name)))
	 ,@(map (λ (slot-name)
		  `(setf (slot-value copy ',slot-name) (slot-value struct ',slot-name)))
		super-slot-names)
	 ,@(map (λ (slot-name)
		  `(setf (slot-value copy ',slot-name) (slot-value struct ',slot-name)))
		(struct-defclass-slot-names type-name field-names))
	 copy))))

(define (struct-define-struct->list-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod struct->list ((struct ,type-name))
       (list
	',(struct-constructor-name type-name)
	,@(map (λ (slot-name) `(slot-value struct ',slot-name)) super-slot-names)
	,@(map (λ (slot-name) `(slot-value struct ',slot-name)) (struct-defclass-slot-names type-name field-names))))))

(define (struct-define-accessor-form type-name slot-name)
  `(define (,slot-name ,type-name)
     (slot-value ,type-name ',slot-name)))

(assert (equal? (struct-define-accessor-form 'point 'point-x)
		'(DEFINE (POINT-X POINT)
		  (SLOT-VALUE POINT 'POINT-X))))

(define (struct-define-struct-accessors-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod struct-accessors ((struct ,type-name))
       '(,@(append
	    (map (λ (slot-name) slot-name) super-slot-names)
	    (map (λ (slot-name) slot-name) (struct-defclass-slot-names type-name field-names)))))))

(define (struct-define-equal?-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod equal? ((object1 ,type-name) (object2 ,type-name))
       (and 
	,@(append
	   (map (λ (slot-name)
		  `(equal? (slot-value object1 ',slot-name)
			   (slot-value object2 ',slot-name)))
		super-slot-names)
	   (map (λ (slot-name)
		  `(equal? (slot-value object1 ',slot-name)
			   (slot-value object2 ',slot-name)))
		(struct-defclass-slot-names type-name field-names)))))))

(define (struct-define-print-object-form type-name)
  `(defmethod print-object ((struct ,type-name) stream)
     (print-object (struct->list struct) stream)))


(define (struct-define-type-predicate-form type-name)
  (let ((predicate-name (intern (string-append (symbol->string type-name) "?"))))
    `(define (,predicate-name datum)
       (typep datum ',type-name))))

(assert (equal? (struct-define-type-predicate-form 'point)
		'(DEFINE (POINT? DATUM)
		  (TYPEP DATUM 'POINT))))

(define (struct-form type-name field-specs struct-options)
  (let* ((parsed-field-specs (map 'parse-struct-field-spec field-specs))
	 (field-names (map 'car parsed-field-specs))
	 (slot-names (struct-defclass-slot-names type-name field-names))
	 (parsed-struct-options (parse-struct-options struct-options))
	 (super-type-name (alist-ref parsed-struct-options :super nil)))
    `(progn
       (set-struct-info! (make-struct-info ',type-name ',super-type-name ',field-names))
       ,(struct-defclass-form type-name field-names super-type-name)
       ,(struct-define-struct-copy-form type-name field-names super-type-name)
       ,@(cond ((alist-has-key? parsed-struct-options :transparent)
		(list
		 (struct-define-struct->list-form type-name field-names super-type-name)
		 (struct-define-struct-accessors-form type-name field-names super-type-name)
		 (struct-define-equal?-form type-name field-names super-type-name)
		 (struct-define-print-object-form type-name)))
	       (t ()))
       ,(struct-define-constructor-form type-name (struct-constructor-name type-name)
					field-names
					super-type-name)
       ,@(map (λ (slot-name) (struct-define-accessor-form type-name slot-name))
	      slot-names)
       ,(struct-define-type-predicate-form type-name))))

(struct-form 'point '(x y) '())
(struct-form 'point3 '(z) '(:super 'point))
(struct-form 'tpoint '(x y) '(:transparent))

(defmacro define-struct (type-name (&rest field-specs) &rest struct-options)
  (struct-form type-name field-specs struct-options))

;; Struct form
'(define-struct <type-name>
  (field-specs...)
  struct-options...)

(define (string-starts-with? string sub-string)
  (string= (subseq string 0 (length sub-string))
	   sub-string))

(define-struct point (x y))
(let ((p (make-point 3 4)))
  (assert (equal? (list (point-x p)	   ;; 3
			(point-y p)	   ;; 4
			(point? p)	   ;; t
			(struct? p)	   ;; t
			(not (equal? (make-point 3 4) (make-point 3 4))) ;; t
			(not (equal? (struct-copy p) p)) ;; t
			(equal? p p))  ;; t
		  (list 3 4 t t t t t)))
  ;; #<POINT...>
  (assert (equal? "#<POINT" (subseq (format nil "~S" p) 0 (length "#<POINT")))))

;; Super types
(define-struct point3d (z)
	       :super 'point)
(let ((p3d (make-point3d 3 4 5)))
  (assert (and-let* ((copy (struct-copy p3d))
		     ((equal? (point-x p3d) (point-x copy)))
		     ((equal? (point3d-z p3d) (point3d-z copy))))
	    (not (equal? p3d copy))))
  (assert (equal? (list (point? p3d)	;; t
			(point3d? p3d)	;; t
			(point-x p3d)	;; 3
			(point-y p3d)	;; 4
			(point3d-z p3d) ;; 5
			(and-let* ((copy (struct-copy p3d))
				   ((equal? (point-x p3d) (point-x copy)))
				   ((equal? (point3d-z p3d) (point3d-z copy))))
			  (not (equal? p3d copy))) ;;t
			(string-starts-with? (format nil "~S" p3d) "#<POINT3D")) ;; #<struct point3d>
		  (list t t 3 4 5 t t))))

;; Super-duper types
(define-struct point4d (w)
	       :super 'point3d)
(let ((p4d (make-point4d 'x 'y 'z 'w)))
  (assert [(conjoin 'struct? 'point? 'point3d? 'point4d?) p4d])
  (list (point-x p4d)
	(point-y p4d)
	(point3d-z p4d)
	(point4d-w p4d)))

;; Transparent structures
(define-struct tpoint (x y) :transparent)
(let ((p (make-tpoint 3 4)))
  (assert (every 'identity
		 (list
		  (equal? (struct->list p) '(make-tpoint 3 4))
		  (equal? (struct-accessors p) '(tpoint-x tpoint-y))
		  (string= (format nil "~S" '(make-tpoint 3 4)) (format nil "~S" p))
		  (equal? p p)
		  (equal? (make-tpoint 3 4) (make-tpoint 3 4))))))

;; Mutable structures
(struct mpoint (x y) :mutable :transparent)
(let ((p (make-mpoint 3 4)))
  (setf (mpoint-x p) 5)
  (setf (mpoint-y p) 6)
  (print p) ;; (make-mpoint 5 6)
  )

;; Mutable fields
(struct mpoint3 (x y (z :mutable)) :transparent)
(let ((p (make-mpoint3 3 4 5)))
  (setf (mpoint3-z p) 20)
  (print p) ;; (make-mpoint3 3 4 20)
  )

;; Guard-expressions
(struct ipoint (x y)
	:guard (λ (x y)
		 (if (not (and (integerp x) (integerp y)))
		     (error "ipoints require integer arguments. got: X=~S Y=~S" x y)
		     (values x y))))

;;(for-macros (uninstall-syntax!))


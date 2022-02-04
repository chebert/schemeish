(in-package #:schemeish.backend)

(install-syntax!)

(for-macros
  (defvar *transform-scm-special-form-table* (make-hash-table)))
(define (register-transform-scm-special-form symbol transform)
  (hash-set! *transform-scm-special-form-table* symbol transform))

(defmacro define-scm-special-transform (name (transformer expression environment) &body body)
  `(for-macros
     (register-transform-scm-special-form ',name (lambda (,transformer ,expression ,environment)
					       (declare (ignorable ,transformer ,expression ,environment))
					       ,@body))))


(defmacro lisp (form)
  "Within SCM, escapes form and evaluates it as if it were in Common-Lisp.
Within LISP, just evalutes form."
  form)

(define (transform* transformer forms)
  "Map transform across forms."
  (map (lcurry #'transform transformer) forms))

(define (transform-ordinary-lambda-list transformer ordinary-lambda-list)
  "Transform the bindings in ordinary-lambda-list."
  (map-ordinary-lambda-list
   (lambda (group parameter)
     (ecase group
       ((:keyword :positional :rest) parameter)
       ((:optional :key :aux)
	(cond
	  ((pair? parameter)
	   (ecase (length parameter)
	     (1 parameter)
	     (2 (list (first parameter) (transform transformer (second parameter))))
	     (3 (list (first parameter) (transform transformer (second parameter)) (third parameter)))))
	  (t parameter)))))
   ordinary-lambda-list))

(define (transform-let-binding transformer binding)
  (if (pair? binding)
      (list (first binding) (transform transformer (second binding)))
      (list binding ())))

(define (transform-function-binding transformer binding)
  `(,(function-binding-name binding) ,(transform-ordinary-lambda-list transformer (function-binding-parameters binding))
    ,@(transform transformer `(lexically ,@(function-binding-body binding)))))

(define (transform-lexical-body transformer body)
  (multiple-value-bind (body declarations names set-forms) (parse-lexical-body body)
    (let ((body (append declarations (transform* transformer body))))
      (if names
	  `((cl:let ,names
	      ,@(transform* transformer set-forms)
	      (cl:let ,(mapcar (cl:lambda (name) (list name name)) names)
		,@body)))
	  body))))

(define (variable? expr environment)
  (and (symbol? expr)
       #+sbcl
       (sb-cltl2:variable-information expr environment)
       #-sbcl
       (error "TODO: Don't know how to get the variable-information of ~S in ~S for lisp implementation: ~S"
	      expr
	      environment
	      (lisp-implementation-type))))

(define-scm-special-transform cl:progn (transformer expr env)
  `(cl:progn ,@(transform-lexical-body transformer (progn-forms expr))))
(define-scm-special-transform cl:lambda (transformer expr env)
  `(cl:lambda ,(transform-ordinary-lambda-list transformer (lambda-parameters expr))
     ,@(transform* transformer (lambda-body expr))))

(define-scm-special-transform lexically (transformer expr env)
  `(lisp ,@(transform-lexical-body transformer (rest expr))))

(define-scm-special-transform cl:block (transformer expr env)
  `(cl:block ,(block-name expr)
     ,@(transform-lexical-body transformer (block-body expr))))
(define-scm-special-transform cl:return-from (transformer expr env)
  `(cl:return-from ,(return-from-name expr) ,(transform transformer (return-from-value expr))))

(define-scm-special-transform cl:tagbody (transformer expr env)
  (define parsed (parse-tagbody (rest expr)))
  (define (transform-tag-and-forms tag-and-forms)
    (cons (first tag-and-forms)
	  (transform* transformer (rest tag-and-forms))))
  (define untagged-forms (transform* transformer (first parsed)))
  (define tags-and-forms (append-map transform-tag-and-forms (rest parsed)))
  `(cl:tagbody
      ,@untagged-forms
      ,@tags-and-forms))
(define-scm-special-transform cl:go (transformer expr env)
  expr)
(define-scm-special-transform cl:quote (transformer expr env)
  expr)
(define-scm-special-transform cl:function (transformer expr env)
  expr)
(define-scm-special-transform cl:declare (transformer expr env)
  expr)

(define-scm-special-transform cl:let (transformer expr env)
  (define bindings (map (lambda (binding) (transform-let-binding transformer binding)) (let-bindings expr)))
  `(cl:let ,bindings
     ,@(transform-lexical-body transformer (let-body expr))))
(define-scm-special-transform cl:let* (transformer expr env)
  (define bindings (map (lambda (binding) (transform-let-binding transformer binding)) (let*-bindings expr)))
  `(cl:let* ,bindings
     ,@(transform-lexical-body transformer (let*-body expr))))

(define-scm-special-transform cl:labels (transformer expr env)
  (define bindings (map (lcurry #'transform-function-binding transformer)
			(labels-bindings expr)))
  `(cl:labels ,bindings
     ,@(transform-lexical-body transformer (labels-body expr))))
(define-scm-special-transform cl:flet (transformer expr env)
  (define bindings (map (lcurry #'transform-function-binding transformer)
			(flet-bindings expr)))
  `(cl:flet ,bindings
     ,@(transform-lexical-body transformer (flet-body expr))))
(define-scm-special-transform cl:macrolet (transformer expr env)
  `(cl:macrolet ,(macrolet-bindings expr)
     ,@(transform-lexical-body transformer (macrolet-body expr))))
(define-scm-special-transform cl:symbol-macrolet (transformer expr env)
  `(cl:symbol-macrolet ,(symbol-macrolet-bindings expr)
     ,@(transform-lexical-body transformer (symbol-macrolet-body expr))))
(define-scm-special-transform cl:throw (transformer expr env)
  `(cl:throw ,(transform transformer (throw-tag expr))))
(define-scm-special-transform cl:catch (transformer expr env)
  `(cl:catch ,(transform transformer (catch-tag expr))
     ,@(transform-lexical-body transformer (catch-forms expr))))
(define-scm-special-transform cl:unwind-protect (transformer expr env)
  `(cl:unwind-protect ,(transform transformer (unwind-protect-protected expr))
     ,@(transform-lexical-body transformer (unwind-protect-cleanup expr))))
(define-scm-special-transform cl:load-time-value (transformer expr env)
  `(cl:load-time-value ,(transform transformer (load-time-value-form expr))
		       ,(transform transformer (load-time-value-read-only-p expr))))
(define-scm-special-transform cl:eval-when (transformer expr env)
  `(cl:eval-when ,(eval-when-situations expr)
     ,@(transform-lexical-body transformer (eval-when-forms expr))))
(define-scm-special-transform cl:multiple-value-prog1 (transformer expr env)
  `(cl:multiple-value-prog1 ,(transform transformer (multiple-value-prog1-values-form expr))
     ,@(transform-lexical-body transformer (multiple-value-prog1-forms expr))))
(define-scm-special-transform cl:multiple-value-call (transformer expr env)
  `(cl:multiple-value-call ,(transform transformer (multiple-value-call-function expr))
     ,@(transform* transformer (multiple-value-call-arguments expr))))
(define-scm-special-transform cl:the (transformer expr env)
  `(cl:the ,(the-value-type expr) ,(transform transformer (the-form expr))))
(define-scm-special-transform cl:if (transformer expr env)
  `(cl:if ,(transform transformer (if-test expr))
	  ,(transform transformer (if-then expr))
	  ,(transform transformer (if-else expr))))
(define-scm-special-transform cl:setq (transformer expr env)
  (define (transform-pair pair)
    (define name (first pair))
    (define value (second pair))
    (list name (transform transformer value)))
  (define pairs (append-map transform-pair (setq-pairs expr)))
  `(setq ,@pairs))

(define-scm-special-transform lisp (transformer expr env)
  (second expr))

(define (transform-proper-list transformer expr env)
  (define fn (first expr))
  (if (or (pair? fn) (variable? fn env))
      `(cl:funcall ,@(transform* transformer expr))
      `(,fn ,@(transform* transformer (rest expr)))))
(define (transform-cyclic-list transformer expr env)
  (declare (ignorable transformer expr env))
  (error "Detected cyclic list."))
(define (transform-dotted-list transformer expr env)
  (declare (ignorable env))
  (define (recurse expr)
    (cond
      ((pair? (rest expr))
       (cons (transform transformer (first expr))
	     (recurse (rest expr))))
      (t (list (transform transformer (first expr)) (transform transformer (rest expr))))))
  `(cl:apply ,@(recurse expr)))

(define (transform-atom transformer expr env)
  (declare (ignorable transformer env))
  (cond
    ((not (symbol? expr)) expr)
    ((and (variable? expr env)
	  ;; Special case: if a a function happens to be named the same as a special/parameter choose the function.
	  (not (and (parameter? expr env)
		    (fboundp expr))))
     expr)
    (t `(function ,expr))))

(for-macros
  (defparameter *scm-transformer*
    (make-transformer *transform-scm-special-form-table*
		      'transform-proper-list
		      'transform-dotted-list
		      'transform-cyclic-list
		      'transform-atom)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (lambda args args))
			      (args '(1 2 3)))
			 (f . args))))

		'(1 2 3)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (lambda args args))
			      (args '(1 2 3)))
			 (list* (list 1 2 3) (f . args)))))
		'((1 2 3) 1 2 3)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (lambda args (+ . args)))
			      (args '(1 2 3)))
			 (list* (+ 1 2 3) (f . args)))))
		'(6 . 6)))


(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ()
			 (define a 1)
			 (define copy (lambda (x)
					(if (empty? x)
					    ()
					    (cons (first x) (copy (rest x))))))

			 (copy (list a a a)))))
		'(1 1 1)))

(defmacro scm (&body body &environment environment)
  "Evaluates body in the SCM langauge. Similar to Common Lisp with the following changes:
If an expression is a proper list, it is transformed into (funcall function args).
If an expression is a dotted list, it is transformed into (apply function args... rest-arg)
If an expression is a symbol, its value is looked up in the variable environment at macro-expansion-time.
If not found, it is assumed to be in the function-namespace.
The following symbols from the COMMON-LISP package are assumed to be functions rather than parameters: (+ ++ +++ * ** *** - / // ///)
A form (LISP form) will escape SCM and process form as if it is in Common-Lisp.
E.g. (SCM (LISP +)) => the last evaluated repl form
     (SCM +) => #'+
DEFINE forms may appear at the top of a lexical body, and are gathered together converted into a LETREC."
  (transform-expression *scm-transformer* `(progn ,@body) environment))

(assert (equal? (scm
		  (define a 1)
		  (define b (+ a 1))
		  (define c (+ b 1))
		  (define (reverse x)
		    (if (empty? x)
			()
			(append (reverse (rest x)) (list (first x)))))
		  (declare (ignore b))
		  (progn
		    (define b 'b)
		    (reverse (list c b a))))

		'(1 b 3)))

(assert (equal? (scm
		  (define (((nested a) b) . c) (list* a b c))
		  (((nested 1) 2) 3 4 5))
		'(1 2 3 4 5)))

(defmacro def (name-field &body body)
  `(scm
     (def ,name-field ,@body)
     (expose-functions ,(schemeish.internals::definition-name-field->name name-field))))

(def 2+ (lcurry + 2))
(def (((test-nested a) b) . c)
  #g((not (list? b)))
  (list* a b c))

(scm (documentation test-nested t))
"

Parameters: (A)
Definition Form: (((TEST-NESTED A) B) . C)

TEST-NESTED has the following guard clauses:
((NOT (LIST? B)))"
(scm (documentation (test-nested 1) t))
"

Parameters: (B)
Definition Form: (((TEST-NESTED A) B) . C)

TEST-NESTED has the following guard clauses:
((NOT (LIST? B)))"
(scm (documentation ((test-nested 1) 2) t))
"

Parameters: C
Definition Form: (((TEST-NESTED A) B) . C)

TEST-NESTED has the following guard clauses:
((NOT (LIST? B)))"
(assert (equal? (scm (((test-nested 1) 2) 3 4 5))
		'(1 2 3 4 5)))


(def (test-fact n)
  #d"Test for a a factorial using DEF."
  #g((not (negative? n)))
  (def (iter n result)
    #d"Iterates from n to 0."
    (cond
      ((zero? n) result)
      (t (iter (1- n) (* result n)))))
  (values (iter n 1) (documentation iter t)))


(assert (equal? (test-fact 4) 24))
(scm (documentation test-fact t))
"Test for a a factorial using DEF.

Parameters: (N)
Definition Form: (TEST-FACT N)

TEST-FACT has the following guard clauses:
((NOT (NEGATIVE? N)))"

(scm (registered-definition-name-field test-fact))
#||
=>
(TEST-FACT N)
T

||#

(assert (equal? (scm
		  (define-values (a b c) (values 1 2 3))
		  (define x 'x)
		  (define-values (d e f) (values 4 5 6))
		  (define (y) 'y)
		  (define-values g (values 7 8 9))
		  (list* x (y) a b c d e f g))
		'(X Y 1 2 3 4 5 6 7 8 9)))

(scm
  (define-values (a _ c) (values 1 2 3))
  (list a c))

(uninstall-syntax!)

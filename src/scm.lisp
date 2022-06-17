(in-package #:schemeish.backend)

(install-syntax!)

(for-macros
  (defvar *transform-scm-special-form-table* (make-hash-table)))
(define (register-transform-scm-special-form symbol transform)
  (hash-set! *transform-scm-special-form-table* symbol transform))

(defmacro define-scm-special-transform (name (expression environment) &body body)
  "Define a special form transformer for the SCM macro-expansion.
Name is the symbol naming the special-form. Transformer will be bound to the *SCM-TRANSFORMER*.
Expression will be bound to the special form being transformed, and environment will be bound to the current lexical environment
for body. Body should evaluate to the transformed form."
  (let ((transformer (unique-symbol 'transformer)))
    `(for-macros
       (register-transform-scm-special-form ',name (lambda (,transformer ,expression ,environment)
						     (declare (ignore ,transformer))
						     (declare (ignorable ,expression ,environment))
						     ,@body)))))

(export-definition
  (defmacro no-scm (form)
    "Within SCM, escapes form and evaluates it as if it were in Common-Lisp.
Within LISP, just evalutes form."
    form))

(for-macros
  (defvar *scm-transformer*))

(define (transform-scm expr)
  `(no-scm ,(transform 'scm expr)))

(define (transform* transformer forms)
  "Map transform across forms."
  (map (lcurry #'transform transformer) forms))

(define (transform-scm* forms)
  (transform* 'scm forms))

(define (transform-ordinary-lambda-list ordinary-lambda-list)
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
	     (2 (list (first parameter) (transform-scm (second parameter))))
	     (3 (list (first parameter) (transform-scm (second parameter)) (third parameter)))))
	  (t parameter)))))
   ordinary-lambda-list))

(define (transform-let-binding binding)
  (if (pair? binding)
      (list (first binding) (transform-scm (second binding)))
      (list binding ())))

(define (transform-function-binding binding)
  `(,(function-binding-name binding) ,(transform-ordinary-lambda-list (function-binding-parameters binding))
    ,@(transform-lexical-body (function-binding-body binding))))

(define (transform-lexical-body body)
  (multiple-value-bind (body declarations names set-forms) (parse-lexical-body body)
    (let ((body (transform-scm* body)))
      (if names
	  `((cl:let ,names
	      ,@declarations
	      ,@(transform-scm* set-forms)
	      ,@body))
	  (append declarations body)))))

(define (variable? expr environment)
  "Return true if EXPR is a variable in the given environment."
  (and (symbol? expr)
       (trivial-cltl2:variable-information expr environment)))

;; IF a CPS macro is encountered, we want it to be processed after the SCM transformation has occuurred
(define-scm-special-transform cps (expr env)
  (define-destructuring (cps-expr) (rest expr))
  `(cps ,(transform-scm cps-expr)))
(define-scm-special-transform no-cps (expr env)
  (define-destructuring (no-cps-expr) (rest expr))
  `(no-cps ,(transform-scm no-cps-expr)))
(define-scm-special-transform fcontrol (expr env)
  (define-destructuring (tag value) (rest expr))
  `(fcontrol ,(transform-scm tag) ,(transform-scm value)))
(define-scm-special-transform % (expr env)
  (define-destructuring (cps-expr &key (tag nil tag-provided?) (handler nil handler-provided?)) (rest expr))
  `(% ,(transform-scm cps-expr) ,@(when tag-provided? `(:tag ,(transform-scm tag))) ,@(when handler-provided? `(:handler ,(transform-scm handler)))))

(define-scm-special-transform cl:progn (expr env)
  `(cl:progn ,@(transform-lexical-body (progn-forms expr))))
(define-scm-special-transform cl:lambda (expr env)
  `(cl:lambda ,(transform-ordinary-lambda-list (lambda-parameters expr))
     ,@(transform-lexical-body (lambda-body expr))))

(define-scm-special-transform lexically (expr env)
  `(no-scm (cl:progn ,@(transform-lexical-body (rest expr)))))

(define-scm-special-transform cl:block (expr env)
  `(cl:block ,(block-name expr)
     ,@(transform-lexical-body (block-body expr))))
(define-scm-special-transform cl:return-from (expr env)
  `(cl:return-from ,(return-from-name expr) ,(transform-scm (return-from-value expr))))

(define-scm-special-transform cl:tagbody (expr env)
  (define parsed (parse-tagbody (rest expr)))
  (define (transform-tag-and-forms tag-and-forms)
    (cons (first tag-and-forms)
	  (transform-scm* (rest tag-and-forms))))
  (define untagged-forms (transform-scm* (first parsed)))
  (define tags-and-forms (append-map transform-tag-and-forms (rest parsed)))
  `(cl:tagbody
      ,@untagged-forms
      ,@tags-and-forms))
(define-scm-special-transform cl:go (expr env)
  expr)
(define-scm-special-transform cl:quote (expr env)
  expr)
(define-scm-special-transform cl:function (expr env)
  expr)
(define-scm-special-transform cl:declare (expr env)
  expr)

(define (transform-scm-let expr env)
  (declare (ignorable env))
  (define bindings (map (lambda (binding) (transform-let-binding binding)) (let-bindings expr)))
  `(cl:let ,bindings
     ,@(transform-lexical-body (let-body expr))))
(define-scm-special-transform cl:let (expr env)
  (transform-scm-let expr env))
(define-scm-special-transform cl:let* (expr env)
  (define bindings (map (lambda (binding) (transform-let-binding binding)) (let*-bindings expr)))
  `(cl:let* ,bindings
     ,@(transform-lexical-body (let*-body expr))))
;; Named let without labels bindings
(define-scm-special-transform let (expr env)
  (cond
    ((and (not (null (second expr))) (symbol? (second expr)))
     ;; Named let
     (destructuring-bind (name bindings . body) (rest expr)
       ;; TODO: let-binding-name let-binding-value in code-transformer
       (let ((parameters (map (lambda (binding) (if (symbol? binding) binding (first binding))) bindings))
	     (arguments (map (lambda (binding) (if (symbol? binding) nil (second binding))) bindings)))
	 `(cl:progn ,@(transform-lexical-body
		       `((define (,name ,@parameters) ,@body)
			 (,name ,@arguments)))))))
    (t (transform-scm-let expr env))))

(define-scm-special-transform cl:labels (expr env)
  (define bindings (map #'transform-function-binding (labels-bindings expr)))
  `(cl:labels ,bindings
     ,@(transform-lexical-body (labels-body expr))))
(define-scm-special-transform cl:flet (expr env)
  (define bindings (map #'transform-function-binding (flet-bindings expr)))
  `(cl:flet ,bindings
     ,@(transform-lexical-body (flet-body expr))))
(define-scm-special-transform cl:macrolet (expr env)
  `(cl:macrolet ,(macrolet-bindings expr)
     ,@(transform-lexical-body (macrolet-body expr))))
(define-scm-special-transform cl:symbol-macrolet (expr env)
  `(cl:symbol-macrolet ,(symbol-macrolet-bindings expr)
     ,@(transform-lexical-body (symbol-macrolet-body expr))))
(define-scm-special-transform cl:throw (expr env)
  `(cl:throw ,(transform-scm (throw-tag expr)) ,(transform-scm (throw-result expr))))
(define-scm-special-transform cl:catch (expr env)
  `(cl:catch ,(transform-scm (catch-tag expr))
     ,@(transform-lexical-body (catch-forms expr))))
(define-scm-special-transform cl:unwind-protect (expr env)
  `(cl:unwind-protect ,(transform-scm (unwind-protect-protected expr))
     ,@(transform-lexical-body (unwind-protect-cleanup expr))))
(define-scm-special-transform cl:load-time-value (expr env)
  `(cl:load-time-value ,(transform-scm (load-time-value-form expr))
		       ,(transform-scm (load-time-value-read-only-p expr))))
(define-scm-special-transform cl:eval-when (expr env)
  `(cl:eval-when ,(eval-when-situations expr)
     ,@(transform-lexical-body (eval-when-forms expr))))
(define-scm-special-transform cl:multiple-value-prog1 (expr env)
  `(cl:multiple-value-prog1 ,(transform-scm (multiple-value-prog1-values-form expr))
     ,@(transform-lexical-body (multiple-value-prog1-forms expr))))
(define-scm-special-transform cl:multiple-value-call (expr env)
  `(cl:multiple-value-call ,(transform-scm (multiple-value-call-function expr))
     ,@(transform-scm* (multiple-value-call-arguments expr))))
(define-scm-special-transform cl:the (expr env)
  `(cl:the ,(the-value-type expr) ,(transform-scm (the-form expr))))
(define-scm-special-transform cl:if (expr env)
  `(cl:if ,(transform-scm (if-test expr))
	  ,(transform-scm (if-then expr))
	  ,(transform-scm (if-else expr))))
(define-scm-special-transform cl:setq (expr env)
  (define (transform-pair pair)
    (define name (first pair))
    (define value (second pair))
    (list name (transform-scm value)))
  (define pairs (append-map transform-pair (setq-pairs expr)))
  `(setq ,@pairs))

(define-scm-special-transform no-scm (expr env)
  expr)

(define (transform-proper-list transformer expr env)
  (declare (ignore transformer))
  (define fn (first expr))
  (if (or (pair? fn) (variable? fn env))
      `(cl:funcall ,@(transform-scm* expr))
      `(,fn ,@(transform-scm* (rest expr)))))
(define (transform-cyclic-list transformer expr env)
  (declare (ignorable transformer expr env))
  (error "Detected cyclic list."))
(define (transform-dotted-list transformer expr env)
  (declare (ignorable transformer env))
  (define (recurse expr)
    (cond
      ((pair? (rest expr))
       (cons (transform-scm (first expr))
	     (recurse (rest expr))))
      (t (list (transform-scm (first expr)) (transform-scm (rest expr))))))
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
  (register-transformer 'scm
			(make-transformer *transform-scm-special-form-table*
					  'transform-proper-list
					  'transform-dotted-list
					  'transform-cyclic-list
					  'transform-atom)))


(assert (equal? (eval (transform-scm
		       '(let ((f (lambda args args))
			      (args '(1 2 3)))
			 (f . args))))

		'(1 2 3)))

(assert (equal? (eval (transform-scm
		       '(let ((f (lambda args args))
			      (args '(1 2 3)))
			 (list* (list 1 2 3) (f . args)))))
		'((1 2 3) 1 2 3)))

(assert (equal? (eval (transform-scm
		       '(let ((f (lambda args (+ . args)))
			      (args '(1 2 3)))
			 (list* (+ 1 2 3) (f . args)))))
		'(6 . 6)))


(assert (equal? (eval (transform-scm
		       '(let ()
			 (define a 1)
			 (define copy (lambda (x)
					(if (empty? x)
					    ()
					    (cons (first x) (copy (rest x))))))

			 (copy (list a a a)))))
		'(1 1 1)))

(export-definition
  (defmacro scm (&body body)
    "Evaluates body in the SCM langauge. Similar to Common Lisp with the following changes:
If an expression is a proper list, it is transformed into (funcall function args).
If an expression is a dotted list, it is transformed into (apply function args... rest-arg)
If an expression is a symbol, its value is looked up in the variable environment at macro-expansion-time.
If expression is not a variable it is assumed to be in the function-namespace.
If a symbol is both SPECIAL and a bound FUNCTION, it is treated as a function rather than a variable.
  This is to deal with the unfortunate cases: (+ * / -).
  Typically functions and specials will not have the same name if the *EAR-MUFF* convention is followed.
 
A (no-scm form) will escape SCM and process form as if it is in Common-Lisp.
E.g. (SCM (no-scm +)) => the last evaluated repl form
     (SCM +) => #'+.
All forms with explicit/implicit blocks/progns are now lisp-1 style lexical-bodies.
For more details see (lexical-body-definition-documentations) for information about 
the available lexical-body-definition expansions.
These lexical-body's are the lisp-1 analogue to the lisp-2 style lexical-bodies defined by LEXICALLY."
    (transform-scm `(progn ,@body))))

(assert (equal? (scm
		  (define a 1)
		  (define b (+ a 1))
		  (define c (+ b 1))
		  (define (reverse x)
		    (if (empty? x)
			()
			(append (reverse (rest x)) (list (first x)))))
		  (progn
		    (define b 'b)
		    (reverse (list c b a))))

		'(1 b 3)))

(assert (equal? (scm
		  (define (((nested a) b) . c) (list* a b c))
		  (((nested 1) 2) 3 4 5))
		'(1 2 3 4 5)))

(for-macros
 (defvar *global-lexical-variable-table* (make-hash-table)))

(export-definition
  (defmacro define-global-lexical-variable (name &optional initial-value)
    `(progn
       (setf (gethash ',name *global-lexical-variable-table*) ,initial-value)
       (define-symbol-macro ,name (gethash ',name *global-lexical-variable-table*))
       ',name)))

(export-definition
  (define (global-lexical-bound? symbol)
    (def-values (_ bound?) (gethash symbol *global-lexical-variable-table*))
    bound?))
(export-definition
  (define (global-lexical-value symbol)
    (gethash symbol *global-lexical-variable-table*)))

(export-definition
  (defparameter *redefine-def-once* nil))

(export-definition
  (defmacro def (name-field &body body)
    "Defines a top-level function using SCM to transform name-field and body from a lisp-1 style.
Essentially expands to (scm (def name-field body...) (expose-functions name)).
The lisp-1 analogue to DEFINE.
See also SCM, EXPOSE."
    (let ((name (schemeish.internals::definition-name-field->name name-field)))
      `(for-macros
	 (fmakunbound ',name)
	 (scm
	   (def ,name-field ,@body)
	   (define-global-lexical-variable ,name ,name)
	   (when (functionp ,name)
	     (expose-functions ,name)))
	 ',name))))


(export-definition
  (defmacro def-once (name &body body)
    (if (and (not *redefine-def-once*)
	     (or (fboundp name) (global-lexical-bound? name)))
	`',name
	`(def ,name ,@body))))

(def just-a-variable '(a b c))
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

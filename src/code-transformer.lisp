(DEFPACKAGE #:SCHEMEISH.CODE-TRANSFORMER
  (:USE #:SCHEMEISH.SCHEMEISH))

;; TODO: (transform definition) => (values variable-names set-form function-bindings)
;;       (transform definition) => (values names set-form)
;; TODO: move DEFINE/DEFINE-VALUES out of lexical-body
;; Actually...what's the best way to export both SCM-DEFINE and SCHEMEISH-DEFINE

(in-package :schemeish.code-transformer)
(install-syntax!)

(define-struct transformer
    (transform-special-form-table
     transform-proper-list
     transform-dotted-list
     transform-cyclic-list
     transform-atom)
    :documentation
    "A set of functions to transform expressions.
Transform-special-form-table is a hash table of SYMBOL -> TRANSFORM
where [transform transformer expr environment] => new-expr.
The remaining fields are also transforms.")

(define (macro-function-application? expr environment)
  "True if, in the given lexical environment, expr is a macro-function application."
  (and (pair? expr)
       (symbol? (first expr))
       (macro-function (first expr) environment)))

(define (transform-expression transformer expression (environment))
  "Returns an expression that has been recursively transformed using TRANSFORMER
in the given lexical environment.
Transform-expression first tests to see if expression is a special-form in the transformer's transform-special-form-table.
If it is not a special-form, it is then macroexpanded in the given environment.
If the form is a non-null proper-list (elements...), it is transformed using transformer's transform-proper-list.
If the form is a dotted-list (elements... . final-element), it is transformed using the transformers's transform-dotted-list.
If the form is a cyclic-list (#1#=(elements...) . #1), it is transformed using the transformer's transform-cyclic-list.
Otherwise, the form is treated as an atom and transformed using the transformer's transform-atom."
  (define transform-special-form
    (when (pair? expression)
      (hash-ref (transformer-transform-special-form-table transformer) (first expression) nil)))
  (cond
    (transform-special-form [transform-special-form transformer expression environment])
    ((macro-function-application? expression environment)
     (transform-expression transformer (macroexpand expression environment) environment))
    ((pair? expression)
     (ecase (list-type expression)
       (:proper [(transformer-transform-proper-list transformer) transformer expression environment])
       (:cyclic [(transformer-transform-cyclic-list transformer) transformer expression environment])
       (:dotted [(transformer-transform-dotted-list transformer) transformer expression environment])))
    (t [(transformer-transform-atom transformer) transformer expression environment])))

(for-macros
  (defvar *lexical-context* ()))
(defmacro transform-in-lexical-environment (transformer expression lexical-context &environment environment)
  "Applies transformer to expression in environment.
*LEXICAL-CONTEXT* will be bound to the given lexical-context for the duration of the transformation."
  (let ((*lexical-context* lexical-context))
    (transform-expression transformer expression environment)))

(define (transform transformer expression)
  "Returns a form that when evaluated, will transform the given expression
in the current *LEXICAL-CONTEXT*."
  `(lisp (transform-in-lexical-environment ,transformer ,expression ,*lexical-context*)))

(define (declare? form)
  "True if form is (CL:DECLARE ...)"
  (and (pair? form) (eq? (first form) 'cl:declare)))

(define (body-declarations body)
  "Return the initial DECLARE forms in body."
  (takef body #'declare?))
(define (body-forms body)
  "Return the forms of body without the initial DECLARE forms."
  (dropf body #'declare?))

(define (function-body-declarations body)
  "Return the initial [documentation] declarations... in body
as (documentation declarations...)"
  (if (and (string? (first body)) (not (empty? (rest body))))
      (cons (first body) (body-declarations (rest body)))
      (body-declarations body)))
(define (function-body-forms body)
  "Return the forms of body with the initial [documentation] declarations...
removed."
  (if (and (string? (first body)) (not (empty? (rest body))))
      (body-forms (rest body))
      (body-forms body)))

;; Special form parsers: all assume that the given expr is well-formed. 
(define (quote-expr expr) (second expr))
(define (function-name expr) (second expr))
(define (progn-forms expr) (rest expr))

(define (lambda-parameters expr) (second expr))
(define (lambda-body expr) (cddr expr))
(define (lambda-body-declarations expr) (body-declarations (lambda-body expr)))
(define (lambda-body-forms expr) (body-forms (lambda-body expr)))

(define (let-bindings expr) (second expr))
(define (let-body expr) (cddr expr))
(define (let-body-declarations expr) (body-declarations (let-body expr)))
(define (let-body-forms expr) (body-forms (let-body expr)))

(define (let*-bindings expr) (let-bindings expr))
(define (let*-body expr) (let-body expr))
(define (let*-body-declarations expr) (body-declarations (let*-body expr)))
(define (let*-body-forms expr) (body-forms (let*-body expr)))

(define (block-name expr) (second expr))
(define (block-body expr) (cddr expr))

(define (return-from-name expr) (second expr))
(define (return-from-value expr)
  (or (and (= (length expr) 3) (third expr))
      nil))

(define (flet-bindings expr) (second expr))
(define (flet-body expr) (cddr expr))
(define (flet-body-declarations expr) (body-declarations (flet-body expr)))
(define (flet-body-forms expr) (body-forms (flet-body expr)))

(define (labels-bindings expr) (second expr))
(define (labels-body expr) (cddr expr))
(define (labels-body-declarations expr) (body-declarations (labels-body expr)))
(define (labels-body-forms expr) (body-forms (labels-body expr)))

(define (function-binding-name expr) (first expr))
(define (function-binding-parameters expr) (second expr))
(define (function-binding-body expr) (cddr expr))
(define (function-binding-body-declarations expr) (function-body-declarations (function-binding-body expr)))
(define (function-binding-body-forms expr) (function-body-forms (function-binding-body expr)))

(define (macrolet-bindings expr) (second expr))
(define (macrolet-body expr) (cddr expr))
(define (macrolet-body-declarations expr) (body-declarations (macrolet-body expr)))
(define (macrolet-body-forms expr) (body-forms (macrolet-body expr)))

(define (symbol-macrolet-bindings expr) (second expr))
(define (symbol-macrolet-body expr) (cddr expr))
(define (symbol-macrolet-body-declarations expr) (body-declarations (symbol-macrolet-body expr)))
(define (symbol-macrolet-body-forms expr) (body-forms (symbol-macrolet-body expr)))

(define (eval-when-situations expr) (second expr))
(define (eval-when-forms expr) (cddr expr))

(define (setq-pairs expr)
  (define (recurse expr pairs)
    (if (empty? expr)
	pairs
	(recurse (cddr expr) (cons (list (first expr) (second expr)) pairs))))
  (nreverse (recurse (rest expr) ())))

(define (if-test expr) (second expr))
(define (if-then expr) (third expr))
(define (if-else expr) (fourth expr))

(define (locally-body expr) (cdr expr))
(define (locally-body-declarations expr) (body-declarations (locally-body expr)))
(define (locally-body-forms expr) (body-forms (locally-body expr)))

(define (tagbody-tags-and-statements expr) (cdr expr))
(define (go-tag expr) (second expr))

(define (the-value-type expr) (second expr))
(define (the-form expr) (third expr))

(define (multiple-value-prog1-values-form expr) (second expr))
(define (multiple-value-prog1-forms expr) (cddr expr))

(define (multiple-value-call-function expr) (second expr))
(define (multiple-value-call-arguments expr) (cddr expr))

(define (load-time-value-form expr) (second expr))
(define (load-time-value-read-only-p expr) (third expr))

(define (catch-tag expr) (second expr))
(define (catch-forms expr) (cddr expr))

(define (throw-tag expr) (second expr))
(define (throw-result expr) (third expr))

(define (unwind-protect-protected expr) (second expr))
(define (unwind-protect-cleanup expr) (cddr expr))

(define (parse-tagbody tags-and-statements)
  "Return (untagged-statements . (tag . statements)...)."
  (define (tag? tag-or-statement)
    (or (symbol? tag-or-statement)
	(integerp tag-or-statement)))
  (define (statement? tag-or-statement)
    (not (tag? tag-or-statement)))

  (define untagged-statements (takef tags-and-statements statement?))
  (define tagged-statements (dropf tags-and-statements statement?))

  (define (tagged-forms-iter tags-and-statements tagged-forms)
    (define (parse-next-tagged-form)
      (define tag (first tags-and-statements))
      (define statements-and-tagged-statements (rest tags-and-statements))
      (define statements (takef statements-and-tagged-statements statement?))
      (define rest-tags-and-statements (dropf statements-and-tagged-statements statement?))

      (tagged-forms-iter rest-tags-and-statements (cons (cons tag statements) tagged-forms)))
    
    (cond
      ((empty? tags-and-statements) tagged-forms)
      (t (parse-next-tagged-form))))
  
  (define tagged-forms
    (nreverse (tagged-forms-iter tagged-statements ())))

  (cons untagged-statements tagged-forms))

;; Lexical body expansion

(for-macros
  (defvar *lexical-body-definition-table* (make-hash-table))

  (define (register-lexical-body-definition symbol transform)
    (hash-set! *lexical-body-definition-table* symbol transform)))

(defvar *definition-form* nil)
(defvar *definition-guard-clauses* nil)
(define (transform-lexical-body-define-pair definition)
  (define name-field (second definition))
  (define body (cddr definition))
  (define guard-clauses (multiple-value-bind (body guard-clauses docs) (parse-function-tags-from-body body)
			  (declare (ignore body docs))
			  guard-clauses))
  (define (recurse name-field body)
    (cond
      ((pair? (first name-field))
       ;; Closure definition: ((...) . parameters)
       (let ((name-field (first name-field))
	     (parameters (rest name-field)))
	 (recurse name-field `((let ((*definition-form* ',(second definition))
				     (*definition-guard-clauses* ',guard-clauses))
				 (lambda ,parameters ,@body))))))
      (t
       ;; Function definition: (name . parameters)
       (let ((name (first name-field))
	     (parameters (rest name-field)))
	 (values (list name) `(setq ,name (let ((*definition-form* ',(second definition))
						(*definition-guard-clauses* ',guard-clauses))
					    (lambda ,parameters ,@body))))))))
  (recurse name-field body))

(define (transform-lexical-body-define-symbol definition)
  (let ((name (second definition))
	(value (third definition)))
    (values (list name) `(setq ,name ,value))))

(define (transform-lexical-body-define-symbol-or-pair definition)
  (define name-field (second definition))
  (cond
    ((symbol? name-field) (transform-lexical-body-define-symbol definition))
    (t (transform-lexical-body-define-pair definition))))

(define (transform-lexical-body-define-values definition)
  (define name-field (second definition))
  (define values-form (third definition))
  (define (ignore? name) (string= (symbol->string name) "_"))
  (define (names-iter given-names names vars ignored-names)
    (cond
      ((empty? given-names) (values (nreverse names) (nreverse vars) (nreverse ignored-names)))
      (t (let ((name (first given-names)))
	   (if (ignore? name)
	       (let ((var (unique-symbol 'ignore)))
		 (names-iter (rest given-names) names (cons var vars) (cons var ignored-names)))
	       (names-iter (rest given-names) (cons name names) (cons name vars) ignored-names))))))
  
  (cond
    ((symbol? name-field)
     (values (list name-field) `(setq ,name-field (multiple-value-list ,values-form))))
    (t
     (multiple-value-bind (names vars ignored-names) (names-iter name-field () () ())
       (values names `(cl:let ,ignored-names
			(multiple-value-setq ,vars ,values-form)))))))

(for-macros
  (register-lexical-body-definition 'define #'transform-lexical-body-define-symbol-or-pair)
  (register-lexical-body-definition 'define-values #'transform-lexical-body-define-values))

(define (lexical-body-definition? form)
  (and (pair? form)
       (hash-ref *lexical-body-definition-table* (first form) nil)))
(define (transform-lexical-body-definition form)
  [(hash-ref *lexical-body-definition-table* (first form)) form])

(define (parse-lexical-body-definitions lexical-body)
  "Returns (values definitions forms)"
  (values (takef lexical-body #'lexical-body-definition?)
	  (dropf lexical-body #'lexical-body-definition?)))

(define (parse-declarations body)
  "Returns (values declarations forms)"
  (values (takef body #'declare?)
	  (dropf body #'declare?)))

(define (collect-lexical-body-definitions-names-and-set-forms definitions)
  "Returns (values names set-forms)"
  (define (iter definitions names set-forms)
    (cond
      ((empty? definitions) (values names (nreverse set-forms)))
      (t (multiple-value-bind (new-names set-form) (transform-lexical-body-definition (first definitions))
	   (iter (rest definitions) (append names new-names) (cons set-form set-forms))))))
  (iter definitions () ()))

(define (parse-lexical-body body)
  "Returns (values body names set-forms)"
  (multiple-value-bind (definitions body) (parse-lexical-body-definitions body)
    (multiple-value-bind (names set-forms) (collect-lexical-body-definitions-names-and-set-forms definitions)
      (values body names set-forms))))

(define (lexical-body-form body)
  (multiple-value-bind (body names set-forms) (parse-lexical-body body)
    (let* ((definition-function-binding
	     (lambda (name)
	       (let ((rest (unique-symbol 'rest)))
		 `(,name (&rest ,rest) (apply ,name ,rest)))))
	   (definition-variable-binding
	     (lambda (name) (list name name))))
      `(cl:let ,names
	 (cl:labels ,(map definition-function-binding names)
	   (declare (ignorable ,@(map (lambda (name) `(function ,name)) names)))
	   ,@set-forms
	   (cl:let ,(map definition-variable-binding names)
	     (declare (ignorable ,@names))
	     ,@body))))))

(defmacro lexical-body (&body body)
  (lexical-body-form body))

(defmacro lambda (parameters &body body)
  (multiple-value-bind (parameters ignorable-parameters) (schemeish.define::arg-list->lambda-list parameters)
    `(cl:lambda ,parameters
       (declare (ignorable ,@ignorable-parameters))
       ,@(let ((declarations (function-body-declarations body))
	       (forms (function-body-forms body)))
	   `(,@declarations (schemeish.lexical-body:lexical-body ,@forms))))))

(schemeish.define2:define (test)
  (define-values (a b c) (values 1 2 3))
  (define d 4)
  (define (((e x) y) z) (list x y z))
  (list* a b c d [[(e 5) 6] 7]))

(assert (equal? (test)
		'(1 2 3 4 5 6 7)))

(assert (equal?
	 (schemeish.lexical-body:lexical-body
	   (define-values (a b c) (values 1 2 3))
	   (define d 4)
	   (define (((e x) y) z) (list x y z))
	   (list* a b c d [[(e 5) 6] 7]))
	 (range 8 1)))

;; Implementation of SCM
(for-macros
  (defvar *transform-special-form-table* (make-hash-table)))
(define (register-transform-special-form symbol transform)
  (hash-set! *transform-special-form-table* symbol transform))

(defmacro define-special-transform (name (transformer expression environment) &body body)
  `(for-macros
     (register-transform-special-form ',name (lambda (,transformer ,expression ,environment)
					       (declare (ignorable ,transformer ,expression ,environment))
					       ,@body))))


(defmacro lisp (form)
  "Within SCM, escapes form and evaluates it as if it were in Common-Lisp.
Within LISP, just evalutes form."
  form)

(define (transform* transformer forms)
  "Map transform across forms."
  (map (lcurry #'transform transformer) forms))

(define (map-parameters proc parameters)
  (define keywords '(cl:&optional cl:&key cl:&rest cl:&aux))
  
  (define (map-keyword keyword parameters result)
    (let ((result (cons [proc :keyword keyword] result)))
      (ecase keyword
	(cl:&optional (map-group :optional parameters result))
	(cl:&key (map-group :key parameters result))
	(cl:&rest (map-group :rest parameters result))
	(cl:&aux (map-group :aux parameters result)))))

  (define (map-group group-name parameters result)
    (cond
      ((empty? parameters) result)
      (t (let ((parameter (first parameters))
	       (rest-parameters (rest parameters)))
	   (cond
	     ((member parameter keywords) (map-keyword parameter rest-parameters result))
	     (t (map-group group-name rest-parameters (cons [proc group-name parameter] result))))))))
  (nreverse (map-group :positional parameters ())))

(assert (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &key (key value) &aux (aux value) &rest rest)))
	  (equal? (map-parameters (lambda args (second args)) parameters)
		  parameters)))

(define (transform-parameters transformer parameters)
  (map-parameters (lambda (group parameter)
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
		  parameters))

(define (parameter-names parameters)
  (append* (map-parameters (lambda (group parameter)
			     (ecase group
			       (:keyword ())
			       ((:positional :rest) (list parameter))
			       ((:optional :key :aux)
				(cond
				  ((pair? parameter)
				   (cond
				     ((= (length parameter) 3) (list (first parameter) (third parameter)))
				     (t (list (first parameter)))))
				  (t (list parameter))))))
			   parameters)))

(assert (equal? (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
				    &key (key value) &aux (aux value) &rest rest)))
		  (parameter-names parameters))
		'(A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? KEY AUX REST)))

(define (bound-variable? expr environment)
  (and (symbol? expr)
       #+sbcl
       (sb-cltl2:variable-information expr environment)
       #-sbcl
       (error "TODO: Don't know how to get the variable-information of ~S in ~S for lisp implementation: ~S"
	      expr
	      environment
	      (lisp-implementation-type))))

(define (parse-documentation-source body)
  (cond
    ((empty? body) (values body nil))
    ((documentation-tag? (first body))
     (values (rest body) (documentation-tag-form (first body))))
    ((and (not (empty? (rest body)))
	  (string? (first body)))
     (values (rest body) (first body)))
    (t (values body nil))))
(define (parse-function-tags-from-body body)
  "Return (values body guard-clauses documentation-source-form)"
  (define (parse-guard-clauses body)
    (if (and (not (empty? body))
	     (guard-tag? (first body)))
	(values (rest body) (guard-tag-clauses (first body)))
	(values body nil)))
  (multiple-value-bind (body documentation-source) (parse-documentation-source body)
    (multiple-value-bind (body guard-clauses) (parse-guard-clauses body)
      (values body guard-clauses documentation-source))))

(define (documentation-string-for-definition parameters definition-form guard-clauses documentation-string)
  #g((string? documentation-string))
  (define name (first (flatten definition-form)))
  (string-append documentation-string
		 (format nil "~&~%~%Parameters: ~S" parameters)
		 (format nil "~&~%Form: ~S" definition-form)
		 (if guard-clauses
		     (format nil "~&~%~%~S has the following guard clauses:~%~S"
			     name guard-clauses)
		     (format nil "~&~%~%~S has no guard clauses." name))))


(define (parameter-bindings-form parameters)
  `(list ,@(map (lambda (name) `(list ',name ,name)) (parameter-names parameters))))

(define (guard-clauses-form guard-clauses parameters)
  "Return a form that processes guard-clauses, causing an error if any clause fails."
  (cons 'progn
	(mapcar (cl:lambda (guard-clause)
		  `(unless ,guard-clause
		     (error "Failed function guard-clause: ~S with the given parameter bindings: ~S" ',guard-clause ,(parameter-bindings-form parameters))))
		guard-clauses)))

(define (register-function-metadata function scm-parameters guard-clauses documentation-source definition-form)
  (when guard-clauses
    (schemeish.define::register-guard-clauses function guard-clauses))
  (setf (documentation function t)
	(documentation-string-for-definition scm-parameters
					     definition-form
					     guard-clauses
					     (if documentation-source
						 (schemeish.define::documentation-string documentation-source)
						 "")))
  (when definition-form
    (schemeish.define::register-define-form function definition-form))
  (when documentation-source
    (schemeish.define::set-object-documentation-source! function documentation-source))
  function)

(define (transform-function transformer scm-parameters body)
  ;; TODO: rename arg-list->lambda-list to scm-parameters->parameters
  (define name (unique-symbol 'function))
  (multiple-value-bind (parameters ignorable-args) (schemeish.define::arg-list->lambda-list scm-parameters)
    (multiple-value-bind (body guard-clauses documentation-source) (parse-function-tags-from-body body)
      `(register-function-metadata
	(cl:lambda ,parameters
	  (declare (ignorable ,@ignorable-args))
	  ,(guard-clauses-form guard-clauses parameters)
	  ,@(transform-function-body transformer body))
	',scm-parameters
	,(if guard-clauses
	     `',guard-clauses
	     '*definition-guard-clauses*)
	,documentation-source
	*definition-form*))))

(define (transform-lexical-body transformer body)
  (multiple-value-bind (body names set-forms) (schemeish.lexical-body:parse-lexical-body body)
    (multiple-value-bind (declarations body) (parse-declarations body)
      (cond
	((empty? names) (transform* transformer body))
	(t `((cl:let ,names
	       ,@(transform* transformer set-forms)
	       (cl:let ,(map (lambda (name) (list name name)) names)
		 ,@declarations
		 ,@(transform* transformer body)))))))))

(define (transform-function-body transformer body)
  ;; function body is ([documentation] declarations... . lexical-body)
  (define declarations (function-body-declarations body))
  (define lexical-body (function-body-forms body))
  `(,@declarations ,@(transform-lexical-body transformer lexical-body)))

(define-special-transform cl:progn (transformer expr env)
  `(cl:progn ,@(transform-lexical-body transformer (progn-forms expr))))
(define-special-transform cl:lambda (transformer expr env)
  `(cl:lambda ,(transform-parameters transformer (lambda-parameters expr))
     ,@(transform-function-body transformer (lambda-body expr))))

(define-special-transform schemeish.lexical-body:lambda (transformer expr env)
  (transform-function transformer (lambda-parameters expr) (lambda-body expr)))

(define-special-transform cl:block (transformer expr env)
  `(cl:block ,(block-name expr)
     ,@(transform-lexical-body transformer (block-body expr))))
(define-special-transform cl:return-from (transformer expr env)
  `(cl:return-from ,(return-from-name expr) ,(transform transformer (return-from-value expr))))

(define-special-transform cl:tagbody (transformer expr env)
  (define parsed (parse-tagbody (rest expr)))
  (define (transform-tag-and-forms tag-and-forms)
    (cons (first tag-and-forms)
	  (transform* transformer (rest tag-and-forms))))
  (define untagged-forms (transform* transformer (first parsed)))
  (define tags-and-forms (append-map transform-tag-and-forms (rest parsed)))
  `(cl:tagbody
      ,@untagged-forms
      ,@tags-and-forms))
(define-special-transform cl:go (transformer expr env)
  expr)
(define-special-transform cl:quote (transformer expr env)
  expr)
(define-special-transform cl:function (transformer expr env)
  expr)

(define (transform-let-binding transformer binding)
  (if (pair? binding)
      (list (first binding) (transform transformer (second binding)))
      (list binding ())))

(define-special-transform cl:let (transformer expr env)
  (define bindings (map (lambda (binding) (transform-let-binding transformer binding)) (let-bindings expr)))
  `(cl:let ,bindings
     ,@(transform-lexical-body transformer (let-body expr))))
(define-special-transform cl:let* (transformer expr env)
  (define bindings (map (lambda (binding) (transform-let-binding transformer binding)) (let*-bindings expr)))
  `(cl:let* ,bindings
     ,@(transform-lexical-body transformer (let*-body expr))))

(define (transform-function-binding transformer binding)
  `(,(function-binding-name binding) ,(transform-parameters transformer (function-binding-parameters binding))
    ,@(transform-function-body transformer (function-binding-body binding))))
(define-special-transform cl:labels (transformer expr env)
  (define bindings (map (lcurry #'transform-function-binding transformer)
			(labels-bindings expr)))
  `(cl:labels ,bindings
     ,@(transform-lexical-body transformer (labels-body expr))))
(define-special-transform cl:flet (transformer expr env)
  (define bindings (map (lcurry #'transform-function-binding transformer)
			(flet-bindings expr)))
  `(cl:flet ,bindings
     ,@(transform-lexical-body transformer (flet-body expr))))
(define-special-transform cl:macrolet (transformer expr env)
  `(cl:macrolet ,(macrolet-bindings expr)
     ,@(transform-lexical-body transformer (macrolet-body expr))))
(define-special-transform cl:symbol-macrolet (transformer expr env)
  `(cl:symbol-macrolet ,(symbol-macrolet-bindings expr)
     ,@(transform-lexical-body transformer (symbol-macrolet-body expr))))
(define-special-transform cl:throw (transformer expr env)
  `(cl:throw ,(transform transformer (throw-tag expr))))
(define-special-transform cl:catch (transformer expr env)
  `(cl:catch ,(transform transformer (catch-tag expr))
     ,@(transform-lexical-body transformer (catch-forms expr))))
(define-special-transform cl:unwind-protect (transformer expr env)
  `(cl:unwind-protect ,(transform transformer (unwind-protect-protected expr))
     ,@(transform-lexical-body transformer (unwind-protect-cleanup expr))))
(define-special-transform cl:load-time-value (transformer expr env)
  `(cl:load-time-value ,(transform transformer (load-time-value-form expr))
		       ,(transform transformer (load-time-value-read-only-p expr))))
(define-special-transform cl:eval-when (transformer expr env)
  `(cl:eval-when ,(eval-when-situations expr)
     ,@(transform-lexical-body transformer (eval-when-forms expr))))
(define-special-transform cl:multiple-value-prog1 (transformer expr env)
  `(cl:multiple-value-prog1 ,(transform transformer (multiple-value-prog1-values-form expr))
     ,@(transform-lexical-body transformer (multiple-value-prog1-forms expr))))
(define-special-transform cl:multiple-value-call (transformer expr env)
  `(cl:multiple-value-call ,(transform transformer (multiple-value-call-function expr))
     ,@(transform* transformer (multiple-value-call-arguments expr))))
(define-special-transform cl:the (transformer expr env)
  `(cl:the ,(the-value-type expr) ,(transform transformer (the-form expr))))
(define-special-transform cl:if (transformer expr env)
  `(cl:if ,(transform transformer (if-test expr))
	  ,(transform transformer (if-then expr))
	  ,(transform transformer (if-else expr))))
(define-special-transform cl:setq (transformer expr env)
  (define (transform-pair pair)
    (define name (first pair))
    (define value (second pair))
    (list name (transform transformer value)))
  (define pairs (append-map transform-pair (setq-pairs expr)))
  `(setq ,@pairs))

(define-special-transform lisp (transformer expr env)
  (second expr))

(define (transform-proper-list transformer expr env)
  (define fn (first expr))
  (if (or (pair? fn) (bound-variable? fn env))
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
    ((and (bound-variable? expr env)
	  ;; Special case: if a a function happens to be named the same as a special/parameter choose the function.
	  (not (and (parameter? expr env)
		    (fboundp expr)))
	  expr)
     expr)
    (t `(function ,expr))))

(for-macros
  (defparameter *scm-transformer*
    (make-transformer *transform-special-form-table*
		      'transform-proper-list
		      'transform-dotted-list
		      'transform-cyclic-list
		      'transform-atom)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (schemeish.lexical-body:lambda args args))
			      (args '(1 2 3)))
			 (f . args))))

		'(1 2 3)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (schemeish.lexical-body:lambda args args))
			      (args '(1 2 3)))
			 (list* (list 1 2 3) (f . args)))))
		'((1 2 3) 1 2 3)))

(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ((f (schemeish.lexical-body:lambda args (+ . args)))
			      (args '(1 2 3)))
			 (list* (+ 1 2 3) (f . args)))))
		'(6 . 6)))



(assert (equal? (eval (transform-expression
		       *scm-transformer*
		       '(let ()
			 (define a 1)
			 (define copy (schemeish.lexical-body:lambda (x)
					(if (empty? x)
					    ()
					    (cons (first x) (copy (rest x))))))

			 (copy (list a a a)))))
		'(1 1 1)))

(defmacro schemeish.scm:scm (&body body &environment environment)
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
  (transform-expression
   *scm-transformer*
   `(progn ,@body)
   environment))

(assert (equal? (schemeish.scm:scm
		  (let ()
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
		      (reverse (list c b a)))))

		'(1 b 3)))

(assert (equal? (schemeish.scm:scm
		  (define (((nested a) b) . c) (list* a b c))
		  (((nested 1) 2) 3 4 5))
		'(1 2 3 4 5)))

(defmacro def (name-field &body body)
  (cond
    ((symbol? name-field)
     (let ((name name-field))
       (multiple-value-bind (body documentation-source) (parse-documentation-source body)
	 (unless (= (length body) 1)
	   (error "Expected exactly one body-form for DEF: got ~S" body))
	 `(for-macros
	    (setf (fdefinition ',name) (schemeish.scm:scm ,(first body)))
	    (schemeish.define::register-define-form #',name ',name)
	    ,@(when documentation-source
		`((setf (documentation ',name 'function) (schemeish.define::documentation-string-for-define-function
							  ',name #',name (documentation-string ,documentation-source)))
		  (schemeish.define::set-object-documentation-source! #',name ,documentation-source)))
	    ',name))))
    (t
     (let ((name (first (flatten name-field))))
       `(for-macros
	  (fmakunbound ',name)
	  (setf (fdefinition ',name) (schemeish.scm:scm (define ,name-field ,@body) ,name))
	  ',name)))))

(schemeish.scm:define (((test-nested a) b) . c)
  #g((not (list? b)))
  (list* a b c))

(schemeish.scm:scm (documentation test-nested t))
(schemeish.scm:scm (documentation (test-nested 1) t))
(schemeish.scm:scm (documentation ((test-nested 1) 2) t))
(assert (equal? (schemeish.scm:scm (((test-nested 1) 2) 3 4 5))
		'(1 2 3 4 5)))

(schemeish.scm:define (test-fact n)
  #d"Test for a a factorial using DEF."
  #g((not (negative? n)))
  (define (iter n result)
    #d"Iterates from n to 0."
    (cond
      ((zero? n) result)
      (t (iter (1- n) (* result n)))))
  (values (iter n 1) (documentation iter t)))

(assert (equal? (test-fact 4) 24))
(schemeish.scm:scm (documentation test-fact t))

(schemeish.scm:scm (define-form test-fact))
(def 2+ (lcurry + 2))

(assert (= 3 (2+ 1)))

(assert (equal? (schemeish.scm:scm
		  (define-values (a b c) (values 1 2 3))
		  (define x 'x)
		  (define-values (d e f) (values 4 5 6))
		  (define (y) 'y)
		  (define-values g (values 7 8 9))
		  (list* x (y) a b c d e f g))
		'(X Y 1 2 3 4 5 6 7 8 9)))

(schemeish.scm:scm
  (define-values (a _ c) (values 1 2 3))
  (list a c))

;; merge with functions in schemeish.DEFINE
;; split off lexical-body into DEFINE

;; split off into scm-transformer and scm package

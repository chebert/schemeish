(DEFPACKAGE #:SCHEMEISH.LEXICAL-BODY
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOW #:LAMBDA)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.ARGUMENTS
        #:SCHEMEISH.FOR-MACROS
        #:SCHEMEISH.NAMED-LET
        #:SCHEMEISH.SYNTAX))

(in-package :schemeish.lexical-body)

(install-syntax!)

(for-macros
  (defvar *guard-clauses-enabled?* t)
  (defun guard-clauses-enabled? () *guard-clauses-enabled?*)
  (defun enable-guard-clauses! () (setq *guard-clauses-enabled?* t))
  (defun disable-guard-clauses! () (setq *guard-clauses-enabled?* nil))

  (defvar *guard-clauses-table* (make-hash-table :weakness :key)
    "A table from function to a list of guard-clauses guarding that function.")

  (defun register-guard-clauses (function guard-clauses)
    "Associates function with guard-clauses in the *guard-clauses-hash-table*"
    (assert (functionp function))
    (assert (listp guard-clauses))
    (setf (gethash function *guard-clauses-table*) guard-clauses)
    function)
  (defun guard-clauses (function)
    "Retrieves the guard-clauses associated with function, or NIL if not present."
    (assert (functionp function))
    (gethash function *guard-clauses-table*))

  (defvar *definition-name-field-table* (make-hash-table)
    "A hash table from function -> definition-name-field.")
  (defun register-definition-name-field (function name-field)
    (assert (functionp function))
    (setf (gethash function *definition-name-field-table*) name-field))

  (defun get-definition-name-field (function)
    "Retrieve the DEFINE name-field used to define function.
Returns nil if not defined using DEFINE."
    (gethash function *definition-name-field-table*))

  (defgeneric documentation-string (documentation)
    (:documentation "Returns a documentation string given the provided documentation object."))
  (defmethod documentation-string ((documentation string)) documentation)
  (defun documentation-source? (object)
    "An object is a documentation-source if it has a method implemented for documentation-string."
    (compute-applicable-methods #'documentation-string (list object)))

  (defvar *variable-documentation-hash-table* (make-hash-table))
  (defvar *type-documentation-hash-table* (make-hash-table))
  (defvar *compiler-macro-documentation-hash-table* (make-hash-table))
  (defvar *setf-documentation-hash-table* (make-hash-table))
  (defvar *object-documentation-hash-table* (make-hash-table :weakness :key))

  (defun check-symbol (symbol)
    (unless (symbolp symbol)
      (error "Symbol expected to be a symbol type, but got: ~S" (type-of symbol))))
  (defun check-name (name)
    (unless (or (symbolp name) (and (listp name)
				    (= 2 (length name))
				    (eq 'cl:setf (first name))
				    (symbolp (second name))))
      (error "Name expected to be a symbol or a list (setf symbol) but got: ~S" name)))
  (defun check-object (object)
    (typecase object
      (function)
      (method-combination)
      (standard-method)
      (package)
      (t (error "Object expected to be of type: function, method-combination, standard-method, or package. But got ~S" (type-of object)))))

  (defun variable-documentation-source (symbol)
    "Returns the documentation source associated with the constant or dynamic variable named symbol."
    (check-symbol symbol)
    (gethash symbol *variable-documentation-hash-table* nil))
  (defun type-documentation-source (symbol)
    "Returns the documentation source associated with the type named by symbol."
    (check-symbol symbol)
    (gethash symbol *type-documentation-hash-table* nil))
  (defun compiler-macro-documentation-source (name)
    "Returns the documentation source associated with the compiler-macro named by NAME."
    (check-name name)
    (gethash name *compiler-macro-documentation-hash-table* nil))
  (defun setf-documentation-source (symbol)
    "Returns the documentation source associated with the setf-expansion named by symbol."
    (check-symbol symbol)
    (gethash symbol *setf-documentation-hash-table* nil))
  (defun object-documentation-source (object)
    "Returns the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
    (check-object object)
    (gethash object *object-documentation-hash-table* nil))

  (defun set-variable-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the constant or dynamic variable named symbol."
    (check-symbol symbol)
    (setf (gethash symbol *variable-documentation-hash-table*) documentation-source))
  (defun set-type-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the type named by symbol."
    (check-symbol symbol)
    (setf (gethash symbol *type-documentation-hash-table*) documentation-source))
  (defun set-compiler-macro-documentation-source! (name documentation-source)
    "Updates the documentation source associated with the compiler-macro named by NAME."
    (check-name name)
    (setf (gethash name *compiler-macro-documentation-hash-table*) documentation-source))
  (defun set-setf-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the setf-expansion named by symbol."
    (check-symbol symbol)
    (setf (gethash symbol *setf-documentation-hash-table*) documentation-source))
  (defun set-object-documentation-source! (object documentation-source)
    "Updates the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
    (check-object object)
    (setf (gethash object *object-documentation-hash-table*) documentation-source))


  (defun definition-name (name-field)
    "Parses the definition name from the (define name-field ....)."
    (if (symbolp name-field)
	name-field
	(definition-name (first name-field))))
  (defun documentation-string-for-definition (parameters definition-name-field guard-clauses documentation-string)
    "Adds documentation for the definition from the parameters, name-field, and guard-clauses."
    (let ((name (definition-name definition-name-field)))
      (concatenate 'string
		   documentation-string
		   (format nil "~&~%~%Parameters: ~S" parameters)
		   (format nil "~&~%Form: ~S" definition-name-field)
		   (if guard-clauses
		       (format nil "~&~%~%~S has the following guard clauses:~%~S"
			       name guard-clauses)
		       (format nil "~&~%~%~S has no guard clauses." name)))))
  (defun documentation-string-for-lambda (parameters guard-clauses documentation-string)
    "Adds documentation for the lambda from the parameters, name-field, and guard-clauses."
    (concatenate 'string
		 documentation-string
		 (format nil "~&~%~%Parameters: ~S" parameters)
		 (if guard-clauses
		     (format nil "~&~%~%Has the following guard clauses:~%~S"
			     guard-clauses)
		     (format nil "~&~%~%Has no guard clauses."))))

  (defparameter *cl-lambda-list-keywords* '(cl:&optional cl:&key cl:&rest cl:&aux))
  (defun map-parameters (proc parameters)
    "Map over the CL lambda-list parameters. Accepted keywords are *CL-LAMBDA-LIST-KEYWORDS*.
Proc is called with either [proc :keyword keyword] or [proc group parameter], where
group is one of (:optional :key :aux :rest :positional)."
    (let ((keywords  *cl-lambda-list-keywords*))
      (labels ((map-keyword (keyword parameters result)
		 (let ((result (cons [proc :keyword keyword] result)))
		   (ecase keyword
		     (cl:&optional (map-group :optional parameters result))
		     (cl:&key (map-group :key parameters result))
		     (cl:&rest (map-group :rest parameters result))
		     (cl:&aux (map-group :aux parameters result)))))
	       (map-group (group-name parameters result)
		 (cond
		   ((null parameters) result)
		   (t (let ((parameter (first parameters))
			    (rest-parameters (rest parameters)))
			(cond
			  ((member parameter keywords) (map-keyword parameter rest-parameters result))
			  (t (map-group group-name rest-parameters (cons [proc group-name parameter] result)))))))))

	(nreverse (map-group :positional parameters ()))))))

(assert (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &key (key value) &aux (aux value) &rest rest)))
	  (equal (map-parameters (cl:lambda (&rest args) (second args)) parameters)
		 parameters)))

(for-macros
  (defun parameter-names (parameters)
    "Returns an ordered list of bound names in the given parameter list."
    (apply #'append (map-parameters (cl:lambda (group parameter)
				      (ecase group
					(:keyword ())
					((:positional :rest) (list parameter))
					((:optional :key :aux)
					 (cond
					   ((consp parameter)
					    (cond
					      ((= (length parameter) 3) (list (first parameter) (third parameter)))
					      (t (list (first parameter)))))
					   (t (list parameter))))))
				    parameters))))

(assert (equal (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
				   &key (key value) &aux (aux value) &rest rest)))
		 (parameter-names parameters))
	       '(A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? KEY AUX REST)))

(for-macros
  (defun parameter-bindings-form (parameters)
    "Returns a form that evaluates to a list of (name value) bindings for the CL lambda-list parameters."
    `(list ,@(mapcar (cl:lambda (name) `(list ',name ,name)) (parameter-names parameters))))

  (defun guard-clauses-form (guard-clauses parameters)
    "Return a form that processes guard-clauses, causing an error if any clause fails.
Checks if *guard-clauses-enabled?* is true before evaluating any guard clauses."
    `(when *guard-clauses-enabled?*
       ,@(mapcar (cl:lambda (guard-clause)
		   `(unless ,guard-clause
		      (error "Failed function guard-clause: ~S with the given parameter bindings: ~S" ',guard-clause ,(parameter-bindings-form parameters))))
		 guard-clauses)))

  (defun register-definition-function-metadata (function scm-parameters guard-clauses documentation-source definition-name-field)
    "Registers metadata associated with function defined using (define (...) ...)."
    (let ((documentation-string (if documentation-source
				    (documentation-string documentation-source)
				    "")))
      (setf (documentation function t)
	    (documentation-string-for-definition scm-parameters definition-name-field guard-clauses documentation-string)))
    (when guard-clauses (register-guard-clauses function guard-clauses))
    (when definition-name-field (register-definition-name-field function definition-name-field))
    (when documentation-source (set-object-documentation-source! function documentation-source))
    function)

  (defun register-lambda-metadata (function scm-parameters guard-clauses documentation-source)
    "Registers metadata associated with function."
    (let ((documentation-string (if documentation-source
				    (documentation-string documentation-source)
				    "")))
      (setf (documentation function t)
	    (documentation-string-for-lambda scm-parameters guard-clauses documentation-string)))
    (when guard-clauses (register-guard-clauses function guard-clauses))
    (when documentation-source (set-object-documentation-source! function documentation-source))
    function))


(for-macros
  (defvar *lexical-body-definition-table* (make-hash-table)
    "Table from SYMBOL -> TRANSFORM for LISP-1 Lexical-Body transformations.")
  (defvar *lexical-body2-definition-table* (make-hash-table)
    "Table from SYMBOL -> TRANSFORM for LISP-2 Lexical-Body transformations.")

  (defun register-lexical-body-definition (symbol transform)
    "Registers transform for lisp-1 lexical-body tranformations.
Transform is a procedure s.t. (transform form) => (values names set-form)"
    (setf (gethash symbol *lexical-body-definition-table*) transform))
  (defun register-lexical-body2-definition (symbol transform)
    "Registers transform for lisp-2 lexical-body transfomrations.
Transform is a procedure s.t. (transform form) => (values names set-form labels-bindings)
It is an error for labels-bindings to be a different length than names."
    (setf (gethash symbol *lexical-body2-definition-table*) transform))

  (defun lexical-body-definition? (form)
    "True if FORM is a registered lexical-body definition for SCM (lisp-1)."
    (and (consp form)
	 (gethash (first form) *lexical-body-definition-table* nil)))
  (defun transform-lexical-body-definition (form)
    "Transform FORM if FORM is a registered lexical-body definition for SCM (lisp-1).
Returns (values names set-form)"
    [(gethash (first form) *lexical-body-definition-table*) form])
  (defun lexical-body2-definition? (form)
    "True if FORM is a registered lexical-body definition for SCHEMEISH (lisp-2)."
    (and (consp form)
	 (gethash (first form) *lexical-body2-definition-table* nil)))
  (defun transform-lexical-body2-definition (form)
    "Transform FORM if FORM is a registered lexical-body definition for SCHEMEISH (lisp-2).
Returns (values variable-names variables-set-form labels-bindings)"
    [(gethash (first form) *lexical-body2-definition-table*) form])


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
  (defun parse-lexical-body-definitions (lexical-body)
    "Returns (values body definitions)"
    (values (dropf lexical-body #'lexical-body-definition?)
	    (takef lexical-body #'lexical-body-definition?)))
  (defun parse-lexical-body2-definitions (lexical-body)
    "Returns (values body definitions). Takes both lisp-1 and lisp-2 definitions."
    (let ((definition? (cl:lambda (definition)
			 (or (lexical-body2-definition? definition)
			     (lexical-body-definition? definition)))))
      (values (dropf lexical-body definition?)
	      (takef lexical-body definition?))))

  (defun declaration? (form)
    "True if form is (cl:declare ...)"
    (and (consp form)
	 (eq (first form) 'cl:declare)))

  (defun parse-declarations (body)
    "Returns (values forms declarations)"
    (values (dropf body #'declaration?)
	    (takef body #'declaration?)))

  (defun collect-lexical-body-definitions-names-and-set-forms (definitions)
    "Returns (values names set-forms) for SCM (lisp-1) lexical-body."
    (let iter ((definitions definitions)
	       (names ())
	       (set-forms ()))
      (cond
	((null definitions) (values names (nreverse set-forms)))
	(t (multiple-value-bind (new-names set-form) (transform-lexical-body-definition (first definitions))
	     (iter (rest definitions) (append names new-names) (cons set-form set-forms)))))))

  (defun default-labels-binding (name)
    "Return a binding with the given name for LABELS which just applies the lexical variable name to its arguments."
    (let ((rest (unique-symbol 'arguments)))
      `(,name (&rest ,rest) (apply ,name ,rest))))

  (defun collect-lexical-body2-definitions-names-and-set-forms (definitions)
    "Returns (values names set-forms labels-bindings) for SCHEMEISH (lisp-2) lexical-body.
If there is no appliciable transformer for a lisp-2 definition, a lisp-1 definition will
be used with a DEFAULT-LABELS-BINDING."
    (let iter ((definitions definitions)
	       (names ())
	       (set-forms ())
	       (labels-bindings ()))
      (cond
	((null definitions) (values names (nreverse set-forms) labels-bindings))
	((lexical-body2-definition? (first definitions))
	 (multiple-value-bind (new-names set-form new-labels-bindings) (transform-lexical-body2-definition (first definitions))
	   (unless (= (length new-names) (length new-labels-bindings))
	     (error "Bad expansion for lexical-body-definition2: ~S. Expected the same number of names ~S as labels-bindings ~S."
		    (first definitions)
		    new-names
		    new-labels-bindings))
	   (iter (rest definitions)
		 (append names new-names)
		 (cons set-form set-forms)
		 (append labels-bindings new-labels-bindings))))
	(t
	 ;; Definition is a lexical-body (lisp-1) definition, not a lexical-body2 (lisp-2) definition
	 ;; Generate default labels bindings (just apply the variable to provided arguments)
	 (multiple-value-bind (new-names set-form) (transform-lexical-body-definition (first definitions))
	   (iter (rest definitions)
		 (append names new-names)
		 (cons set-form set-forms)
		 (append labels-bindings (mapcar #'default-labels-binding new-names))))))))

  (defun parse-lexical-body (body)
    "Returns (values body names set-forms) for SCM (lisp-1) lexical-body.
A lexical body is (definitions... declarations... forms...)"
    (multiple-value-bind (body definitions) (parse-lexical-body-definitions body)
      (multiple-value-bind (names set-forms) (collect-lexical-body-definitions-names-and-set-forms definitions)
	(values body names set-forms))))
  (defun parse-lexical-body2 (body)
    "Returns (values body names set-forms labels-bindings) for SCHEMEISH (lisp-2) lexical-body.
A lexical body is (definitions... declarations... forms...)"
    (multiple-value-bind (body definitions) (parse-lexical-body2-definitions body)
      (multiple-value-bind (names set-forms labels-bindings) (collect-lexical-body2-definitions-names-and-set-forms definitions)
	(values body names set-forms labels-bindings))))


  (defun parse-documentation-source (body)
    "Returns (values body documentation-source).
Assumes body is (string form forms...) or ([documentation-tag] forms...)"
    (cond
      ((null body) (values body nil))
      ((documentation-tag? (first body))
       (values (rest body) (documentation-tag-form (first body))))
      ((and (not (null (rest body)))
	    (stringp (first body)))
       (values (rest body) (first body)))
      (t (values body nil))))

  (defun parse-guard-clauses (body)
    "Returns (values body guard-clauses). Assumes body is ([guard-tag] forms...)"
    (if (and (not (null body))
	     (guard-tag? (first body)))
	(values (rest body) (guard-tag-clauses (first body)))
	(values body nil)))

  (defun parse-metadata-from-function-body (function-body)
    "Return (values body documentation-source-form guard-clauses declarations). 
A function-body is ([documentation-source] [guard-tag] declarations... forms...)"
    (multiple-value-bind (body documentation-source) (parse-documentation-source function-body)
      (multiple-value-bind (body guard-clauses) (parse-guard-clauses body)
	(multiple-value-bind (body declarations) (parse-declarations body)
	  (values body documentation-source guard-clauses declarations)))))

  (defun function-body-guard-clauses (function-body)
    "Return (or guard-claueses nil) from function-body. See PARSE-METADATA-FROM-FUNCTION-BODY."
    (multiple-value-bind (body docs guard-clauses) (parse-metadata-from-function-body function-body)
      (declare (ignore body docs))
      guard-clauses))


  (defun lexical-body2-form (body)
    "Expands LISP-2 lexical-body definitions in BODY. A lexical-body is (definitions... declarations... forms...)"
    (multiple-value-bind (body names set-forms labels-bindings) (parse-lexical-body2 body)
      `(cl:let ,names
	 (cl:labels ,labels-bindings
	   ;; Declare local function bindings ignorable.
	   (declare (ignorable ,@(mapcar (cl:lambda (name) `(function ,name)) names)))
	   ,@set-forms
	   (cl:let ,(mapcar (cl:lambda (name) (list name name)) names)
	     ;; Declare names ignorable.
	     (declare (ignorable ,@names))
	     ,@body)))))

  (defun lexical-body-form (body)
    "Expands LISP-1 lexical-body definitions in BODY. A lexical-body is (definitions... declarations... forms...)"
    (multiple-value-bind (body names set-forms) (parse-lexical-body body)
      `(cl:let ,names
	 ,@set-forms
	 (cl:let ,(mapcar (cl:lambda (name) (list name name)) names)
	   ,@body)))))

(defmacro lexical-body (&body lexical-body)
  "Expands lexical-body definitions in body. Lexical-body is (definitions... declarations... forms...).
If definition is LISP-2 it is transformed and its labels-bindings are used. If definition is not LISP-2, but is LISP-1
it is transformed and a DEFAULT-LABELS-BINDING is used.
Creates mutually-recursive variable and function bindings for all definitions."
  (lexical-body2-form lexical-body))

(for-macros
  (defun parse-function (scm-parameters body)
    "Returns (values parameters body ignorable-parameters documentation-source guard-clauses declarations)"
    (multiple-value-bind (parameters ignorable-parameters) (arg-list->lambda-list scm-parameters)
      (multiple-value-bind (body documentation-source guard-clauses declarations) (parse-metadata-from-function-body body)
	(values parameters body ignorable-parameters documentation-source guard-clauses declarations)))))

(for-macros
  (defun declare-ignorable-forms (ignorable-names)
    (when ignorable-names `((declare (ignorable ,@ignorable-names))))))

(for-macros
  (defun lambda-form (parameters body ignorable-parameters guard-clauses declarations)
    "Return a lambda form that:
- declares ignorable-parameters alongside declarations
- tests against guard-clauses
- places body in a lexical-body"
    `(cl:lambda ,parameters
       ,@(declare-ignorable-forms ignorable-parameters)
       ,@declarations
       ;; TODO: pass in parent parameters?
       ,@(when guard-clauses `(,(guard-clauses-form guard-clauses parameters)))
       (lexical-body ,@body))))

(defmacro lambda (scm-parameters &body body)
  "Lambda that uses SCHEMEISH scm-parameters. Registers metadata: parameters, guard-clauses and documentation.
Tests for provided guard-clauses. Evaluates body in a lexical-body form. See DEFINE for explanation of scm-parameters."
  (multiple-value-bind (parameters body ignorable-parameters documentation-source guard-clauses declarations)
      (parse-function scm-parameters body)
    `(register-lambda-metadata
      ,(lambda-form parameters body ignorable-parameters guard-clauses declarations)
      ',scm-parameters
      ',guard-clauses
      ,documentation-source)))

(defmacro define (name-field &body body)
  "define can define names for function.
   (define name #'function)
   
   define can define functions with scheme-inspired lambda lists and a function-body.
   (define (name . lambda-list) . function-body)

   define can define nested closures similar to how functions are defined. 
   (define (((name . lambda-list0) . lambda-list1) . lambda-list2) . function-body)
      this would define a function similar to:
         (defun name lambda-list0
            (lambda lambda-list1
               (lambda lambda-list2
                  . body)))

   a lambda-list can be (in this documentation, {} denotes optional values, ... denotes 0 or more values).
   (positional-arguments... optional-arguments...)
   (positional-arguments... keyword-arguments...)
   (positional-arguments... . rest-argument)

   a positional arugment is a symbol.
   an optional argument is either:  (symbol) or (symbol default-value)
   a keyword argument is a keyword:  :name (:name default-value)
     if the keyword argument :name is used, the symbol name will be bound in the function body.
   a rest-argument is a symbol.

   positional, optional, and rest-arguments may be autmomatically declared as ignorable by prefixing with an _.
     if a symbol named _ is used without any suffix, a unique symbol is generated for that argument, and it is declared ignorable. 

   a function-body is structured as follows:
     ({documentation} declare-forms... {guard} . lexical-body)

   documentation may either be a string or a documentation-tag
   declare-forms are a list of common lisp declare forms.
   a guard is a guard-tag containing a list of guard-clauses.
    if provided, each guard-clause will be evaluated when the function is called.
    if any guard-clause evaluates to false, an error is signaled.
  
   a lexical-body is of the form:
     (define-forms... declare-forms... . body-forms)

   it is an error for any of body-forms to be a define, declare, or guard form.

the behavior of define forms within a lexical-body is slightly modified and extended.
   nested define forms define both lexically scoped variables and functions.
   a group of defines in the same lexical-body are mutually recursive.

   to define a non-function lexical variable the following form is used:
   (define symbol value)

   a local function is also generated for symbol, meaning that if value is a function the following works:
   (symbol arguments...)

   similarly, if a function is defined like:
   (define (symbol . args) . body)
   a lexical variable is generated with its value set to #'symbol."
  (let ((name (definition-name name-field)))
    `(for-macros
       (fmakunbound ',name)
       (setf (fdefinition ',name)
	     (lexical-body
	       (define ,name-field ,@body)
	       ,name))
       ',name)))

(for-macros
  (defun definition-name-field (definition)
    (second definition))
  (defun definition-function-body (definition)
    (cddr definition))
  (defun definition-value (definition)
    (third definition))

  (defun definition-lambda-form (definition-name-field definition-guard-clauses scm-parameters body)
    "Return a form that creates a lambda with the given parameters. Sets the definition metadata for lambda."
    (multiple-value-bind (parameters body ignorable-parameters documentation-source guard-clauses declarations)
	(parse-function scm-parameters body)
      `(register-definition-function-metadata
	,(lambda-form parameters body ignorable-parameters guard-clauses declarations)
	',scm-parameters
	',definition-guard-clauses
	,documentation-source
	',definition-name-field)))

  (defun transform-lexical-body-define-pair (definition)
    "Transforms SCM (define (...) . function-body) for lexical-body."
    (let* ((definition-name-field (definition-name-field definition))
	   (function-body (definition-function-body definition))
	   (guard-clauses (function-body-guard-clauses function-body)))
      (let recurse ((name-field definition-name-field)
		    (body function-body))
	(let ((name (first name-field))
	      (parameters (rest name-field)))
	  (cond
	    ((consp name)
	     ;; Iteration: Closure definition.
	     (recurse name (list (definition-lambda-form definition-name-field guard-clauses parameters body))))
	    (t
	     ;; Base: Function definiton.
	     (values (list name) `(setq ,name ,(definition-lambda-form definition-name-field guard-clauses parameters body)))))))))

  (defun transform-lexical-body2-define-pair (definition)
    "Transforms SCHEMEISH (lisp-2) (define (...) . function-body) for lexical-body."
    (let* ((definition-name-field (definition-name-field definition))
	   (function-body (definition-function-body definition)))
      (multiple-value-bind (body documentation-source guard-clauses declarations)
	  (parse-metadata-from-function-body function-body)
	(let recurse ((name-field definition-name-field)
		      (body (append declarations `((lexical-body ,@body)))))
	  (let ((name (first name-field))
		(parameters (rest name-field)))
	    (cond
	      ((consp name)
	       ;; Iteration: Closure definition.
	       (recurse name (list (definition-lambda-form definition-name-field guard-clauses parameters body))))
	      (t
	       ;; Base: Function definiton.
	       (values (list name)
		       `(setq ,name (register-definition-function-metadata
				     (function ,name)
				     ',parameters
				     ',guard-clauses
				     ,documentation-source
				     ',definition-name-field))
		       (multiple-value-bind (parameters ignorable-parameters) (arg-list->lambda-list parameters)
			 (list `(,name ,parameters ,@(declare-ignorable-forms ignorable-parameters) ,@body)))))))))))

  (defun transform-lexical-body-define-symbol (definition)
    "Transforms SCM (lisp-1) (define symbol value) for lexical-body."
    (let ((name (definition-name-field definition))
	  (value (definition-value definition)))
      (values (list name) `(setq ,name ,value))))

  (defun transform-lexical-body-define-symbol-or-pair (definition)
    "Transforms SCM (lisp-1) (define ...) for lexical-body."
    (let ((name-field (definition-name-field definition)))
      (cond
	((symbolp name-field) (transform-lexical-body-define-symbol definition))
	(t (transform-lexical-body-define-pair definition)))))

  (defun transform-lexical-body2-define-symbol-or-pair (definition)
    "Transforms SCHEMEISH (lisp-2) (define ...) for lexical-body."
    (let ((name-field (definition-name-field definition)))
      (cond
	((symbolp name-field)
	 (multiple-value-bind (names set-form) (transform-lexical-body-define-symbol definition)
	   (values names set-form (mapcar #'default-labels-binding names))))
	(t (transform-lexical-body2-define-pair definition)))))

  (defun transform-lexical-body-define-values (definition)
    "Transforms SCM (define-values name-or-names values-form) for lexical-body."
    (let ((name-field (definition-name-field definition))
	  (values-form (definition-value definition)))
      (flet ((ignore? (symbol) (string= (symbol-name symbol) "_")))
	(cond
	  ((symbolp name-field) (values (list name-field) `(setq ,name-field (multiple-value-list ,values-form))))
	  (t
	   (let iter ((given-names name-field)
		      (names ())
		      (vars ())
		      (ignored-names ()))
	     (cond
	       ((null given-names)
		(values (nreverse names)
			`(cl:let ,ignored-names
			   (multiple-value-setq ,(nreverse vars) ,values-form))))
	       (t (let ((name (first given-names))
			(rest-names (rest given-names)))
		    (if (ignore? name)
			(let ((var (unique-symbol 'ignore)))
			  (iter rest-names names (cons var vars) (cons var ignored-names)))
			(iter rest-names (cons name names) (cons name vars) ignored-names))))))))))))

(for-macros
  (register-lexical-body-definition 'define #'transform-lexical-body-define-symbol-or-pair)
  (register-lexical-body-definition 'define-values #'transform-lexical-body-define-values)
  (register-lexical-body2-definition 'define #'transform-lexical-body2-define-symbol-or-pair))

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

(uninstall-syntax!)

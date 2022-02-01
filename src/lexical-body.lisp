(DEFPACKAGE #:SCHEMEISH.LEXICAL-BODY
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.ARGUMENTS
        #:SCHEMEISH.FOR-MACROS
        #:SCHEMEISH.NAMED-LET
        #:SCHEMEISH.SYNTAX)
  (:EXPORT #:COMPILER-MACRO-DOCUMENTATION-SOURCE
	   #:DEF
           #:DEFAULT-LABELS-BINDING
           #:DEFINE
           #:DISABLE-GUARD-CLAUSES!
           #:DOCUMENTATION-SOURCE?
           #:DOCUMENTATION-STRING
           #:ENABLE-GUARD-CLAUSES!
           #:EXPOSE
           #:EXPOSE-FUNCTIONS
           #:EXPOSE-VARIABLES
           #:GUARD-CLAUSES
           #:GUARD-CLAUSES-ENABLED?
           #:LAMBDA
           #:LEXICAL-BODY-DEFINITION-DOCUMENTATIONS
           #:LEXICAL-BODY-DEFINITION?
           #:LEXICAL-BODY2-DEFINITION-DOCUMENTATIONS
           #:LEXICAL-BODY2-DEFINITION?
           #:LEXICALLY
           #:OBJECT-DOCUMENTATION-SOURCE
           #:PARSE-DOCUMENTATION-SOURCE
           #:PARSE-FUNCTION
           #:PARSE-METADATA-FROM-FUNCTION-BODY
           #:REGISTER-LEXICAL-BODY-DEFINITION
           #:REGISTER-LEXICAL-BODY2-DEFINITION
           #:REGISTERED-DEFINITION-NAME-FIELD
           #:SCM-PARAMETERS->ORDINARY-LAMBDA-LIST
           #:SET-COMPILER-MACRO-DOCUMENTATION-SOURCE!
           #:SET-OBJECT-DOCUMENTATION-FROM-DOCUMENTATION-SOURCE!
           #:SET-OBJECT-DOCUMENTATION-SOURCE!
           #:SET-SETF-DOCUMENTATION-SOURCE!
           #:SET-TYPE-DOCUMENTATION-SOURCE!
           #:SET-VARIABLE-DOCUMENTATION-SOURCE!
           #:SETF-DOCUMENTATION-SOURCE
           #:TRANSFORM-LEXICAL-BODY-DEFINE-DESTRUCTURING
           #:TRANSFORM-LEXICAL-BODY-DEFINE-SYMBOL-OR-PAIR
           #:TRANSFORM-LEXICAL-BODY-DEFINE-VALUES
           #:TRANSFORM-LEXICAL-BODY2-DEFINE-SYMBOL-OR-PAIR
           #:TYPE-DOCUMENTATION-SOURCE
           #:UNDEFINE
           #:UNREGISTER-LEXICAL-BODY-DEFINITION
           #:UNREGISTER-LEXICAL-BODY2-DEFINITION
           #:VARIABLE-DOCUMENTATION-SOURCE)
  (:SHADOW #:LAMBDA))

(in-package :schemeish.lexical-body)

(install-syntax!)

(for-macros
  (defvar *guard-clauses-enabled?* t)
  (export (defun guard-clauses-enabled? () *guard-clauses-enabled?*))
  (export (defun enable-guard-clauses! () (setq *guard-clauses-enabled?* t)))
  (export (defun disable-guard-clauses! () (setq *guard-clauses-enabled?* nil)))
  (export (defmacro with-guard-clauses-enabled (&body body)
	    `(cl:let ((*guard-clauses-enabled?* t))
	       ,@body)))
  (export (defmacro with-guard-clauses-disabled (&body body)
	    `(cl:let ((*guard-clauses-enabled?* nil))
	       ,@body)))

  (defvar *guard-clauses-table* (make-hash-table :weakness :key)
    "A table from function to a list of guard-clauses guarding that function.")

  (defun register-guard-clauses (function guard-clauses)
    "Associates function with guard-clauses in the *guard-clauses-hash-table*"
    (assert (functionp function))
    (assert (listp guard-clauses))
    (setf (gethash function *guard-clauses-table*) guard-clauses)
    function)
  (export
   (defun guard-clauses (function)
     "Retrieves the guard-clauses associated with function, or NIL if not present."
     (assert (functionp function))
     (gethash function *guard-clauses-table*)))

  (defvar *definition-name-field-table* (make-hash-table)
    "A hash table from function -> definition-name-field.")
  (defun register-definition-name-field (function name-field)
    (assert (functionp function))
    (setf (gethash function *definition-name-field-table*) name-field))

  (export
   (defun registered-definition-name-field (function)
     "Retrieve the DEFINE name-field used to define function.
Returns nil if not defined using DEFINE."
     (gethash function *definition-name-field-table*)))

  (defgeneric documentation-string (documentation)
    (:documentation "Returns a documentation string given the provided documentation object."))
  (defmethod documentation-string ((documentation string)) documentation)
  (export 'documentation-string)
  (export
   (defun documentation-source? (object)
     "An object is a documentation-source if it has a method implemented for documentation-string."
     (compute-applicable-methods #'documentation-string (list object))))

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

  (export
   (defun variable-documentation-source (symbol)
     "Returns the documentation source associated with the constant or dynamic variable named symbol."
     (check-symbol symbol)
     (gethash symbol *variable-documentation-hash-table* nil)))
  (export
   (defun type-documentation-source (symbol)
     "Returns the documentation source associated with the type named by symbol."
     (check-symbol symbol)
     (gethash symbol *type-documentation-hash-table* nil)))
  (export
   (defun compiler-macro-documentation-source (name)
     "Returns the documentation source associated with the compiler-macro named by NAME."
     (check-name name)
     (gethash name *compiler-macro-documentation-hash-table* nil)))
  (export
   (defun setf-documentation-source (symbol)
     "Returns the documentation source associated with the setf-expansion named by symbol."
     (check-symbol symbol)
     (gethash symbol *setf-documentation-hash-table* nil)))
  (export
   (defun object-documentation-source (object)
     "Returns the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
     (check-object object)
     (gethash object *object-documentation-hash-table* nil)))

  (export
   (defun set-variable-documentation-source! (symbol documentation-source)
     "Updates the documentation source associated with the constant or dynamic variable named symbol."
     (check-symbol symbol)
     (setf (gethash symbol *variable-documentation-hash-table*) documentation-source)
     symbol))
  (export
   (defun set-type-documentation-source! (symbol documentation-source)
     "Updates the documentation source associated with the type named by symbol."
     (check-symbol symbol)
     (setf (gethash symbol *type-documentation-hash-table*) documentation-source)
     symbol))
  (export
   (defun set-compiler-macro-documentation-source! (name documentation-source)
     "Updates the documentation source associated with the compiler-macro named by NAME."
     (check-name name)
     (setf (gethash name *compiler-macro-documentation-hash-table*) documentation-source)
     name))
  (export
   (defun set-setf-documentation-source! (symbol documentation-source)
     "Updates the documentation source associated with the setf-expansion named by symbol."
     (check-symbol symbol)
     (setf (gethash symbol *setf-documentation-hash-table*) documentation-source)
     symbol))
  (export
   (defun set-object-documentation-source! (object documentation-source)
     "Updates the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
     (check-object object)
     (setf (gethash object *object-documentation-hash-table*) documentation-source)
     object))


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

  (defparameter *cl-ordinary-lambda-list-keywords* '(cl:&optional cl:&key cl:&rest cl:&aux cl:&allow-other-keys))
  (defun map-ordinary-lambda-list-parameters (proc ordinary-lambda-list)
    "Map over the CL lambda-list parameters. Accepted keywords are *CL-ORDINARY-LAMBDA-LIST-KEYWORDS*.
Proc is called with either [proc :keyword keyword] or [proc group parameter], where
group is one of (:optional :key :aux :rest :positional)."
    (let ((keywords  *cl-ordinary-lambda-list-keywords*))
      (labels ((map-keyword (keyword parameters result)
		 (let ((result (cons [proc :keyword keyword] result)))
		   (ecase keyword
		     (cl:&optional (map-group :optional parameters result))
		     (cl:&key (map-group :key parameters result))
		     (cl:&allow-other-keys (map-group :key parameters result))
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
	
	(nreverse (map-group :positional ordinary-lambda-list ())))))

  (defparameter *destructuring-lambda-list-keywords* '(cl:&whole cl:&optional cl:&rest cl:&body cl:&key cl:&allow-other-keys cl:&aux))
  (defun map-destructuring-lambda-list-parameters (proc destructuring-lambda-list)
    (let ((keywords  *destructuring-lambda-list-keywords*))
      (labels ((map-keyword (keyword parameters result)
		 (let ((result (cons [proc :keyword keyword] result)))
		   (ecase keyword
		     (cl:&optional (map-group :optional parameters result))
		     (cl:&key (map-group :key parameters result))
		     (cl:&allow-other-keys (map-group :key parameters result))
		     ((cl:&rest cl:&body) (map-group :rest parameters result))
		     (cl:&aux (map-group :aux parameters result)))))
	       (map-group (group-name parameters result)
		 (cond
		   ((null parameters) result)
		   (t (let ((parameter (first parameters))
			    (rest-parameters (rest parameters)))
			(cond
			  ((member parameter keywords) (map-keyword parameter rest-parameters result))
			  (t (map-group group-name rest-parameters (cons [proc group-name parameter] result)))))))))
	(nreverse
	 (if (eq (first destructuring-lambda-list) 'cl:&whole)
	     (map-group :positional (rest (rest destructuring-lambda-list))
			(list [proc :whole (second destructuring-lambda-list)] [proc :keyword 'cl:&whole]))
	     (map-group :positional destructuring-lambda-list ())))))))

(assert (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &key (key value) &aux (aux value) &rest rest)))
	  (equal (map-ordinary-lambda-list-parameters (cl:lambda (&rest args) (second args)) parameters)
		 parameters)))

(assert (let ((parameters '(&whole whole a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &body body
			    &key (key value) &aux (aux value))))
	  (equal (map-destructuring-lambda-list-parameters (cl:lambda (&rest args) (second args)) parameters)
		 parameters)))

(for-macros
  (defun ordinary-lambda-list-parameter-names (parameters)
    "Returns an ordered list of bound names in the given parameter list."
    (apply #'append (map-ordinary-lambda-list-parameters (cl:lambda (group parameter)
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
		 (ordinary-lambda-list-parameter-names parameters))
	       '(A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? KEY AUX REST)))

(for-macros
  (defun destructuring-lambda-list-parameter-names (parameters)
    "Returns an ordered list of bound names in the given parameter list."
    (apply #'append (map-destructuring-lambda-list-parameters
		     (cl:lambda (group parameter)
		       (ecase group
			 (:keyword ())
			 ((:positional :rest :whole) (list parameter))
			 ((:optional :key :aux)
			  (cond
			    ((consp parameter)
			     (cond
			       ((= (length parameter) 3) (list (first parameter) (third parameter)))
			       (t (list (first parameter)))))
			    (t (list parameter))))))
		     parameters))))

(assert (equal (let ((parameters '(&whole whole a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
				   &body body
				   &key (key value) &aux (aux value))))
		 (destructuring-lambda-list-parameter-names parameters))
	       '(WHOLE A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? BODY KEY AUX)))

(for-macros
  (defun parameter-bindings-form (parameters)
    "Returns a form that evaluates to a list of (name value) bindings for the CL lambda-list parameters."
    `(list ,@(mapcar (cl:lambda (name) `(list ',name ,name)) (ordinary-lambda-list-parameter-names parameters))))

  (defun enforce-guard-clauses-form (guard-clauses parameters)
    "Return a form that processes guard-clauses, causing an error if any clause fails.
Checks if *guard-clauses-enabled?* is true before evaluating any guard clauses."
    `(when *guard-clauses-enabled?*
       ,@(mapcar (cl:lambda (guard-clause)
		   `(unless ,guard-clause
		      (error "Failed function guard-clause: ~S with the given parameter bindings: ~S" ',guard-clause ,(parameter-bindings-form parameters))))
		 guard-clauses))))

(for-macros
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

  (export
   (defun register-lexical-body-definition (symbol transform)
     "Registers transform for lisp-1 lexical-body tranformations.
Transform is a procedure s.t. (transform definition-form) => (values names set-form)"
     (setf (gethash symbol *lexical-body-definition-table*) transform)))
  (export
   (defun register-lexical-body2-definition (symbol transform)
     "Registers transform for lisp-2 lexical-body transformations.
Transform is a procedure s.t. (transform definition-form) => (values names set-form labels-bindings)"
     (setf (gethash symbol *lexical-body2-definition-table*) transform)))

  (export
   (defun unregister-lexical-body-definition (symbol)
     "Unregisters transform for lisp-1 lexical-body tranformations."
     (remhash symbol *lexical-body-definition-table*)))
  (export
   (defun unregister-lexical-body2-definition (symbol)
     "Unregisters transform for lisp-2 lexical-body transformations."
     (remhash symbol *lexical-body2-definition-table*)))
  
  (export
   (defun lexical-body-definition? (form)
     "True if FORM is a registered lisp-1 style lexical-body definition."
     (and (consp form)
	  (gethash (first form) *lexical-body-definition-table* nil))))
  (defun transform-lexical-body-definition (form)
    "Transform FORM if FORM is a registered lisp-1 style lexical-body definition.
Returns (values names set-form)"
    [(gethash (first form) *lexical-body-definition-table*) form])
  (export
   (defun lexical-body2-definition? (form)
     "True if FORM is a registered lisp-2 style lexical-body definition."
     (and (consp form)
	  (gethash (first form) *lexical-body2-definition-table* nil))))
  (defun transform-lexical-body2-definition (form)
    "Transform FORM if FORM is a registered lisp-2 style lexical-body definition.
Returns (values variable-names variables-set-form labels-bindings)"
    [(gethash (first form) *lexical-body2-definition-table*) form])
  
  (export
   (defun lexical-body-definition-documentations ()
     "Returns a list of (symbol documentation) for all currently registered lisp-1 style lexical-body definitions."
     (let ((result ()))
       (maphash (cl:lambda (symbol transform)
		  (push (cons symbol (documentation transform t)) result))
		*lexical-body-definition-table*)
       result)))
  (export
   (defun lexical-body2-definition-documentations ()
     "Returns a list of (symbol documentation-source) for all currently registered lisp-2 style lexical-body definitions.
Includes results for lisp-1 style lexical-body definitions if there are no applicable transforms for lisp-2 style lexical-body."
     (let ((result ())
	   (body1-result (lexical-body-definition-documentations)))
       (maphash (cl:lambda (symbol transform)
		  (push (cons symbol (documentation transform t)) result))
		*lexical-body2-definition-table*)
       (union result (set-difference body1-result result :key #'first))))))

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
    "Returns (values names set-forms) for lisp-1 style lexical-body."
    (let iter ((definitions definitions)
	       (names ())
	       (set-forms ()))
      (cond
	((null definitions) (values names (nreverse set-forms)))
	(t (multiple-value-bind (new-names set-form) (transform-lexical-body-definition (first definitions))
	     (iter (rest definitions) (append names new-names) (cons set-form set-forms)))))))

  (export
   (defun default-labels-binding (name)
     "Return a binding with the given name for LABELS which just applies the lexical variable name to its arguments."
     (let ((rest (unique-symbol 'arguments)))
       `(,name (&rest ,rest) (apply ,name ,rest)))))

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
    "Returns (values body names set-forms) for lisp-1 lexical-body.
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

  (export
   (defun parse-documentation-source (body)
     "Returns (values forms documentation-source).
Body is (documentation-string form forms...) or ([documentation-tag] forms...)
The documentation-source returned is either a DOCUMENTATION-TAG-FORM, a string, or nil.
For more information about documentation-tags, see DOCUMENTATION-TAG, DOCUMENTATION-STRING, and DOCUMENTATION-SOURCE?"
     (cond
       ((null body) (values body nil))
       ((documentation-tag? (first body))
	(values (rest body) (documentation-tag-form (first body))))
       ((and (not (null (rest body)))
	     (stringp (first body)))
	(values (rest body) (first body)))
       (t (values body nil)))))

  (defun parse-guard-clauses (body)
    "Returns (values body guard-clauses). Assumes body is ([guard-tag] forms...)"
    (if (and (not (null body))
	     (guard-tag? (first body)))
	(values (rest body) (guard-tag-clauses (first body)))
	(values body nil)))

  (export
   (defun parse-metadata-from-function-body (function-body)
     "Return (values lexical-body documentation-source-form guard-clauses declarations). 
A function-body is ([documentation-source] [guard-tag] declarations... lexical-body...)
For more information about DOCUMENTATION-SOURCE, see PARSE-DOCUMENTATION-SOURCE.
For more information about guard-tags, see GUARD-TAG.
For more information about lexical-body, see LEXICALLY."
     (multiple-value-bind (body documentation-source) (parse-documentation-source function-body)
       (multiple-value-bind (body guard-clauses) (parse-guard-clauses body)
	 (multiple-value-bind (body declarations) (parse-declarations body)
	   (values body documentation-source guard-clauses declarations))))))

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

(for-macros
  (export
   (defmacro lexically (&body lexical-body)
     "Expands lisp-2 style lexical-body definitions in body. 
A lexical-body is (lexical-body-definitions... declarations... forms...).
If definition is LISP-2 it is transformed and its labels-bindings are used.
If definition is not LISP-2, but is LISP-1 it is transformed and a DEFAULT-LABELS-BINDING is used.
Creates mutually-recursive variable and function bindings for all definitions.
See REGISTER-LEXICAL-BODY-DEFINITION and REGISTER-LEXICAL-BODY2-DEFINITION for more information about how
to extend LEXICALLY.
See the results of evaluating (lexical-body2-definition-documentations) and (lexical-body-definition-documentations)
For documentation on the currently registered definition transformations."
     (lexical-body2-form lexical-body))))

(for-macros
  (export
   (defun scm-parameters->ordinary-lambda-list (scm-parameters)
     "Returns (values ordinary-lambda-list ignorable-names).
Translates a SCHEMEISH style lambda-list into a Common Lisp ordinary-lambda-list.

SCM-PARAMETERS can be one of:
   (positional-arguments... optional-arguments...)
   (positional-arguments... keyword-arguments...)
   (positional-arguments... . rest-argument)
   rest-argument

Where:
   a positional arugment is a symbol
      e.g. (pos1 pos2 pos3)
   an optional argument is either:  (symbol) or (symbol default-value)
     e.g. (pos1 pos2 (optional1) (optional2 42))
   a keyword argument is a keyword:  :name (:name default-value)
     if the keyword argument :NAME is used, the symbol NAME will be bound in the function body.
     e.g. (pos1 pos2 :keyword1 (:keyword2 42))
   a rest-argument is a symbol.
     e.g. (p1 p2 . rest)

   positional, optional, and rest-arguments may be autmomatically declared as ignorable by prefixing with an _.
     if a symbol named _ is used without any suffix, a unique symbol is generated for that argument, and it is declared ignorable.
     e.g. (_ignorable-pos1 (_ignorable-optional1) (_)), where _ is not bound in the function body."
     (arg-list->lambda-list scm-parameters))))

(for-macros
  (export
   (defun parse-function (scm-parameters function-body)
     "Returns (values ordinary-lambda-list body ignorable-parameters documentation-source guard-clauses declarations)
Converts scm-parameters to an ordinary-lambda-list, and parses the metadata from function-body.
For more information about scm-parameters, SCM-PARAMETERS->ORDINARY-LAMBDA-LIST.
For more information about function-body, see PARSE-METADATA-FROM-FUNCTION-BODY."
     (multiple-value-bind (ordinary-lambda-list ignorable-parameters) (scm-parameters->ordinary-lambda-list scm-parameters)
       (multiple-value-bind (body documentation-source guard-clauses declarations) (parse-metadata-from-function-body function-body)
	 (values ordinary-lambda-list body ignorable-parameters documentation-source guard-clauses declarations))))))

(for-macros
  (defun declare-ignorable-forms (ignorable-names)
    "Returns a list of forms that declare ignorable-names to be ignorable"
    (when ignorable-names `((declare (ignorable ,@ignorable-names))))))

(for-macros
  (defun lambda-form (parameters body ignorable-parameters guard-clauses declarations)
    "Return a lambda form that:
- declares ignorable-parameters alongside declarations
- enforces guard-clauses
- places body in a lexically"
    `(cl:lambda ,parameters
       ,@(declare-ignorable-forms ignorable-parameters)
       ,@declarations
       ;; TODO: pass in parent parameters?
       ,@(when guard-clauses `(,(enforce-guard-clauses-form guard-clauses parameters)))
       (lexically ,@body))))

(for-macros
  (export
   (defmacro lambda (scm-parameters &body function-body)
     "Lambda that uses SCHEMEISH scm-parameters.
Registers metadata associated with function.
See PARSE-FUNCTION for explanation of scm-parameters and function-body."
     (multiple-value-bind (parameters lexical-body ignorable-parameters documentation-source guard-clauses declarations)
	 (parse-function scm-parameters function-body)
       `(register-lambda-metadata
	 ,(lambda-form parameters lexical-body ignorable-parameters guard-clauses declarations)
	 ',scm-parameters
	 ',guard-clauses
	 ,documentation-source)))))

(for-macros
  (export
   (defun set-object-documentation-from-documentation-source! (object documentation-source)
     "If documentation-source is non-nil, sets the object documentation-source and documentation string."
     (when documentation-source
       (setf (documentation object t) (documentation-string documentation-source))
       (set-object-documentation-source! object documentation-source))
     object)))

(for-macros
  (defun lexical-name->parameter-name (symbol)
    "Adds *ear-muffs* to symbol to make it look like a parameter, interning it."
    (intern (concatenate 'string "*" (symbol-name symbol) "*"))))

(for-macros
  (defun check-spec (spec name)
    (unless (or (symbolp spec)
		(and (= (length spec) 2)
		     (symbolp name)))
      (error "Malformed spec ~S: Expected NAME or (GLOBAL-NAME VALUE)" spec)))
  (defun expose-function-form (fn-spec)
    "Returns a (setf fdefinition) form for fn-spec."
    (let* ((pair? (consp fn-spec))
	   (name (if pair? (first fn-spec) fn-spec))
	   (value (if pair? (second fn-spec) fn-spec)))
      (check-spec fn-spec name)
      `(progn (setf (fdefinition ',name) ,value) ',name)))

  (defun expose-variable-form (var-spec)
    "Returns a defparameter form for var-spec."
    ;; TODO: allow for documentation
    (let* ((pair? (consp var-spec))
	   (name (if pair? (first var-spec) (lexical-name->parameter-name var-spec)))
	   (value (if pair? (second var-spec) var-spec)))
      (check-spec var-spec name)
      `(defparameter ,name ,value))))

(for-macros
  (export
   (defmacro expose ((&rest fn-specs) (&rest var-specs))
     "Define var-specs as parameters in the global scope via DEFPARAMETER.
Define fn-specs as functions in the global scope via (SETF FDEFINITION).

Fn-spec is one of:
  fn-name: Expands to (setf (fdefinition 'fn-name) fn-name)
  (global-fn-name value): Expands to (setf (fdefinition 'global-fn-name) value)

Var-spec one of:
  VAR-NAME: *Ear-muffs* are added to symbol to create *VAR-NAME*. Expands to (defparameter *var-name* var-name).
  (*global-special-name* value): Expands to (defparameter *global-special-name* value).

The return value is (PARAMETER-NAMES... GLOBAL-FN-NAMES ...)"
     `(list ,@(mapcar #'expose-variable-form var-specs) ,@(mapcar #'expose-function-form fn-specs))))

  (export
   (defmacro expose-functions (&rest fn-specs)
     "Expands to (EXPOSE (fn-specs...) ())"
     `(expose (,@fn-specs) ())))

  (export
   (defmacro expose-variables (&rest var-specs)
     "Expands to (EXPOSE () (var-specs...))"
     `(expose () (,@var-specs)))))


(for-macros
  (export
   (defmacro define (name-field &body body)
     "Essentially expands to (lexically (define name-field body...) (expose-functions ,name)).

For more information about lexically, see LEXICALLY.
For more information about expose, see EXPOSE.
For more information about the lisp-2 style lexical-body definition DEFINE, see TRANSFORM-LEXICAL-BODY2-DEFINE-SYMBOL-OR-PAIR."
     (let ((name (definition-name name-field)))
       `(for-macros
	  (fmakunbound ',name)
	  (lexically (define ,name-field ,@body) (expose-functions ,name))
	  ',name)))))

(for-macros
  (export
   (defmacro undefine (name-field &body ignored-body)
     "expands to (fmakunbound name)."
     (declare (ignore ignored-body))
     (let ((name (definition-name name-field)))
       `(for-macros (fmakunbound ',name))))))

(for-macros
  (defun definition-name-field (definition)
    "Returns name-field of (define name-field ...)"
    (second definition))
  (defun definition-function-body (definition)
    "Returns function-body of (define name-field function-body...)"
    (cddr definition))
  (defun definition-value (definition)
    "Returns value of (define name-field value)"
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
    "Transforms (define (...) . function-body) for lisp-1 style lexical-body."
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
    "Transforms (define (...) . function-body) for lisp-2 style lexical-body."
    (let* ((definition-name-field (definition-name-field definition))
	   (function-body (definition-function-body definition)))
      (multiple-value-bind (body documentation-source guard-clauses declarations)
	  (parse-metadata-from-function-body function-body)
	(let recurse ((name-field definition-name-field)
		      (body (append declarations `((lexically ,@body)))))
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
		       (multiple-value-bind (ordinary-lambda-list ignorable-parameters) (scm-parameters->ordinary-lambda-list parameters)
			 (list `(,name ,ordinary-lambda-list ,@(declare-ignorable-forms ignorable-parameters) ,@body)))))))))))

  (defun transform-lexical-body-define-symbol (definition)
    "Transforms (define symbol [documentation] value) for lisp-1 style lexical-body."
    (multiple-value-bind (body documentation-source) (parse-documentation-source (definition-function-body definition))
      (let ((name (definition-name-field definition))
	    (value (first body)))
	(values (list name)
		`(setq ,name (set-object-documentation-from-documentation-source! ,value ,documentation-source))))))

  (export
   (defun transform-lexical-body-define-symbol-or-pair (definition)
     "Transforms (define name-field ...) for lisp-1 style lexical-body.
If name-field is a symbol the expected form is (define symbol [documentation-source] value).
  A let binding is created for symbol, and value is assigned to it.
  The documentation-source and documentation string for value is set.
If name-field is a pair, the expected form is (define name-field function-body...)
  If name-field is a pair: ((...) . scm-parameters)
    A closure is created with the given scm-parameters, and define is recursively applied.
    E.g. (define (((nested x) y) z) function-body...) => 
         (define (nested x) (lambda (y) (lambda (z) function-body...)))

  If name-field is a pair: (symbol . scm-parameters)
    A lambda is created with the given scm-parameters and function-body, expanded using PARSE-FUNCTION."
     (let ((name-field (definition-name-field definition)))
       (cond
	 ((symbolp name-field) (transform-lexical-body-define-symbol definition))
	 (t (transform-lexical-body-define-pair definition))))))

  (export
   (defun transform-lexical-body2-define-symbol-or-pair (definition)
     "Transforms (define name-field ...) for lisp-2 style lexical-body.
If name-field is a symbol the expected form is (define symbol [documentation-source] value).
  A let binding is created for symbol, and value is assigned to it.
  A DEFAULT-LABELS-BINDING is created for symbol.
  The documentation-source and documentation string for value is set.
If name-field is a pair, the expected form is (define name-field function-body...)
  If name-field is a pair: ((...) . scm-parameters)
    A closure is created with the given scm-parameters, and define is recursively applied.
    E.g. (define (((nested x) y) z) function-body...) => 
         (define (nested x) (lambda (y) (lambda (z) function-body...)))
  If name-field is a pair: (symbol . scm-parameters)
    A labels binding is created with the given scm-parameters and function-body, expanded using PARSE-FUNCTION.
    A let binding is created for symbol, with #'symbol assigned to it.

See also: LEXICALLY, PARSE-FUNCTION."
     (let ((name-field (definition-name-field definition)))
       (cond
	 ((symbolp name-field)
	  (multiple-value-bind (names set-form) (transform-lexical-body-define-symbol definition)
	    (values names set-form (mapcar #'default-labels-binding names))))
	 (t (transform-lexical-body2-define-pair definition))))))

  (export
   (defun transform-lexical-body-define-values (definition)
     "Transforms (define-values name-or-names values-form) for lisp-1 style lexical-body.
If name-or-names is a symbol:
  A let binding is created, and the (multiple-values-list values-form) is assigned to it.
If name-or-names is a list of symbols:
  A let binding is created for each symbol, and they are bound using multiple-value-setq.
See also: LEXICALLY."
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

  (export
   (defun transform-lexical-body-define-destructuring (definition)
     "Transforms (define-destructuring destructuring-lambda-list expression) for lisp-1 style lexical-body.
Uses DESTRUCTURING-BIND to destructure expression and creates bindings for each name in destructuring-lambda-list."
     (let* ((lambda-list (definition-name-field definition))
	    (names (destructuring-lambda-list-parameter-names lambda-list))
	    (expression (definition-value definition)))
       (transform-lexical-body-define-values `(define-values ,names (destructuring-bind ,lambda-list ,expression (values ,@names))))))))


;; Register define and define-values
(for-macros
  ;; Lisp-1 style Lexical body definitions
  (register-lexical-body-definition 'define #'transform-lexical-body-define-symbol-or-pair)
  (register-lexical-body-definition 'define-values #'transform-lexical-body-define-values)
  (register-lexical-body-definition 'define-destructuring #'transform-lexical-body-define-destructuring)
  (register-lexical-body-definition 'def #'transform-lexical-body-define-symbol-or-pair)

  ;; Lisp-2 style lexical body definitions
  (register-lexical-body2-definition 'define #'transform-lexical-body2-define-symbol-or-pair)
  (register-lexical-body2-definition 'def #'transform-lexical-body2-define-symbol-or-pair))

(assert (equal (lexically
		 (define-destructuring (&whole whole r1 r2
					       &optional (o1 3 o1-provided?)
					       &body body)
		     '(r1 r2 o1 :k1 k1))
		 (list whole o1-provided?))
	       '((R1 R2 O1 :K1 K1) T)))


(define 2+ "Adds 2." (cl:lambda (&rest numbers) (apply #'+ 2 numbers)))
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

		 '(*LEXICAL-TEST-X* *lexical-test-w* LEXICAL-TEST-Y LEXICAL-TEST-Z))))

;;(uninstall-syntax!)

;; TODO: replace define pkg, delete lexically pkg, copy splitf, dropf, takef,
;; Package management:
;;  SCHEMEISH.INTERNAL
;;  SCHEMEISH

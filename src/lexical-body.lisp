(in-package #:schemeish.internals)

(install-syntax!)

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
(export
 (defun lexical-body2-definition? (form)
   "True if FORM is a registered lisp-2 style lexical-body definition."
   (and (consp form)
	(gethash (first form) *lexical-body2-definition-table* nil))))

(defun transform-lexical-body-definition (form)
  "Transform FORM if FORM is a registered lisp-1 style lexical-body definition.
Returns (values names set-form)"
  [(gethash (first form) *lexical-body-definition-table*) form])
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
     (union result (set-difference body1-result result :key #'first)))))

(defun parse-lexical-body-definitions (lexical-body)
  "Returns (values definitions body)"
  (splitf lexical-body #'lexical-body-definition?))
(defun parse-lexical-body2-definitions (lexical-body)
  "Returns (values definitions body). Takes both lisp-1 and lisp-2 definitions."
  (let ((definition? (cl:lambda (definition)
		       (or (lexical-body2-definition? definition)
			   (lexical-body-definition? definition)))))
    (splitf lexical-body definition?)))

(defun collect-lexical-body-definitions-names-and-set-forms (definitions)
  "Returns (values names set-forms) for lisp-1 style lexical-body."
  (labels ((iter (definitions names set-forms)
	     (cond
	       ((null definitions) (values names (nreverse set-forms)))
	       (t (multiple-value-bind (new-names set-form) (transform-lexical-body-definition (first definitions))
		    (iter (rest definitions) (append names new-names) (cons set-form set-forms)))))))
    (iter definitions () ())))

(defun default-labels-binding (name)
  "Return a binding with the given name for LABELS which just applies the lexical variable name to its arguments."
  (let ((rest (unique-symbol 'arguments)))
    `(,name (&rest ,rest) (apply ,name ,rest))))

(defun collect-lexical-body2-definitions-names-and-set-forms (definitions)
  "Returns (values names set-forms labels-bindings) for SCHEMEISH (lisp-2) lexical-body.
If there is no appliciable transformer for a lisp-2 definition, a lisp-1 definition will
be used with a DEFAULT-LABELS-BINDING."
  (labels ((iter (definitions names set-forms labels-bindings)
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
    (iter definitions () () ())))

(export
 (defun parse-lexical-body (body)
   "Returns (values forms declarations names set-forms) for lisp-1 lexical-body.
A lexical body is (definitions... declarations... forms...)"
   (multiple-value-bind (definitions body) (parse-lexical-body-definitions body)
     (multiple-value-bind (names set-forms) (collect-lexical-body-definitions-names-and-set-forms definitions)
       (multiple-value-bind (declarations body) (parse-declarations body)
	 (values body declarations names set-forms))))))
(defun parse-lexical-body2 (body)
  "Returns (values body names set-forms labels-bindings) for SCHEMEISH (lisp-2) lexical-body.
A lexical body is (definitions... declarations... forms...)"
  (multiple-value-bind (definitions body) (parse-lexical-body2-definitions body)
    (multiple-value-bind (names set-forms labels-bindings) (collect-lexical-body2-definitions-names-and-set-forms definitions)
      (values body names set-forms labels-bindings))))

(export
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
	    ,@body))))))

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
   (lexical-body2-form lexical-body)))

(lexical-body2-definition-documentations)


(uninstall-syntax!)

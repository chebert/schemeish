(in-package #:schemeish.internals)

(defvar *object-documentation-hash-table* (make-hash-table :weakness :key))
(export
 (defun documentable-object? (object)
   (or (typep object 'function)
       (typep object 'method-combination)
       (typep object 'standard-method)
       (typep object 'package))))

(export
 (defun set-object-documentation-source! (object documentation-source)
   "Updates the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package.
Returns object."
   (assert (documentable-object? object))
   (setf (gethash object *object-documentation-hash-table*) documentation-source)
   object))

(set-object-documentation-source!
 #'documentable-object?
 (function-doc :name 'documentable-object?
	       :syntax '(documentable-object? object)
	       :arguments-and-values `((object "An object"))
	       :description "Returns true if object can have its documentation set using (setf (document object t) ...)"))
(set-object-documentation-source!
 #'set-object-documentation-source!
 (function-doc :name 'set-object-documentation-source!
	       :syntax '(set-object-documentation-source! object documentation-source)
	       :arguments-and-values `((object "An object that satisfies " ,(function-reference 'documentable-object?))
				       (documentation-source "An object that will satisfy " ,(function-reference 'documentation-source?)))
	       :description '("Updates the " documentation-source " associated with the given " 'object ". Returns " 'object ".")
	       :examples ()
	       :side-effects `(("Updates " ,(variable-reference '*object-documentation-hash-table*)))
	       :affected-by ()
	       :exceptional-situations `(("Error if " 'object " does not satisfy " ,(function-reference 'documentation-object?)))
	       :see-also `(,(function-reference 'documentation-object?) ,(function-reference 'documentation-string) ,(function-reference 'documentation-source?)
									,(function-reference 'object-documentation-source))
	       :notes ()))

(defvar *variable-documentation-hash-table* (make-hash-table))
(defvar *type-documentation-hash-table* (make-hash-table))
(defvar *compiler-macro-documentation-hash-table* (make-hash-table))
(defvar *setf-documentation-hash-table* (make-hash-table))

(defun check-symbol (symbol)
  (unless (symbolp symbol)
    (error "Symbol expected to be a symbol type, but got: ~S" (type-of symbol))))
(defun check-name (name)
  (unless (or (symbolp name) (and (listp name)
				  (= 2 (length name))
				  (eq 'cl:setf (first name))
				  (symbolp (second name))))
    (error "Name expected to be a symbol or a list (setf symbol) but got: ~S" name)))


(export
 (defun set-variable-documentation-source! (symbol documentation-source)
   "Updates the documentation source associated with the constant or dynamic variable named symbol. Returns symbol."
   (check-symbol symbol)
   (setf (gethash symbol *variable-documentation-hash-table*) documentation-source)
   symbol))
(export
 (defun set-type-documentation-source! (symbol documentation-source)
   "Updates the documentation source associated with the type named by symbol. Returns symbol."
   (check-symbol symbol)
   (setf (gethash symbol *type-documentation-hash-table*) documentation-source)
   symbol))
(export
 (defun set-compiler-macro-documentation-source! (name documentation-source)
   "Updates the documentation source associated with the compiler-macro named by NAME. Returns name."
   (check-name name)
   (setf (gethash name *compiler-macro-documentation-hash-table*) documentation-source)
   name))
(export
 (defun set-setf-documentation-source! (symbol documentation-source)
   "Updates the documentation source associated with the setf-expansion named by symbol. Returns symbol."
   (check-symbol symbol)
   (setf (gethash symbol *setf-documentation-hash-table*) documentation-source)
   symbol))

(set-object-documentation-source!
 #'set-variable-documentation-source!
 (function-doc :name 'set-variable-documentation-source!
	       :syntax '(set-variable-documentation-source! symbol documentation-source)
	       :arguments-and-values `((symbol "An object that satisfies " ,(function-reference 'symbolp))
				       (documentation-source "An object that will satisfy " ,(function-reference 'documentation-source?)))
	       :description '("Updates the " 'documentation-source " associated with the given variable named " 'symbol ". Returns " 'symbol ".")
	       :side-effects `(("Updates " ,(variable-reference '*variable-documentation-hash-table*)))
	       :exceptional-situations `(("Error if OBJECT does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-string) ,(function-reference 'documentation-source?)
								       ,(function-reference 'variable-documentation-source))))
(set-object-documentation-source!
 #'set-type-documentation-source!
 (function-doc :name 'set-type-documentation-source!
	       :syntax '(set-type-documentation-source! symbol documentation-source)
	       :arguments-and-values `((symbol "An object that satisfies " ,(function-reference 'symbolp))
				       (documentation-source "An object that will satisfy " ,(function-reference 'documentation-source?)))
	       :description '("Updates the " 'documentation-source " associated with the given type named " 'symbol ". Returns " 'symbol ".")
	       :side-effects `(("Updates " ,(variable-reference '*type-documentation-hash-table*)))
	       :exceptional-situations `(("Error if OBJECT does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-string) ,(function-reference 'documentation-source?)
								       ,(function-reference 'type-documentation-source))))
(set-object-documentation-source!
 #'set-compiler-macro-documentation-source!
 (function-doc :name 'set-compiler-macro-documentation-source!
	       :syntax '(set-compiler-macro-documentation-source! name documentation-source)
	       :arguments-and-values `((name "A function-name. Either a symbol or (cl:setf symbol)")
				       (documentation-source "An object that will satisfy " ,(function-reference 'documentation-source?)))
	       :description '("Updates the " 'documentation-source " associated with the given compiler-macro named " 'name ". Returns " 'name ".")
	       :side-effects `(("Updates " ,(variable-reference '*compiler-macro-documentation-hash-table*)))
	       :exceptional-situations `(("Error if OBJECT does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-string) ,(function-reference 'documentation-source?)
								       ,(function-reference 'compiler-macro-documentation-source))))
(set-object-documentation-source!
 #'set-setf-documentation-source!
 (function-doc :name 'set-setf-documentation-source!
	       :syntax '(set-setf-documentation-source! symbol documentation-source)
	       :arguments-and-values `((symbol "An object that satisfies " ,(function-reference 'symbolp))
				       (documentation-source "An object that will satisfy " ,(function-reference 'documentation-source?)))
	       :description '("Updates the " 'documentation-source " associated with the given setf expansion named " 'symbol ". Returns " 'symbol ".")
	       :side-effects `(("Updates " ,(variable-reference '*setf-documentation-hash-table*)))
	       :exceptional-situations `(("Error if OBJECT does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-string) ,(function-reference 'documentation-source?)
								       ,(function-reference 'setf-documentation-source))))

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
   (assert (documentable-object? object))
   (gethash object *object-documentation-hash-table* nil)))

(set-object-documentation-source!
 #'variable-documentation-source
 (function-doc :name 'variable-documentation-source
	       :syntax '(variable-documentation-source symbol)
	       :arguments-and-values `((symbol "An object that satisfies " ,(function-reference 'symbolp)))
	       :description '("Returns the " documentation-source " associated with the variable named by " symbol ", or NIL.")
	       :affected-by `(,(variable-reference '*variable-documentation-hash-table*))
	       :exceptional-situations `(("Error if " symbol " does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-source?) ,(function-reference 'set-variable-documentation-source!))))
(set-object-documentation-source!
 #'type-documentation-source
 (function-doc :name 'type-documentation-source
	       :syntax '(type-documentation-source symbol)
	       :arguments-and-values `((symbol "An object that satisfies " ,(function-reference 'symbolp)))
	       :description '("Returns the " documentation-source " associated with the type named by " symbol ", or NIL.")
	       :affected-by `(,(variable-reference '*type-documentation-hash-table*))
	       :exceptional-situations `(("Error if " symbol " does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-source?) ,(function-reference 'set-type-documentation-source!))))
(set-object-documentation-source!
 #'compiler-macro-documentation-source
 (function-doc :name 'compiler-macro-documentation-source
	       :syntax '(compiler-macro-documentation-source name)
	       :arguments-and-values `((name "A function-name."))
	       :description '("Returns the " documentation-source " associated with the compiler-macro named by " name ", or NIL.")
	       :affected-by `(,(variable-reference '*compiler-macro-documentation-hash-table*))
	       :exceptional-situations `(("Error if " name " is not a function-name"))
	       :see-also `(,(function-reference 'documentation-source?) ,(function-reference 'set-compiler-macro-documentation-source!))))
(set-object-documentation-source!
 #'setf-documentation-source
 (function-doc :name 'setf-documentation-source
	       :syntax '(setf-documentation-source symbol)
	       :arguments-and-values `((symbol "A symbol."))
	       :description '("Returns the " documentation-source " associated with the setf expansion named by " symbol ", or NIL.")
	       :affected-by `(,(variable-reference '*setf-documentation-hash-table*))
	       :exceptional-situations `(("Error if " symbol " does not satisfy " ,(function-reference 'symbolp)))
	       :see-also `(,(function-reference 'documentation-source?) ,(function-reference 'set-setf-documentation-source!))))

(export
 (defun set-object-documentation-from-documentation-source! (object documentation-source)
   "If documentation-source is non-nil, sets both the object documentation-source and documentation string."
   (when documentation-source
     (setf (documentation object t) (documentation-string documentation-source))
     (set-object-documentation-source! object documentation-source))
   object))

(defstruct documentation-tag
  "A documentation-tag is an form that has been tagged as documentation for use in a function-body.
See also PARSE-METADATA-FROM-FUNCTION-BODY.
With SCHEMEISH syntax enabled, #dDOCUMENTATION-FORM => #.(make-documentation-tag :form DOCUMENTATION-FORM)."
  form)

(setf (fdefinition 'documentation-tag?) #'documentation-tag-p)
(export '(documentation-tag make-documentation-tag documentation-tag-form documentation-tag?))

(defun read-documentation-tag (stream char n)
  "Dispatch-macro reader for documentation-tags. #dDOCUMENTATION-FORM => #.(make-documentation-tag :form DOCUMENTATION-FORM)"
  (declare (ignore char n))
  (let ((form (read stream t (values) t)))
    (make-documentation-tag :form form)))
(defmethod print-object ((object documentation-tag) stream)
  (format stream "#D~S" (documentation-tag-form object)))

(assert (string= (print-object (make-documentation-tag :form "Documentation") nil)
		 "#D\"Documentation\""))

(defgeneric documentation-string (documentation)
  (:documentation "Returns a documentation string given the provided documentation object."))
(defmethod documentation-string ((documentation string)) documentation)
(export 'documentation-string)
(export
 (defun documentation-source? (object)
   "An object is a documentation-source if it has a method implemented for documentation-string."
   (compute-applicable-methods #'documentation-string (list object))))

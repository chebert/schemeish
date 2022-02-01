(in-package #:schemeish.internals)

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
(export
 (defun set-object-documentation-source! (object documentation-source)
   "Updates the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package.
Returns object."
   (check-object object)
   (setf (gethash object *object-documentation-hash-table*) documentation-source)
   object))

(export
 (defun set-object-documentation-from-documentation-source! (object documentation-source)
   "If documentation-source is non-nil, sets both the object documentation-source and documentation string."
   (when documentation-source
     (setf (documentation object t) (documentation-string documentation-source))
     (set-object-documentation-source! object documentation-source))
   object))
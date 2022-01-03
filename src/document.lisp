(in-package :schemeish.document)

(defvar *function-docs-hash-table* (make-hash-table :weakness :key))
(defvar *function-symbol-docs-hash-table* (make-hash-table))
(export (defun function-documentation (symbol-or-function)
	  "Retrieve the rich-documentation for symbol-or-function if available, otherwise
return the docstring."
	  (if (symbolp symbol-or-function)
	      (or (gethash symbol-or-function *function-symbol-docs-hash-table*)
		  (documentation symbol-or-function 'function))
	      (or (gethash symbol-or-function *function-docs-hash-table*)
		  (documentation symbol-or-function t)))))
(defvar *variable-and-constant-docs-hash-table* (make-hash-table))
(export (defun variable-documentation (symbol)
	  "Retrieve the rich-documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *variable-and-constant-docs-hash-table*)
	      (documentation symbol 'variable))))
(export (defun constant-documentation (symbol)
	  "Retrieve the rich-documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *variable-and-constant-docs-hash-table*)
	      (documentation symbol 'constant))))
(defvar *type-docs-hash-table* (make-hash-table))
(export (defun type-documentation (symbol)
	  "Retrieve the rich-documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *type-docs-hash-table*)
	      (documentation symbol 'type))))


(defgeneric documentation-string (documentation)
  (:documentation "Resolves a documentation object to a string."))
(defmethod documentation-string ((s string))
  s)
(export 'documentation-string)

;; TODO: document-package
;; undocument*
;; TODO: deprecate-

(export
 (defun document (documentation symbol-or-function)
   "Sets the documentation of the provided symbol, function, generic-function, or method.
Symbol may name a function, macro-function, generic-function, or method."
   (let* ((function (if (symbolp symbol-or-function)
			(symbol-function symbol-or-function)
			symbol-or-function))
	  (doc-string (documentation-string documentation)))
     (setf (documentation function t) doc-string)
     (setf (gethash function *function-docs-hash-table*) documentation)
     (when (symbolp symbol-or-function)
       (setf (documentation symbol-or-function 'function) doc-string)
       (setf (gethash symbol-or-function *function-symbol-docs-hash-table*) documentation))
     symbol-or-function)))
(export
 (defun document-variable (documentation symbol)
   "Sets the documentation of the special variable associated with symbol."
   (setf (documentation symbol 'variable) (documentation-string documentation))
   (setf (gethash symbol *variable-and-constant-docs-hash-table*) documentation)
   symbol))
(export
 (defun document-constant (documentation symbol)
   "Sets the documentation of the constant value associated with symbol."
   (setf (documentation symbol 'constant) (documentation-string documentation))
   (setf (gethash symbol *variable-and-constant-docs-hash-table*) documentation)
   symbol))
(export
 (defun document-type (documentation symbol)
   "Sets the documentation of the type named by symbol."
   (setf (documentation symbol 'type) (documentation-string documentation))
   (setf (gethash symbol *type-docs-hash-table*) documentation)
   symbol))

(export (defstruct doc objects))
(export (defstruct doc-value
	  "A lisp value which can be formatted as a string using (format nil \"~S\" value)"
	  value))
(export (defstruct doc-string
	  "A lisp string."
	  value))
(defstruct doc-reference
  "An untyped reference to a symbol."
  symbol)
(export (defstruct (doc-function-reference (:include doc-reference))
	  "A doc-reference to a function symbol."))
(export (defstruct (doc-variable-reference (:include doc-reference))
	  "A doc-reference to a variable symbol."))
(export (defstruct (doc-constant-reference (:include doc-reference))
	  "A doc-reference to a constant symbol."))
(export (defstruct (doc-type-reference (:include doc-reference))
	  "A doc-reference to a type symbol."))
(export (defstruct (doc-group-reference (:include doc-reference))
	  "A doc-reference to a named group."))

(export 'doc-value-value)
(export 'doc-objects)
(export 'doc-reference-symbol)

(defmethod documentation-string ((doc doc-value))
  (format nil "the value ~S" (doc-value-value doc)))
(defmethod documentation-string ((doc doc-string))
  (doc-string-value doc))
(defmethod documentation-string ((doc doc-function-reference))
  (format nil "the function ~S" (doc-reference-symbol doc)))
(defmethod documentation-string ((doc doc-variable-reference))
  (format nil "the variable ~S" (doc-reference-symbol doc)))
(defmethod documentation-string ((doc doc-constant-reference))
  (format nil "the constant ~S" (doc-reference-symbol doc)))
(defmethod documentation-string ((doc doc-group-reference))
  (format nil "the group ~S" (doc-reference-symbol doc)))
(defmethod documentation-string ((doc doc-type-reference))
  (format nil "the type ~S" (doc-reference-symbol doc)))

(defmethod documentation-string ((doc doc))
  (with-output-to-string (s)
    (loop for doc in (doc-objects doc) do
      (format s "~A" (documentation-string doc)))))

(export
 (defun doc-func-ref (symbol)
   (make-doc-function-reference :symbol symbol)))
(export
 (defun doc-variable-ref (symbol)
   (make-doc-variable-reference :symbol symbol)))
(export
 (defun doc-constant-ref (symbol)
   (make-doc-constant-reference :symbol symbol)))
(export
 (defun doc-type-ref (symbol)
   (make-doc-type-reference :symbol symbol)))
(export
 (defun doc-group-ref (symbol)
   (make-doc-group-reference :symbol symbol)))

(export (defun newlines (&optional (count 1))
	  (make-string count :initial-element #\newline)))
(export (defun tabs (&optional (count 1))
	  (make-string count :initial-element #\tab)))
(export
 (defun doc (&rest objects)
   (make-doc :objects (mapcar (lambda (object)
				(cond ((doc-reference-p object) object)
				      ((stringp object) (make-doc-string :value object))
				      (t (make-doc-value :value object))))
			      objects))))

(document-type
 (doc "DOC is an object for storing rich documentation. It can contain any of" (newlines)
      (doc-type-ref 'doc-value) ", "
      (doc-type-ref 'doc-string) ", "
      (doc-type-ref 'doc-function-reference) ", "
      (doc-type-ref 'doc-variable-reference) ", " (newlines)
      (doc-type-ref 'doc-constant-reference) ", "
      (doc-type-ref 'doc-group-reference) ", or "
      (doc-type-ref 'doc-type-reference) ".")
 'doc)

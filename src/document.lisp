(in-package :schemeish.document)

(defvar *function-docs-hash-table* (make-hash-table :weakness :key))
(defvar *function-symbol-docs-hash-table* (make-hash-table))
(export (defun function-documentation (symbol-or-function)
	  "Retrieve the marked up documentation for symbol-or-function if available, otherwise
return the docstring."
	  (if (symbolp symbol-or-function)
	      (or (gethash symbol-or-function *function-symbol-docs-hash-table*)
		  (documentation symbol-or-function 'function))
	      (or (gethash symbol-or-function *function-docs-hash-table*)
		  (documentation symbol-or-function t)))))
(defvar *variable-and-constant-docs-hash-table* (make-hash-table))
(export (defun variable-documentation (symbol)
	  "Retrieve the marked up documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *variable-and-constant-docs-hash-table*)
	      (documentation symbol 'variable))))
(export (defun constant-documentation (symbol)
	  "Retrieve the marked up documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *variable-and-constant-docs-hash-table*)
	      (documentation symbol 'constant))))
(defvar *type-docs-hash-table* (make-hash-table))
(export (defun type-documentation (symbol)
	  "Retrieve the marked up documentation for symbol if available, otherwise
return the docstring."
	  (or (gethash symbol *type-docs-hash-table*)
	      (documentation symbol 'type))))
(defvar *package-docs-hash-table* (make-hash-table :weakness :key))
(export (defun package-documentation (package-designator)
	  "Retrieve the marked up documentation for package-designator if available, otherwise
return the docstring. If the package does not exist, return nil."
	  (let ((package (find-package package-designator)))
	    (when package
	      (or (gethash package *package-docs-hash-table*)
		  (documentation package 'type))))))

(defgeneric render-markup (markup output-spec stream)
  (:documentation "Renders the markup object to the stream based on output-spec.
Output-spec describes the markup format e.g. :string :html.
Context describes things like the indentation and the number of characters per line."))

(defmethod render-markup (value (output-spec (eql :string)) context stream)
  (format stream "~S" value))
(defmethod render-markup ((string string) (output-spec (eql :string)) context stream)
  (format stream "~A" string))

(defun documentation-string (markup)
  "Renders markup object to a documentation string."
  (with-output-to-string (stream)
    (render-markup markup :string (make-markup-context :indentation-level 0) stream)))

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
(export
 (defun document-package (documentation package-designator)
   "Sets the documentation of the package if it exists. Returns package."
   (let ((package (find-package package-designator)))
     (when package
       (setf (documentation package t) (documentation-string documentation))
       (setf (gethash package *type-docs-hash-table*) documentation)
       package))))

(export (defstruct doc objects))
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

(defmethod render-markup ((doc doc-value) (output-spec (eql :string)) context stream)
  (format stream "the value ~S" (doc-value-value doc)))
(defmethod render-markup ((doc doc-string) (output-spec (eql :string)) context stream)
  (format stream "~A" (doc-string-value doc)))
(defmethod render-markup ((doc doc-function-reference) (output-spec (eql :string)) context stream)
  (format stream "the function ~S" (doc-reference-symbol doc)))
(defmethod render-markup ((doc doc-variable-reference) (output-spec (eql :string)) context stream)
  (format stream "the variable ~S" (doc-reference-symbol doc)))
(defmethod render-markup ((doc doc-constant-reference) (output-spec (eql :string)) context stream)
  (format stream "the constant ~S" (doc-reference-symbol doc)))
(defmethod render-markup ((doc doc-group-reference) (output-spec (eql :string)) context stream)
  (format stream "the group ~S" (doc-reference-symbol doc)))
(defmethod render-markup ((doc doc-type-reference) (output-spec (eql :string)) context stream)
  (format stream "the type ~S" (doc-reference-symbol doc)))

(defmethod render-markup ((doc doc) (output-spec (eql :string)) context stream)
  (loop for doc in (doc-objects doc) do
    (format stream "~A" (documentation-string doc))))

(export
 (defun doc-func-ref (symbol)
   "Construct a documentation markup object that references the function value of symbol."
   (make-doc-function-reference :symbol symbol)))
(export
 (defun doc-variable-ref (symbol)
   "Construct a documentation markup object that references the variable/parameter value of symbol."
   (make-doc-variable-reference :symbol symbol)))
(export
 (defun doc-constant-ref (symbol)
   "Construct a documentation markup object that references the constant value of symbol."
   (make-doc-constant-reference :symbol symbol)))
(export
 (defun doc-type-ref (symbol)
   "Construct a documentation markup object that references the type value of symbol."
   (make-doc-type-reference :symbol symbol)))
(export
 (defun doc-group-ref (symbol)
   "Construct a documentation markup object that references the group named by symbol."
   (make-doc-group-reference :symbol symbol)))

(export (defun newlines (&optional (count 1))
	  "Return a string with COUNT newlines."
	  (make-string count :initial-element #\newline)))
(export (defun tabs (&optional (count 1))
	  "Return a string with COUNT tabs."
	  (make-string count :initial-element #\tab)))
(export
 (defun doc (&rest markup-objects)
   (make-doc :objects markup-objects)))

(document-type
 (doc "DOC is a markup object for storing documentation. It can contain a list of objects markup objects."
      (newlines) "See " (doc-func-ref 'render-markup) " and " (doc-func-ref 'documentation-string) ".")
 'doc)

(document
 (doc "Constructs a documentation markup object given a list of markup objects. See " (doc-type-ref 'doc) ".")
 'doc)

(document-package
 (doc "Provides tools for creating marked up documentation.")
 :schemeish.document)

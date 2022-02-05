(in-package #:schemeish.internals)

;; TODO: Use these to make some better documentation

(defclass markup-object () ())
(defun markup-object? (object)
  (typep object 'markup-object))

(defclass markup-ordered-list (markup-object)
  ((elements :initarg :elements
	     :reader markup-ordered-list-elements)))
(defun markup-ordered-list (&rest elements)
  "Elements should satisfy markup-object?"
  (assert (every #'markup-object? elements))
  (make-instance 'markup-ordered-list :elements elements))


(defclass markup-unordered-list (markup-object)
  ((elements :initarg :elements
	     :reader markup-unordered-list-elements)))
(defun markup-unordered-list (&rest elements)
  "Elements should satisfy markup-object?"
  (assert (every #'markup-object? elements))
  (make-instance 'markup-unordered-list :elements elements))

(defclass markup-code (markup-object)
  ((inline-text :initarg :inline-text
		:reader markup-code-inline-text)))
(defun markup-code (inline-text)
  "Inline-text should satisfy INLINE-TEXT?"
  (assert (inline-text? inline-text))
  (make-instance 'markup-code :inline-text inline-text))

(defclass markup-code-block (markup-object)
  ((preformatted-text :initarg :preformatted-text
		      :reader markup-code-block-preformatted-text)))
(defun markup-code-block (preformatted-text)
  "Preformatted-text should be a string."
  (assert (stringp preformatted-text))
  (make-instance 'markup-code-block :preformatted-text preformatted-text))

(defclass markup-horizontal-bar (markup-object) ())
(defun markup-horizontal-bar ()
  (make-instance 'markup-horizontal-bar))

(defclass markup-link (markup-object)
  ((url :initarg :url
	:reader markup-link-url)
   (inline-text :initarg :inline-text
		:reader markup-link-inline-text)))
(defun markup-link (url &optional inline-text)
  "Construct an inline-markup object which represents a link to a url. [link text](url) or <url>
URL and INLINE-TEXT (if provided) must satisfy INLINE-TEXT?."
  (assert (inline-text? url))
  (assert (or (null inline-text) (inline-text? inline-text)))
  (make-instance 'markup-link :url url :inline-text inline-text))


(defclass markup-seq (markup-object)
  ((markups :initarg :markups
	    :reader markup-seq-markups)))

(defun markup-seq (&rest markups)
  "Return a sequence of markups. Inline markups will be rendered together directly or separated by a newline.
Non-inline markups will be rendered with a fresh line before/after."
  (assert (every #'markup-object? markups))
  (make-instance 'markup-seq :markups markups))


(defclass markup-table (markup-object)
  ((column-headers :initarg :column-headers
		   :reader markup-table-column-headers)
   (rows :initarg :rows
	 :reader markup-table-rows)
   (column-alignments :initarg :column-alignments
		      :reader markup-table-column-alignments)))

(defun markup-table-column-alignment? (object)
  "True if object is a valid markup-table column-alignment: one of '(:LEFT :RIGHT :CENTER)."
  (member object '(:left :right :center)))

(defun markup-table (column-headers rows &optional (column-alignments (mapcar (constantly :left) column-headers)))
  "Create a table given a list of column-headers and a list of rows. Each row must be a list
with the same number of elements as column-headers. Column-alignments must also
be the same length as column-headers, and each alignment should be one of '(:LEFT :RIGHT :CENTER).

| Header 1 | Header 2 |
| -------- | -------- |
| Row 1 C1 | Row 1 C2 |
| Row 2 C1 | Row 2 C2 |"
  (assert (every #'markup-table-column-alignment? column-alignments))
  (assert (every #'markup-object? column-headers))
  (assert (every (lambda (row) (every #'markup-object? row)) rows))

  (make-instance 'markup-table :column-alignments column-alignments :rows rows :column-headers column-headers))

;; Hyperspec Style documentation

(defstruct function-reference symbol)
(defstruct variable-reference symbol)
(defstruct package-reference symbol)
(defstruct type-reference symbol)

(defun function-reference (symbol)
  (make-function-reference :symbol symbol))
(defun variable-reference (symbol)
  (make-variable-reference :symbol symbol))
(defun package-reference (symbol)
  (make-package-reference :symbol symbol))
(defun type-reference (symbol)
  (make-type-reference :symbol symbol))

(defstruct function-style-doc
  function-type
  name
  syntax
  arguments-and-values
  description
  examples
  side-effects
  affected-by
  exceptional-situations
  see-also
  notes)

(defun function-doc (&key name
		       syntax
		       arguments-and-values
		       description
		       examples
		       side-effects
		       affected-by
		       exceptional-situations
		       see-also
		       notes)
  (make-function-style-doc :function-type :function
			   :name name
			   :syntax syntax
			   :arguments-and-values arguments-and-values
			   :description description
			   :examples examples
			   :side-effects side-effects
			   :affected-by affected-by
			   :exceptional-situations exceptional-situations
			   :see-also see-also
			   :notes notes))
(defun symbol-doc (&key name
		     syntax
		     arguments-and-values
		     description
		     examples
		     side-effects
		     affected-by
		     exceptional-situations
		     see-also
		     notes)
  (make-function-style-doc :function-type :symbol
			   :name name
			   :syntax syntax
			   :arguments-and-values arguments-and-values
			   :description description
			   :examples examples
			   :side-effects side-effects
			   :affected-by affected-by
			   :exceptional-situations exceptional-situations
			   :see-also see-also
			   :notes notes))
(defun macro-doc (&key name
		    syntax
		    arguments-and-values
		    description
		    examples
		    side-effects
		    affected-by
		    exceptional-situations
		    see-also
		    notes)
  (make-function-style-doc :function-type :macro
			   :name name
			   :syntax syntax
			   :arguments-and-values arguments-and-values
			   :description description
			   :examples examples
			   :side-effects side-effects
			   :affected-by affected-by
			   :exceptional-situations exceptional-situations
			   :see-also see-also
			   :notes notes))

(defstruct variable-doc
  name
  value-type
  initial-value
  description
  examples
  affected-by
  see-also
  notes)

(defstruct type-doc
  name
  class-precedence-list
  description
  see-also)

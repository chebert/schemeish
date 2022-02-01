(in-package #:schemeish.for-macros)

(defvar *unique-symbol* #'gensym "Given a string create a unique symbol. Typically bound to gensym.")
(defmacro with-readable-symbols (&body body)
  "Establishes a dynamic context around body where UNIQUE-SYMBOL will use INTERN instead of GENSYM."
  `(let ((*unique-symbol* #'intern))
     ,@body))
(defun unique-symbol (name-or-symbol)
  "Typically calls gensym on the string value of name-or-symbol. If in the dynamic context established by WITH-READABLE-SYMBOLS,
will call intern on the string value of name-or-symbol."
  (funcall *unique-symbol* (if (symbolp name-or-symbol)
			       (symbol-name name-or-symbol)
			       name-or-symbol)))


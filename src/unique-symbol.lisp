(in-package #:schemeish.internals)

(defvar *unique-symbol* #'gensym "Given a string create a unique symbol. Typically bound to gensym.")
(export
 (defmacro with-readable-symbols (&body body)
   "Establishes a dynamic context around body where UNIQUE-SYMBOL will use INTERN instead of GENSYM."
   `(let ((*unique-symbol* (lambda (s) (intern (symbol-name (gensym s))))))
      ,@body)))
(export
 (defun unique-symbol (name-or-symbol)
   "Typically calls gensym on the string value of name-or-symbol. If in the dynamic context established by WITH-READABLE-SYMBOLS,
will call intern on the string value of name-or-symbol."
   (funcall *unique-symbol* (if (symbolp name-or-symbol)
				(symbol-name name-or-symbol)
				name-or-symbol))))
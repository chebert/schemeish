(in-package #:schemeish.schemeish)

(for-macros
  ;; Re-export functions which have parameters bound to them as just functions.
  (setf (symbol-function '+) (symbol-function 'cl:+)
	(symbol-function '-) (symbol-function 'cl:-)
	(symbol-function '*) (symbol-function 'cl:*)
	(symbol-function '/) (symbol-function 'cl:/)))

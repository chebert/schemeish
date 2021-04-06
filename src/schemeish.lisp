(in-package #:schemeish)

(for-macros
  ;; Re-export functions which have parameters bound to them as just functions.
  (setf (symbol-function 'schemeish:+) (symbol-function 'cl:+)
	(symbol-function 'schemeish:-) (symbol-function 'cl:-)
	(symbol-function 'schemeish:*) (symbol-function 'cl:*)
	(symbol-function 'schemeish:/) (symbol-function 'cl:/)))

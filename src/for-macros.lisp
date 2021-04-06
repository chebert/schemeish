(in-package #:schemeish.for-macros)

(defmacro for-macros (&body body)
  "Evaluates to `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body).
Used to annotate functions that are used in macros."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

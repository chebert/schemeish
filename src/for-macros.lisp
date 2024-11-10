(in-package #:schemeish.internals)

(defmacro for-macros (&body body)
  "Expands to `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body).
Used to annotate functions and/or variable definitions that are used in macros."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(defmacro no-compile (&body body)
  "Wrap body in an (eval-when (:load-toplevel :execute)).
Useful for example code or top-level ASSERTs."
  `(eval-when (:load-toplevel :execute)
     ,@body))

(defmacro export-definition (&body body)
  "Expands to (FOR-MACROS (EXPORT (PROGN BODY...))) for use in top-level definition forms."
  `(for-macros
     (cl:export (progn ,@body))))
(export '(for-macros export-definition no-compile))

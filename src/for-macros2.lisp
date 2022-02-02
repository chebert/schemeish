(in-package #:schemeish.internals)

(export
 (defmacro for-macros (&body body)
   "Expands to `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body).
Used to annotate functions and/or variable definitions that are used in macros."
   `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body)))

(export
 (defmacro no-compile (&body body)
   "Wrap body in an (eval-when (:load-toplevel :execute)).
Useful for example code or top-level ASSERTs.xo"
   `(eval-when (:load-toplevel :execute)
      ,@body)))



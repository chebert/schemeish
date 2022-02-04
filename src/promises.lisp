(in-package #:schemeish.backend)

(install-syntax!)

;;; DELAY

(defmacro delay (&body body)
  "Delays body."
  `(memo-proc (lambda () ,@body)))
(export 'delay)
(export
 (define (force promise)
   "Evaluates promise."
   [promise]))

(uninstall-syntax!)

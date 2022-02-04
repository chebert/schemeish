(in-package #:schemeish.backend)

(install-syntax!)

;;; Set!

(defmacro set! (id expression)
  `(setq ,id ,expression))
(export 'set!)
(export
 (define (set-car! pair value) (setf (car pair) value)))
(export
 (define (set-cdr! pair value) (setf (cdr pair) value)))

(uninstall-syntax!)

(in-package #:schemeish.backend)

(install-syntax!)

;;; Output

(export
 (define (newline (out *standard-output*)) (format out "~%")))
(export
 (define (display datum (out *standard-output*)) (format out "~A" datum)))
(export
 (define (displayln datum (out *standard-output*))
   (display datum out)
   (newline out)))

(uninstall-syntax!)

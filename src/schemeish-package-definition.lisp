(in-package #:schemeish.backend)

(install-syntax!)

(define (define-schemeish-package)
  (define-package :schemeish.schemeish
      (nickname-package :schemeish :scm)  
    (package-use-and-export-shadowing :cl :schemeish.internals :schemeish.backend)))

(define (schemeish-package-file-contents)
  (package-file-contents (map #'find-package '(:schemeish.internals :schemeish.backend :schemeish.schemeish))))

(define (write-schemeish-package-file! (file-path "./src/package.lisp"))
  (with-open-file (stream file-path
			  :direction :output
			  :if-exists :supersede)
    (format stream "~A" (schemeish-package-file-contents))))

;; (write-schemeish-package-file!)

(uninstall-syntax!)

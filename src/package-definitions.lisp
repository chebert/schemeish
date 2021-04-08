(in-package #:schemeish.package-definitions)

(install-syntax!)

(defparameter *schemeish-prefix* (ensure-string :schemeish.))
(define (schemeish-packages) (all-packages-with-string-prefix *schemeish-prefix*))
(define (resolve-schemeish-package-designator package-designator)
  (cond
    ;; If name exists as a package, then it's not part of our namespace
    ((find-package package-designator) package-designator)
    (t (make-symbol (string-append *schemeish-prefix* (ensure-string package-designator))))))

(defmacro with-schemeish-designators (&body body)
  `(let ((*resolve-package-designator* #'resolve-schemeish-package-designator))
     ,@body))

(define (write-package-file! (file-path "src/package.lisp"))
  (with-open-file (stream file-path
			  :direction :output
			  :if-exists :supersede)
    (format stream "~A" (package-file-contents (schemeish-packages)))))

(uninstall-syntax!)

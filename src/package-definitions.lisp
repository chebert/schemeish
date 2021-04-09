(in-package #:schemeish.package-definitions)

(install-syntax!)

(defparameter *schemeish-prefix*
  (ensure-string :schemeish.))
(define (schemeish-packages)
  "All packages that are in the SCHEMEISH. namespace."
  (all-packages-with-string-prefix *schemeish-prefix*))

(define (resolve-schemeish-package-designator package-designator)
  "If package-designator is not a package or a name of a package,
prefix it with the schemish prefix."
  (cond
    ;; If name exists as a package, then it's not part of our namespace
    ((find-package package-designator) package-designator)
    (t (make-symbol (string-append *schemeish-prefix* (ensure-string package-designator))))))

(defmacro with-schemeish-designators (&body body)
  "Evaluate body with unresolved-package-designators resolving to packages in the schemeish namespace."
  `(let ((*resolve-package-designator* #'resolve-schemeish-package-designator))
     ,@body))

(define (write-package-file! (file-path "src/package.lisp"))
  "Writes the current contents of (SCHEMISH-PACKAGES) as defpackage forms to file-path."
  (with-open-file (stream file-path
			  :direction :output
			  :if-exists :supersede)
    (format stream "~A" (package-file-contents (schemeish-packages)))))


(uninstall-syntax!)

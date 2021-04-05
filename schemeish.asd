;;;; schemeish.asd

(asdf:defsystem #:schemeish
  :description "Describe schemeish here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:trivial-arguments)
  :components ((:file "package")
	       (:file "for-macros")
	       (:file "named-let")
               (:file "schemeish")))

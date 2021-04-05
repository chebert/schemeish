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
	       (:file "syntax")
	       (:file "arguments")
	       (:file "expand-define")
	       (:file "lambda")
	       (:file "define")
	       (:file "base")
	       (:file "expand-stream-collect")
	       (:file "and-let")
	       (:file "bundle")
	       (:file "struct")
	       (:file "define-struct")
	       (:file "queue")
	       (:file "expand-lexically")
	       (:file "lexically")
	       (:file "schemeish")))

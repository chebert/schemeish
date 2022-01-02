;;;; schemeish.asd

(asdf:defsystem #:schemeish
  :description "Provide Scheme style syntax/macros/functions in a Common Lisp environment."
  :author "Christopher Hebert <hebert.christopherj@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (:trivial-arguments)
  :components ((:file "package")
	       (:file "document")
	       (:file "for-macros")
	       (:file "named-let")
	       (:file "syntax")
	       (:file "arguments")
	       (:file "define")
	       (:file "base")
	       (:file "expand-stream-collect")
	       (:file "stream-collect")
	       (:file "and-let")
	       (:file "cut")
	       (:file "bundle")
	       (:file "struct")
	       (:file "define-struct")
	       (:file "queue")
	       (:file "lexically")
	       (:file "package-utils")
	       (:file "package-definitions")))

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
	       (:file "markup")
	       (:file "package-definitions")))

(asdf:defsystem #:schemeish2
  :description "Provide Scheme style syntax/macros/functions in a Common Lisp environment."
  :author "Christopher Hebert <hebert.christopherj@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (:trivial-arguments)
  :components ((:file "package2")
	       ;; SCHEMEISH.INTERNALS
	       (:file "named-let2")
	       (:file "for-macros2")
	       (:file "unique-symbol")
	       (:file "guard")
	       (:file "documentation")
	       (:file "syntax2")
	       (:file "splitf")
	       (:file "lambda-list")
	       (:file "lexical-body2")
	       (:file "expose")
	       (:file "function-body")
	       (:file "lexical-body-definitions")
	       (:file "define2")

	       ;; SCHEMEISH.BACKEND
	       (:file "lambda")
	       (:file "base2")
	       (:file "expand-stream-collect2")
	       (:file "stream-collect2")
	       (:file "and-let2")
	       (:file "cut2")
	       (:file "bundle2")
	       (:file "struct2")
	       (:file "define-struct2")
	       (:file "queue2")
	       (:file "package-utils2")
	       (:file "markup2")
	       (:file "package-definitions2")))

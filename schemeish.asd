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
	       (:file "markup2")
	       (:file "documentation")
	       (:file "for-macros2")
	       (:file "unique-symbol")
	       (:file "guard")
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
	       (:file "symbols")
	       (:file "numbers")
	       (:file "logic")
	       (:file "lists")
	       (:file "letrec")
	       (:file "procedures")
	       (:file "alists")
	       (:file "sets")
	       (:file "strings")
	       (:file "trees")
	       (:file "output")
	       (:file "set")
	       (:file "promises")
	       (:file "streams")
	       (:file "arities")
	       (:file "group")
	       (:file "hash-tables")
	       (:file "vectors")
	       (:file "expand-struct")
	       (:file "define-struct2")
	       (:file "code-transformer2")
	       (:file "scm")
	       (:file "cut2")
	       (:file "markup-renderer")
	       (:file "and-let2")
	       (:file "expand-stream-collect2")
	       (:file "stream-collect2")
	       (:file "bundle2")
	       (:file "queue2")
	       (:file "package-utils2")
	       (:file "schemeish-package-definition")))

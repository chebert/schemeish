;;;; schemeish.asd

(asdf:defsystem #:schemeish
  :description "Provide Scheme style syntax/macros/functions in a Common Lisp environment."
  :author "Christopher Hebert <hebert.christopherj@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (:trivial-arguments :trivial-cltl2)
  :components ((:file "package")
	       ;; SCHEMEISH.INTERNALS
	       (:file "named-let")
	       (:file "markup")
	       (:file "documentation")
	       (:file "for-macros")
	       (:file "unique-symbol")
	       (:file "guard")
	       (:file "syntax")
	       (:file "splitf")
	       (:file "lambda-list")
	       (:file "lexical-body")
	       (:file "expose")
	       (:file "function-body")
	       (:file "lexical-body-definitions")
	       (:file "define")

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
	       (:file "define-struct")
	       (:file "code-transformer")
	       (:file "scm")
	       (:file "cut")
	       (:file "markup-renderer")
	       (:file "and-let")
	       (:file "expand-stream-collect")
	       (:file "stream-collect")
	       (:file "bundle")
	       (:file "queue")
	       (:file "package-utils")
	       (:file "schemeish-package-definition")))

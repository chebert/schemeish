(defpackage #:schemeish.package-definitions
  (:use :schemeish.schemeish :schemeish.package-utils))

(in-package #:schemeish.package-definitions)

(install-syntax!)

(defparameter *schemeish-prefix* (ensure-string :schemeish.))
(define (schemeish-packages) (all-packages-with-string-prefix *schemeish-prefix*))
(define (resolve-schemeish-package-designator package-designator)
  (cond
    ;; If name exists as a package, then it's not part of our namespace
    ((find-package package-designator) package-designator)
    (t (make-symbol (string-append *schemeish-prefix* (ensure-string package-designator))))))

(setq *resolve-package-designator* #'resolve-schemeish-package-designator)

(define-package :for-macros
    (document "Provides FOR-MACROS which expands to (EVAL-WHEN ...)")
  (package-use :cl)
  (package-export :for-macros))

(define-package :named-let
    (document "Provides an optionally named LET which can be used to write a locally recursive form.")
  (package-use :cl :for-macros)
  (package-shadowing-export :let))

(define-package :syntax
    (document "Provides install/uninstall-syntax! for expanding [fn-value args...] => (funcall fn-value args...)")
  (package-use :cl :for-macros)
  (package-export :install-syntax! :uninstall-syntax!))

(define-package :arguments
    (document "Tools to translate scheme style argument lists to CL style argument lists.")
  (package-use :cl :for-macros)
  (package-export :arg-list->lambda-list))


(define-package :basic-syntax
    (document "Provides some basic syntax of scheme: FOR-MACROS NAMED-LET, [] reader syntax")
  (package-use-shadowing :cl :for-macros :named-let :syntax)
  (package-re-export-shadowing :for-macros :named-let :syntax))

(define-package :expand-define
    (document "Tools to expand define and define-like forms.")
  (package-use-shadowing :cl :basic-syntax :arguments)
  (package-export :expand-function-body :expand-top-level-define))

(define-package :define
    (document "Provides DEFINE. See DEFINE's docs for more details.")
  (package-use-shadowing :cl :basic-syntax)
  (package-export :define :expand-function-body :expand-top-level-define)
  (package-shadowing-export :lambda))

(define-package :base
    (document "Provides many core functions and simple macros in addition to basic-syntax, including
  - symbols
  - lists
  - procedures
  - alists
  - sets
  - strings
  - output
  - mutation")
  (package-use-shadowing :cl :basic-syntax :define)
  (package-shadow :map :sort :stream)
  (package-export
   ;; Symbols
   :symbol?
   :symbol->string
   :make-keyword

   ;; Lists
   :map
   :append*
   :empty?
   :for-each
   :filter
   :pair?
   :null?
   :list?
   :list-ref
   :list-tail
   :foldl
   :foldr
   :negative?
   :positive?
   :andmap
   :ormap
   :remq
   :remove*
   :remq*
   :sort
   :memf
   :findf
   :list-update
   :list-set
   :take
   :drop
   :split-at
   :even?
   :odd?
   :zero?
   :compose
   :filter-map
   :range
   :append-map
   :map-successive
   :filter-not
   :partition
   :flatten
   :group
   
   ;; Procedures
   :procedure?
   :rcurry
   :lcurry
   :swap-args
   :memo-proc
   :document!
   :has-specific-arity?
   :procedure-arity
   :procedure-arguments
   :procedure-arguments-required-arguments
   :procedure-arguments-optional-arguments
   :procedure-arguments-key-arguments
   :procedure-arguments-rest-argument
   :procedure-arguments-allow-other-keys?

   ;; Alists
   :alist-ref
   :alist-remove
   :alist-set
   :alist-update
   :alist-map
   :alist-keys
   :alist-values
   :alist-has-key?
   :alist-set*
   :alist

   ;; Booleans
   :eq?
   :equal?
   :disjoin*
   :disjoin
   :conjoin*
   :conjoin
   :for-all*
   :for-all
   :there-exists*
   :there-exists
   :const
   :nand
   :nor
   :xor

   ;; Numbers
   :quotient
   :number->string
   :degrees->radians
   :radians->degrees
   :sqr
   :sgn
   :number?

   ;; Sets
   :set-member?
   :set-add
   :set-remove
   :set-empty?
   :set-count
   :set->stream
   :set-union
   :set-intersect
   :set-subtract
   :subset?
   :set=?

   ;; Streams
   :delay
   :force
   :*the-empty-stream*
   :stream-cons
   :stream-car
   :stream-cdr
   :stream-empty?
   :stream-for-each
   :stream-length
   :stream->list
   :stream-first
   :stream-rest
   :stream?
   :list->stream
   :stream
   :stream-map
   :stream-fold
   :stream-filter
   :stream-drop
   :stream-take
   :stream-ref
   :stream-append
   :stream-flatten
   :stream-range
   :stream-flatmap
   :stream-map-successive
   :random-stream

   ;; Strings
   :string-append
   :string?
   :string-starts-with?

   ;; Output
   :newline
   :display
   :displayln

   ;; Mutation
   :set-car!
   :set-cdr!
   :set!)
  (package-re-export-shadowing :basic-syntax :define))

(define-package :expand-stream-collect
    (document "Provides tools to expand a stream-collect macro form.")
  (package-use-shadowing :cl :base)
  (package-export :stream-collect-form))

(define-package :stream-collect
    (document "Provides the stream-collect macro.")
  (package-use-shadowing :cl :base :expand-stream-collect)
  (package-export :stream-collect))

(define-package :and-let
    (document "Provides the and-let* macro.")
  (package-use-shadowing :cl :base)
  (package-export :and-let*))

(define-package :struct
    (document "Provides the basis and expansions for define-struct.")
  (package-use-shadowing :cl :base :and-let)
  (package-export :struct
		  :struct?
		  :struct-copy
		  :struct->list
		  :struct-accessors
		  :struct-form))

(define-package :define-struct
    (document "Provides define-struct.")
  (package-use-shadowing :cl :base :and-let :struct)
  (package-export :define-struct))

(define-package :bundle
    (document "Provides bundle and make-bundle-predicate for creating dispatch-style closures.")
  (package-use-shadowing :cl :base :and-let)
  (package-export :bundle
		  :bundle-documentation
		  :bundle-permissions
		  :bundle-list
		  :bundle?
		  :make-bundle-predicate))

(define-package :queue
    (document "Provides a bundle-based implementation of a queue.")
  (package-use-shadowing :cl :base :and-let :bundle)
  (package-export :make-queue
		  :queue?
		  :queue-empty?
		  :queue-front
		  :queue-insert!
		  :queue-delete!))

(define-package :expand-lexically
    (document "Provides tools to expand lexically/expose macros.")
  (package-use-shadowing :cl :base :and-let)
  (package-export :lexical-bindings
		  :parameter-name->lexical-name
		  :parameter-name?
		  :special-form?
		  :special?
		  :lexical-name->parameter-name))

(define-package :lexically
    (document "Provides the lexically and expose macros.")
  (package-use-shadowing :cl :base :and-let :expand-lexically)
  (package-export :lexically :expose))


(define-package :serialize
    (document "Provides the serialize function which can recursively serialize lisp data, bundles, and structs
into a form ready for EVAL.")
  (package-use-shadowing :cl :base :and-let :bundle :struct :define-struct :queue)
  (package-export :serialize))

(define-package :schemeish
    (document "Provides everything in the schemeish-library. Re-exports CL so that packates can (:use #:schemeish) instead of (:use #:cl)")
  (package-use-shadowing :cl :base :and-let :stream-collect
			 :bundle :queue :struct :define-struct
			 :lexically :serialize)
  ;; Remove non *EAR-MUFFED* special symbols
  (package-shadow :++ :+++ :// :/// :** :***)
  ;; Re-export these symbols as non-special
  (package-shadowing-export :+ :/ :* :-) 
  (package-re-export-shadowing :cl :base :and-let :stream-collect
			       :bundle :queue :struct :define-struct
			       :lexically :serialize))

(define-package :package-utils
    (document "Provides tools for dealing with CL packages.")
  (package-use-shadowing :cl
			 :base
			 :and-let
			 :bundle
			 :struct
			 :define-struct
			 :queue)
  (package-export :package?
		  :package-find
		  :*resolve-package-designator*
		  :package-symbols
		  :package-external-symbols
		  :package-exported-symbols
		  :symbol-in-package?
		  :package-imported-symbols
		  :group-by-package
		  :package-non-shadowing-symbols
		  :package-used-symbols
		  :package-unused-symbols
		  :symbols-in-package
		  :symbols-interned-in-package
		  :package-external-symbols-from
		  :package-use
		  :package-use-shadowing
		  :package-re-export-shadowing
		  :document
		  :package-export
		  :package-shadow
		  :package-shadowing-export
		  :package-import-from
		  :package-shadowing-import-from
		  :nickname-package
		  :unique-package-name
		  :extend-package*
		  :extend-package
		  :ensure-string
		  :uninterned
		  :package-import-froms
		  :package-shadowing-import-froms
		  :defpackage-form
		  :with-temporary-package
		  :define-package-form
		  :define-package
		  :package-delete
		  :package-dependencies
		  :independent-package?
		  :independent-packages
		  :package-hierarchy
		  :hierarchical-defpackage-forms
		  :filter-packages
		  :all-packages-with-string-prefix
		  :package-file-contents))

(define-package :package-definitions
    (document "Source of all of the package definitions in SCHEMEISH.
Provides write-package-file! which writes the current schemeish-packages to a file.")
  (package-use-shadowing :schemeish :package-utils)
  (package-export :WRITE-PACKAGE-FILE! :SCHEMEISH-PACKAGES))

(define (write-package-file! (file-path "src/package.lisp"))
  (with-open-file (stream file-path
			  :direction :output
			  :if-exists :supersede)
    (format stream "~A" (package-file-contents (schemeish-packages)))))

(uninstall-syntax!)

(in-package #:schemeish.package-utils)

(install-syntax!)

(define (package-symbols package)
  "Returns a list of all symbols internal and external in package."
  (let ((symbols ()))
    (do-symbols (symbol package)
      (push symbol symbols))
    (nreverse symbols)))
(define (package-external-symbols package)
  "Returns a list of all external symbols in package."
  (let ((symbols ()))
    (do-external-symbols (symbol package)
      (push symbol symbols))
    (nreverse symbols)))

(define (group key-fn list)
  "Groups elements of list that have the same key-fn into an alist."
  (define (rec list result)
    (cond
      ((null? list)
       (alist-map result
		  (lambda (key list)
		    (cons key (nreverse list)))))
      (t (let ((item (first list)))
	   (let ((key [key-fn item]))
	     (rec (rest list)
		  (alist-update result key (lambda (vals) (cons item vals)) ())))))))
  (rec list ()))

(define (symbol-in-package? package symbol)
  (eq? (symbol-package symbol) package))

(define (package-imported-symbols package-designator)
  (let ((package (find-package package-designator)))
    (filter-not (lcurry 'symbol-in-package? package) (package-symbols package-designator))))

(define (group-by-package symbols)
  (alist-map (group (compose 'package-name 'symbol-package) symbols)
	     (lambda (name symbols)
	       (cons (make-symbol name)
		     (map 'uninterned symbols)))))

(define (package-unshadowed-symbols package)
  (set-difference (package-symbols package) (package-shadowing-symbols package)))

(define (package-unshadowed-imported-symbols package)
  (set-difference (package-imported-symbols package) (package-shadowing-symbols package)))

(define (package-used-symbols package)
  (let ((use-list (package-use-list package)))
    (filter (lambda (symbol) (member (symbol-package symbol) use-list))
	    (package-symbols package))))
(define (package-unused-symbols package)
  (set-difference (package-symbols package) (package-used-symbols package)))

(define (package-import-froms package)
  (map (lambda (list)
	 (cons :import-from list))
       (group-by-package (intersection
			  (intersection (package-unshadowed-symbols package)
					(package-imported-symbols package))
			  (package-unused-symbols package)))))

(define (package-shadowing-import-froms package)
  (map (lambda (list)
	 (cons :shadowing-import-from list))
       (group-by-package (intersection (package-imported-symbols package)
				       (package-shadowing-symbols package)))))

(define (use-shadowing-package shadowing-package package)
  "Uses SHADOWING-PACKAGE in PACKAGE, but first shadowing-imports all of the shadowing symbols in shadowing-package.
Returns the package."
  (let ((shadowing-symbols (package-shadowing-symbols shadowing-package)))
    (shadowing-import shadowing-symbols package)
    (use-package shadowing-package package))
  package)

(define (re-export-shadowing-package re-exported-package package)
  "Exports all symbols from re-exported-package from package,
shadowing any symbols that re-exported-package shadows.
If a symbol is not part of package it is not exported.
Returns the package."
  (let ((package-symbols (package-symbols package)))
    (let ((shadowing-symbols (package-shadowing-symbols re-exported-package))
	  (external-symbols (intersection (package-external-symbols re-exported-package)
					  package-symbols)))
      (shadow (intersection shadowing-symbols external-symbols) package)
      (export external-symbols package)))
  package)

(define (use-shadowing-packages shadowing-packages package)
  "Each shadowing package is used in package. See USE-SHADOWING-PACKAGE.
Returns the package."
  (mapcar (lambda (p) (use-shadowing-package p package)) shadowing-packages)
  package)
(define (re-export-shadowing-packages shadowing-packages package)
  "Each shadowing package is exported from package. See RE-EXPORT-SHADOWING-PACKAGE.
Returns the package."
  (mapcar (lambda (p) (re-export-shadowing-package p package)) shadowing-packages)
  package)

(define (document-package documentation package)
  "Sets the package documentation for package and returns the package."
  (setf (documentation package t) documentation)
  package)

(define (package-export symbols package)
  "Interns and exports each symbol-name in symbols.
Returns the package."
  (export (mapcar (lambda (s) (intern (symbol-name s) package)) symbols) package)
  package)

(define (package-shadow symbols package)
  "Shadows symbols in package. Returns the package."
  (shadow symbols package)
  package)

(define (package-shadowing-export symbols package)
  "Shadows and exports each symbol in symbols.
Returns the package."
  (package-shadow symbols package)
  (package-export symbols package)
  package)

(define (build-package name)
  "Creates a new package with the given name. If a package with that name exists, it is deleted."
  (let ((existing (find-package name)))
    (when existing (delete-package existing)))
  (make-package name))

(define (define-package name documentation
	  :use-shadowing-packages
	  :re-export-shadowing-packages
	  :shadowing-exports
	  :shadows
	  :exports)
  [(compose
    (lcurry #'re-export-shadowing-packages re-export-shadowing-packages)
    (lcurry #'package-export exports)
    (lcurry #'package-shadowing-export shadowing-exports)
    (lcurry #'package-shadow shadows)
    (lcurry #'use-shadowing-packages use-shadowing-packages)
    (lcurry #'document-package documentation))
   (build-package name)])

(define (uninterned symbol)
  (make-symbol (symbol-name symbol)))

(define (package-exports package)
  (map 'uninterned (package-external-symbols package)))

(define (defpackage-form package-designator)
  (let* ((package (find-package package-designator))
	 (documentation (documentation package t))
	 (use-list (map (compose 'make-symbol 'package-name) (package-use-list package)))
	 (exports (package-exports package))
	 (shadow (map 'uninterned (package-shadowing-symbols package)))
	 (nicknames (package-nicknames package)))
    `(cl:defpackage ,(make-symbol (package-name package))
       ,@(when documentation `((:documentation ,documentation)))
       ,@(package-import-froms package)
       ,@(package-shadowing-import-froms package)
       ,@(when use-list `((:use ,@use-list)))
       ,@(when exports `((:export ,@exports)))
       ,@(when shadow `((:shadow ,@shadow)))
       ,@(when nicknames `((:nicknames ,@nicknames))))))

(defpackage #:for-macros
  (:documentation "Provides FOR-MACROS which expands to (EVAL-WHEN ...)")
  (:use #:cl)
  (:export #:for-macros))

(defpackage #:named-let
  (:documentation "Provides an optionally named LET which can be used to write a locally recursive form.")
  (:use #:cl)
  (:shadow #:let)
  (:export #:let))

(defpackage #:syntax
  (:documentation "Provides install/uninstall-syntax! for expanding [fn-value args...] => (funcall fn-value args...)")
  (:use #:cl)
  (:export #:install-syntax! #:uninstall-syntax!))

(defpackage #:arguments
  (:documentation "Tools to translate scheme style argument lists to CL style argument lists.")
  (:use #:cl)
  (:export #:arg-list->lambda-list))

(define-package :basic-syntax
  "Provides some basic syntax of scheme: FOR-MACROS NAMED-LET, [] reader syntax"
  :use-shadowing-packages '(:cl
			    :for-macros
			    :named-let
			    :syntax)
  :re-export-shadowing-packages '(:for-macros
				  :named-let
				  :syntax))

(define-package :expand-define
  "Tools to expand define and define-like forms."
  :use-shadowing-packages '(:cl :basic-syntax :arguments)
  :exports '(:expand-function-body :expand-top-level-define))

(define-package :lambda
  "Replaces LAMBDA with a scheme style argument list and a body that can have local defines."
  :use-shadowing-packages '(:cl :basic-syntax :arguments :expand-define)
  :shadowing-exports '(:lambda))

(define-package :define
  "Provides DEFINE. See DEFINE's docs for more details."
  :use-shadowing-packages '(:cl :basic-syntax :expand-define)
  :exports '(:define))

;; Expand define uses define and lambda symbols.
(use-shadowing-packages :expand-define :define :lambda)

(define-package :base
  "Provides many core functions and simple macros in addition to basic-syntax, including
  - symbols
  - lists
  - procedures
  - alists
  - sets
  - strings
  - output
  - mutation"
  :use-shadowing-packages '(:cl :basic-syntax :lambda :define)
  :shadows '(:map :sort :stream)
  :exports
  '(;; Symbols
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
  :re-export-shadowing-packages '(:basic-syntax :lambda :define))

(define-package :expand-stream-collect
  "Provides tools to expand a stream-collect macro form."
  :use-shadowing-packages '(:cl :base)
  :exports '(:stream-collect-form))

(define-package :stream-collect
  "Provides the stream-collect macro."
  :use-shadowing-packages '(:cl :base :expand-stream-collect)
  :exports '(:stream-collect))

(define-package :and-let
  "Provides the and-let* macro."
  :use-shadowing-packages '(:cl :base)
  :exports '(:and-let*))

(define-package :struct
  "Provides the basis and expansions for define-struct."
  :use-shadowing-packages '(:cl :base :and-let)
  :exports '(:struct
	     :struct?
	     :struct-copy
	     :struct->list
	     :struct-accessors
	     :struct-form))


(define-package :define-struct
  "Provides define-struct."
  :use-shadowing-packages '(:cl :base :and-let :struct)
  :exports '(:define-struct))


(define-package :bundle
  "Provides bundle and make-bundle-predicate for creating dispatch-style closures."
  :use-shadowing-packages '(:cl :base :and-let)
  :exports '(:bundle
	     :bundle-documentation
	     :bundle-permissions
	     :bundle-list
	     :bundle?
	     :make-bundle-predicate))

(define-package :queue
  "Provides a bundle-based implementation of a queue."
  :use-shadowing-packages '(:cl :base :and-let :bundle)
  :exports '(:make-queue
	     :queue?
	     :queue-empty?
	     :queue-front
	     :queue-insert!
	     :queue-delete!))

(define-package :expand-lexically
  "Provides tools to expand lexically/expose macros."
  :use-shadowing-packages '(:cl :base :and-let)
  :exports '(:lexical-bindings
	     :parameter-name->lexical-name
	     :parameter-name?
	     :special-form?
	     :special?
	     :lexical-name->parameter-name))

(define-package :lexically
  "Provides the lexically and expose macros."
  :use-shadowing-packages '(:cl :base :and-let :expand-lexically)
  :exports '(:lexically :expose))


(define-package :serialize
  "Provides the serialize function which can recursively serialize lisp data, bundles, and structs
into a form ready for EVAL."
  :use-shadowing-packages '(:cl :base :and-let :bundle :struct :define-struct :queue)
  :exports '(:serialize))


(define-package :schemeish ; TODO: rename
  "Provides everything in the schemeish-library. Re-exports CL so that packates can (:use #:schemeish) instead of (:use #:cl)"
  :use-shadowing-packages '(:cl :base :and-let :stream-collect
			    :bundle :queue :struct :define-struct
			    :lexically :serialize)
  ;; Remove non *EAR-MUFFED* special symbols
  :shadows '(:++ :+++ :// :/// :** :***)
  ;; Re-export these symbols as non-special
  :shadowing-exports '(:+ :/ :* :-) 

  :re-export-shadowing-packages '(:cl :base :and-let :stream-collect
				  :bundle :queue :struct :define-struct
				  :lexically :serialize))

(uninstall-syntax!)

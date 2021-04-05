;;;; package.lisp

(defpackage #:schemeish.for-macros
  (:use #:cl)
  (:export #:for-macros))

(defpackage #:schemeish.named-let
  (:use #:cl)
  (:shadow #:let)
  (:export #:let))

(defpackage #:schemeish.syntax
  (:use #:cl)
  (:export #:install-syntax! #:uninstall-syntax!))

(defpackage #:schemeish.arguments
  (:use #:cl)
  (:export
   #:arg-list->lambda-list))

(defpackage #:schemeish.expand-define
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.named-let
	#:schemeish.syntax
	#:schemeish.arguments)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:export
   ;; TODO: rename to expand-body
   #:expand-function-body
   #:expand-top-level-define))

(defpackage #:schemeish.lambda
  (:use #:cl
	#:schemeish.arguments
	#:schemeish.expand-define)
  (:shadow #:lambda)
  (:export #:lambda))
(defpackage #:schemeish.define
  (:use #:cl
	#:schemeish.expand-define
	#:schemeish.for-macros
	#:schemeish.syntax)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:export #:define))

(import '(schemeish.define:define) :schemeish.expand-define)
(shadowing-import '(schemeish.lambda:lambda) :schemeish.expand-define)

(defpackage #:schemeish.base
  (:use #:cl
	#:schemeish.define
	#:schemeish.for-macros
	#:schemeish.syntax)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadow #:map
	   #:sort)
  (:export
   #:eq?
   #:equal?
   #:symbol?
   #:symbol->string
   #:make-keyword
   #:procedure?
   #:map
   #:append*
   #:empty?
   #:for-each
   #:filter
   #:pair?
   #:null?
   #:list?
   #:list-ref
   #:list-tail
   #:foldl
   #:foldr
   #:negative?
   #:positive?
   #:andmap
   #:ormap
   #:remq
   #:remove*
   #:remq*
   #:sort
   #:memf
   #:findf
   #:list-update
   #:list-set
   #:take
   #:drop
   #:split-at
   #:even?
   #:odd?
   #:zero?
   #:compose
   #:filter-map
   #:range
   #:append-map
   #:map-successive
   #:filter-not
   #:partition
   #:rcurry
   #:lcurry
   #:swap-args
   #:memo-proc
   #:alist-ref
   #:alist-remove
   #:alist-set
   #:alist-update
   #:alist-map
   #:alist-keys
   #:alist-values
   #:alist-has-key?
   #:alist-set*
   #:alist
   #:disjoin*
   #:disjoin
   #:conjoin*
   #:conjoin
   #:for-all*
   #:for-all
   #:there-exists*
   #:there-exists
   #:const
   #:nand
   #:nor
   #:xor
   #:quotient
   #:number->string
   #:degrees->radians
   #:radians->degrees
   #:sqr
   #:sgn
   #:number?
   #:set-member?
   #:set-add
   #:set-remove
   #:set-empty?
   #:set-count
   #:set->stream
   #:set-union
   #:set-intersect
   #:set-subtract
   #:subset?
   #:set=?
   #:flatten))

(defpackage #:schemeish.bundle
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort)
  (:export
   #:bundle
   #:bundle-documentation
   #:bundle-permissions
   #:bundle-list
   #:bundle?
   #:make-bundle-predicate))

(defpackage #:schemeish
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.named-let
	#:schemeish.syntax
	#:schemeish.arguments
	#:schemeish.expand-define
	#:schemeish.define
	#:schemeish.lambda
	#:schemeish.base
	#:schemeish.bundle)
  (:shadowing-import-from
   #:schemeish.named-let #:let)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort)
  (:shadow #:stream

	   ;; Special symbols are shadowed so that they can be bound lexically
	   #:+
	   #:/
	   #:-
	   #:*
	   #:++
	   #:+++
	   #://
	   #:///
	   #:**
	   #:***)
  (:export
   #:NIL
   ;; Re-export the un-special functions
   #:+
   #:/
   #:-
   #:*
   
   ;; Reader macros
   #:install-syntax!
   #:uninstall-syntax!

   ;; Definition macros
   #:for-macros
   #:let
   #:lambda
   #:define
   #:and-let*
   #:lexically
   #:expose
   
   ;; Symbols
   #:make-keyword
   #:symbol->string
   #:symbol?

   ;; Strings
   #:string-append   
   #:string?
   #:newline
   #:display
   #:displayln

   ;; Bundles
   #:make-bundle-predicate
   #:bundle-predicate?
   #:bundle
   #:bundle-permissions
   #:bundle-list
   #:bundle?
   #:bundle-predicate-symbol
   #:bundle-documentation

   ;; Lists
   #:append*
   #:map
   #:empty?
   #:filter
   #:pair?
   #:null?
   #:list?
   #:list-ref
   #:list-tail
   #:foldr
   #:foldl
   #:andmap
   #:ormap
   #:for-each
   #:remq
   #:remq*
   #:remove*
   #:sort
   #:memf
   #:findf
   #:list-update
   #:take
   #:drop
   #:split-at
   #:dropf
   #:takef
   #:splitf-at
   #:flatten
   #:filter-map
   #:partition
   #:range
   #:append-map
   #:filter-not
   #:map-successive

   ;; Numbers
   #:positive?
   #:negative?
   #:even?
   #:odd?
   #:zero?
   #:quotient
   #:number->string
   #:radians->degrees
   #:degrees->radians
   #:sqr
   #:number?

   ;; Procedures
   #:procedure?
   #:compose
   #:swap-args
   #:rcurry
   #:lcurry
   #:disjoin*
   #:conjoin*
   #:conjoin
   #:const
   #:memo-proc
   #:procedure-arity
   #:procedure-arguments
   #:has-specific-arity?
   #:document!

   ;; Streams
   #:delay
   #:force
   #:stream-cons
   #:stream-car
   #:stream-cdr
   #:stream-for-each
   #:*the-empty-stream*
   #:stream-empty?
   #:stream-first
   #:stream-rest
   #:list->stream
   #:stream?
   #:stream-map
   #:stream-fold
   #:stream-filter
   #:stream
   #:stream-length
   #:stream-drop
   #:stream-take
   #:stream-ref
   #:stream-append
   #:stream-flatten
   #:stream-flatmap
   #:stream-range
   #:stream-collect
   #:stream-map-successive

   ;; Association lists
   #:alist-set
   #:alist-remove
   #:alist-ref
   #:alist-update
   #:alist-map
   #:alist-keys
   #:alist
   #:alist-set*
   #:alist-has-key?
   #:alist-values
   
   ;; Booleans
   #:eq?
   #:equal?
   #:nand
   #:nor
   #:xor
   #:for-all
   #:for-all*
   #:there-exists
   #:there-exists*

   ;; Lists as sets
   #:set-member?
   #:set-add
   #:set-empty?
   #:set-count
   #:set->stream
   #:set-union
   #:set-intersect
   #:set-subtract
   #:subset?
   #:set=?

   ;; Structures
   #:define-struct
   #:struct?
   #:struct-copy
   #:struct->list
   #:struct-accessors

   #:serialize
   
   ;; Mutable lists
   #:set!
   #:set-car!
   #:set-cdr!

   ;; Queue
   #:make-queue
   #:queue-front
   #:queue-empty?
   #:queue-insert!
   #:queue-delete!))


(cl:do-external-symbols (symbol (cl:find-package :cl))
  (cl:multiple-value-bind (symbol accessibility)
      (cl:find-symbol (cl:symbol-name symbol) (cl:find-package :schemeish))
    (cl:unless (cl:eq accessibility :external)
      (cl:export symbol (cl:find-package :schemeish)))))

;;;; package.lisp

(defpackage #:schemeish.for-macros
  (:documentation "Provides FOR-MACROS which expands to (EVAL-WHEN ...)")
  (:use #:cl)
  (:export #:for-macros))

(defpackage #:schemeish.named-let
  (:documentation "Provides an optionally named LET which can be used to write a locally recursive form.")
  (:use #:cl)
  (:shadow #:let)
  (:export #:let))

(defpackage #:schemeish.syntax
  (:documentation "Provides install/uninstall-syntax! for expanding [fn-value args...] => (funcall fn-value args...)")
  (:use #:cl)
  (:export #:install-syntax! #:uninstall-syntax!))

(defpackage #:schemeish.arguments
  (:documentation "Tools to translate scheme style argument lists to CL style argument lists.")
  (:use #:cl)
  (:export #:arg-list->lambda-list))

(defpackage #:schemeish.expand-define
  (:documentation "Tools to expand define and define-like forms.")
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
  (:documentation "Replaces LAMBDA with a scheme style argument list and a body that can have local defines.")
  (:use #:cl
	#:schemeish.arguments
	#:schemeish.expand-define)
  (:shadow #:lambda)
  (:export #:lambda))
(defpackage #:schemeish.define
  (:documentation "Provides DEFINE. See DEFINE's docs for more details.")
  (:use #:cl
	#:schemeish.expand-define
	#:schemeish.for-macros
	#:schemeish.syntax)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:export #:define))

(import '(schemeish.define:define) :schemeish.expand-define)
(shadowing-import '(schemeish.lambda:lambda) :schemeish.expand-define)

(defpackage #:schemeish.base
  (:documentation
   "Provides many core functions and simple macros, including
  - symbols
  - lists
  - procedures
  - alists
  - sets
  - strings
  - output
  - mutation")
  (:use #:cl
	#:schemeish.define
	#:schemeish.for-macros
	#:schemeish.syntax)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadow #:map
	   #:sort
	   #:stream)
  (:export
   ;; Symbols
   #:symbol?
   #:symbol->string
   #:make-keyword

   ;; Lists
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
   #:flatten

   ;; Procedures
   #:procedure?
   #:rcurry
   #:lcurry
   #:swap-args
   #:memo-proc
   #:document!
   #:has-specific-arity?
   #:procedure-arity
   #:procedure-arguments
   #:procedure-arguments-required-arguments
   #:procedure-arguments-optional-arguments
   #:procedure-arguments-key-arguments
   #:procedure-arguments-rest-argument
   #:procedure-arguments-allow-other-keys?

   ;; Alists
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

   ;; Booleans
   #:eq?
   #:equal?
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

   ;; Numbers
   #:quotient
   #:number->string
   #:degrees->radians
   #:radians->degrees
   #:sqr
   #:sgn
   #:number?

   ;; Sets
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

   ;; Streams
   #:delay
   #:force
   #:*the-empty-stream*
   #:stream-cons
   #:stream-car
   #:stream-cdr
   #:stream-empty?
   #:stream-for-each
   #:stream-length
   #:stream->list
   #:stream-first
   #:stream-rest
   #:stream?
   #:list->stream
   #:stream
   #:stream-map
   #:stream-fold
   #:stream-filter
   #:stream-drop
   #:stream-take
   #:stream-ref
   #:stream-append
   #:stream-flatten
   #:stream-range
   #:stream-flatmap
   #:stream-map-successive
   #:random-stream

   ;; Strings
   #:string-append
   #:string?
   #:string-starts-with?

   ;; Output
   #:newline
   #:display
   #:displayln

   ;; Mutation
   #:set-car!
   #:set-cdr!
   #:set!))

(defpackage #:schemeish.expand-stream-collect
  (:documentation "Provides tools to expand a stream-collect macro form.")
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
			  #:sort
			  #:stream)
  (:export #:stream-collect-form))

(defpackage #:schemeish.stream-collect
  (:documentation "Provides the stream-collect macro.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.expand-stream-collect
	#:schemeish.base)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export #:stream-collect))

(defpackage #:schemeish.and-let
  (:documentation "Provides the and-let* macro. See its docs for more details.")
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
			  #:sort
			  #:stream)
  (:export #:and-let*))


(defpackage #:schemeish.struct
  (:documentation "Provides the basis and expansions for define-struct.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:struct
   #:struct?
   #:struct-copy
   #:struct->list
   #:struct-accessors
   #:struct-form))

(defpackage #:schemeish.define-struct
  (:documentation "Provides define-struct.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let
	#:schemeish.struct)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export #:define-struct))

(defpackage #:schemeish.bundle
  (:documentation "Provides bundle and make-bundle-predicate for creating dispatch-style closures.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:bundle
   #:bundle-documentation
   #:bundle-permissions
   #:bundle-list
   #:bundle?
   #:make-bundle-predicate))

(defpackage #:schemeish.queue
  (:documentation "Provides a bundle-based implementation of a queue.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let
	#:schemeish.bundle)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:make-queue
   #:queue?
   #:queue-empty?
   #:queue-front
   #:queue-insert!
   #:queue-delete!))

(defpackage #:schemeish.expand-lexically
  (:documentation "Provides tools to expand lexically/expose macros.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:lexical-bindings
   #:parameter-name->lexical-name
   #:parameter-name?
   #:special-form?
   #:special?
   #:lexical-name->parameter-name))

(defpackage #:schemeish.lexically
  (:documentation "Provides the lexically and expose macros.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.expand-define
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let
	#:schemeish.expand-lexically)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:lexically
   #:expose))


(defpackage #:schemeish.serialize
  (:documentation "Provides the lexically and expose macros.")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.expand-define
	#:schemeish.define
	#:schemeish.syntax
	#:schemeish.base
	#:schemeish.and-let
	#:schemeish.bundle
	#:schemeish.struct
	#:schemeish.define-struct
	#:schemeish.queue)
  (:shadowing-import-from #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.named-let #:let)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:export
   #:serialize))

(defpackage #:schemeish
  (:documentation "Provides everything in the schemeish-library. Re-exports CL so that packates can (:use #:schemeish) instead of (:use #:cl)")
  (:use #:cl
	#:schemeish.for-macros
	#:schemeish.named-let
	#:schemeish.syntax
	#:schemeish.arguments
	#:schemeish.define
	#:schemeish.lambda
	#:schemeish.base
	#:schemeish.and-let
	#:schemeish.stream-collect
	#:schemeish.bundle
	#:schemeish.queue
	#:schemeish.struct
	#:schemeish.define-struct
	#:schemeish.lexically
	#:schemeish.serialize)
  (:shadowing-import-from
   #:schemeish.named-let #:let)
  (:shadowing-import-from
   #:schemeish.lambda #:lambda)
  (:shadowing-import-from #:schemeish.base
			  #:map
			  #:sort
			  #:stream)
  (:shadow
   ;; Special symbols without *ear-muffs* are shadowed
   ;; so that they can be bound lexically
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
   ;; Re-export shadowed functions
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
      (cl:export (list symbol) (cl:find-package :schemeish)))))

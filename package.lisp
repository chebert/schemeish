;;;; package.lisp

(defpackage #:schemeish
  (:use #:cl)
  (:shadow #:let
	   #:lambda
	   #:map
	   #:sort
	   #:stream)
  (:export
   #:NIL
   
   ;; Reader macros
   #:install-syntax!
   #:uninstall-syntax!

   ;; Definition macros
   #:for-macros
   #:let
   #:lambda
   #:λ
   #:define
   #:and-let*
   
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

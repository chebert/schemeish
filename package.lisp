;;;; package.lisp

(defpackage #:schemeish
  (:use #:cl)
  (:shadow #:let
	   #:lambda
	   #:map
	   #:sort
	   #:stream)
  (:export
   ;; Reader macros
   #:install-syntax!
   #:uninstall-syntax!

   ;; Definition macros
   #:for-macros
   #:let
   #:lambda
   #:λ
   #:define

   ;; Symbols
   #:make-keyword
   #:symbol->string
   
   ;; Bundles
   #:make-bundle-predicate
   #:bundle
   #:bundle-permissions
   #:bundle-list

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

   ;; Numbers
   #:positive?
   #:negative?
   #:even?
   #:odd?
   #:zero?
   
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
   
   ;; Equality
   #:eq?
   #:equal?))

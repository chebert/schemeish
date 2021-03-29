;;;; package.lisp

(defpackage #:schemeish
  (:use #:cl)
  (:shadow #:map
	   #:let
	   #:sort)
  (:export
   #:for-macros
   #:map
   #:let
   #:install-syntax!
   #:group
   #:λ
   #:append*
   #:empty?
   #:define
   #:make-bundle-predicate
   #:*get-bundle-list*
   #:make-keyword
   #:bundle
   #:bundle-permissions
   #:bundle-list
   #:filter
   #:pair?
   #:null?
   #:list?
   #:list-ref
   #:list-tail
   #:foldr
   #:foldl
   #:positive?
   #:eq?
   #:equal?
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
   #:symbol->string
   #:take
   #:drop
   #:split-at
   #:even?
   #:dropf
   #:takef
   #:splitf-at
   #:flatten
   #:filter-map
   #:partition
   #:range
   #:append-map))

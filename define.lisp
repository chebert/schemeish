(in-package #:schemeish.define)


(defmacro define (name-or-form &body body)
  "Definition form.
  (define *variable-name* value) ;; Expands to (defparameter *variable-name* value)
  (define (function-name arg1 arg2 . args) 
    body...) ;; Expands to
  (defun function-name (arg1 arg2 &rest args)
    body...)

  ;; Optional arguments are specified as (arg-name default-value-form) or (arg-name) which defaults to nil.
  (define (fn pos1 pos2 (opt1) (opt2 \"default\"))
    (list pos1 pos2 opt1 opt2))

  ;; Keyword arguments are specified as :arg-name, (:arg-name) both of which default to nil, or (:arg-name default-value-form).
  (define (fn pos1 pos2 (:k1 \"default\") :k2)
    (list pos1 pos2 k1 k2))

  ;; Rest/Optional/Keyword arguments are not compatible with each other.
    
  ;; Defines can be nested like in scheme.
  (define (outer-function-name oarg1 oarg2 . oargs)
    (define (inner-function-name a1 a2) (list a1 a2))

    ;; Definitions can be mutually recursive.
    (define (mutually-recursive-function1 a) (mutually-recursive-function2 a))
    (define (mutually-recursive-function2 a) (mutually-recursive-function1 a))

    ;; Nested defines bind both function/lexical variable
    (inner-function-name oarg1 oarg2) ; as a function
    inner-function-name) ; returned as a value
  
  ;; Define can define functions which return closures:
  (define ((left-curry . args) f)
    ...)
  ;; Expands to
  (defun left-curry (&rest args)
    (lambda (f)
       ...))
  
  It is an error to mix defines with expressions.
  Special case: if the first form of a post-define body is a let or a let* you can place defines in that form.
  Example:
  (define (outer)
    (let ((x :x) (y :y))
      (define (inner1) (cons x y))
      (let ((z :z))
        (define (inner2) (list x y z))
        (inner2))
      (inner1)))"
  `(macrolet ((define (&whole inner-whole &body ignored)
		(declare (ignore ignored))
		(error "Improperly nested define: ~S in expansion for ~S" inner-whole ',name-or-form)))
     ,(expand-top-level-define name-or-form body)))

(for-macros (install-syntax!))
(define (((test-nested-defines x) y . yargs) . zargs)
  "Returns a thing"
  `(,x ,y ,@yargs ,@zargs))

(assert (equal [[(test-nested-defines :x) :y :z] :a :b :c]
	       '(:x :y :z :a :b :c)))
(define (test-inner-nested-defines)
  "Also returns a thing"
  (define ((inner-nested x) y)
    (list x y))
  inner-nested)

(assert (equal [[(test-inner-nested-defines) :x] :y]
	       '(:x :y)))

(for-macros (uninstall-syntax!))

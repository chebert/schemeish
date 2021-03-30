# schemeish
### Chebert

Provide Scheme/Racket style naming conventions and objects in Common Lisp.

Two things in particular, define and [] syntax:

    (define *variable-name* value) ;; Expands to (defparameter *variable-name* value)
    (define (function-name arg1 arg2 . args) 
      body...) ;; Expands to
    (defun function-name (arg1 arg2 &rest args)
      body...)
      
    ;; Defines can be nested like in scheme.
    (define (outer-function-name oarg1 oarg2 . oargs)
      ;; Binds both function/lexical variable
      (define (inner-function-name a1 a2) (list a1 a2))
      (define (mutually-recursive-function1 a) (mutually-recursive-function2 a))
      (defien (mutually-recursive-function2 a) (mutually-recursive-function1 a))
      (inner-function-name oarg1 oarg2)
      inner-function-name)
    
    ;; Define nested functions.
    (define ((left-curry . args) f)
      ...)
    
    ;; [] Brackets evaluate the function argument (as if funcalling it)
    [(left-curry 5) 4] ;; => 1

## License

MIT

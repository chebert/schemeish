# schemeish
### Chebert

Provide Scheme/Racket style naming conventions and objects in Common Lisp.

Three things in particular, named let, define and [] syntax:

    ;; Named let allows for local recursion.
    (let rec ((n 10)
              (result '()))
      (if (= n 0)
          result
          (rec (1- n) (cons n result))))
    ;; => '(1 2 3 4 5 6 7 8 9 10)

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
    
    ;; λ is like a lambda but it uses scheme-style argument lists
    (λ (arg1 arg2 arg3) (list arg1 arg2 arg3)) ;; Arity: 3
    (λ (arg1 . args) (list* arg1 args)) ;; Arity: at least 1
    (λ args args) ;; Arity: at least 0

    
The following adjustments to emacs init.el enhance the experience:

    (defun enable-squares-as-parens-in-syntax-table (syntax-table)
       (modify-syntax-entry ?\[ "(]" syntax-table)
       (modify-syntax-entry ?\] ")[" syntax-table))

     (enable-squares-as-parens-in-syntax-table lisp-mode-syntax-table)

     (define-key paredit-mode-map (kbd "M-{")
        'paredit-wrap-square)
        
     (global-set-key (kbd "C-\\") "λ")

## License

MIT

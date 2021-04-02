# schemeish
### Chebert

Provide Scheme/Racket style naming conventions and objects in Common Lisp.

The SCHEMEISH package can be used in place of CL.

    (defpackage my-new-package
       (:shadowing-import-from :cl :lambda)
       (:use :schemeish))
       
To use [] syntax (described below) you can use `INSTALL-SYNTAX!` and to disable it again you can use `UNINSTALL-SYNTAX!`. 
It's recommended that you do these in a call to `FOR-MACROS` at the top and bottom of the file.


The following CL symbols are replaced by Schemeish:

 - `LAMBDA` uses scheme-style argument lists with nestable defines. I recommend shadowing this symbol and use λ (or some other name) instead.
 - `LET` is fully compatible with `CL:LET`, but with named let syntax described below
 - `SORT` is a non-modifying sort for lists
 - `MAP` maps a function over a list (equivalent to `MAPCAR`)
 - `STREAM` constructs a Scheme-style stream of elements.

The rest of Schemeish is fully compatible with the `CL` package. If you'd like to favor `CL` in conflicts you can use `:SHADOWING-IMPORT-FROM`.

Three things I find particularly valuable, named let, define and [] syntax:

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
      (define (mutually-recursive-function2 a) (mutually-recursive-function1 a))
      (inner-function-name oarg1 oarg2)
      inner-function-name)
    
    ;; Define nested functions.
    (define ((left-curry . args) f)
      ...)
    
    ;; [] Brackets evaluate the function argument (as if funcalling it)
    [[(left-curry 5 4) '-] 3 2] ;; => (- 5 4 3 2)
    
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
     
    
    (defun swap-square-and-round ()
      "Change |(..) to |[..]. | is point position."
      (interactive)
      (let* ((char (string (char-after)))
	         (paren? (string= char "("))
	         (square? (string= char "[")))
        (cond
         (paren? (paredit-open-square 1))
         (square? (paredit-open-round 1)))
        (when (or paren? square?)
          (right-char 1)
          (paredit-splice-sexp)
          (left-char 1))))

    (define-key paredit-mode-map (kbd "M-[") 'swap-square-and-round)


## License

MIT

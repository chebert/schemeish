# schemeish
### Chebert

Provide Scheme/Racket style naming conventions and objects in Common Lisp.

With a couple of minor exceptions Schemeish is designed to enhance Common Lisp by providing new functionality.
There are still separate namespaces for functions and values. `LET`, `DEFUN`, and `DEFMACRO` all work the same.

The SCHEMEISH package can be used in place of CL.

    (defpackage my-new-package
       (:shadowing-import-from :cl :lambda)
       (:use :schemeish))

The following CL symbols are replaced by Schemeish:

 - `LAMBDA` uses scheme-style argument lists with nestable defines.
 - `LET` is fully compatible with `CL:LET`, but with named let syntax described below
 - `SORT` is a non-modifying sort for lists
 - `MAP` maps a function over a list (equivalent to `MAPCAR`)
 - `STREAM` constructs a Scheme-style stream of elements.

The rest of Schemeish is fully compatible with the `CL` package. If you'd like to favor `CL` in conflicts you can use `:SHADOWING-IMPORT-FROM`.

## SYNTAX

Schemeish provides `[...]` syntax which expands to `(funcall ...)`.
To use [] syntax (described below) you can use `INSTALL-SYNTAX!` and to disable it again you can use `UNINSTALL-SYNTAX!`. 

## DEFINE

`DEFINE` is Schemeish's major provision. Defines have two similar behaviors depending on if they are nested.

If a define is at the top level, it is only used to define functions, since CL doesn't have a global lexical environment for values.

    (define (add2 n) (+ 2 n))

If define is just given a name, it sets the function value of the symbol:

    (define add2 (lcurry #'+ 2))

If define is nested or at the top level it can be used to define functions:

Define/Lambda can take rest parameters:

    (define (add2 . args) (apply #'+ 2 args))

It can take optional arguments:

    (define (greet name (greeting "Hello") (out))
      (format out "~A ~A" greeting name))
	  
Or it can take keyword arguments:

    (define (greet name (:greeting "Hello") (:out))
      (format out "~A ~A" greeting name))
	  
If a define is nested within another define, it is used to define mutually recursive functions/variables.

    (define (top-level)
	  (define x 3)
      (define f (lambda (v) (g v 2)))
	  (define (g x y) (+ x y))
      (f x))

Value-slots and function-slots are bound to the function object created by a nested define.

## NAMED LET

In all cases the `LET` provided by Schemeish works the same as `CL:LET`

Additionally you can create a local named procedure with `LET`

    (let recurse ((result 1)
                  (n 5))
      (if (= n 0)
          result
          (recurse (* result n) (1- n))))

## SCHEMISH BASE

Schemeish provides utilities for working with:

- Alists
- Function objects: e.g. currying, composing, arity,
- Lists
- Hash tables
- Scheme-style streams
- Vectors

## CUT

Cut provides a macro for creating curried functions. See documentation for `CUT`.

## Lexically

Lexically provides a new lexical environment which can have nested defines. Functions/variables can be exported to global scope using
expose. See `DEFINE` and `EXPOSE`. 

## AND-LET*

AND-LET* will evaluate each clause from first to last until one is false. If all are true, evaluate body.
Each clause is one of: identifier, (expression), or (identifier expression).
If the clause is (identifier expression) it creates a binding for the rest of the clauses and the body.

    (and-let* ((list (compute-list))
               ((pair? list))
               (item (car list))
               ((integer? item)))
      (sqrt item))

## BUNDLE

Bundles are closure objects which provide an object-oriented style interface. See documentation of `BUNDLE`.

## DEFINE-STRUCT

Define-struct provides a syntax for creating records. See documentation of `DEFINE-STRUCT`. The underlying type is a CLOS object, but this is subject to change.

## PACKAGE-UTILS

Schemeish also exports tools for dealing with packages, including reconstructing `DEFPACKAGE` forms from the current state of a package.
See `DEFPACKAGE-FORM`.

## EMACS

The following adjustments to emacs init.el enhance the experience:

    (defun enable-squares-as-parens-in-syntax-table (syntax-table)
       (modify-syntax-entry ?\[ "(]" syntax-table)
       (modify-syntax-entry ?\] ")[" syntax-table))

     (enable-squares-as-parens-in-syntax-table lisp-mode-syntax-table)

     (define-key paredit-mode-map (kbd "M-{")
        'paredit-wrap-square)
        
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

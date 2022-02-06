# schemeish
## Introduction

A Scheme/Racket inspired library for Common Lisp. 

- Lisp-2 (separate function and variable namespaces) definition macro, which binds values/functions in both namespaces
  - Clean `[...]` syntax for `(FUNCALL ...)` 
- Lisp-1 (unified function and variable namespace) definition macro, which performs code-walking to achieve a single namespace
- Compact, scheme-style argument lists with an easy syntax for specifying optional, keyword, rest, and ignorable arguments
- Enriched package functions which bring packages to the level of first class objects
- Optional guard clauses which are enforced by functions, and provide documentation
- Enriched documentation for functions allow objects other than strings
  - A markup language which can be rendered as plain-text or HTML, which can be used for rich documentation
- Named `LET` for generating a locally recursive form
- Scheme-inspired `STREAM`s and `BUNDLE`s
- Record-style `DEFINE-STRUCT`
- Utility functions for: lists, alists, list-based sets, hash-tables, procedures, procedure arities, strings
- `CUT` macro for easily creating curried functions
- A code transformer for transforming SEXP-based languages
- TODO: Planned implementation of delimited continuations with similar semantics to `CONTROL`/`RUN` as used by Racket
  - These continuations would be respectful of dynamic forms such as `CATCH` and `UNWIND-PROTECT`

The SCHEMEISH package can be used in place of CL.

    (defpackage my-new-package
       (:use #:schemeish))

With a few minor exceptions Schemeish is designed to enhance Common Lisp by providing new functionality, rather than replacing exisiting symbols.
The following CL symbols are replaced by Schemeish:

 - `LAMBDA` uses scheme-style argument lists with nestable defines.
 - `LET` is fully compatible with `CL:LET`, but with additionaly named let syntax
 - `SORT` is a non-modifying sort for lists
 - `MAP` maps a function over a list (equivalent to `MAPCAR`)
 - `STREAM` constructs a Scheme-style stream of elements.

The rest of Schemeish is fully compatible with the `CL` package. If you'd like to favor `CL` in conflicts you can use `:SHADOWING-IMPORT-FROM`.

```
(defpackage my-streamless-package
   (:use #:schemeish)
   (:shadowing-import-from #:cl #:stream))
```


## SYNTAX

Schemeish provides:
 - `[...]` syntax which expands to `(funcall ...)`.
 - #G(guard-clauses...) syntax which is used to annotate function-definitions. These create guard clauses for functions as well as for documentation.
 - #Ddocumentation-source syntax which is used to annotate function-definitions. These enable rich documentation (more than just documentation-strings).
   A documentation-source is any object which implements the `DOCUMENTATION-STRING` method.

To use Schemeish syntax you can use `INSTALL-SYNTAX!` and to disable it again you can use `UNINSTALL-SYNTAX!`.
Typically this is done at the start/end of each file, but it may be helpful to leave out the `UNINSTALL-SYNTAX!` during development.

```
(install-syntax!)
[(lcurry #'+ 2) 1 2 3] ;; => 8

#g((number? x) (string? y)) ;; => #g((number? x) (string? y)) 
#d(format nil "Hello") ;; => #d(format nil "Hello")
```

## DEFINE: Lisp-2 Definitions for Separate Function/Variable Namespaces

At the top-level, `DEFINE` defines a global function.

`DEFINE` takes one of three forms

- `(define name [documentation] function)` defines a function named name

```
(define 2+ "Adds (+ 2 args...)" (lcurry #'+ 2)) ;; => 2+
(2+ 1 2 3) ;; => 8
(documentation #'2+ t) ;; => "Adds (+ 2 args...)"
```

- `(define (name . scm-parameters) function-body...)` defines a function named name with the given `scm-parameters` and `function-body` 

```
(define (frobnicate a b (:keyword1 3))
  (list a b keyword1)) ;; => frobnicate

(frobnicate 1 2 :keyword1 'c) ;; => (1 2 C)
```

- `(define ((...) . scm-parameters) function-body...)` defines a function which takes `scm-parameters` and returns a closure whose body is function-body

```
(define (((nested-foo a) b) . c)
  (list* a b c)) ;; => nested-foo

[[(nested-foo 1) 2] 3 4 5] => (1 2 3 4 5)
(funcall (funcall (nested-foo 1) 2) 3 4 5) => (1 2 3 4 5)
```

## FUNCTION-BODY

Function bodies in Schemeish take documentation objects, guard tags, and expand defintions in a lexical-body.
A function body takes the form `([documentation] [guard] lexical-body...)`

If provided, `documentation` may either be a `documentation-tag` (a documentation object prepended with "#D") or a string.
  - A `documentation-tag`'s object is evaluated each time the function is compiled.
If provided, `guard` is a `guard-tag` (an unevaluated list of guard clauses prepended with "#G")

```
(define (guarded-foo index vector)
  #d(format nil "Returns the value at (aref vector index)")
  #g((number? index) (vectorp vector) (< index (length vector)))
  (aref vector index)) ;; => guarded-foo

(guarded-foo 3 #(1 2)) ;; => ERROR: Failed function guard-clause: (< INDEX (LENGTH VECTOR))
(guarded-foo 1 #(1 2)) ;; => 2
(documentation #'guarded-foo t)
;; =>
"Returns the value at (aref vector index)

Parameters: (INDEX VECTOR)
Definition Form: (GUARDED-FOO INDEX VECTOR)

GUARDED-FOO has the following guard clauses:
((NUMBER? INDEX) (VECTORP VECTOR) (< INDEX (LENGTH VECTOR)))"

```

## LEXICAL-BODY

A `LEXICAL-BODY` in Schemeish expands mutually recursive definitions (a la `LETREC` or `LABELS`).
There are two types of `lexical-body`s: Lisp-1 style (unified function/variable namespace) and Lisp-2 style (separate function/variable namespaces).
A `lexical-body` takes the form `(definitions declarations forms...)`, where definitions are `lexical-body-definition`s, declarations are of the form `(cl:declare ...)`,
and forms... are evaluated in an implicit progn.

The following definition forms are available by default:

- `DEFINE` and `DEF`: Follows the same rules as `DEFINE`, but defines a local function using `labels`, and a variable using `let`.
- `(DEFINE-VALUES (names...) values-form)`: Uses `multiple-value-setq` to assign names
- `(DEFINE-DESTRUCTURING destructuring-lambda-list form)`: Uses `destructuring-bind` to assign values to the parameters named by `destructuring-lambda-list`

```
(define (foo)
  (define-values (number remainder) (truncate 3 4))
  (define-destructuring (&whole whole a b c &rest more)
    (list :a :b :c :r1 :r2 :r3))
  (define v :v)
  (define (f x) (list 'f x))
  (define (((g x) y) z) `(((g ,x) ,y) ,z))
  
  (list number remainder
        whole a b c more
        v
        (f :x)
        [[(g :x) :y] :z])) ;; => foo
(foo) ;; => (0 3 (:A :B :C :R1 :R2 :R3) :A :B :C (:R1 :R2 :R3) :V (F :X) (((G :X) :Y) :Z))
```

More definition forms can be added using `register-lexical-body-definition` and `register-lexical-body2-definition` (for Lisp-2 style).

`lexically` can be used to create an explicit lisp-2 style `lexical-body`
`scm` (see `DEF`) can be used to create an explicit lisp-1 style `lexical-body` 

## DEF: Lisp-1 Definitions for a Unified Function/Variable Namespace

At the top-level `DEF` is very similar to `DEFINE`, except that it performs code-transformation to create a Lisp-1 style unified function/variable namespace.
The `SCM` macro performs the code transformation, and can be used to create Lisp-1 contexts.

The following are the same examples as in `DEFINE`, but using `DEF` instead.


```
(def 2+ "Adds (+ 2 args...)" (lcurry + 2)) ;; => 2+
(2+ 1 2 3) ;; => 8
(scm (documentation 2+ t)) ;; => "Adds (+ 2 args...)"

(def (frobnicate a b (:keyword1 3))
  (list a b keyword1)) ;; => frobnicate

(frobnicate 1 2 :keyword1 'c) ;; => (1 2 C)

(def (((nested-foo a) b) . c)
  (list* a b c)) ;; => nested-foo

(scm (((nested-foo 1) 2) 3 4 5)) => (1 2 3 4 5)
```

## PACKAGE-UTILS

Schemeish provides enriched package utilities. Some examples:

- `(package-symbols package)`: returns a list of all symbols in package
- `(defpackage-form package)`: Returns a `DEFPACKAGE` form
- `(hierarchical-defpackage-forms packages)`: Returns a list of `DEFPACKAGE` forms that have been organized hierarchically for use in `package.lisp`
- `((package-re-export-shadowing . re-exported-packages) package)`: Uses and re-exports `re-exported-packages` from `package`.
- Many more

## GUARD CLAUSES

Guard clauses provide both documentation and enforcement:


```
(define (guarded-foo index vector)
  #d(format nil "Returns the value at (aref vector index)")
  #g((number? index) (vectorp vector) (< index (length vector)))
  (aref vector index)) ;; => guarded-foo

(guarded-foo 3 #(1 2)) ;; => ERROR: Failed function guard-clause: (< INDEX (LENGTH VECTOR))
(guarded-foo 1 #(1 2)) ;; => 2
(documentation #'guarded-foo t)
;; =>
"Returns the value at (aref vector index)

Parameters: (INDEX VECTOR)
Definition Form: (GUARDED-FOO INDEX VECTOR)

GUARDED-FOO has the following guard clauses:
((NUMBER? INDEX) (VECTORP VECTOR) (< INDEX (LENGTH VECTOR)))"

```

The runtime-enforcement of guard-clauses can be dynamically circumvented using `with-guard-clauses-disabled` 

```
[(lambda (x y) #g((number? x) (string? y)) (list x y)) nil nil] ;; => ERROR: Failed function guard-clause: (NUMBER? X)
(with-guard-clauses-disabled [(lambda (x y) #g((number? x) (string? y)) (list x y)) nil nil]) ;; => (nil nil)
```

## RICH DOCUMENTATION

Schemeish provides an extensible markup language and a means of providing richer text for documentation.
This can be used to generate both human-readable docstrings as well as HTML.
Schemeish provides a basic text renderer for its markup language, which renders to Markdown-like plain text.

```
(define (fancy-docs)
		  #d(paragraph "This is some fancy documentation with "
			       (italic (inline-text "italics"))
			       " with "
			       (bold (inline-text "bolds"))
			       " with " (br)
			       (block-quote (inline-text "Quotes")))
		  :ok) ;; => fancy-docs
		  
(documentation #'fancy-docs t)
;; =>
"This is some fancy
documentation with 
_italics_ with 
*bolds* with 
> Quotes



Parameters: NIL
Definition Form: (FANCY-DOCS)

FANCY-DOCS has no guard clauses."

(type-of (object-documentation-source #'fancy-docs)) ;; markup

```

Any object which implements the `documentation-string` method can be used with `documentation-tag`s.


## NAMED LET

In all cases the `LET` provided by Schemeish works the same as `CL:LET`

Additionally you can create a local named procedure with `LET`

    (let recurse ((result 1)
                  (n 5))
      (if (= n 0)
          result
          (recurse (* result n) (1- n))))



## CUT

Cut provides a macro for creating curried functions using placeholders.

```
(scm ((cut (list 1 _ 3 . _)) 2 4 5 6)) ;; => (1 2 3 4 5 6)
```



## AND-LET*

AND-LET* will evaluate each clause from first to last until one is false. If all are true, evaluate body.
Each clause is one of: identifier, (expression), or (identifier expression).
If the clause is (identifier expression) it creates a binding for the rest of the clauses and the body.

    (and-let* ((list (compute-list))
               ((pair? list))
               (item (car list))
               ((integer? item)))
      (sqrt item))

## STREAMS

Provides functions for creating and manipulating streams e.g.: `stream-cons`, `stream-append`, `stream-map`.

Provides the `stream-collect` macro for efficient implementation of combinatorial algorithms. 

```
(define (prime? num)
  (let ((root (floor (sqrt num))))
    (not (find-if (lambda (div) (zerop (rem num div))) (range (1+ root) 2)))))

(define (prime-sum-pairs n)
  (stream-collect
   (list i j (+ i j))
   ((i (stream-range 1 n))
    (j (stream-range 1 (1- i))))
   (prime? (+ i j))))

(stream->list (prime-sum-pairs 6)) ;; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))))
```

## DEFINE-STRUCT

Define-struct provides a syntax for creating records. See documentation of `DEFINE-STRUCT`.
The underlying type is a CLOS object.
Transparent structs (the default) can be deep-compared using `equal?`, and have a readable implementation of the `print-object` method.

```
(define-struct p2
  (x y)) ;; => (P2 MAKE-P2 P2? P2-X P2-Y)

(p2? (make-p2 3 4)) ;; => T
(let ((p (make-p2 3 4)))
  (sqrt (+ (* (p2-x p) (p2-x p))
        (* (p2-y p) (p2-y p))))) ;; => 5.0
```

Structs have simple inheritance (one parent).

```
(define-struct p3
  (z)
  :super 'p2) ;; => (P3 MAKE-P3 P3? P3-Z)
  
(p2? (make-p3 1 2 3)) ;; => T
(p3? (make-p3 1 2 3)) ;; => T

(make-p3 1 2 3) ;; => (make-p3 1 2 3)

(def x p2-x)
(def y p2-y)
(def z p2-z)

(let ((p (make-p3 1 2 3)))
  (list (x p) (y p) (z p))) ;; => (1 2 3)
```

By default structs are immutable, but `SET-` functions can be generated.

## UTILITIES

There are quite a number of general purpose utilities. Here are a few interesting ones:

- Alist functions for creating and manipulating association-lists
- `(group key-fn list)` Groups elements of list that have the same key-fn into an alist.
- `(hash-map table proc)` Maps [proc key value] over the keys and values of table, producing a list as a result.
- `(has-specific-arity? arity-list fixed-arity-n)` Returns true if an arity-list (retrieved from procedure-arity) has the specific fixed arity.
- `(string-map proc string)` Applies proc to each character in string, returning a new string 
of the results appended together. Proc is expected to return a character or string

## CODE-TRANSFORMER

A code transformer (or code walker) can transform a SEXP-based expression from language to another, expanding any macros along the way.
The transformation is defined by a `TRANSFORMER` structure, which has transform functions for special forms, lists (function application in CL), and atoms.
Each transform function has access to the the transformer, the expression being transformed, and the environment (from a macro's `&ENVIRONMNENT`).
See `scm.lisp` for an example usage of code-transformation.

## BUNDLE

Bundles are closure objects which provide an object-oriented style interface. See documentation of `BUNDLE`.

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

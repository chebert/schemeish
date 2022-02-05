# schemeish
### Chebert

Provide Scheme/Racket style naming conventions and objects in Common Lisp.

With a couple of minor exceptions Schemeish is designed to enhance Common Lisp by providing new functionality.
There are still separate namespaces for functions and values. `LET`, `DEFUN`, and `DEFMACRO` all work the same.

The SCHEMEISH package can be used in place of CL.

    (defpackage my-new-package
       (:use :schemeish))

You can find examples of this usage in the `examples/` folder of this project.

The following CL symbols are replaced by Schemeish:

 - `LAMBDA` uses scheme-style argument lists with nestable defines.
 - `LET` is fully compatible with `CL:LET`, but with named let syntax described below
 - `SORT` is a non-modifying sort for lists
 - `MAP` maps a function over a list (equivalent to `MAPCAR`)
 - `STREAM` constructs a Scheme-style stream of elements.

The rest of Schemeish is fully compatible with the `CL` package. If you'd like to favor `CL` in conflicts you can use `:SHADOWING-IMPORT-FROM`.

## SYNTAX

Schemeish provides:
 - `[...]` syntax which expands to `(funcall ...)`.
 - #G(guard-clauses...) syntax which is used to annotate function-definitions. These create guard clauses for functions as well as for documentation.
 - #Ddocumentation-source syntax which is used to annotate function-definitions. These enable rich documentation (more than just documentation-strings).
   A documentation-source is any object which implements the `DOCUMENTATION-STRING` method.

To use Schemeish syntax you can use `INSTALL-SYNTAX!` and to disable it again you can use `UNINSTALL-SYNTAX!`.
Typically this is done at the start/end of each file, but it may be helpful to leave out the `UNINSTALL-SYNTAX!` during development.

## LEXICAL-BODY

There are two styles of lexical-body. Lisp-2 style, which has a separated function/variable namespace, and Lisp-1 style for a unified function/variable namespace.
`LEXICALLY` enables lisp-2 style lexical bodies, while `SCM` performs code-walking to enable lisp-1 style lexical bodies.

### LEXICALLY

From the doc-strings:

```
Expands lisp-2 style lexical-body definitions in body. 
A lexical-body is (lexical-body-definitions... declarations... forms...).
If definition is LISP-2 it is transformed and its labels-bindings are used.
If definition is not LISP-2, but is LISP-1 it is transformed and a DEFAULT-LABELS-BINDING is used.
Creates mutually-recursive variable and function bindings for all definitions.
See REGISTER-LEXICAL-BODY-DEFINITION and REGISTER-LEXICAL-BODY2-DEFINITION for more information about how
to extend LEXICALLY.
See the results of evaluating (lexical-body2-definition-documentations) and (lexical-body-definition-documentations)
For documentation on the currently registered definition transformations.
```

### LEXICAL BODY DEFINITIONS

The following lexical body definitions are accepted by default for lisp-2 style lexical bodies.
From the doc-strings:

 - DEFINE: ```Transforms (define name-field ...) for lisp-2 style lexical-body.
If name-field is a symbol the expected form is (define symbol [documentation-source] value).
  A let binding is created for symbol, and value is assigned to it.
  A DEFAULT-LABELS-BINDING is created for symbol.
  The documentation-source and documentation string for value is set.
If name-field is a pair, the expected form is (define name-field function-body...)
  If name-field is a pair: ((...) . scm-parameters)
    A closure is created with the given scm-parameters, and define is recursively applied.
    E.g. (define (((nested x) y) z) function-body...) => 
         (define (nested x) (lambda (y) (lambda (z) function-body...)))
  If name-field is a pair: (symbol . scm-parameters)
    A labels binding is created with the given scm-parameters and function-body, expanded using PARSE-FUNCTION.
    A let binding is created for symbol, with #'symbol assigned to it.```

 - DEFINE-VALUES: ```Transforms (define-values name-or-names values-form) for lisp-1 style lexical-body.
If name-or-names is a symbol:
  A let binding is created, and the (multiple-values-list values-form) is assigned to it.
If name-or-names is a list of symbols:
  A let binding is created for each symbol, and they are bound using multiple-value-setq.
See also: LEXICALLY.
```

 - DEFINE-DESTRUCTURING: ```Transforms (define-destructuring destructuring-lambda-list expression) for lisp-1 style lexical-body.
Uses DESTRUCTURING-BIND to destructure expression and creates bindings for each name in destructuring-lambda-list.
```


### SCM

From the doc-strings:

```
Evaluates body in the SCM langauge. Similar to Common Lisp with the following changes:
If an expression is a proper list, it is transformed into (funcall function args).
If an expression is a dotted list, it is transformed into (apply function args... rest-arg)
If an expression is a symbol, its value is looked up in the variable environment at macro-expansion-time.
If expression is not a variable it is assumed to be in the function-namespace.
If a symbol is both SPECIAL and a bound FUNCTION, it is treated as a function rather than a variable.
  This is to deal with the unfortunate cases: (+ * / -).
  Typically functions and specials will not have the same name if the \*EAR-MUFF\* convention is followed.
 
A (LISP form) will escape SCM and process form as if it is in Common-Lisp.
E.g. (SCM (LISP +)) => the last evaluated repl form
     (SCM +) => #'+.
All forms with explicit/implicit blocks/progns are now lisp-1 style lexical-bodies.
For more details see (lexical-body-definition-documentations) for information about 
the available lexical-body-definition expansions.
These lexical-body's are the lisp-1 analogue to the lisp-2 style lexical-bodies defined by LEXICALLY.
```

### Lisp-1 Style Lexical Body Expansions

The same definitions types are accepted by default as the lisp-2 style lexical body.

## EXPOSE

```
Define var-specs as parameters in the global scope via DEFPARAMETER.
Define fn-specs as functions in the global scope via (SETF FDEFINITION).
Designed to be used within a lexical-body. See LEXICALLY, DEFINE, SCM, DEF.

Fn-spec is one of:
  fn-name: Expands to (setf (fdefinition 'fn-name) fn-name)
  (global-fn-name value): Expands to (setf (fdefinition 'global-fn-name) value)

Var-spec one of:
  VAR-NAME: \*Ear-muffs\* are added to symbol to create \*VAR-NAME\*. Expands to (defparameter \*var-name\* var-name).
  (\*global-special-name* value): Expands to (defparameter \*global-special-name\* value).

The return value is (PARAMETER-NAMES... GLOBAL-FN-NAMES ...)
```

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

## DEF

`DEF` is the lisp-1 analogue to `DEFINE`, providing the same functionality, but in a lisp-1 style context.

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

(in-package #:schemeish.backend)

(install-syntax!)

(export-definition
  (define-struct transformer
      (transform-special-form-table
       transform-proper-list
       transform-dotted-list
       transform-cyclic-list
       transform-atom)
      :documentation
      "A set of functions to transform expressions.
Transform-special-form-table is a hash table of SYMBOL -> TRANSFORM
where [transform transformer expr environment] => new-expr.
The remaining fields are also transforms."))

(export-definition
  (define (macro-function-application? expr environment)
    "True if, in the given lexical environment, expr is a macro-function application."
    (and (pair? expr)
	 (symbol? (first expr))
	 (macro-function (first expr) environment))))

(export-definition
  (define (transform-expression transformer expression (environment))
    "Returns an expression that has been recursively transformed using TRANSFORMER
in the given lexical environment.
Transform-expression first tests to see if expression is a special-form in the transformer's transform-special-form-table.
If it is not a special-form, it is then macroexpanded in the given environment.
If the form is a non-null proper-list (elements...), it is transformed using transformer's transform-proper-list.
If the form is a dotted-list (elements... . final-element), it is transformed using the transformers's transform-dotted-list.
If the form is a cyclic-list (#1#=(elements...) . #1), it is transformed using the transformer's transform-cyclic-list.
Otherwise, the form is treated as an atom and transformed using the transformer's transform-atom."
    (define transform-special-form
      (when (pair? expression)
	(hash-ref (transformer-transform-special-form-table transformer) (first expression) nil)))
    (cond
      (transform-special-form [transform-special-form transformer expression environment])
      ((macro-function-application? expression environment)
       (transform-expression transformer (macroexpand-1 expression environment) environment))
      ((pair? expression)
       (ecase (list-type expression)
	 (:proper [(transformer-transform-proper-list transformer) transformer expression environment])
	 (:cyclic [(transformer-transform-cyclic-list transformer) transformer expression environment])
	 (:dotted [(transformer-transform-dotted-list transformer) transformer expression environment])))
      (t [(transformer-transform-atom transformer) transformer expression environment]))))

(for-macros
  (defvar *transformer-table* (make-hash-table))
  (export
   (define (register-transformer transformer-name transformer)
     (hash-set! *transformer-table* transformer-name transformer)))
  (define (registered-transformer name)
    (hash-ref *transformer-table* name)))

(export-definition
  (for-macros
    (defvar *lexical-context* ())))
(export-definition
  (defmacro transform-in-lexical-environment (transformer-name expression lexical-context &environment environment)
    "Applies transformer to expression in environment.
*LEXICAL-CONTEXT* will be bound to the given lexical-context for the duration of the transformation."
    (let ((*lexical-context* lexical-context))
      (transform-expression (registered-transformer transformer-name) expression environment))))

(export-definition
  (define (transform transformer-name expression)
    "Returns a form that when evaluated, will transform the given expression
in the current *LEXICAL-CONTEXT*."
    `(transform-in-lexical-environment ,transformer-name ,expression ,*lexical-context*)))

(export-definition
  (define (declare? form)
    "True if form is (CL:DECLARE ...)"
    (and (pair? form) (eq? (first form) 'cl:declare))))

(export-definition
  (define (body-declarations body)
    "Return the initial DECLARE forms in body."
    (takef body #'declare?)))
(export-definition
  (define (body-forms body)
    "Return the forms of body without the initial DECLARE forms."
    (dropf body #'declare?)))

(export-definition
  (define (function-body-declarations body)
    "Return the initial [documentation] declarations... in body
as (documentation declarations...)"
    (if (and (string? (first body)) (not (empty? (rest body))))
	(cons (first body) (body-declarations (rest body)))
	(body-declarations body))))
(export-definition
  (define (function-body-forms body)
    "Return the forms of body with the initial [documentation] declarations...
removed."
    (if (and (string? (first body)) (not (empty? (rest body))))
	(body-forms (rest body))
	(body-forms body))))

;; Special form parsers: all assume that the given expr is well-formed. 
(define (quote-expr expr) (second expr))
(define (function-name expr) (second expr))
(define (progn-forms expr) (rest expr))

(define (lambda-parameters expr) (second expr))
(define (lambda-body expr) (cddr expr))
(define (lambda-body-declarations expr) (body-declarations (lambda-body expr)))
(define (lambda-body-forms expr) (body-forms (lambda-body expr)))

(define (let-bindings expr) (second expr))
(define (let-body expr) (cddr expr))
(define (let-body-declarations expr) (body-declarations (let-body expr)))
(define (let-body-forms expr) (body-forms (let-body expr)))
(define (let-binding-name expr)
  (if (pair? expr)
      (first expr)
      expr))
(define (let-binding-value expr)
  (if (pair? expr)
      (second expr)
      nil))

(define (let*-bindings expr) (let-bindings expr))
(define (let*-body expr) (let-body expr))
(define (let*-body-declarations expr) (body-declarations (let*-body expr)))
(define (let*-body-forms expr) (body-forms (let*-body expr)))

(define (block-name expr) (second expr))
(define (block-body expr) (cddr expr))

(define (return-from-name expr) (second expr))
(define (return-from-value expr)
  (or (and (= (length expr) 3) (third expr))
      nil))

(define (flet-bindings expr) (second expr))
(define (flet-body expr) (cddr expr))
(define (flet-body-declarations expr) (body-declarations (flet-body expr)))
(define (flet-body-forms expr) (body-forms (flet-body expr)))

(define (labels-bindings expr) (second expr))
(define (labels-body expr) (cddr expr))
(define (labels-body-declarations expr) (body-declarations (labels-body expr)))
(define (labels-body-forms expr) (body-forms (labels-body expr)))

(define (function-binding-name expr) (first expr))
(define (function-binding-parameters expr) (second expr))
(define (function-binding-body expr) (cddr expr))
(define (function-binding-body-declarations expr) (function-body-declarations (function-binding-body expr)))
(define (function-binding-body-forms expr) (function-body-forms (function-binding-body expr)))

(define (macrolet-bindings expr) (second expr))
(define (macrolet-body expr) (cddr expr))
(define (macrolet-body-declarations expr) (body-declarations (macrolet-body expr)))
(define (macrolet-body-forms expr) (body-forms (macrolet-body expr)))

(define (symbol-macrolet-bindings expr) (second expr))
(define (symbol-macrolet-body expr) (cddr expr))
(define (symbol-macrolet-body-declarations expr) (body-declarations (symbol-macrolet-body expr)))
(define (symbol-macrolet-body-forms expr) (body-forms (symbol-macrolet-body expr)))

(define (eval-when-situations expr) (second expr))
(define (eval-when-forms expr) (cddr expr))

(define (setq-pairs expr)
  (define (recurse expr pairs)
    (if (empty? expr)
	pairs
	(recurse (cddr expr) (cons (list (first expr) (second expr)) pairs))))
  (nreverse (recurse (rest expr) ())))

(define (if-test expr) (second expr))
(define (if-then expr) (third expr))
(define (if-else expr) (fourth expr))

(define (locally-body expr) (cdr expr))
(define (locally-body-declarations expr) (body-declarations (locally-body expr)))
(define (locally-body-forms expr) (body-forms (locally-body expr)))

(define (tagbody-tags-and-statements expr) (cdr expr))
(define (go-tag expr) (second expr))

(define (the-value-type expr) (second expr))
(define (the-form expr) (third expr))

(define (multiple-value-prog1-values-form expr) (second expr))
(define (multiple-value-prog1-forms expr) (cddr expr))

(define (multiple-value-call-function expr) (second expr))
(define (multiple-value-call-arguments expr) (cddr expr))

(define (load-time-value-form expr) (second expr))
(define (load-time-value-read-only-p expr) (third expr))

(define (catch-tag expr) (second expr))
(define (catch-forms expr) (cddr expr))

(define (throw-tag expr) (second expr))
(define (throw-result expr) (third expr))

(define (unwind-protect-protected expr) (second expr))
(define (unwind-protect-cleanup expr) (cddr expr))

(define (parse-tagbody tags-and-statements)
  "Return (untagged-statements . (tag . statements)...)."
  (define (tag? tag-or-statement)
    (or (symbol? tag-or-statement)
	(integerp tag-or-statement)))
  (define (statement? tag-or-statement)
    (not (tag? tag-or-statement)))

  (define untagged-statements (takef tags-and-statements statement?))
  (define tagged-statements (dropf tags-and-statements statement?))

  (define (tagged-forms-iter tags-and-statements tagged-forms)
    (define (parse-next-tagged-form)
      (define tag (first tags-and-statements))
      (define statements-and-tagged-statements (rest tags-and-statements))
      (define statements (takef statements-and-tagged-statements statement?))
      (define rest-tags-and-statements (dropf statements-and-tagged-statements statement?))

      (tagged-forms-iter rest-tags-and-statements (cons (cons tag statements) tagged-forms)))
    
    (cond
      ((empty? tags-and-statements) tagged-forms)
      (t (parse-next-tagged-form))))
  
  (define tagged-forms
    (nreverse (tagged-forms-iter tagged-statements ())))

  (cons untagged-statements tagged-forms))

(uninstall-syntax!)

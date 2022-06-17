(in-package #:schemeish.internals)

(install-syntax!)

(defvar *definition-name-field-table* (make-hash-table :weakness :key)
  "A hash table from function -> definition-name-field.")
(defun register-definition-name-field (function name-field)
  (assert (functionp function))
  (setf (gethash function *definition-name-field-table*) name-field))

(export
 (defun registered-definition-name-field (function)
   "Retrieve the DEFINE name-field used to define function.
Returns nil if not defined using DEFINE."
   (gethash function *definition-name-field-table*)))

(defun definition-name-field (definition)
  "Returns name-field of (define name-field ...)"
  (second definition))
(defun definition-function-body (definition)
  "Returns function-body of (define name-field function-body...)"
  (cddr definition))
(defun definition-value (definition)
  "Returns value of (define name-field value)"
  (third definition))
(defun definition-name-field->name (name-field)
  "Parses the definition name from the (define name-field ....)."
  (if (symbolp name-field)
      name-field
      (definition-name-field->name (first name-field))))

(defun documentation-string-for-definition (scm-parameters definition-name-field guard-clauses documentation-string)
  "Adds documentation for the definition from the scm-parameters, name-field, and guard-clauses."
  (let ((name (definition-name-field->name definition-name-field)))
    (concatenate 'string
		 documentation-string
		 (format nil "~&~%~%")
		 (documentation-string-for-scm-parameters scm-parameters)
		 (format nil "~&~%Definition Form: ~S" definition-name-field)
		 (format nil "~&~%~%")
		 (guard-clauses-documentation-string guard-clauses name))))

(defun register-definition-function-metadata (function scm-parameters guard-clauses documentation-source definition-name-field)
  "Registers metadata associated with function defined using (define (...) ...)."
  (let ((documentation-string (if documentation-source
				  (documentation-string documentation-source)
				  "")))
    (setf (documentation function t)
	  (documentation-string-for-definition scm-parameters definition-name-field guard-clauses documentation-string)))
  (when guard-clauses (register-guard-clauses function guard-clauses))
  (when definition-name-field (register-definition-name-field function definition-name-field))
  (when documentation-source (set-object-documentation-source! function documentation-source))
  function)

(defun definition-lambda-form (definition-name-field definition-guard-clauses scm-parameters function-body)
  "Return a form that creates a lambda, with metadata from a definition form.
Registers the definition metadata for lambda, using definition-name-field and definition-guard-clauses 
instead of the parsed metadata.
See PARSE-FUNCTION, REGISTER-DEFINITION-FUNCTION-METADATA, PARSED-FUNCTION->LAMBDA-FORM."
  (multiple-value-bind (ordinary-lambda-list ignorable-parameters body documentation-source guard-clauses declarations)
      (parse-function scm-parameters function-body)
    `(register-definition-function-metadata
      ,(parsed-function->lambda-form ordinary-lambda-list ignorable-parameters body guard-clauses declarations)
      ',scm-parameters
      ',definition-guard-clauses
      ,documentation-source
      ',definition-name-field)))

(defun transform-lexical-body-define-pair (definition)
  "Transforms (define (...) . function-body) for lisp-1 style lexical-body."
  (let* ((definition-name-field (definition-name-field definition))
	 (function-body (definition-function-body definition))
	 (guard-clauses (multiple-value-bind (lexical-body documentation-source-form guard-clauses declarations)
			    (parse-metadata-from-function-body function-body)
			  (declare (ignore lexical-body documentation-source-form declarations))
			  guard-clauses)))
    (let recurse ((name-field definition-name-field)
		  (body function-body))
      (let ((name (first name-field))
	    (parameters (rest name-field)))
	(cond
	  ((consp name)
	   ;; Iteration: Closure definition.
	   (recurse name (list (definition-lambda-form definition-name-field guard-clauses parameters body))))
	  (t
	   ;; Base: Function definiton.
	   (values (list name) `(setq ,name ,(definition-lambda-form definition-name-field guard-clauses parameters body)))))))))

(defun transform-lexical-body2-define-pair (definition)
  "Transforms (define (...) . function-body) for lisp-2 style lexical-body."
  (let* ((definition-name-field (definition-name-field definition))
	 (function-body (definition-function-body definition)))
    (multiple-value-bind (body documentation-source guard-clauses declarations)
	(parse-metadata-from-function-body function-body)
      (let recurse ((name-field definition-name-field)
		    (body (append declarations (enforce-guard-clauses-forms guard-clauses ()) `((lexically ,@body)))))
	(let ((name (first name-field))
	      (parameters (rest name-field)))
	  (cond
	    ((consp name)
	     ;; Iteration: Closure definition.
	     (recurse name (list (definition-lambda-form definition-name-field guard-clauses parameters body))))
	    (t
	     ;; Base: Function definiton.
	     (values (list name)
		     `(setq ,name (register-definition-function-metadata
				   (function ,name)
				   ',parameters
				   ',guard-clauses
				   ,documentation-source
				   ',definition-name-field))
		     (multiple-value-bind (ordinary-lambda-list ignorable-parameters) (scm-parameters->ordinary-lambda-list parameters)
		       (list `(,name ,ordinary-lambda-list ,@(declare-ignorable-forms ignorable-parameters)
				     ,@body)))))))))))

(defun transform-lexical-body-define-symbol (definition)
  "Transforms (define symbol [documentation] value) for lisp-1 style lexical-body."
  (multiple-value-bind (body documentation-source) (parse-documentation-source (definition-function-body definition))
    (let ((name (definition-name-field definition))
	  (value (first body)))
      (values (list name)
	      `(setq ,name (set-object-documentation-from-documentation-source! ,value ,documentation-source))))))

(export
 (defun transform-lexical-body-define-symbol-or-pair (definition)
   "Transforms (define name-field ...) for lisp-1 style lexical-body.
If name-field is a symbol the expected form is (define symbol [documentation-source] value).
  A let binding is created for symbol, and value is assigned to it.
  The documentation-source and documentation string for value is set.
If name-field is a pair, the expected form is (define name-field function-body...)
  If name-field is a pair: ((...) . scm-parameters)
    A closure is created with the given scm-parameters, and define is recursively applied.
    E.g. (define (((nested x) y) z) function-body...) => 
         (define (nested x) (lambda (y) (lambda (z) function-body...)))

  If name-field is a pair: (symbol . scm-parameters)
    A lambda is created with the given scm-parameters and function-body, expanded using PARSE-FUNCTION."
   (let ((name-field (definition-name-field definition)))
     (cond
       ((symbolp name-field) (transform-lexical-body-define-symbol definition))
       (t (transform-lexical-body-define-pair definition))))))

(export
 (defun transform-lexical-body2-define-symbol-or-pair (definition)
   "Transforms (define name-field ...) for lisp-2 style lexical-body.
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
    A let binding is created for symbol, with #'symbol assigned to it.

See also: LEXICALLY, PARSE-FUNCTION."
   (let ((name-field (definition-name-field definition)))
     (cond
       ((symbolp name-field)
	(multiple-value-bind (names set-form) (transform-lexical-body-define-symbol definition)
	  (values names set-form (mapcar #'default-labels-binding names))))
       (t (transform-lexical-body2-define-pair definition))))))

(export
 (defun transform-lexical-body-define-values (definition)
   "Transforms (define-values name-or-names values-form) for lisp-1 style lexical-body.
If name-or-names is a symbol:
  A let binding is created, and the (multiple-values-list values-form) is assigned to it.
If name-or-names is a list of symbols:
  A let binding is created for each symbol, and they are bound using multiple-value-setq.
See also: LEXICALLY."
   (let ((name-field (definition-name-field definition))
	 (values-form (definition-value definition)))
     (flet ((ignore? (symbol) (string= (symbol-name symbol) "_")))
       (cond
	 ((symbolp name-field) (values (list name-field) `(setq ,name-field (multiple-value-list ,values-form))))
	 (t
	  (let iter ((given-names name-field)
		     (names ())
		     (vars ())
		     (ignored-names ()))
	    (cond
	      ((null given-names)
	       (values (nreverse names)
		       `(cl:let ,ignored-names
			  (multiple-value-setq ,(nreverse vars) ,values-form))))
	      (t (let ((name (first given-names))
		       (rest-names (rest given-names)))
		   (if (ignore? name)
		       (let ((var (unique-symbol 'ignore)))
			 (iter rest-names names (cons var vars) (cons var ignored-names)))
		       (iter rest-names (cons name names) (cons name vars) ignored-names))))))))))))

(export
 (defun transform-lexical-body-define-destructuring (definition)
   "Transforms (define-destructuring destructuring-lambda-list expression) for lisp-1 style lexical-body.
Uses DESTRUCTURING-BIND to destructure expression and creates bindings for each name in destructuring-lambda-list."
   (let* ((lambda-list (definition-name-field definition))
	  (names (destructuring-lambda-list-parameter-names lambda-list))
	  (expression (definition-value definition)))
     (transform-lexical-body-define-values `(define-values ,names (destructuring-bind ,lambda-list ,expression (values ,@names)))))))
(export
 (defun transform-lexical-body-define-unique-symbols (definition)
   (let ((names (rest definition)))
     (values names `(progn ,@(mapcar (cl:lambda (name)
				       `(setq ,name (unique-symbol ',name)))
				     names))))))

;; Register define and define-values
;; Lisp-1 style Lexical body definitions
(register-lexical-body-definition 'define #'transform-lexical-body-define-symbol-or-pair)
(register-lexical-body-definition 'define-values #'transform-lexical-body-define-values)
(register-lexical-body-definition 'def-values #'transform-lexical-body-define-values)
(register-lexical-body-definition 'define-destructuring #'transform-lexical-body-define-destructuring)
(register-lexical-body-definition 'def-destructuring #'transform-lexical-body-define-destructuring)
(register-lexical-body-definition 'define-unique-symbols #'transform-lexical-body-define-unique-symbols)
(register-lexical-body-definition 'def-unique-symbols #'transform-lexical-body-define-unique-symbols)
(register-lexical-body-definition 'def #'transform-lexical-body-define-symbol-or-pair)

;; Lisp-2 style lexical body definitions
(register-lexical-body2-definition 'define #'transform-lexical-body2-define-symbol-or-pair)
(register-lexical-body2-definition 'def #'transform-lexical-body2-define-symbol-or-pair)
(export '(def define-values def-values define-destructuring def-destructuring define-unique-symbols def-unique-symbols))

#;
(assert (equal (lexically
		 (define-destructuring (&whole whole r1 r2
					       &optional (o1 3 o1-provided?)
					       &body body)
		     '(r1 r2 o1 :k1 k1))
		 (list whole o1-provided?))
	       '((R1 R2 O1 :K1 K1) T)))

(uninstall-syntax!)

(in-package #:schemeish.define)

(install-syntax!)

(import '(schemeish.syntax:documentation-tag
	  schemeish.syntax:documentation-tag?
	  schemeish.syntax:documentation-tag-form
	  schemeish.syntax:guard-tag
	  schemeish.syntax:guard-tag?
	  schemeish.syntax:guard-tag-clauses))

(unexport 'expand-function-body)

(export 'expand-defines-in-lexical-body)

(for-macros
  (defun flatten (v)
    "Flatten tree v into a list."
    (cond
      ((consp v) (append (flatten (car v))
			 (flatten (cdr v))))
      ((null v) ())
      (t (list v)))))

(assert (equal (flatten '((a) b (c (d) . e) ()))
	       '(a b c d e)))

(for-macros
  (defun define? (form)
    "True if form is a (define ...) form."
    (and (consp form) (eq (first form) 'schemeish.define:define))))
(assert (define? '(define name var)))

(for-macros
  (defun define-name-field (form)
    "Return the name-field of a (define name-field  body...)."
    (second form))
  (defun define-name (form)
    "Return the name of a (define name ...) or (define (name . args) ...) 
or (define (((name . args) . args) . args) ...) style form."
    (first (flatten (define-name-field form))))

  (defun define-value-field (form)
    "Return the value of a (define name value) style form."
    (third form))
  (defun define-body (form)
    "Return the body of a (define name-field body...) style form."
    (cddr form)))

(assert (equal (define-name '(define (((nested-fn) x) y) z))
	       'nested-fn))

(for-macros (defun append* (lists)
	      "Append lists."
	      (apply #'append lists)))
(assert (equal
	 (append* '((1 2 3) (4 5 6)))
	 '(1 2 3 4 5 6)))

(for-macros (defun append-map (proc list)
	      (append* (mapcar proc list))))

(for-macros (defun declaration? (form)
	      "True if form is a (declare ...) special-form."
	      (and (consp form) (eq 'cl:declare (first form)))))
(for-macros (defun declare? (form)
	      "True if form is a (declare ...) special-form."
	      (and (consp form) (eq 'cl:declare (first form)))))
(assert (declaration? '(declare (ignore x))))

(for-macros (defun takef (list predicate)
	      "Takes initial elements of list that satisfy pred."
	      (let rec ((list list)
			(result '()))
		(if (or (null list) (not [predicate (first list)]))
		    (nreverse result)
		    (rec
		     (rest list)
		     (cons (first list) result))))))

(assert (equal (takef '(2 4 5 8) 'evenp)
	       '(2 4)))

(for-macros (defun dropf (list predicate)
	      "Drops initial elements of list that don't satisfy pred."
	      (let rec ((list list))
		(if (or (null list) (not [predicate (first list)]))
		    list
		    (rec (rest list))))))


(assert (equal (dropf '(2 4 5 8) 'evenp)
	       '(5 8)))

(for-macros (defun splitf-at (list predicate)
	      "Returns (list (takef list predicate) (dropf list predicate))"
	      (list (takef list predicate) (dropf list predicate))))


(for-macros (defun define-function? (form)
	      "True if form is (define (name . args) ...) or (define (((name . args) . args) . args) ...) style form."
	      (and (define? form)
		   (consp (define-name-field form)))))


(for-macros (defun ignorable-declaration (ignorable-args)
	      "Returns a declaration which marks ignorable-args as ignorable."
	      `(declare (ignorable ,@ignorable-args))))

(assert (equal (ignorable-declaration '(a b c))
	       '(DECLARE (IGNORABLE A B C))))
(assert (equal (ignorable-declaration '())
	       '(declare (ignorable))))

(for-macros
  (defparameter *define-form-hash-table* (make-hash-table)
    "A hash table from function -> define-form.")

  (defun register-define-form (function form)
    "Register the define-form in the *define-form-hash-table*."
    (assert (functionp function))
    (assert (define? form))
    (setf (gethash function *define-form-hash-table*) form))

  (defun unregister-define-form (function)
    "Removes any registered define-form associated with symbol from the
*DEFINE-FORM-HASH-TABLE*"
    (remhash function *define-form-hash-table*))

  (defun define-form (function)
    "Retrieve the DEFINE form used to define function.
Returns nil if not defined using DEFINE. Does not include any outer
lexical environment, and so it may not always be used to redefine the function."
    (gethash function *define-form-hash-table*)))

(defmacro undefine (&rest symbols)
  "Undefines globally defined functions using fmakunbound."
  `(progn
     ,@(mapcar (lambda (name)
		 `(progn
		    (fmakunbound ',name)
		    (unregister-define-form #',name)))
	       symbols)))

(for-macros
  (defvar *guard-clauses-hash-table* (make-hash-table :weakness :key)
    "A table from function to a list of guard-clauses guarding that function.")

  (defun register-guard-clauses (function guard-clauses)
    "Associates function with guard-clauses in the *guard-clauses-hash-table*"
    (assert (functionp function))
    (assert (listp guard-clauses))
    (setf (gethash function *guard-clauses-hash-table*) guard-clauses)
    function)

  (defun guard-clauses (function)
    "Retrieves the guard-clauses associated with function, or NIL if not present."
    (assert (functionp function))
    (gethash function *guard-clauses-hash-table*)))

(for-macros
  (defgeneric documentation-string (documentation)
    (:documentation "Returns a documentation string given the provided documentation object."))
  (defmethod documentation-string ((documentation string)) documentation)
  (defun documentation-source? (object)
    "An object is a documentation-source if it has a method implemented for documentation-string."
    (compute-applicable-methods #'documentation-string (list object))))

(for-macros
  (defvar *variable-documentation-hash-table* (make-hash-table))
  (defvar *type-documentation-hash-table* (make-hash-table))
  (defvar *compiler-macro-documentation-hash-table* (make-hash-table))
  (defvar *setf-documentation-hash-table* (make-hash-table))
  (defvar *object-documentation-hash-table* (make-hash-table :weakness :key))

  (defun check-symbol (symbol)
    (unless (symbolp symbol)
      (error "Symbol expected to be a symbol type, but got: ~S" (type-of symbol))))
  (defun check-name (name)
    (unless (or (symbolp name) (and (listp name)
				    (= 2 (length name))
				    (eq 'cl:setf (first name))
				    (symbolp (second name))))
      (error "Name expected to be a symbol or a list (setf symbol) but got: ~S" name)))
  (defun check-object (object)
    (typecase object
      (function)
      (method-combination)
      (standard-method)
      (package)
      (t (error "Object expected to be of type: function, method-combination, standard-method, or package. But got ~S" (type-of object)))))
  
  (defun variable-documentation-source (symbol)
    "Returns the documentation source associated with the constant or dynamic variable named symbol."
    (check-symbol symbol)
    (gethash symbol *variable-documentation-hash-table* nil))
  (defun type-documentation-source (symbol)
    "Returns the documentation source associated with the type named by symbol."
    (check-symbol symbol)
    (gethash symbol *type-documentation-hash-table* nil))
  (defun compiler-macro-documentation-source (name)
    "Returns the documentation source associated with the compiler-macro named by NAME."
    (check-name name)
    (gethash name *compiler-macro-documentation-hash-table* nil))
  (defun setf-documentation-source (symbol)
    "Returns the documentation source associated with the setf-expansion named by symbol."
    (check-symbol symbol)
    (gethash symbol *setf-documentation-hash-table* nil))
  (defun object-documentation-source (object)
    "Returns the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
    (check-object object)
    (gethash object *object-documentation-hash-table* nil))

  (defun set-variable-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the constant or dynamic variable named symbol."
    (check-symbol symbol)
    (setf (gethash symbol *variable-documentation-hash-table*) documentation-source))
  (defun set-type-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the type named by symbol."
    (check-symbol symbol)
    (setf (gethash symbol *type-documentation-hash-table*) documentation-source))
  (defun set-compiler-macro-documentation-source! (name documentation-source)
    "Updates the documentation source associated with the compiler-macro named by NAME."
    (check-name name)
    (setf (gethash name *compiler-macro-documentation-hash-table*) documentation-source))
  (defun set-setf-documentation-source! (symbol documentation-source)
    "Updates the documentation source associated with the setf-expansion named by symbol."
    (check-symbol symbol)
    (setf (gethash symbol *setf-documentation-hash-table*) documentation-source))
  (defun set-object-documentation-source! (object documentation-source)
    "Updates the documentation source associated with the given object.
Object may be a function, method-combination, standard-method, or package."
    (check-object object)
    (setf (gethash object *object-documentation-hash-table*) documentation-source)))

(for-macros
  (defun documentation-string-for-define-function (name function documentation-string)
    (assert (stringp documentation-string))
    (concatenate 'string
		 documentation-string
		 (let ((form (define-form function)))
		   (if form
		       (format nil "~&~%~%Form: ~S" (define-name-field form))
		       ""))
		 (let ((guards (guard-clauses function)))
		   (if guards
		       (format nil "~&~%~%~S has the following guard clauses:~%~S" name guards)
		       "")))))

(for-macros
  (defun documentation-source-form (string-or-documentation-tag)
    (assert (or (stringp string-or-documentation-tag)
		(documentation-tag? string-or-documentation-tag)))
    (if (stringp string-or-documentation-tag)
	string-or-documentation-tag
	(documentation-tag-form string-or-documentation-tag)))
  (defun documentation-string-form (name documentation)
    `(documentation-string-for-define-function ',name #',name (documentation-string ,(documentation-source-form documentation))))

  (defun set-function-documentation-form-for-symbol-and-function (name documentation)
    "Form that sets the documentation of 'name and #'name."
    `(progn
       (setf (documentation ',name 'function) ,(documentation-string-form name documentation)
	     (documentation #',name t) (documentation ',name 'function))
       (set-object-documentation-source! #',name ,(documentation-source-form documentation))))
  (defun set-function-documentation-form-for-symbol (name documentation)
    "Form that sets the documentation of 'name."
    `(progn
       (setf (documentation ',name 'function) ,(documentation-string-form name documentation))
       (set-object-documentation-source! #',name ,(documentation-source-form documentation))))
  (defun set-function-documentation-form-for-function (name documentation)
    "Form that sets the documentation of #'name."
    `(progn
       (setf (documentation #',name t) ,(documentation-string-form name documentation))
       (set-object-documentation-source! #',name ,(documentation-source-form documentation)))))

(for-macros
  (defun top-level-define-name-form (name documentation body)
    "Returns a form which sets the function definition of name to the first value of body."
    (unless (= 1 (length body))
      (error "Invalid body for (DEFINE ~S function). Expected a single value, but got ~S" name body))
    (let ((function-value (unique-symbol 'fn)))
      `(for-macros
	 (let ((,function-value ,(first body)))
	   (unless (functionp ,function-value)
	     (error "DEFINE: Expected a function value as the second argument to (DEFINE ~S [documentation] function-value), but got ~S."
		    ',name ,function-value))
	   (register-define-form #',name '(define ,name ,@body))
	   ;; Set the function definition
	   (setf (fdefinition ',name) ,function-value)
	   ,@(when documentation (list (set-function-documentation-form-for-symbol name documentation)))
	   ;; Return the name.
	   ',name)))))

(for-macros
  (defun parse-documentation-from-body (body)
    "Return (values (or documentation nil) rest-body)."
    (cond
      ((null body) (values nil ()))
      (t
       (let ((form (first body)))
	 (cond
	   ;; If the form is a documentation-tag, remove it from body and return it.
	   ((documentation-tag? form) (values form (rest body)))
	   ;; There must be at least 2 forms for a string to be documentation.
	   ((and (not (null (rest body)))
		 (stringp form))
	    (values form (rest body)))
	   ;; Otherwise no documentation was found.
	   (t (values nil body))))))))


(for-macros
  (defun parse-guard-clauses-from-body (body)
    "Return (values (or guard-clauses nil) rest-body)."
    (cond
      ((null body) (values nil ()))
      (t
       (let ((form (first body)))
	 (cond
	   ;; If the form is a guard-tag, remove it from body and return its clauses.
	   ((guard-tag? form) (values (guard-tag-clauses form) (rest body)))
	   ;; Otherwise no guard-tag was found.
	   (t (values nil body))))))))

(for-macros
  (defun parse-initial (list keep?)
    "Return (values initial-values remaining-list) where each of initial-values satisfies keep?
and the first element of remaining-list does not satisfy keep?"
    (let recurse ((list list)
		  (result ()))
      (cond
	((null list) (values (nreverse result) list))
	(t (let ((x (first list)))
	     (cond
	       ([keep? x] (recurse (rest list) (cons x result)))
	       (t (values (nreverse result) list)))))))))

(for-macros
  (defun parse-initial-declares-from-body (body)
    "Return (values declares rest-body)"
    (parse-initial body #'declare?)))

(for-macros
  (defun parse-initial-defines-from-body (body)
    "Return (values defines rest-body)"
    (parse-initial body #'define?)))

(for-macros
  (defun guard-clauses-form (guard-clauses)
    "Return a form that processes guard-clauses, causing an error if any clause fails."
    (cons 'progn
	  (mapcar (cl:lambda (guard-clause)
		    `(unless ,guard-clause
		       (error "Failed function guard-clause: ~S" ',guard-clause)))
		  guard-clauses))))

(guard-clauses-form '((stringp text)
		      (inline? value)
		      (consp list)))

(for-macros (defun lexical-form? (form)
	      "True if form creates a new lexical-scope."
	      (and (consp form)
		   (member (first form) '(let cl:let let* flet labels macrolet)))))
(for-macros (defun map-lexical-form-body (form proc)
	      "Returns a lexical-form form with proc applied to its body."
	      (cond
		((and (eq 'let (first form))
		      (not (listp (second form))))
		 ;; Named-let form
		 (list* 'let (second form) (third form) [proc (cdddr form)]))
		(t
		 ;; Normal LET, LET*, LABELS, FLET, MACROLET form
		 (list* (first form) (second form) [proc (cddr form)])))))

(for-macros
  (defun lexical-variables (defines)
    (mapcar #'define-name defines))
  (defun ignorable-lexical-variable-functions-declaration (lexical-variables)
    `(declare (ignorable ,@(mapcar (cl:lambda (variable) `(function ,variable)) lexical-variables))))
  (defun ignorable-lexical-variables-declaration (lexical-variables)
    `(declare (ignorable ,@lexical-variables))))

(for-macros
  (defun body-with-ignorable-args (ignorable-args body)
    `(,@(when ignorable-args (list (ignorable-declaration ignorable-args)))
      ,@body)))

(for-macros
  (defun parse-define-function (arg-list body)
    "Return (values lambda-list documentation guard-clauses expanded-body)"
    (multiple-value-bind (documentation body) (parse-documentation-from-body body)
      (multiple-value-bind (declares body) (parse-initial-declares-from-body body)
	(multiple-value-bind (guard-clauses body) (parse-guard-clauses-from-body body)
	  (multiple-value-bind (lambda-list ignorable-args) (arg-list->lambda-list arg-list)
	    (let ((expanded-body (body-with-ignorable-args
				  ignorable-args
				  `(,@declares
				    ,@(when guard-clauses (list (guard-clauses-form guard-clauses)))
				    ,@(expand-defines-in-lexical-body body)))))
	      (values lambda-list documentation guard-clauses expanded-body))))))))

(for-macros
  (defun parse-define-closure (name-field documentation guard-clauses body)
    (let ((arg-list (rest name-field))
	  (name (first name-field)))
      (multiple-value-bind (lambda-list ignorable-args) (arg-list->lambda-list arg-list)
	(cond
	  ((symbolp name)
	   ;; Base case: we are at the top level function definition
	   (values name lambda-list documentation guard-clauses
		   (body-with-ignorable-args ignorable-args body)))
	  (t
	   ;; Unravel the next closure.
	   (parse-define-closure name documentation guard-clauses
				 `((cl:lambda ,lambda-list
				     ,@(body-with-ignorable-args ignorable-args body))))))))))

(for-macros
  (defun parse-define-function-or-closure (name-field body)
    "Return (values name lambda-list documentation expanded-body)"
    (multiple-value-bind (lambda-list documentation guard-clauses body)
	(parse-define-function (rest name-field) body)
      (cond
	((symbolp (first name-field))
	 (values (first name-field) lambda-list documentation guard-clauses body))
	(t
	 (parse-define-closure (first name-field)
			       documentation
			       guard-clauses
			       `((cl:lambda ,lambda-list ,@body))))))))

(for-macros
  (defstruct local-function-definition
    "Represents a local function definition."
    name form guard-clauses documentation)

  (defun local-function-form (name lambda-list body)
    "Form that defines a local function."
    `(,name ,lambda-list ,@body))
  (defun local-function-definitions (defines)
    "A list of local-function-definitions to define local function given defines."
    (mapcar #'define->local-function-definition defines))

  (defun define->local-function-definition (define)
    "Convert a DEFINE form to a LOCAL-FUNCTION-DEFINITION structure."
    (let ((name (define-name define)))
      (cond
	((define-function? define)
	 ;; IF this is a function definition,
	 ;; parse the define form, and recursively expand the function body.
	 (multiple-value-bind (name lambda-list documentation guard-clauses expanded-body)
	     (parse-define-function-or-closure (define-name-field define) (define-body define))
	   (make-local-function-definition :name name
					   :form (local-function-form name lambda-list expanded-body)
					   :guard-clauses guard-clauses
					   :documentation documentation)))
	(t
	 ;; If we are defining a variable,
	 ;; create a local function that takes an arbitrary number of arguments
	 ;; and applies the variable to those arguments.
	 (make-local-function-definition
	  :name name
	  :form (let ((args (unique-symbol 'args)))
		  (local-function-form name `(&rest ,args) `((apply ,name ,args))))
	  :guard-clauses ()
	  :documentation (format nil "Applies the lexical variable ~S to the provided arguments." name)))))))

(for-macros
  (defun lexical-variable-assignment-form (define)
    (let ((name (define-name define)))
      (cond
	;; If the definition is a function,
	;; set the variable to the function value.
	((define-function? define)`(,name #',name))
	;; If the definition is a variable,
	;; set the variable to the definition's value.
	(t `(,name ,(define-value-field define))))))
  (defun lexical-variable-assignments-form (defines)
    `(setq ,@(append* (mapcar #'lexical-variable-assignment-form defines)))))

(for-macros
  (defun set-local-function-documentation-forms (local-function)
    "Return a body of forms to set the local-functions documentation."
    (let ((name (local-function-definition-name local-function))
	  (documentation (local-function-definition-documentation local-function)))
      (when documentation
	(list (set-function-documentation-form-for-function name documentation))))))

(for-macros
  (defun register-guard-clauses-forms (name guard-clauses)
    "Return a body of forms to register the guard-clauses with #'name."
    (when guard-clauses
      (list `(register-guard-clauses #',name ',guard-clauses))))
  
  (defun register-local-function-guard-clauses-forms (local-function)
    "Return a body of forms to register the guard-clauses with the local-function."
    (let ((name (local-function-definition-name local-function))
	  (guard-clauses (local-function-definition-guard-clauses local-function)))
      (register-guard-clauses-forms name guard-clauses))))

(for-macros
  (defun expand-defines-in-body (defines declares body)
    "Returns an expanded body."
    (dolist (define defines)
      (unless (or (and (symbolp (define-name-field define))
		       (= 3 (length define)))
		  (consp (define-name-field define)))
	(error "Malformed DEFINE ~S. Expected (DEFINE symbol-name value) or (DEFINE (name . args) ...)" define)))
    (cond
      ((null defines) `(,@declares ,@body))
      (t (let ((lexical-variables (lexical-variables defines))
	       (local-functions (local-function-definitions defines)))
	   `(
	     ;; Create lexical bindings for all defines.
	     (let ,lexical-variables
	       ;; Allow the user to ignore any lexical variable binding
	       ,(ignorable-lexical-variables-declaration lexical-variables)
	       ;; Create mutually recursive function definitions for all defines
	       (labels ,(mapcar #'local-function-definition-form local-functions)
		 ;; Allow the user to ignore any function bindings
		 ,(ignorable-lexical-variable-functions-declaration lexical-variables)
		 ;; Insert declares provided by the user.
		 ,@declares
		 ;; Assign values to all lexical variables
		 ,(lexical-variable-assignments-form defines)
		 ;; Register define-forms for defines.
		 ,@(mapcar (lambda (define-form)
			     `(register-define-form #',(define-name define-form) ',define-form))
			   defines)
		 ;; Register guard-clauses
		 ,@(append-map #'register-local-function-guard-clauses-forms local-functions)
		 ;; Register documentation for local functions
		 ,@(append-map #'set-local-function-documentation-forms local-functions)
		 ,@body))))))))


(for-macros
  (defun expand-defines-in-lexical-body (body)
    "Returns an expanded body."
    (multiple-value-bind (defines body) (parse-initial-defines-from-body body)
      (multiple-value-bind (declares body) (parse-initial-declares-from-body body)
	(when (null body)
	  (error "DEFINE: Empty body"))
	(dolist (form body)
	  (cond
	    ((define? form) (error "DEFINE definitions mixed with code."))
	    ((declare? form) (error "DECLARE declarations mixed with code."))
	    ((guard-tag? form) (error "GUARD clauses mixed with code."))
	    ((documentation-tag? form) (error "DOCUMENTATION mixed with code."))))
	(expand-defines-in-body
	 defines declares
	 (mapcar
	  (cl:lambda (form)
	    (if (lexical-form? form)
		(map-lexical-form-body form #'expand-defines-in-lexical-body)
		form))
	  body))))))

(for-macros
  (defun top-level-define-form (name-field body)
    "Returns a top-level form that appropriately defines name-field"
    (cond
      ((symbolp name-field)
       ;; (define symbol {documentation} function)
       (multiple-value-bind (documentation body) (parse-documentation-from-body body)
	 (top-level-define-name-form name-field documentation body)))
      ((consp name-field)
       ;; (define (symbol . args) . function-body) OR
       ;; (define (((symbol . args) . args) . args) . function-body)
       (multiple-value-bind (name lambda-list documentation guard-clauses expanded-body)
	   (parse-define-function-or-closure name-field body)
	 `(for-macros
	    (defun ,name ,lambda-list ,@expanded-body)
	    (register-define-form #',name '(define ,name-field ,@(remove-if (cl:lambda (form) (or (documentation-tag? form) (guard-tag? form))) body)))
	    ,@(when guard-clauses (register-guard-clauses-forms name guard-clauses))
	    ,@(when documentation (list (set-function-documentation-form-for-symbol-and-function name documentation)))
	    ',name)))
      (t (error "DEFINE: Expected one of symbol, function name (symbol args...), or closure name e.g. (((symbol args...) args...) args...) for the name field, but got ~S" name-field)))))


(defmacro define (name-field &body body)
  "DEFINE has two behaviours depending on whether it is being compiled within a lexical-body.

   If un-nested DEFINE can be used to define functions. There are three cases for defining functions:

   DEFINE can define names for function (setting the fdefinition of name to the function).
   (define name [documentation] #'function)
   
   DEFINE can define functions with scheme-inspired lambda lists and a function-body.
   (define (name . lambda-list) . function-body)

   DEFINE can define nested closures similar to how functions are defined. 
   (define (((name . lambda-list0) . lambda-list1) . lambda-list2) . function-body)
      This would define a function similar to:
         (defun name lambda-list0
            (lambda lambda-list1
               (lambda lambda-list2
                  . body)))

   A lambda-list can be (in this documentation, {} denotes optional values, ... denotes 0 or more values).
   (positional-arguments... optional-arguments...)
   (positional-arguments... keyword-arguments...)
   (positional-arguments... . rest-argument)

   A positional arugment is a symbol.
   An optional argument is either:  (symbol) OR (symbol default-value)
   A keyword argument is a keyword:  :name (:name default-value)
     If the keyword argument :NAME is used, the symbol NAME will be bound in the function body.
   A rest-argument is a symbol.

   Positional, optional, and rest-arguments may be autmomatically declared as ignorable by prefixing with an _.
     If a symbol named _ is used without any suffix, a unique symbol is generated for that argument, and it is declared ignorable. 

   A function-body is structured as follows:
     ({documentation} declare-forms... {guard} . lexical-body)

   Documentation may either be a string or a DOCUMENTATION-TAG
   Declare-forms are a list of Common Lisp DECLARE forms.
   A guard is a GUARD-TAG containing a list of guard-clauses.
    If provided, each guard-clause will be evaluated when the function is called.
    If any guard-clause evaluates to false, an error is signaled.
  
   A lexical-body is of the form:
     (define-forms... declare-forms... . body-forms)

   It is an error for any of body-forms to be a DEFINE, DECLARE, or GUARD form.

The behavior of DEFINE forms within a lexical-body is slightly modified and extended.
   Nested DEFINE forms define both lexically scoped variables and functions.
   A group of DEFINEs in the same lexical-body are mutually recursive.

   To define a non-function lexical variable the following form is used:
   (define symbol value)

   A local function is also generated for symbol, meaning that if value is a function the following works:
   (symbol arguments...)

   Similarly, if a function is defined like:
   (define (symbol . args) . body)
   A lexical variable is generated with its value set to #'symbol.

The following forms receive special treatment if they appear in the body-forms of a lexical-body: LET, LET*, LABELS, FLET, MACROLET.
  The bodies of these forms are treated as if they are lexical-bodies, and therefore may contain define-forms.
  This ONLY true if these forms appear within a lexical-body's body-forms."
  `(macrolet ((define (&whole inner-whole &body ignored)
		(declare (ignore ignored))
		(error "Improperly nested define: ~S in expansion for ~S" inner-whole ',name-field)))
     ,(top-level-define-form name-field body)))

(defmacro lambda (arg-list &body body)
  "A lambda with scheme style argument lists.
See DEFINE for more information on function-bodies and lambda-lists."
  (multiple-value-bind (lambda-list ignorable-args) (arg-list->lambda-list arg-list)
    `(cl:lambda ,lambda-list
       ,@(when ignorable-args (list (ignorable-declaration ignorable-args)))
       ,@(expand-defines-in-lexical-body body))))


(define ((((nested-foo _a) . _bs) _c) _)
  #d"documentation"
  #g((numberp _a)
     (numberp _c)
     (< _a _c)
     (listp _bs))

  (let ()
    (define x 3)
    (define y 4)
    (print (* x 3)))
  (let* ()
    (define v (list* _a _c _bs))
    v))

(define (lexical-bodies)
  (let ()
    (define x 3)
    (define y 4)
    (print (* x 3)))
  (labels ()
    (define x 2)
    (print x))
  (flet ()
    (define y 'y)
    (print y))
  (let* ()
    (define z 'z)
    (print z))
  (let recurse ()
    (define x 2)
    (print x))
  (cl:let ()
    (define x 'x)
    (print x))
  (macrolet ()
    (define z 'z)
    (print z))

  (let ()
    (print 'a)
    (let ()
      (print 'b)
      (let ()
	(define x 'x)
	(print x))))
  :ok)

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

(define (definition-with-definitions-nested-inside-let)
  (define a 1)
  (define steak :sauce)
  (let ((here :marker))
    (define (okay? var)
      (if (eq var :sauce)
	  :yeah-its-kay))
    (let* ((uh-oh :hi))
      (define (steak-sauce) steak)
      (okay? (and here uh-oh (steak-sauce))))))

(assert (eq :yeah-its-kay (definition-with-definitions-nested-inside-let)))

;;(uninstall-syntax!)


;; TODO: selectively disable debug features: REGISTER-DEFINE-FORM, GUARDs, etc.

(in-package #:schemeish.expand-define)

(for-macros
  (install-syntax!))

(defun flatten (v)
  "Flatten tree v into a list."
  (cond
    ((consp v) (append (flatten (car v))
		       (flatten (cdr v))))
    ((null v) ())
    (t (list v))))

(assert (equal (flatten '((a) b (c (d) . e) ()))
	       '(a b c d e)))

(defun define? (form) (and (consp form) (eq (first form) 'schemeish.define:define)))
(assert (define? '(define name var)))

(defun define-name (form)
  (let ((name-form (second form)))
    (first (flatten name-form))))

(assert (equal (define-name '(define (((nested-fn) x) y) z))
	       'nested-fn))

(defun append* (lists)
  "Append lists."
  (apply #'append lists))
(assert (equal
	 (append* '((1 2 3) (4 5 6)))
	 '(1 2 3 4 5 6)))

(defun declaration? (form) (and (consp form) (eq 'declare (first form))))
(assert (declaration? '(declare (ignore x))))

(defun takef (list predicate)
  "Takes initial elements of list that satisfy pred."
  (let rec ((list list)
	    (result '()))
    (if (or (null list) (not [predicate (first list)]))
	(nreverse result)
	(rec
	 (rest list)
	 (cons (first list) result)))))

(assert (equal (takef '(2 4 5 8) 'evenp)
	       '(2 4)))

(defun dropf (list predicate)
  "Drops initial elements of list that don't satisfy pred."
  (let rec ((list list))
    (if (or (null list) (not [predicate (first list)]))
	list
	(rec (rest list)))))


(assert (equal (dropf '(2 4 5 8) 'evenp)
	       '(5 8)))

(defun splitf-at (list predicate)
  "Returns (list (takef list predicate) (dropf list predicate))"
  (list (takef list predicate) (dropf list predicate)))


(defun split-function-body (body)
  "Split the function body into '(declarations defines body)"
  (let* ((doc-list (cond
		     ((stringp (first body))
		      (cond ((null (rest body)) ())
			    (t (list (first body)))))
		     (t ())))
	 (body (if doc-list (rest body) body)))
    (destructuring-bind (declarations body) (splitf-at body 'declaration?)
      (unless (null (remove-if-not 'declaration? body))
	(error "declarations intermixed with definitions or code."))
      (destructuring-bind (defines body) (splitf-at body 'define?)
	(unless body
	  (error "Empty body."))
	(unless (null (remove-if-not 'define? body))
	  (error "definitions intermixed with code."))
	(list (append doc-list declarations)
	      defines
	      body)))))

;; if the body is just a string, then it's code.
(assert (equal (split-function-body '("result"))
	       '(() () ("result"))))

;; otherwise the string is a docstring
(assert (equal (split-function-body '("doc" result))
	       '(("doc") () (result))))

(assert (equal (split-function-body '("doc" (declare) "result"))
	       '(("doc" (DECLARE)) NIL ("result"))))

(assert (equal (split-function-body '("doc" (declare) (define) result))
	       '(("doc" (DECLARE)) ((DEFINE)) (RESULT))))


(assert (null (ignore-errors (split-function-body '((declare) (define) (declare) result)))))
(assert (null (ignore-errors (split-function-body '((declare) (define) result (define))))))
(assert (null (ignore-errors (split-function-body '("doc" (declare) result (declare))))))
(assert (null (ignore-errors (split-function-body '("doc" (declare) (define))))))

(defun define-function? (form)
  (and (define? form)
       (consp (second form))))

(defun expand-define-closure-or-function (name-and-arg-list body expand-define-function)
  (let ((name (first name-and-arg-list))
	(arg-list (rest name-and-arg-list)))
    (cond
      ;; e.g. name-and-arglist is (((nested-fn x) y) z)
      ((consp name)
       (expand-define-closure-or-function
	name
	(list (expand-define-closure arg-list body))
	expand-define-function))
      ;; e.g. name-and-arglist is (fn x y z)
      ((symbolp name) [expand-define-function name arg-list body])
      (t (error "Bad thing to define: ~s" name)))))

(defun expand-define-function-for-labels (name arg-list body)
  (list* name (arg-list->lambda-list arg-list) body))

(defun named-let? (form)
  (and (listp form)
       (>= (length form) 3)
       (eq (first form) 'let)
       (symbolp (second form))
       (consp (third form))))
(defun let? (form)
  (and (listp form)
       (>= (length form) 2)
       (member (first form) '(cl:let let))
       (consp (second form))))
(defun let*? (form)
  (and (listp form)
       (>= (length form) 2)
       (eq (first form) 'cl:let*)
       (consp (second form))))

(defun make-named-let (name bindings body)
  `(let ,name ,bindings ,@body))
(defun named-let-bindings (form)
  (third form))
(defun named-let-name (form)
  (second form))
(defun named-let-body (form)
  (cdddr form))
(defun let-body (form)
  (cddr form))
(defun let-bindings (form)
  (second form))
(defun make-let (bindings body)
  `(let ,bindings ,@body))
(defun make-let* (bindings body)
  `(cl:let* ,bindings ,@body))

(defun expand-define-let-or-let* (body)
  (cond
    ((null body) body)
    (t
     (let ((form (first body)))
       (cond
	 ((named-let? form)
	  (cons (make-named-let (named-let-name form)
				(named-let-bindings form)
				(expand-function-body (named-let-body form)))
		(rest body)))
	 ((let? form)
	  (cons (make-let (let-bindings form)
			  (expand-function-body (let-body form)))
		(rest body)))
	 ((let*? form)
	  (cons (make-let* (let-bindings form)
			   (expand-function-body (let-body form)))
		(rest body)))
	 (t body))))))

(defun expand-function-body-definitions (definitions body)
  (cond
    ((null definitions) body)
    (t (let* ((function-definitions (remove-if-not 'define-function? definitions)))
	 `((let ,(mapcar 'define-name definitions)
	     (labels ,(mapcar (cl:lambda (form)
				(expand-define-closure-or-function (second form)
								   (expand-function-body (cddr form))
								   'expand-define-function-for-labels))
		       function-definitions)
	       (setq ,@(append* (mapcar (cl:lambda (form)
					  (cond ((define-function? form)
						 (let ((name (define-name form)))
						   `(,name #',name)))
						;; Variable definition.
						(t `(,(second form) ,(third form)))))
					definitions)))
	       ,@(expand-define-let-or-let* body))))))))
(defun expand-function-body (body)
  (destructuring-bind (declarations definitions body) (split-function-body body)
    `(,@declarations
      ,@(expand-function-body-definitions definitions body))))

(defun expand-define-closure (arg-list body)
  `(lambda ,arg-list
     ,@(expand-function-body body)))

(assert (equal (expand-define-closure 'args '((print args)
					      (apply '+ args)))
	       '(lambda args
		 (PRINT args)
		 (apply '+ args))))

(assert (equal (expand-define-closure '(x y z) '((print (list x y z))
						 (+ x y z)))
	       '(lambda (X Y Z)
		 (PRINT (LIST X Y Z))
		 (+ X Y Z))))

(assert (equal (expand-function-body-definitions '() '(body))
	       '(body)))

(assert (equal (expand-function-body-definitions '((define x 1) (define (f x) (+ x 1))) '(body))
	       '((LET (X F)
		   (LABELS ((F (X) (+ X 1)))
		     (SETQ X 1 F #'F)
		     BODY)))))

(assert (equal (expand-function-body-definitions '((define ((f x) y)
						     (list x y)))
						 '([(f 1) 2]))
	       '((LET (F)
		   (LABELS ((F (X)
			      (LAMBDA (Y)
				(LIST X Y))))
		     (SETQ F #'F)
		     (FUNCALL (F 1) 2))))))


(defun expand-top-level-define-function (name arg-list body)
  `(defun ,name ,(arg-list->lambda-list arg-list)
     ,@(expand-function-body body)))

(assert (equal (expand-top-level-define-function 'fn-name '(x y z) '(body))
	       '(DEFUN FN-NAME (X Y Z) BODY)))

(defun expand-top-level-define-closure-or-function (name-and-arg-list body)
  (expand-define-closure-or-function name-and-arg-list body 'expand-top-level-define-function))

(assert (equal (expand-top-level-define-closure-or-function '(((nested-fn x) y) z) '(body))
	       '(DEFUN NESTED-FN (X)
		 (lambda (Y)
		   (lambda (Z)
		     BODY)))))
(defun expand-top-level-define-parameter (name body)
  `(defparameter ,name ,@body))

(assert (equal (expand-top-level-define-parameter '*name* '(value "docs"))
	       '(DEFPARAMETER *NAME* VALUE "docs")))

(defun expand-top-level-define (name body)
  (cond
    ;; name is (name . args)
    ((consp name) (expand-top-level-define-closure-or-function name body))
    ((symbolp name) (expand-top-level-define-parameter name body))
    (t (error "Bad thing to define: ~S" name))))

(assert (equal (expand-top-level-define '*name* '(value))
	       '(DEFPARAMETER *NAME* VALUE)))

(assert (equal (expand-top-level-define '(test-inner-nested-defines)
					'("Also returns a thing"
					  (define ((inner-nested x) y)
					    (list x y))
					  inner-nested))
	       '(DEFUN TEST-INNER-NESTED-DEFINES ()
		 "Also returns a thing"
		 (LET (INNER-NESTED)
		   (LABELS ((INNER-NESTED (X)
			      (LAMBDA (Y) (LIST X Y))))
		     (SETQ INNER-NESTED #'INNER-NESTED)
		     INNER-NESTED)))))

(for-macros (uninstall-syntax!))

(cl:in-package :schemeish.continuations)

;; Idea: (WITH-CONTINUATIONS K body...)
;; Expand body into a form that takes/recieves implicit continuations
;; adds (call/cc fn) special form

(install-syntax!)

(defmacro with-continuations (final-continuation-form &body body)
  "Convert body into an equivalent expression that uses continuation-passing-style.
The values of body will be passed to the final-continuation 
via (multiple-value-call final-continuation-form body-values)"
  (error "TODO"))

(defvar *block-table* ())

(for-macros
  (defvar *cps-functions* ()))
(define (cps-function? name)
  "True if name names a function defined in CPS style."
  (member name *cps-functions*))
(defmacro with-cps-functions (names &body body)
  "Append names to *CPS-FUNCTIONS* for the duration of body."
  `(let ((*cps-functions* (append ,names *cps-functions*)))
     ,@body))

(define (expr->cps expr)
  "Return the form of a function that takes a continuation, 
and calls that continuation with the values of expr."
  (cond
    ;; Special Forms
    ((progn? expr) (progn->cps (progn-body expr)))
    ((let? expr) (let->cps (let-bindings expr) (let-body expr)))
    ((let*? expr) (let*->cps (let*-bindings expr) (let*-body expr)))
    ((block? expr) (block->cps (block-name expr) (block-body expr)))
    ((return-from? expr) (return-from->cps (return-from-name expr) (return-from-value expr)))
    ((labels? expr) (labels->cps (labels-bindings expr) (labels-body expr)))
    ((flet? expr) (flet->cps (flet-bindings expr) (flet-body expr)))
    ((function? expr) (function->cps (function-name expr)))

    ;; Untouched forms
    ((lambda? expr) (lambda->cps (lambda-parameters expr) (lambda-body expr)))
    ((quote? expr) (quote->cps expr))

    ((function-application? expr) (function-application->cps (function-application-function expr) (function-application-arguments expr)))
    ((atom? expr) (atom->cps expr))))

(define (quote? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:quote)
       (= 2 (length expr))))
(define (quote->cps expr)
  (atom->cps expr))

(define (function? expr)
  (and (pair? expr)
       (= 2 (length expr))
       (eq? (first expr) 'cl:function)))
(define (function-name expr)
  (second expr))
(define (function->cps name)
  (if (cps-function? name)
      (let ((args (unique-symbol 'args)))
	(atom->cps `(cl:lambda (&rest ,args) (apply #'name #'values ,args))))
      (atom->cps `(function ,name))))

(funcall (first (list (cl:lambda (&rest args) args))))

(define (function-application? expr)
  (and (list? expr)
       (positive? (length expr))
       (or (not (list? (first expr)))
	   (error "Can't handle non-symbol function application yet."))))
(define (function-application-function expr)
  (first expr))
(define (function-application-arguments expr)
  (rest expr))
(define (function-application->cps function arguments)
  (define (recurse arguments names n)
    (cond
      ;; Base case: No more arguments to evaluate.
      ;; Apply Function to the evaluated arguments.
      ((empty? arguments)
       (let ((names (nreverse names)))
	 (cond
	   ((cps-function? function)
	    ;; If function is a cps function, we need to pass the continuation.
	    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-~S" function))))
	      `(cl:lambda (,continuation) (,function ,continuation ,@names))))
	   (t
	    ;; If function is non-local, just call it.
	    (atom->cps `(,function ,@names))))))
      ;; Iteration: (function argument arguments...)
      (t (let ((argument (first arguments))
	       (name (unique-symbol (format nil "~A-ARGUMENT-~A-" (symbol->string function) n))))
	   ;; Evaluate the next argument and store the name of it.
	   `(funcall ,(expr->cps argument)
		     (cl:lambda (,name)
		       ,(recurse (rest arguments) (cons name names) (1+ n))))))))
  (recurse arguments () 0))

(assert (equal? (funcall (eval (function-application->cps 'values '(1 2 3))) #'list)
		'(1 2 3)))


(define (atom? expr)
  t)
(define (atom->cps atom)
  (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-ATOM"))))
    `(cl:lambda (,continuation) (multiple-value-call ,continuation ,atom))))

(assert (equal? (funcall (eval (atom->cps '(values 1 2 3))) #'list)
		'(1 2 3)))

(define (progn? expr)
  (and (pair? expr)
       (eq? (car expr) 'cl:progn)))
(define (progn-body expr)
  (rest expr))
(define (progn->cps body)
  (cond
    ;; Base case: (progn) => NIL
    ((empty? body) (atom->cps nil))
    ;; Base case: (progn FORM) => FORM
    ((empty? (rest body)) (expr->cps (first body)))
    ;; Iteration: (progn form forms...)
    (t (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-PROGN")))
	     (ignored (unique-symbol 'ignored)))
	 `(cl:lambda (,continuation)
	    ;; Evaluate first form, discarding result
	    (funcall ,(expr->cps (first body))
		     (cl:lambda (&rest ,ignored)
		       (declare (ignore ,ignored))
		       ;; Pass the continuation to the rest of the forms
		       (funcall ,(progn->cps (rest body)) ,continuation))))))))

(assert (equal? (funcall (eval (progn->cps '((values :no) (values nil) (values 1 2 3)))) #'list)
		'(1 2 3)))

(define (lambda? expr)
  (and (pair? expr)
       (eq? (car expr) 'cl:lambda)
       (or (list? (lambda-parameters expr))
	   (error "Lambda: malformed parameters in ~S" expr))
       (or (list? (lambda-body expr))
	   (error "Lambda: malformed body in ~S" expr))))
(define (lambda-parameters expr)
  (second expr))
(define (lambda-body expr)
  (cddr expr))
(define (lambda->cps parameters body)
  #;(let ((continuation (unique-symbol (format nil "CONTINUE-FROM-LAMBDA"))))
      `(cl:lambda ,(cons continuation parameters)
	 (funcall ,(progn->cps body) ,continuation)))
  `(cl:lambda ,parameters ,@body))

(assert (equal? (multiple-value-list (funcall (eval (lambda->cps '(&rest args) '((values-list args)))) 1 2 3))
		'(1 2 3)))

(define (let? expr)
  (and (pair? expr)
       (eq? (car expr) 'cl:let)
       (or (list? (let-bindings expr))
	   (error "LET: malformed bindings in ~S" expr))
       (or (for-all #'let-binding? (let-bindings expr))
	   (error "LET: malformed bindings in ~S" expr))
       (or (list? (let-body expr))
	   (error "LET: malformed bindings in ~S" expr))))
(define (let-bindings expr)
  (second expr))
(define (let-body expr)
  (cddr expr))

(define (let*? expr)
  (and (pair? expr)
       (eq? (car expr) 'cl:let*)
       (or (list? (let-bindings expr))
	   (error "LET*: malformed bindings in ~S" expr))
       (or (for-all #'let-binding? (let-bindings expr))
	   (error "LET*: malformed bindings in ~S" expr))
       (or (list? (let-body expr))
	   (error "LET*: malformed bindings in ~S" expr))))
(define (let*-bindings expr)
  (let-bindings expr))
(define (let*-body expr)
  (let-body expr))

(define (let-binding? expr)
  (or (and (pair? expr)
	   (or (= (length expr) 1)
	       (= (length expr) 2)))
      (and (not (null? expr))
	   (symbol? expr))))
(define (let-binding-name expr)
  (if (pair? expr)
      (first expr)
      expr))
(define (let-binding-value expr)
  (if (pair? expr)
      (second expr)
      nil))

(define (let->cps bindings body)
  (define binding-names (map #'let-binding-name bindings))
  (define (recurse bindings names)
    (cond
      ;; Base case: Assign bindings to evaluated values.
      ((empty? bindings)
       (let* ((continuation (unique-symbol (format nil "CONTINUE-FROM-LET")))
	      (new-bindings (map #'list binding-names (nreverse names))))
	 `(cl:lambda (,continuation)
	    ;; Use LET to assign bindings so that we automatically handle dynamic variables.
	    (cl:let ,new-bindings
	      (funcall ,(progn->cps body) ,continuation)))))
      ;; Iteration case: evaluate value of next binding, and store its name
      (t (let* ((binding (first bindings))
		(name (unique-symbol (format nil "LET-BINDING-VALUE-~S-" (let-binding-name binding))))
		(value (let-binding-value binding)))
	   ;; Evaluate the next binding's value
	   `(funcall ,(expr->cps value)
		     (cl:lambda (,name)
		       ,(recurse (rest bindings) (cons name names))))))))

  (recurse bindings ()))


(assert (equal? (funcall (eval (let->cps '((x 1)
					   y
					   (z))
					 '((print x)
					   (print y)
					   (print z)
					   (values x 2 3))))
			 #'list)
		'(1 2 3)))

(define (let*->cps bindings body)
  (define (recurse bindings)
    (cond
      ;; Base case: Evaluate body
      ((empty? bindings) (progn->cps body))
      ;; Iteration case: evaluate value of next binding, and bind it.
      (t (let* ((binding (first bindings))
		(name (let-binding-name binding))
		(value (let-binding-value binding)))
	   ;; Evaluate the next binding's value, binding it to name.
	   `(funcall ,(expr->cps value) (cl:lambda (,name) ,(recurse (rest bindings))))))))
  (recurse bindings))

(assert (equal? (funcall (eval (let*->cps '((x 1)
					    (y (1+ x))
					    (z (1+ y)))
					  '((values x y z))))
			 #'list)
		'(1 2 3)))

(define (block? expr)
  (and (pair? expr)
       (>= 2 (length expr))
       (eq? (first expr) 'cl:block)
       (symbol? (block-name expr))
       (list? (block-body expr))))
(define (block-name expr)
  (second expr))
(define (block-body expr)
  (cddr expr))
(define (block->cps name body)
  (let* ((continuation (unique-symbol (format nil "CONTINUE-FROM-BLOCK-~S-" name)))
	 (*block-table* (alist-set *block-table* name continuation)))
    `(lambda (,continuation)
       (funcall ,(progn->cps body) ,continuation))))

(define (return-from? expr)
  (and (pair? expr)
       (member (length expr) '(2 3))
       (eq? (first expr) 'cl:return-from)
       (and (symbol? (return-from-name expr)))))
(define (return-from-name expr)
  (second expr))
(define (return-from-value expr)
  (or (and (= (length expr) 3) (third expr))
      nil))
(define (return-from->cps name value)
  (let ((continuation (alist-ref *block-table* name))
	(ignored-continuation (unique-symbol 'ignored-continuation)))
    (unless continuation
      (error "RETURN-FROM: No block named ~S" name))
    `(cl:lambda (,ignored-continuation)
       (declare (ignore ,ignored-continuation))
       (multiple-value-call ,continuation ,value))))

(assert (equal? (funcall (eval (block->cps 'name '((return-from name (values 1 2 3))
						   (return-from name :nope))))
			 #'list)
		'(1 2 3)))

(define (flet? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:flet)
       (or (pair? (flet-bindings expr))
	   (error "Malformed FLET bindings: ~S" expr))
       (or (for-all #'flet-binding? (flet-bindings expr))
	   (error "Malformed FLET bindings: ~S" expr))
       (or (list? (flet-body expr))
	   (error "Malformed FLET body: ~S" expr))))
(define (flet-bindings expr)
  (second expr))
(define (flet-body expr)
  (cddr expr))
(define (flet-binding? expr)
  (and (pair? expr)
       (>= 2 (length expr))
       (and (not (null? (flet-binding-name expr)))
	    (symbol? (flet-binding-name expr)))
       (list? (flet-binding-parameters expr))
       (list? (flet-binding-body expr))))
(define (flet-binding-name expr)
  (first expr))
(define (flet-binding-parameters expr)
  (second expr))
(define (flet-binding-body expr)
  (cddr expr))
(define (flet->cps bindings body)
  ;; Each binding must be modified to take a continuation
  (define (binding->cps binding)
    (define name (flet-binding-name binding))
    (define continuation
      (unique-symbol (format nil "CONTINUE-FROM-FLET-~S-" name)))

    ;; Take continuation as the first parameter
    (define parameters (cons continuation (flet-binding-parameters binding)))
    ;; Replace the body with CPS.
    (define body
      `((funcall ,(block->cps name (flet-binding-body binding)) ,continuation)))

    (list* name parameters body))

  (define names (map #'flet-binding-name bindings))
  (define continuation (unique-symbol 'continue-from-flet))
  `(cl:lambda (,continuation)
     (cl:flet ,(map binding->cps bindings)
       (funcall
	;; Lexically bind names for local functions in body. 
	,(with-cps-functions names (progn->cps body))
	,continuation))))

(assert (equal? (funcall (eval (flet->cps '((foo (x y z) (values x y z)))
					  '((foo 1 2 3))))
			 #'list)
		'(1 2 3)))

(define (labels? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:labels)
       (or (pair? (labels-bindings expr))
	   (error "Malformed LABELS bindings: ~S" expr))
       (or (for-all #'labels-binding? (labels-bindings expr))
	   (error "Malformed LABELS bindings: ~S" expr))
       (or (list? (labels-body expr))
	   (error "Malformed LABELS body: ~S" expr))))
(define (labels-bindings expr)
  (second expr))
(define (labels-body expr)
  (cddr expr))
(define (labels-binding? expr)
  (and (pair? expr)
       (>= 2 (length expr))
       (and (not (null? (labels-binding-name expr)))
	    (symbol? (labels-binding-name expr)))
       (list? (labels-binding-parameters expr))
       (list? (labels-binding-body expr))))
(define (labels-binding-name expr)
  (first expr))
(define (labels-binding-parameters expr)
  (second expr))
(define (labels-binding-body expr)
  (cddr expr))

(define (labels->cps bindings body)
  ;; Each binding must be modified to take a continuation
  (define (binding->cps binding)
    (define name (labels-binding-name binding))
    (define continuation
      (unique-symbol (format nil "CONTINUE-FROM-LABELS-~S-" name)))

    ;; Take continuation as the first parameter
    (define parameters (cons continuation (labels-binding-parameters binding)))
    ;; Replace the body with CPS.
    (define body
      `((funcall ,(block->cps name (labels-binding-body binding)) ,continuation)))

    (list* name parameters body))

  (define names (map #'labels-binding-name bindings))
  (define continuation (unique-symbol 'continue-from-labels))
  ;; Lexically bind names in body AND in bindings.
  (with-cps-functions names
    `(cl:lambda (,continuation)
       (cl:labels ,(map binding->cps bindings)
	 (funcall
	  ,(progn->cps body)
	  ,continuation)))))

(assert (equal? (funcall (eval (labels->cps '((foo (x y z) (foo2 x y z))
					      (foo2 (&rest args) (values-list args)))
					    '((foo 1 2 3))))
			 #'list)
		'(1 2 3)))

;; Forms I will handle.

;;(eval-when (situation...) implicit-progn-forms...)
;;(macrolet bindings implicit-progn-forms...)
;;(locally declarations... implicit-progn-forms...)
;;(symbol-macrolet ((symbol expansion-form)...) declarations... implicit-progn-forms...)

;;(setq pairs...)
;; Where a pair is symbolic-name value-form
;;(if test-form then-form [else-form])
;;(multiple-value-prog1 first-form forms...)
;;(the value-type form)
;;(multiple-value-call function arg arguments...)

;; Enables (go tag) form anywhere within the tagbody.
;;(tagbody tags-or-statements...)

;; I can handle statements easily, but go is complicated, and tagbody doesn't really allow for re-entrance.

;; No idea what to do with load-time-value
;;(load-time-value form read-only-p)

;; CATCH/THROW/UNWIND-PROTECT all involve manipulating the dynamic stack and therefore cannot be used with continuations
;; PROGV Creates new dynamic bindings, but it has an implicit-progn body.
#;(let ((*x* 3)) 
    (progv '(*x*) '(4) 
      (list *x* (symbol-value '*x*))))
#;(progv symbols values implicit-progn-forms...)

(uninstall-syntax!)

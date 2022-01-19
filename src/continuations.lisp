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

(defvar *lexical-cps-function-names* ())
(define (lexical-cps-function-name? name)
  "True if name names a function defined in CPS style."
  (member name *lexical-cps-function-names*))
(defmacro with-lexical-cps-function-names (names &body body)
  "Append names to *LEXICAL-CPS-FUNCTION-NAMES* for the duration of body."
  `(let ((*lexical-cps-function-names* (append ,names *lexical-cps-function-names*)))
     ,@body))

(defvar *function->cps-function-table* (make-hash-table :weakness :key))
(define (cps-function function)
  "Return the cps-function associated with function or nil if there is none."
  (hash-ref *function->cps-function-table* function nil))
(define (set-cps-function! function cps-function)
  "Associate the cps-function with the given function."
  (hash-set! *function->cps-function-table* function cps-function))
(define (cps-function->function cps-function)
  (let ((fn (lcurry cps-function #'values)))
    (set-cps-function! fn cps-function)
    fn))

(define (funcall/c continuation function . arguments)
  "Call function with arguments, passing the resulting values to continuation. 
If function has an associated CPS-FUNCTION, call it with the given continuation and arguments."
  (let ((cps-function (cps-function function)))
    (if cps-function
	(apply cps-function continuation arguments)
	(multiple-value-call continuation (apply function arguments)))))
(define (apply/c continuation function argument . arguments)
  "Apply function to arguments. If function has a CPS-FUNCTION, apply it with the given continuation."
  (when (and (empty? arguments) (not (list? argument)))
    (error "Non-list argument ~S to apply/c" argument))
  (apply #'funcall/c function continuation argument (nconc (butlast arguments) (first (last arguments)))))

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
    ((quote? expr) (quote->cps expr))
    ((eval-when? expr) (eval-when->cps (eval-when-situations expr) (eval-when-body expr)))
    ((setq? expr) (setq->cps (setq-pairs expr)))
    ((if? expr) (if->cps (if-test expr) (if-then expr) (if-else expr)))
    ((call/cc? expr) (call/cc->cps (call/cc-function expr)))

    ((lambda? expr) (lambda->cps (lambda-parameters expr) (lambda-body expr)))

    ((function-application? expr) (function-application->cps (function-application-function expr) (function-application-arguments expr)))
    ((atom? expr) (atom->cps expr))))

(define (check-cps-for-expr expr)
  (let ((cps-result (funcall (eval (expr->cps expr)) #'list))
	(result (multiple-value-list (eval expr))))
    (unless (equal? result cps-result)
      (error "CPS-RESULT ~S not the same as RESULT ~S" cps-result result))
    result))

(export 'call/cc)

(define (call/cc? expr)
  (and (pair? expr)
       (= 2 (length expr))
       (eq? (first expr) 'call/cc)))
(define (call/cc-function expr)
  (second expr))
(define (call/cc->cps function)
  (let ((function-name (unique-symbol 'call-cc-function))
	(continuation (unique-symbol 'continue-from-call/cc)))
    `(cl:lambda (,continuation)
       (funcall ,(expr->cps function)
		(cl:lambda (,function-name)
		  (funcall ,continuation (funcall ,function-name ,continuation)))))))

(defvar *the-continuation*)
(funcall (eval (expr->cps '(progn
			    (print 'before)
			    (call/cc (cl:lambda (k) (setq *the-continuation* k)))
			    (print 'after))))
	 #'values)


(define (quote? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:quote)
       (= 2 (length expr))))
(define (quote->cps expr)
  (atom->cps expr))

(check-cps-for-expr '(quote (the quick brown fox)))

(define (function? expr)
  (and (pair? expr)
       (= 2 (length expr))
       (eq? (first expr) 'cl:function)))
(define (function-name expr)
  (second expr))
(define (function->cps name)
  (if (lexical-cps-function-name? name)
      (atom->cps `(cps-function->function #',name))
      (atom->cps `#',name)))

(define (function-application? expr)
  (and (pair? expr)
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
	   ((lexical-cps-function-name? function)
	    ;; If function is a cps function, we need to pass the continuation.
	    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-~S" function))))
	      `(cl:lambda (,continuation) (,function ,continuation ,@names))))
	   ((eq? function 'cl:funcall)
	    ;; Special case for funcall
	    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-~S" function))))
	      ;; Use funcall/c and pass the continuation
	      `(cl:lambda (,continuation) (funcall/c ,continuation ,@names))))
	   ((eq? function 'cl:apply)
	    ;; Special case for apply
	    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-~S" function))))
	      ;; Use apply/c and pass the continuation
	      `(cl:lambda (,continuation) (apply/c ,continuation ,@names))))
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

(check-cps-for-expr '(values 1 2 3))
(check-cps-for-expr '(funcall #'values 1 2 3))

(check-cps-for-expr '(funcall (cl:lambda (&rest args) (values-list args)) 1 2 3))
(check-cps-for-expr '(apply (cl:lambda (&rest args) (values-list args)) 1 2 '(3 4)))
(check-cps-for-expr '(apply (cl:lambda (&rest args) (values-list args)) 1 2 3 ()))

(define (atom? expr)
  t)
(define (atom->cps atom)
  (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-ATOM"))))
    `(cl:lambda (,continuation) (multiple-value-call ,continuation ,atom))))

(check-cps-for-expr '1)

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

(check-cps-for-expr '(progn
		      (values :no)
		      (values nil)
		      (values 1 2 3)))

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
  ;; Creates a CPS function and a regular function (with #'VALUES as the continuation).
  (define cps-lambda
    (let ((continuation (unique-symbol 'continue-from-lambda)))
      `(cl:lambda ,(cons continuation parameters)
	 (funcall ,(progn->cps body) ,continuation))))
  
  (atom->cps `(cps-function->function ,cps-lambda)))

(check-cps-for-expr '(funcall (cl:lambda (&rest args) args) 1 2 3))

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


(check-cps-for-expr '(cl:let ((x 1)
			      y
			      (z))
		      (print x)
		      (print y)
		      (print z)
		      (values x 2 3)))

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
	   `(funcall ,(expr->cps value)
		     (cl:lambda (,name) ,(recurse (rest bindings))))))))
  (recurse bindings))

(check-cps-for-expr '(cl:let* ((x 1)
			       (y (1+ x))
			       (z (1+ y)))
		      (values x y z)))

(define (block? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:block)
       (symbol? (block-name expr))
       (list? (block-body expr))))
(define (block-name expr)
  (second expr))
(define (block-body expr)
  (cddr expr))

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

(defvar *block-table* ())
(define (block->cps name body)
  (let* ((continuation (unique-symbol (format nil "CONTINUE-FROM-BLOCK-~S-" name)))
	 (*block-table* (alist-set *block-table* name continuation)))
    `(lambda (,continuation)
       (funcall ,(progn->cps body) ,continuation))))

(define (return-from->cps name value)
  (let ((continuation (alist-ref *block-table* name))
	(ignored-continuation (unique-symbol 'ignored-continuation)))
    (unless continuation
      (error "RETURN-FROM: No block named ~S" name))
    `(cl:lambda (,ignored-continuation)
       (declare (ignore ,ignored-continuation))
       (multiple-value-call ,continuation ,value))))

(check-cps-for-expr '(cl:block name
		      (cl:return-from name (values 1 2 3))
		      (cl:return-from name :nope)))

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
       (>= (length expr) 2)
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
    
    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-FLET-~S-" name))))
      ;; Take continuation as the first parameter
      (define parameters (cons continuation (flet-binding-parameters binding)))
      ;; Replace the body with CPS.
      (define body
	`((funcall ,(block->cps name (flet-binding-body binding)) ,continuation)))

      (list* name parameters body)))

  (define names (map #'flet-binding-name bindings))
  (let ((continuation (unique-symbol 'continue-from-flet)))
    `(cl:lambda (,continuation)
       (cl:flet ,(map binding->cps bindings)
	 (funcall
	  ;; Lexically bind names for local functions in body. 
	  ,(with-lexical-cps-function-names names (progn->cps body))
	  ,continuation)))))

(check-cps-for-expr '(flet ((foo (x y z) (values x y z)))
		      (foo 1 2 3)))

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
       (>= (length expr) 2)
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
    (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-LABELS-~S-" name))))
      ;; Take continuation as the first parameter
      (define parameters (cons continuation (labels-binding-parameters binding)))
      ;; Replace the body with CPS.
      (define body
	`((funcall ,(block->cps name (labels-binding-body binding)) ,continuation)))

      (list* name parameters body)))

  (define names (map #'labels-binding-name bindings))
  (let ((continuation (unique-symbol 'continue-from-labels)))
    ;; Lexically bind names in body AND in bindings.
    (with-lexical-cps-function-names names
      `(cl:lambda (,continuation)
	 (cl:labels ,(map binding->cps bindings)
	   (funcall
	    ,(progn->cps body)
	    ,continuation))))))

(check-cps-for-expr '(labels ((foo (x y z) (foo2 x y z))
			      (foo2 (&rest args) (values-list args)))
		      (foo 1 2 3)))

(define (eval-when? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:eval-when)
       (>= (length expr) 2)
       (or (list? (eval-when-situations expr))
	   (error "Badly-formed eval-when situations: ~S" expr))))
(define (eval-when-situations expr)
  (second expr))
(define (eval-when-body expr)
  (cddr expr))
(define (eval-when->cps situations body)
  `(eval-when ,situations
     (progn->cps ,body)))

(define (setq? expr)
  (and (pair? expr)
       (eq (first expr) 'cl:setq)
       (>= (length expr) 3)
       (or (odd? (length expr))
	   (error "badly formed setq pairs: ~S" expr))
       (or (for-all (lambda (pair) (symbol? (first pair))) (setq-pairs expr))
	   (error "badly formed setq pairs: ~S" expr))))
(define (setq-pairs expr)
  (define (recurse expr pairs)
    (if (empty? expr)
	pairs
	(recurse (cddr expr) (cons (list (first expr) (second expr)) pairs))))
  (nreverse (recurse (rest expr) ())))
(define (setq->cps pairs)
  (define (pairs->cps pairs)
    (define (pair->cps pair last-pair?)
      (define name (first pair))
      (define value (second pair))
      (define value-name (unique-symbol (format nil "SETQ-~S-VALUE" name)))
      (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-SETQ-~S" name))))
	`(cl:lambda (,continuation)
	   (funcall ,(expr->cps value)
		    (cl:lambda (,value-name)
		      (setq ,name ,value-name)
		      ,(if last-pair?
			   ;; If this is the last pair, call the continuation with the value.
			   `(funcall ,continuation ,value-name)
			   ;; Otherwise pass the continuation along.
			   `(funcall ,(pairs->cps (rest pairs)) ,continuation)))))))

    (cond
      ;; Base case: 1 pair remaining
      ((empty? (rest pairs)) (pair->cps (first pairs) t))
      (t (pair->cps (first pairs) nil))))
  (pairs->cps pairs))

(check-cps-for-expr '(cl:let (name name2 name3)
		      (list (setq name 1
			     name2 2
			     name3 3)
		       name name2 name3)))

(define (if? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:if)
       (member (length expr) '(3 4))))
(define (if-test expr)
  (second expr))
(define (if-then expr)
  (third expr))
(define (if-else expr)
  (fourth expr))
(define (if->cps test then else)
  (let ((continuation (unique-symbol 'continue-from-if))
	(test-result (unique-symbol 'if-test-result)))
    `(lambda (,continuation)
       (funcall ,(expr->cps test)
		(cl:lambda (,test-result)
		  (funcall (cl:if ,test-result
				  ,(expr->cps then)
				  ,(expr->cps else))
			   ,continuation))))))

(check-cps-for-expr (if (or t nil)
			:true
			:false))
(check-cps-for-expr (if (and t nil)
			:true
			:false))

;; Forms I will handle.

;;(macrolet bindings implicit-progn-forms...)
;;(symbol-macrolet ((symbol expansion-form)...) declarations... implicit-progn-forms...)
;;(locally declarations... implicit-progn-forms...)

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

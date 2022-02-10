(in-package :schemeish.backend)

(install-syntax!)

(for-macros
 (defvar *lexical-cps-function-names* ())
 (defvar *function->cps-function-table* (make-hash-table :weakness :key))
 (defvar *block-table* ())
 (defvar *tag->function-name-table* ()))

(define (lexical-cps-function-name? name)
  "True if name names a function defined in CPS style."
  (member name *lexical-cps-function-names*))
(defmacro with-lexical-cps-function-names (names &body body)
  "Append names to *LEXICAL-CPS-FUNCTION-NAMES* for the duration of body."
  `(let ((*lexical-cps-function-names* (append ,names *lexical-cps-function-names*)))
     ,@body))

(define (cps-function function)
  "Return the cps-function associated with function or nil if there is none."
  (hash-ref *function->cps-function-table* function nil))
(define (set-cps-function! function cps-function)
  "Associate the cps-function with the given function."
  (hash-set! *function->cps-function-table* function cps-function))

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
  (if (empty? arguments)
      (apply #'funcall/c continuation function argument)
      (apply #'funcall/c continuation function argument (nconc (butlast arguments) (first (last arguments))))))

(set-cps-function! #'funcall #'funcall/c)
(set-cps-function! #'apply #'apply/c)

(for-macros
  (defvar *transform-cps-special-form-table* (make-hash-table)))
(define (register-transform-cps-special-form symbol transform)
  (hash-set! *transform-cps-special-form-table* symbol transform))

(for-macros
  (defvar *cps-transformer*))
(define (transform-cps form)
  (transform *cps-transformer* form))
(define (transform-cps* forms)
  (transform* *cps-transformer* forms))

(defmacro define-transform-cps-special-form (name (expression environment) &body body)
  "Define a special form transformer for the CPS macro-expansion.
Name is the symbol naming the special-form.
Expression will be bound to the special form being transformed, and environment will be bound to the current lexical environment
for body. Body should evaluate to the transformed form."
  (let ((transformer (unique-symbol 'transformer)))
    `(for-macros
       (register-transform-cps-special-form
	',name
	(cl:lambda (,transformer ,expression ,environment)
	  (cl:declare (ignore ,transformer))
	  (cl:declare (ignorable ,expression ,environment))
	  (scm ,@body))))))


;; TODO: Move this
(for-macros
  (register-lexical-body-definition
   'define-unique-symbols
   (lambda (definition)
     (define names (rest definition))
     (values names `(progn ,@(map (lambda (name)
				    `(setq ,name (unique-symbol ',name)))
				  names))))))

;; TODO: move this
(define (let-binding-name expr)
  (if (pair? expr)
      (first expr)
      expr))
(define (let-binding-value expr)
  (if (pair? expr)
      (second expr)
      nil))


(def (atom->cps expression)
  (define-unique-symbols continue-from-atom)
  `(cl:lambda (,continue-from-atom)
     (multiple-value-call ,continue-from-atom ,expression)))

(def (transform-cps-atom _ expression _)
  "Return a form that evaluates an atom in CPS."
  (atom->cps expression))

(def (cps->primary-value cps-form name continuation-body)
  "Form that evaluates cps-form, passing the results to a continuation
that can accept multiple values. Binds name to the primary value for body."
  (define-unique-symbols form-values)
  `(funcall ,(transform-cps cps-form)
	    (cl:lambda (&rest ,form-values)
	      (cl:let ((,name (first ,form-values)))
		,@continuation-body))))

(def (eval-arguments->cps-form argument-forms body-proc)
  "Return a form that evalautes argument-forms from left to right in CPS,
before evaluating (body-proc arguments)"
  (let iterate ((arguments ())
		(argument-forms argument-forms))
    (cond
      ((empty? argument-forms) (body-proc (nreverse arguments)))
      (t
       (define-unique-symbols argument)
       (define-destructuring (argument-form . rest-argument-forms) argument-forms)
       (cps->primary-value
	argument-form argument
	(list (iterate (cons argument arguments) rest-argument-forms)))))))

(def (transform-cps-proper-list _ expression _)
  "Return a form that evaluates function-application in CPS.
Special cases currently exist for funcall and apply."
  (define-unique-symbols continue-from-function-application)
  (define-destructuring (function-name . argument-forms) expression)
  `(cl:lambda (,continue-from-function-application)
     ;; Evaluate all arguments from left to right
     ,(eval-arguments->cps-form
       argument-forms
       (lambda (arguments)
	 ;; Perform function application, using FUNCALL/C
	 `(funcall/c ,continue-from-function-application (function ,function-name) ,@arguments)))))

(define-transform-cps-special-form cl:function (expression environment)
  (atom->cps expression))
(define-transform-cps-special-form cl:quote (expression environment)
  (atom->cps expression))

(def (progn->cps forms)
  (cond
    ;; (progn) => NIL
    ((empty? forms) (atom->cps nil))
    (t
     (define-destructuring (form . rest-of-forms) forms)
     (cond
       ;; (progn form) => form
       ((empty? rest-of-forms) (transform-cps form))
       ;; (progn form . rest-of-forms)
       (t (define-unique-symbols continue ignored)
	  `(cl:lambda (,continue)
	     (funcall ,(transform-cps form)
		      (cl:lambda (&rest ,ignored)
			(declare (ignore ,ignored))
			(funcall ,(progn->cps rest-of-forms) ,continue)))))))))

(define-transform-cps-special-form cl:progn (expression environment)
  (define forms (rest expression))
  (progn->cps forms))

(define-transform-cps-special-form cl:let (expression environment)
  (define-destructuring (bindings . body) (rest expression))
  (define-values (declarations forms) (schemeish.internals::parse-declarations body))
  (def binding-names (map let-binding-name bindings))
  (def (iterate bindings binding-values)
    (cond
      ((empty? bindings)
       (define-unique-symbols continue-from-let)
       (def new-bindings (map list binding-names (nreverse binding-values)))
       `(cl:lambda (,continue-from-let)
	  ;; Establish bindings from name->let-binding-value-name in let
	  (cl:let ,new-bindings
	    ,@declarations
	    (funcall ,(transform-forms forms) ,continue-from-let))))
      (t
       (define-destructuring (binding . rest-of-bindings) bindings)
       (define-unique-symbols binding-value)
       (define value-form (let-binding-value binding))
       ;; Bind value to a unique name
       (cps->primary-value value-form binding-value
			   (list (iterate rest-of-bindings (cons binding-value binding-values)))))))
  (define transform-forms progn->cps)
  (iterate bindings ()))

(define-transform-cps-special-form cl:lambda (expression environment)
  (define-destructuring (ordinary-lambda-list . body) (rest expression))
  (define-values (declarations forms) (schemeish.internals::parse-declarations body))
  ;; TODO: Convert (lambda (... (arg arg-default-form arg-provided?) ...) body...)
  ;; into (lambda (... (arg NIL arg-provided?) ...) ... (unless arg-provided? (setq arg arg-default-form)) ... body...)
  ;; before converting to CPS
  (define-unique-symbols function continue-from-lambda continue-from-creating-lambda)
  ;; The value being returned is the un-transformed lambda expression
  `(cl:let ((,function ,expression))
     ;; Transform the lambda expression and register it as the CPS-function of the function
     (set-cps-function!
      ,function
      (cl:lambda ,(cons continue-from-lambda ordinary-lambda-list)
	,@declarations
	(funcall ,(transform-cps `(cl:progn ,@forms)) ,continue-from-lambda)))
     ;; Return a function which takes a continuation and returns the un-transformed lambda expression.
     (cl:lambda (,continue-from-creating-lambda)
       (funcall ,continue-from-creating-lambda ,function))))

(define-transform-cps-special-form cl:setq (expression environment)
  (def pairs (setq-pairs expression))
  (define (pairs->cps pairs)
    (define (pair->cps pair rest-of-pairs last-pair?)
      (define-destructuring (name value-form) pair)
      (define-unique-symbols value continue-from-setq)
      `(cl:lambda (,continue-from-setq)
	 ,(cps->primary-value
	   value-form value
	   `((setq ,name ,value)
	     ,(if last-pair?
		  ;; If this is the last pair, call the continuation with the value.
		  `(funcall ,continue-from-setq ,value)
		  ;; Otherwise pass the continuation along.
		  `(funcall ,(pairs->cps rest-of-pairs) ,continue-from-setq))))))

    (cond
      ;; Base case: 0 pairs
      ((empty? pairs) (atom->cps nil))
      (t
       (define-destructuring (pair . rest-of-pairs) pairs)
       (cond
	 ;; Base case: 1 pair remaining
	 ((empty? rest-of-pairs) (pair->cps pair rest-of-pairs t))
	 (t (pair->cps pair rest-of-pairs nil))))))
  (pairs->cps pairs))

(define-transform-cps-special-form cl:if (expression environment)
  (define-destructuring (test-form then-form &optional else-form) (rest expression))
  (define-unique-symbols test)
  (cps->primary-value test-form test
		      `((cl:if ,test
			       ,(transform-cps then-form)
			       ,(transform-cps else-form)))))



(def (catch-tags-in-thunk tags thunk)
  "Equivalent to (catch tag1 (catch tag2 ... (catch tagN (thunk))))"
  (if (empty? tags)
      (thunk)
      (catch-tags-in-thunk (rest tags) (lambda () (catch (first tags) (thunk))))))

(define-struct fcontrol-signal
    (tag value continuation)
    :opaque)

(for-macros (defvar *current-run-tags* ()))
(def (run tag cps-thunk handler)
  "Perform CPS-THUNK catching any tag in the list tags.
If thunk exits normally (no FCONTROL is encountered), the result of thunk is returned.
If thunk returns multiple-values, only the first value is returned.
If thunk does not exit normally (FCONTROL aborts the continuation), 
the (handler tag value continuation) is invoked.

CPS-THUNK is expected to be a function defined in continuation passing style. 
E.g. (lambda (continue) (continue values...))"
  (let ((thunk-result (let ((*current-run-tags* (cons tag *current-run-tags*)))
			(catch-tags-in-thunk *current-run-tags*
					     (lambda () (cps-thunk values))))))
    ;; If the result is an fcontrol-signal, it means an (FCONTROL value) form was encountered.
    (cond
      ((fcontrol-signal? thunk-result)
       ;; TODO: Destructure structures
       (define fcontrol-tag (fcontrol-signal-tag thunk-result))
       (define value (fcontrol-signal-value thunk-result))
       (define continuation (fcontrol-signal-continuation thunk-result))
       (cond
	 ((eq? tag fcontrol-tag)
	  ;; This is the tag we are catching, invoke the handler.
	  ;; Invoke handler on the signal's value and continuation
	  (handler value continuation))
	 ((not (member fcontrol-tag *current-run-tags*))
	  ;; This tag has no catch. throw an error.
	  (error "No prompt exists to catch ~S in the dynamic context." fcontrol-tag))
	 (t
	  ;; Tag is meant for an outer run, re-throw with setting us up again as part of the continuation
	  (throw fcontrol-tag
	    (make-fcontrol-signal
	     fcontrol-tag
	     value
	     (lambda arguments
	       (run tag
		    (lambda (continue) ((apply continuation arguments) continue))
		    handler)))))))
      ;; Otherwise, we encountered a normal exit: return the result
      (t thunk-result))))

;; (fcontrol tag value)
;; Evaluates tag, then value.
;; throws to tag with (make-fcontrol-signal tag value current-continuation), aborting the current continuation
(define-transform-cps-special-form fcontrol (expression environment)
  (define-unique-symbols continue-from-fcontrol)
  (define arguments (rest expression))
  `(cl:lambda (,continue-from-fcontrol)
     ,(eval-arguments->cps-form
       arguments
       (lambda (argument-names)
	 (define-destructuring (tag value) argument-names)
	 `(throw ,tag (make-fcontrol-signal ,tag ,value ,continue-from-fcontrol))))))
(defmacro fcontrol (&whole whole tag value)
  "Evaluates tag, then value, throwing tag, value, and the current continuation
to the dynamically nearest established RUN, aborting the current continuation.
If FCONTROL is evaluated in a non-CPS function, it issues a warning and evaluates to VALUE."
  (declare (ignore tag))
  `(progn
     (warn "Attempt to ~S in a non-CPS environment. Evaluating to value." ',whole)
     ,value))



(for-macros
  (scm
    (expose-variables
     (*cps-transformer* (make-transformer *transform-cps-special-form-table*
					  transform-cps-proper-list
					  (lambda (_ expression _) (error "Attempt to compile invalid dotted list: ~S" expression))
					  (lambda (_ expression _) (error "Attempt to compile invalid cyclic list: ~S" expression))
					  transform-cps-atom)))))

(defmacro cps (expr)
  "Transforms EXPR into continuation-passing-style."
  `(funcall ,(transform-cps expr) #'values))


(defmacro cps-form-equal? (form)
  (let ((results (unique-symbol 'results)))
    `(let ((,results (multiple-value-list ,form)))
       (values (equal? (multiple-value-list (cps ,form)) ,results)
	       ,results))))





;; atoms
(assert (cps-form-equal? 1))
;; function application
(assert (cps-form-equal? (values 1 2 3 4 5)))
(assert (cps-form-equal? (+ (values 1 2) (values 3 4))))
(assert (cps-form-equal? (+ 1 2 (+ 3 4))))
;; Function
(assert (cps-form-equal? #'+))
;; Quote
(assert (cps-form-equal? '(the quick brown fox)))
;; Progn
(assert (cps-form-equal? (progn)))
(assert (cps-form-equal? (progn (values 1 2 3))))
(assert (cps-form-equal? (progn 1 2 3 4 (values 1 2 3))))

;; TODO: it would be cool to have (define-output-to-string stream) ... just a thought
(assert (equal? (with-output-to-string (s)
		  (assert (equal? (multiple-value-list (cps (progn (format s "c") (format s "b") (format s "a") (values 1 2 3))))
				  '(1 2 3))))
		"cba"))
(assert (cps-form-equal? (progn '(the quick brown fox) #'+)))
(assert (cps-form-equal? (progn #'+ '(the quick brown fox))))

;; Let
(assert (cps-form-equal? (let () (values 1 2 3))))
(assert (cps-form-equal? (let ((a 1) (b :steak)) (values a b))))
(assert (cps-form-equal? (let ((a (values 1 2 3)) (b (values :steak :sauce))) (values a b))))
;; Verifies that names aren't visible, but causes a warning
;; (assert (not (ignore-errors (cps (let ((a 1) (b a)) (values a b))))))


;; Lambda
(assert (equal? (multiple-value-list (funcall (cps (cl:lambda (a b c) (values a b c))) 1 2 3))
		'(1 2 3)))
(assert (equal? (funcall (cps-function (cps (cl:lambda (a b c) (values a b c)))) #'list 1 2 3)
		'(1 2 3)))
(assert (cps-form-equal? (funcall (cl:lambda (a b c) (values a b c)) 1 2 3)))
(assert (equal? (funcall/c #'list (cps (funcall (cl:lambda (f) (cl:lambda (&rest args) (apply f args)))
						(cl:lambda (&rest args) (apply #'values args))))
			   1 2 3)
		'(1 2 3)))

;; Setq
(assert (cps-form-equal? (setq)))
(assert (let (a b c)
	  (cps-form-equal? (setq a 1
				 b (1+ a)
				 c (1+ b)))))

;; If
(assert (cps-form-equal? (if (values t nil nil)
			     (progn 1 2 3)
			     (values 4 5 6))))
(assert (cps-form-equal? (if (values nil t nil)
			     (progn 1 2 3)
			     (values 4 5 6))))



;; RUN/FCONTROL
(defmacro % (tag expr handler)
  `(run ,tag (cps-function (cl:lambda () ,expr)) ,handler))

(assert (equal? (cps
		 (% :abort
		    (list (fcontrol :abort :one) 2 3)
		    (cl:lambda (value continuation)
		      (list value (funcall continuation 1)))))
		'(:one (1 2 3))))

;; Re-establish inner prompts when continuing from an outer prompt:
(assert (equal? (cps
		 (% :outer-abort
		    (% :inner-abort
		       (list (fcontrol :outer-abort :one) (fcontrol :inner-abort :two) 3)
		       (cl:lambda (value continuation)
			 (declare (ignore value))
			 (funcall continuation 2)))
		    (cl:lambda (value continuation)
		      (declare (ignore value))
		      (funcall continuation 1))))
		'(1 2 3)))

;; Simple-exit example
(assert (= (cps
	    (cl:let (product)
	      (setq product
		    (cl:lambda (numbers) 
		      (% :product
			 (cl:let (recurse)
			   (setq recurse
				 (cl:lambda (numbers)
				   (if (empty? numbers)
				       1
				       (cl:let ((number (first numbers)))
					 ;; Short-circut if any number is 0
					 (if (zero? number)
					     (fcontrol :product 0)
					     (* number (funcall recurse (rest numbers))))))))
			   (funcall recurse numbers))
			 (cl:lambda (result continuation)
			   (declare (ignore continuation))
			   result))))
	      (funcall product '(1 2 3 4 5))))
	   (* 1 2 3 4 5)))

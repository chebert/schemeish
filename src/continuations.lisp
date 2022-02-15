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

(defmacro cps-lambda (continuation-name &body body)
  `(cl:lambda (,continuation-name)
     ,@body))

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
  `(cps-lambda ,continue-from-atom
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
  `(cps-lambda ,continue-from-function-application
     ;; Evaluate all arguments from left to right
     ,(eval-arguments->cps-form
       argument-forms
       (cl:lambda (arguments)
	 ;; Perform function application, using FUNCALL/C
	 `(funcall/c ,continue-from-function-application (function ,function-name) ,@arguments)))))

(define-transform-cps-special-form no-cps (expression environment)
  (atom->cps expression))

(define-transform-cps-special-form cl:function (expression environment)
  (atom->cps expression))
(define-transform-cps-special-form cl:quote (expression environment)
  (atom->cps expression))

(def (progn->cps forms)
  (cond
    ;; (progn) => NIL
    ((empty? forms) (atom->cps nil))
    ;; (progn . forms)
    (t
     (define-destructuring (form . rest-of-forms) forms)
     (cond
       ;; (progn form) => form
       ((empty? rest-of-forms) (transform-cps form))
       ;; (progn form . rest-of-forms)
       (t (define-unique-symbols continue ignored)
	  `(cps-lambda ,continue
	     (funcall ,(transform-cps form)
		      (cl:lambda (&rest ,ignored)
			(declare (ignore ,ignored))
			(funcall ,(progn->cps rest-of-forms) ,continue)))))))))

(define-transform-cps-special-form cl:progn (expression environment)
  (define forms (rest expression))
  (progn->cps forms))

(define-transform-cps-special-form cl:let (expression environment)
  (define-destructuring (bindings . body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  (define-unique-symbols continue-from-let)
  (def binding-names (map let-binding-name bindings))
  (def (iterate bindings binding-values)
    (cond
      ((empty? bindings)
       (def new-bindings (map list binding-names (nreverse binding-values)))
       ;; Establish bindings from name->let-binding-value-name in let
       `(cl:let ,new-bindings
	  ,@declarations
	  (funcall ,(progn->cps forms) ,continue-from-let)))
      (t
       (define-destructuring (binding . rest-of-bindings) bindings)
       (define-unique-symbols binding-value)
       (define value-form (let-binding-value binding))
       ;; Bind value to a unique name
       (cps->primary-value value-form binding-value
			   (list (iterate rest-of-bindings (cons binding-value binding-values)))))))
  `(cps-lambda ,continue-from-let
     ,(iterate bindings ())))


(define-transform-cps-special-form cl:let* (expression environment)
  (define-destructuring (bindings . body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  (define-unique-symbols continue-from-let*)
  (def (iterate bindings)
    (cond
      ;; Base case: Evaluate body
      ;; Unfortunately declarations can only apply to the body.
      ;; This is inevitable, since each binding depends on the previous binding.
      ((empty? bindings)
       ;; Unfortunately, using locally will cause most declarations to be out of scope.
       ;; The only real solution for this is to parse the declarations ourselves,
       ;; and sort them to be with the definition of the binding they are declaring.
       `(cl:locally
	    ,@declarations
	  (funcall ,(progn->cps forms) ,continue-from-let*)))
      ;; Iteration case: evaluate value of next binding, and bind it.
      (t (define-destructuring (binding . rest-of-bindings) bindings)
	 (define name (let-binding-name binding))
	 (define value (let-binding-value binding))
	 ;; Evaluate the next binding's value, binding it to name.
	 (cps->primary-value value name
			     (list `(declare (ignorable ,name))
				   (iterate rest-of-bindings))))))
  `(cps-lambda ,continue-from-let*
     ,(iterate bindings)))

(def (full-ordinary-lambda-list ordinary-lambda-list)
  "Return a lambda list with optional/keywords fully expanded to their (name default-value provided?) form.
If necessary, unique symbols will be generated for provided? names.
Aux variables will be (name value)"
  (append*
   (map-ordinary-lambda-list
    (lambda (key parameter)
      (ecase key
	((:positional :rest) (list parameter))
	((:optional :key)
	 (cond
	   ((pair? parameter)
	    (define-destructuring (name &optional default-value provided?) parameter)
	    (list (list name default-value (if provided? provided? (unique-symbol (symbolicate name '-provided?))))))
	   (t (list (list parameter nil (unique-symbol (symbolicate parameter '-provided?)))))))
	(:keyword (list parameter))
	(:aux
	 (cond
	   ((pair? parameter)
	    (define-destructuring (name &optional default-value) parameter)
	    (list (list name default-value)))
	   (t (list (list parameter nil)))))))
    ordinary-lambda-list)))

(def (full-ordinary-lambda-list->function-argument-list-form ordinary-lambda-list)
  (cons 'append (append*
		 (let (rest-provided?)
		   (map-ordinary-lambda-list
		    (lambda (key parameter)
		      (ecase key
			(:positional (list `(list ,parameter)))
			(:optional
			 (define-destructuring (name default-value provided?) parameter)
			 (list `(when ,provided? (list ,name))))
			(:keyword ())
			(:key
			 (unless rest-provided?
			   (define-destructuring (name default-value provided?) parameter)
			   (list `(when ,provided? (list (make-keyword ',name) ,name)))))
			(:rest
			 (set! rest-provided? t)
			 (list parameter))
			(:aux ())))
		    ordinary-lambda-list)))))

(define-transform-cps-special-form cl:lambda (expression environment)
  (define-destructuring (ordinary-lambda-list . body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  ;; TODO: Convert (lambda (... (arg arg-default-form arg-provided?) ...) body...)
  ;; into (lambda (... (arg NIL arg-provided?) ...) ... (unless arg-provided? (setq arg arg-default-form)) ... body...)
  ;; before converting to CPS
  (define-unique-symbols function continue-from-lambda continue-from-creating-lambda cps-function)
  (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
  #;
  (def continuation-lambda-list (cons continue-from-lambda
				      (map-ordinary-lambda-list
				       (lambda (key parameter)
					 (ecase key
					   (:positional parameter)
					   ((:optional :key)
					    (list (first parameter) nil (third parameter)))
					   (:keyword parameter)
					   (:rest parameter)))
				       full-lambda-list)))
  ;; The value being returned is the un-transformed lambda expression
  `(cl:let* ((,cps-function (cl:lambda ,(cons continue-from-lambda full-lambda-list)
			      ,@declarations
			      ;; TODO: establish bindings from full-lambda-list at the beginning of forms
			      (funcall ,(transform-cps `(cl:progn ,@forms)) ,continue-from-lambda)))
	     ;; TODO: create a function-call from an ordinary-lambda-list
	     (,function (cl:lambda ,full-lambda-list (apply ,cps-function #'values ,(full-ordinary-lambda-list->function-argument-list-form full-lambda-list)))))
     ;; Transform the lambda expression and register it as the CPS-function of the function
     (set-cps-function! ,function ,cps-function)
     ;; Return a function which takes a continuation and returns the un-transformed lambda expression.
     (cps-lambda ,continue-from-creating-lambda
       (funcall ,continue-from-creating-lambda ,function))))

(define-transform-cps-special-form cl:setq (expression environment)
  (def pairs (setq-pairs expression))
  (define (pairs->cps pairs)
    (define (pair->cps pair rest-of-pairs last-pair?)
      (define-destructuring (name value-form) pair)
      (define-unique-symbols value continue-from-setq)
      `(cps-lambda ,continue-from-setq
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
  (define-unique-symbols test continue-from-if)
  `(cps-lambda ,continue-from-if
     ,(cps->primary-value test-form test
			  `((cl:if ,test
				   (funcall ,(transform-cps then-form) ,continue-from-if)
				   (funcall ,(transform-cps else-form) ,continue-from-if))))))

(define-transform-cps-special-form cl:the (expression environment)
  (define-destructuring (type-form value-form) (rest expression))
  (define-unique-symbols results continue-from-the)
  `(cps-lambda ,continue-from-the
     (funcall ,(transform-cps value-form)
	      (cl:lambda (&rest ,results)
		(funcall ,(atom->cps `(the ,type-form (values-list ,results))) ,continue-from-the)))))

(define-transform-cps-special-form cl:multiple-value-call (expression environment)
  (define-destructuring (function-form . arguments) (rest expression))
  (define-unique-symbols function accumulated-argument-values continue-from-multiple-value-call)
  
  (define arguments->cps
    (let iterate ((arguments arguments))
      (cond
	;; Base Case: no arguments, just return no values
	((empty? arguments)
	 (define-unique-symbols continuation)
	 `(cl:lambda (,continuation) (funcall ,continuation)))
	(t
	 (define-unique-symbols continuation argument-values accumulated-argument-values)
	 (define-destructuring (argument . rest-of-arguments) arguments)
	 ;; A function that takes CONTINUATION and applies it to (append argument-values accumulated-argument-values)
	 `(cps-lambda ,continuation
	    ;; evaluate the next argument...
	    (funcall ,(transform-cps argument)
		     (cl:lambda (&rest ,argument-values)
		       ;; ...capturing the values in argument-values
		       ;; Then evaluate the rest of the argument-values...
		       (funcall ,(iterate (rest arguments))
				(cl:lambda (&rest ,accumulated-argument-values)
				  ;; ...capturing the values in accumulated-argument-values
				  ;; Apply the continuation to the first argument-values appended to the rest of the accumulated-argument-values
				  (apply ,continuation (append ,argument-values ,accumulated-argument-values)))))))))))

  `(cps-lambda ,continue-from-multiple-value-call
     ;;  evalaute the primary value of function-form and capture it in FUNCTION
     ,(cps->primary-value
       function-form function
       `(
	 ;; evaluate the arguments...
	 (funcall ,arguments->cps
		  (cl:lambda (&rest ,accumulated-argument-values)
		    ;; ...capturing the values in accumulated-argument-values
		    ;; Apply the function to the accumulated-arguments-results with the given continuation.
		    (apply/c ,continue-from-multiple-value-call ,function ,accumulated-argument-values)))))))



;; TODO
#;
(cps (scm
       (def vs ())
       (def (v x) (push x vs) x)
       (tagbody
	  (v :start)
	:tag1
	  (v :tag1)
	  (unwind-protect
	       (progn
		 (v :inside-protected)
		 (go :tag2))
	    (v :cleanup))
	:tag2
	  (v :tag2))
       (nreverse vs)))



;; TODO
;; tagbody should be dynamic

;; Use unwind-protect for definition of unwind-protect
;;   what about fcontrol inside cleanup forms of unwind-protect?
;; use %/fcontrol for definitions of block/tagbody

;; I Think this should work for unwind-protect, once I switch from throw tag to throw :fcontrol



(def (catch-tags-in-thunk tags thunk)
  "Equivalent to (catch tag1 (catch tag2 ... (catch tagN (thunk))))"
  (if (empty? tags)
      (thunk)
      (catch-tags-in-thunk (rest tags) (lambda () (catch (first tags) (thunk))))))

(define-struct fcontrol-signal
    (tag value continuation)
    :opaque)

(for-macros (defvar *fcontrol-tag* (unique-symbol "FCONTROL-TAG")))
(for-macros (defvar *prompt-established?* nil))

(def (run continue-from-run tag thunk handler)
  "Perform THUNK in a dynamic context that catches all tags (cons tag *current-run-tags*).
Return the results to continue-from-run.
If tag is caught because of (fcontrol tag value), the (handler value rest-of-thunk) is invoked with
the rest-of-thunk.
If a different tag is caught because of (fcontrol another-tag value), the control is re-signaled
with a continuation: rest-of-thunk in the re-established dynamic context before continuing to continue-from-run."
  (def thunk-result (let ((*prompt-established?* t))
		      (catch *fcontrol-tag* (multiple-value-list (thunk)))))
  ;; If the result is an fcontrol-signal, it means an (FCONTROL value) form was encountered.
  (cond
    ((fcontrol-signal? thunk-result)
     ;; TODO: Destructure structures
     (define fcontrol-tag (fcontrol-signal-tag thunk-result))
     (define value (fcontrol-signal-value thunk-result))
     (define rest-of-thunk (fcontrol-signal-continuation thunk-result))
     (cond
       ((eq? tag fcontrol-tag)
	;; This is the tag we are catching, invoke the handler.
	;; Invoke handler on the signal's value and continuation
	(multiple-value-call continue-from-run (handler value rest-of-thunk)))
       (t
	;; Tag is meant for an outer run, re-throw with setting us up again as part of the continuation
	(throw *fcontrol-tag*
	  (make-fcontrol-signal
	   fcontrol-tag
	   value
	   (lambda arguments
	     (run continue-from-run
		  tag
		  (lambda () (apply rest-of-thunk arguments))
		  handler)))))))
    ;; Otherwise, we encountered a normal exit: return the results
    (t (apply continue-from-run thunk-result))))

;; (fcontrol tag values-form)
;; Evaluates tag, then value.
;; throws to tag with (make-fcontrol-signal tag value current-continuation), aborting the current continuation
(define-transform-cps-special-form fcontrol (expression environment)
  (define-unique-symbols continue-from-fcontrol)
  (define arguments (rest expression))
  `(cps-lambda ,continue-from-fcontrol
     ,(eval-arguments->cps-form
       arguments
       (lambda (argument-names)
	 (define-destructuring (tag value) argument-names)
	 `(progn
	    (unless *prompt-established?*
	      (error "Attempt to (FCONTROL ~S ~S) without an established prompt" ,tag ,value))
	    (throw *fcontrol-tag* (make-fcontrol-signal ,tag ,value ,continue-from-fcontrol)))))))
(defmacro fcontrol (tag value)
  "Evaluates tag, then value, throwing tag, value, and the current continuation
to the dynamically nearest established RUN, aborting the current continuation.
If FCONTROL is evaluated in a non-CPS function, it issues a warning and evaluates to VALUE."
  (declare (ignore tag))
  `(progn
     (warn "Attempt to FCONTROL in a non-CPS environment. Evaluating to value.")
     ,value))

(def (default-prompt-tag) nil)
(def (default-prompt-handler value _continuation) value)


(define-transform-cps-special-form cl:block (expression environment)
  (define-destructuring (name . forms) (rest expression))
  (define-unique-symbols continue-from-block block-tag)
  (define (update-lexical-context context)
    (alist-update context
		  :block-tag-alist
		  (cut (alist-set _ name block-tag))))
  (let ((*lexical-context* (update-lexical-context *lexical-context*)))
    `(cps-lambda ,continue-from-block
       (no-cps
	(run ,continue-from-block
	     ',block-tag
	     (cl:lambda () (funcall ,(transform-cps `(cl:progn ,@forms)) #'values))
	     (cl:lambda (results k)
	       (declare (ignore k))
	       (values-list results)))))))


(define-transform-cps-special-form cl:return-from (expression environment)
  (define-destructuring (name &optional values-form) (rest expression))
  (define block-tag (alist-ref (alist-ref *lexical-context* :block-tag-alist) name))
  (define-unique-symbols continue-from-return-from results)
  (unless block-tag
    (error "Could not find BLOCK named ~S in the current lexical environment." name))
  `(cps-lambda ,continue-from-return-from
     (funcall ,(transform-cps values-form)
	      (cl:lambda (&rest ,results)
		(funcall ,(transform-cps `(fcontrol ',block-tag ,results)) ,continue-from-return-from)))))


(define-transform-cps-special-form cl:catch (expression environment)
  (define-destructuring (tag-form . forms) (rest expression))
  (define-unique-symbols continue-from-catch tag)
  `(cps-lambda ,continue-from-catch
     ,(cps->primary-value
       tag-form tag
       `((no-cps (run ,continue-from-catch
		      ,tag
		      (cl:lambda () (funcall ,(transform-cps `(progn ,@forms)) #'values))
		      (cl:lambda (results k)
			(declare (ignore k))
			(values-list results))))))))

(define-transform-cps-special-form cl:throw (expression environment)
  (define-destructuring (tag-form results-form) (rest expression))
  (define-unique-symbols continue-from-throw tag results)
  `(cps-lambda ,continue-from-throw
     ,(cps->primary-value tag-form tag
			  `((funcall ,(transform-cps results-form)
				     (cl:lambda (&rest ,results)
				       (funcall ,(transform-cps `(fcontrol ,tag ,results)) ,continue-from-throw)))))))

(def (dynamic-wind before-thunk thunk after-thunk)
  "Executes (progn (before-thunk) (thunk) (after-thunk)) returning the results of thunk.
AFTER-THUNK will be run after thunk even if a CONTROL is signaled.
If thunk signals a CONTROL instead, dynamic-wind will signal a CONTROL with the tag and value signaled by THUNK,
and with a continuation that will execute (dynamic-wind before-thunk rest-of-thunk after-thunk).
This way, every time thunk is executed, before-thunk will be run before and after-thunk will run after."
  (before-thunk)
  (let ((result (catch *fcontrol-tag* (multiple-value-list (thunk)))))
    (after-thunk)
    (cond
      ((fcontrol-signal? result)
       (def tag (fcontrol-signal-tag result))
       (def rest-of-thunk (fcontrol-signal-continuation result))
       ;; Re-signal
       (throw *fcontrol-tag*
	 (make-fcontrol-signal
	  tag
	  (fcontrol-signal-value result)
	  ;; Return a continuation that has dynamic-wind before/after applied
	  (lambda arguments
	    (dynamic-wind before-thunk
			  (lambda () (rest-of-thunk . arguments))
			  after-thunk)))))
      (t (values-list result)))))

;; unwind-protect
(define-transform-cps-special-form cl:unwind-protect (expression environment)
  (define-destructuring (protected &body cleanup) (rest expression))
  (define-unique-symbols results normal-exit?
    continue-from-unwind-protect result arguments finished-cleanup?)
  `(cps-lambda ,continue-from-unwind-protect
     (let (,normal-exit? ,results ,finished-cleanup?)
       (unwind-protect
	    (let ((,result
		    (catch *fcontrol-tag*
		      ;; todo: replace cps with transform-cps
		      ;; code-paths? for better debugging
		      (progn (set! ,results (funcall ,(transform-cps protected) #'list))
			     (set! ,normal-exit? t)))))
	      (when (fcontrol-signal? ,result)
		(throw *fcontrol-tag*
		  (make-fcontrol-signal (fcontrol-signal-tag ,result)
					(fcontrol-signal-value ,result)
					;; Don't allow re-entry into the protected froms
					(lambda _ (error "Attempt to re-enter a protected form."))))))
	 (let ((,result (catch *fcontrol-tag* (cps (progn ,@cleanup)))))
	   ;; Re-signal an fcontrol-signal, but with a modified continuation
	   (when (fcontrol-signal? ,result)
	     (throw *fcontrol-tag*
	       (make-fcontrol-signal (fcontrol-signal-tag ,result)
				     (fcontrol-signal-value ,result)
				     (cl:lambda (&rest ,arguments)
				       (apply (fcontrol-signal-continuation ,result) ,arguments)
				       ;; Continue from unwind-protect if we had a normal exit.
				       ;; Otherwise this is a dead end.
				       (if ,normal-exit?
					   (apply ,continue-from-unwind-protect ,results)
					   (values))))))
	   (set! ,finished-cleanup? t)))
       ;; Continue, but outside of the cleanup body, and only if we had a normal exit from
       ;; the protected and a normal exit from the cleanup.
       (if (and ,normal-exit? ,finished-cleanup?)
	   ;; Continue with the results of the protected form.
	   (apply ,continue-from-unwind-protect ,results)
	   (values)))))

;; %
(define-transform-cps-special-form % (expression environment)
  (define-unique-symbols continue-from-% tag-name handler-name)
  (define-destructuring (expr &key tag (handler ''default-prompt-handler)) (rest expression))
  `(cps-lambda ,continue-from-%
     (multiple-value-call ,continue-from-%
       ,(cps->primary-value
	 tag tag-name
	 `(,(cps->primary-value
	     handler handler-name
	     `((no-cps (run ,continue-from-% ,tag-name (cl:lambda () (cps ,expr)) ,handler-name)))))))))
;; TODO: Merge with above
(defmacro % (expr &key tag (handler ''default-prompt-handler))
  `(no-cps (run #'values ,tag (cl:lambda () (cps ,expr)) ,handler)))

(defvar *tagbody-go-tag* (unique-symbol 'tagbody-go-tag))

;; Tagbody:
;; tags have lexical scope and dynamic extent
;; if there is no matching tag visible to go, results are undefined.
(define-transform-cps-special-form cl:tagbody (expression environment)
  (define-destructuring (untagged-statements . tagged-forms) (parse-tagbody (rest expression)))
  (define-unique-symbols continue-from-tagbody go-tag->function-alist continue-from-run handler)
  (define tags (map first tagged-forms))

  (define go-tag->function-alist-form
    `(list ,@(map (lambda (tag statements next-tag)
		    `(cons ',tag (cl:lambda ()
				   (funcall ,(transform-cps
					      `(progn
						 ,@statements
						 ,@(when next-tag
						     `((funcall (alist-ref ,go-tag->function-alist ',next-tag))))))
					    #'values))))
		  tags
		  (map rest tagged-forms)
		  (append (rest tags) (list nil)))))

  `(cps-lambda ,continue-from-tagbody
     (let ((,go-tag->function-alist)
	   (,continue-from-run (cl:lambda (&rest results) (declare (ignore results)) (funcall ,continue-from-tagbody nil)))
	   (,handler))
       (setq ,go-tag->function-alist ,go-tag->function-alist-form)
       (setq ,handler
	     (cl:lambda (tag continue-from-go)
	       (declare (ignore continue-from-go))
	       (let ((tag-thunk (alist-ref ,go-tag->function-alist tag)))
		 (if tag-thunk
		     ;; Ignore results from tag-thunk
		     (run (lambda results (declare (ignore results)) nil)
			  *tagbody-go-tag*
			  tag-thunk
			  ,handler)
		     ;; If we don't have go-tag, then we need to re-throw
		     (throw *fcontrol-tag*
		       (make-fcontrol-signal
			*tagbody-go-tag*
			tag
			;; Continuation is irrelevant since either
			;;   an outer tagbody handles this and sets the continuation
			;;   no tagbody is found with the given tag.
			(lambda _ (error "Should not happen."))))))))

       (run ,continue-from-run
	    *tagbody-go-tag*
	    (cl:lambda ()
	      (funcall ,(transform-cps `(progn
					  ,@untagged-statements
					  ,@(unless (empty? tags)
					      `((funcall (alist-ref ,go-tag->function-alist ',(first tags)))))))
		       #'values))
	    ,handler))))


(define-transform-cps-special-form cl:go (expression environment)
  (define-unique-symbols continue-from-go)
  (define-destructuring (tag) (rest expression))
  ;; TODO: check lexical body for tag names
  `(no-cps
    (cps-lambda ,continue-from-go
      (funcall ,(transform-cps `(fcontrol *tagbody-go-tag* ',tag)) ,continue-from-go))))


;; TODO:
;; Labels
;; FLet
;; Eval-when
;; Macrolet
;; symbol-Macrolet
;; locally
;; progv
;; load-time-value
;; multiple-value-prog1


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

(defmacro no-cps (expr) expr)

(defmacro cps-form-equal? (form)
  (let ((results (unique-symbol 'results)))
    `(let ((,results (multiple-value-list ,form)))
       (values (equal? (multiple-value-list (cps ,form)) ,results)
	       ,results))))


;; atoms
(assert (cps-form-equal? 1))
(assert (cps-form-equal? (no-cps (values 1 2 3))))
(assert (cps-form-equal? (no-cps (cps (values 1 2 3)))))
;; Function
(assert (cps-form-equal? #'+))
;; Quote
(assert (cps-form-equal? '(the quick brown fox)))

;; function application
(assert (cps-form-equal? (values 1 2 3 4 5)))
(assert (cps-form-equal? (+ (values 1 2) (values 3 4))))
(assert (cps-form-equal? (+ 1 2 (+ 3 4))))

(assert (equal? (scm
		  (let* ((xs ())
			 (push-v (lambda (x) (push x xs) x)))
		    (list
		     (% (+
			 ;; Called once
			 (push-v 1)
			 ;; Called each time continue is called. (twice)
			 (push-v (fcontrol :pause :value))
			 ;; Called each time continue is called. (twice)
			 (push-v 3))
			:tag :pause
			:handler
			;; Called once.
			(lambda (value continue)
			  ;; (:VALUE (+ 1 2 3) (+ 1 3 3)) => (:value 6 7) 
			  (list value (continue 2) (continue 3))))
		     (nreverse xs))))
		'((:VALUE 6 7) (1 2 3 3 3))))


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


;; (progn forms... (fcontrol ...))
(assert (equal? (scm
		  (% (progn (fcontrol :abort :value))
		     :tag :abort
		     :handler
		     (lambda (v k)
		       (list v
			     (multiple-value-list (k 1 2 3))
			     (multiple-value-list (k 4 5 6))))))
		'(:value (1 2 3) (4 5 6))))
;; (progn forms... (fcontrol ...) forms...)
(assert (equal? (scm
		  (let* ((vs ())
			 (v (lambda (x) (push x vs) x)))
		    (list (% (progn (v (fcontrol :abort :value)) (v 2) (v 3))
			     :tag :abort
			     :handler
			     (lambda (v k)
			       ;; vs: (3 2 1)
			       (k 1)
			       ;; vs: (3 2 :one 3 2 1)
			       (k :one)
			       ;; :value
			       v))
			  ;; (3 2 :one 3 2 1)
			  vs)))
		'(:VALUE (3 2 :ONE 3 2 1))))

;; Let
(assert (cps-form-equal? (let () (values 1 2 3))))
(assert (cps-form-equal? (let ((a 1) (b :steak)) (values a b))))
(assert (cps-form-equal? (let ((a (values 1 2 3)) (b (values :steak :sauce))) (values a b))))
;; Verifies that names aren't visible, but causes a warning
;; (assert (not (ignore-errors (cps (let ((a 1) (b a)) (values a b))))))


(assert (equal? (scm
		  (% (let ()
		       (fcontrol :abort :value))
		     :tag :abort
		     :handler
		     (lambda (v k)
		       (list v (multiple-value-list (k 1 2 3))))))
		'(:value (1 2 3))))

(assert (equal? (scm
		  (define vs ())
		  (define (v x) (push x vs) x)
		  (list (% (let ((binding1 (v 1))
				 (binding2 (v (fcontrol :abort :value)))
				 (binding3 (v 3)))
			     (values binding1 binding2 binding3))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list
			      ;; :value
			      v
			      ;; (1 2 3)
			      (multiple-value-list (k 2 :ignored))
			      ;; (1 :two 3)
			      (multiple-value-list (k :two :ignored)))))
			(nreverse vs)))
		'((:VALUE (1 2 3) (1 :TWO 3)) (1 2 3 :TWO 3))))


;; LET*
(assert (cps-form-equal? (let* () (values 1 2 3))))
(assert (cps-form-equal? (let* ((a 1) (b (1+ a)) (c (1+ b)))
			   c
			   (values a b))))
(assert (cps-form-equal? (let* ((a 1) b (c (1+ a)))
			   b
			   (values a c))))


(assert (equal? (scm
		  (% (let* ()
		       (fcontrol :abort :value))
		     :tag :abort
		     :handler
		     (lambda (v k)
		       (list v (multiple-value-list (k 1 2 3))))))
		'(:value (1 2 3))))

(assert (equal? (scm
		  (define vs ())
		  (define (v x) (push x vs) x)
		  (list (% (let* ((binding1 (v 1))
				  (binding2 (v (fcontrol :abort :value)))
				  (binding3 (v (+ binding1 binding2))))
			     (values binding1 binding2 binding3))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list
			      ;; :value
			      v
			      ;; (1 2 (+ 1 2))
			      (multiple-value-list (k 2 :ignored))
			      ;; (1 4 (+ 1 4))
			      (multiple-value-list (k 4 :ignored)))))
			(nreverse vs)))
		'((:VALUE (1 2 3) (1 4 5)) (1 2 3 4 5))))


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


(assert (equal? (scm
		  (define vs ())
		  (define (v x) (push x vs) x)
		  (list (% (funcall (cl:lambda (a b c) (values (v a) (v (fcontrol :abort b)) (v c))) 1 :value 3)
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list
			      ;; :value
			      v
			      ;; (1 2 3)
			      (multiple-value-list (k 2))
			      ;; (1 :two 3)
			      (multiple-value-list (k :two)))))
			(nreverse vs)))
		'((:VALUE (1 2 3) (1 :TWO 3)) (1 2 3 :TWO 3))))


;; Setq
(assert (cps-form-equal? (setq)))
(assert (let (a b c)
	  (cps-form-equal? (setq a 1
				 b (1+ a)
				 c (1+ b)))))

(assert (equal? (scm
		  (define vs ())
		  (define (v x) (push x vs) x)
		  (let (a b c)
		    (list (% (setq a (v :a)
				   b (v (fcontrol :abort :value))
				   c (v b))
			     :tag :abort
			     :handler (lambda (v k)
					(list v
					      (list a b c)
					      (k :b :ignored)
					      (list a b c)
					      (k :bee :ignored)
					      (list a b c))))
			  (nreverse vs))))
		'((:VALUE (:A NIL NIL) :B (:A :B :B) :BEE (:A :BEE :BEE)) (:A :B :B :BEE :BEE))))


;; If
(assert (cps-form-equal? (if (values t nil nil)
			     (progn 1 2 3)
			     ;; Unreachable
			     (values 4 5 6))))
(assert (cps-form-equal? (if (values nil t nil)
			     (progn 1 2 3)
			     (values 4 5 6))))

(assert (equal? (scm
		  (define vs ())
		  (define (v x) (push x vs) x)
		  (list (% (if (v (fcontrol :abort :value))
			       (v :true)
			       (v :false))
			   :tag :abort
			   :handler (lambda (v k)
				      (list v (k t :ignored) (k nil :ignored))))
			(nreverse vs)))
		'((:value :true :false)
		  (t :true nil :false))))


;; The
(assert (cps-form-equal? (the (values number string &optional) (values 3 "string"))))
(assert (equal? (scm
		  (list (% (the (values number string &optional) (fcontrol :abort :value))
			   :tag :abort
			   :handler (lambda (v k)
				      (list v (multiple-value-list (k 3 "hello")))))))
		'((:VALUE (3 "hello")))))



;; Multiple-value-call
(assert (cps-form-equal? (multiple-value-call (values #'list 2 3 4) (values) (values))))
(assert (cps-form-equal? (multiple-value-call (values #'list 2 3 4))))
(assert (cps-form-equal? (multiple-value-call (values #'values 2 3 4) (values 1 2) (values 3 4) (values 5 6))))


(assert (equal?
	 (scm
	   (define vs ())
	   (define (v x) (push x vs) x)
	   (list
	    (% (multiple-value-call list (values (v 1) (v 2)) (fcontrol :abort :value) (values (v 5) (v 6)))
	       :tag :abort
	       :handler
	       (lambda (v k)
		 (list v (k 3 4) (k :three :four))))
	    (nreverse vs)))
	 '((:VALUE (1 2 3 4 5 6) (1 2 :THREE :FOUR 5 6))
	   (1 2 5 6 5 6))))


;; Block/Return-from
(assert (cps-form-equal? (block blah 1)))
(assert (cps-form-equal? (block blah (values 1 2 3))))
(assert (cps-form-equal? (block outer (block inner (return-from outer :inner)) (values 1 2 3))))
(assert (cps-form-equal? (block blah (values 1 (return-from blah (values 2 :two :dos)) 3))))

(assert (cps-form-equal? (scm (block name
				(let ((f (lambda () (return-from name :ok))))
				  (f))))))

;;Error block NAME no longer exists, todo: better error message for cps
#;
(scm ((block name
	(let ((f (lambda () (return-from name (lambda () :ok)))))
	  f))))

;;Error:
#;(return-from name)
#;(cps (return-from name))


(assert (equal? (scm
		  (% (block name (fcontrol :abort :value))
		     :tag :abort
		     :handler (lambda (value k)
				(list value (multiple-value-list (k 1 2 3))))))
		'(:VALUE (1 2 3))))
(assert (equal? (scm
		  (% (block name (return-from name (fcontrol :abort :value)) (error "unreached"))
		     :tag :abort
		     :handler (lambda (value k)
				(list value (multiple-value-list (k 1 2 3))))))
		'(:VALUE (1 2 3))))
(assert (equal? (scm
		  (% (block name
		       (fcontrol :abort :value)
		       (return-from name (values 1 2 3))
		       (error "unreached"))
		     :tag :abort
		     :handler (lambda (value k)
				(list value (multiple-value-list (k))))))
		'(:VALUE (1 2 3))))
(assert (equal? (scm
		  (% (block name
		       (let ((f (cl:lambda () (return-from name (values 1 2 3)))))
			 (fcontrol :abort :value)
			 (f)
			 (error "unreached")))
		     :tag :abort
		     :handler (lambda (value k)
				(list value (multiple-value-list (k))))))
		'(:VALUE (1 2 3))))
(assert (equal? (scm
		  (% (block name
		       (let ((f (cl:lambda () (fcontrol :abort :value) (return-from name (values 1 2 3)))))
			 (f)
			 (error "unreached")))
		     :tag :abort
		     :handler (lambda (value k)
				(list value (multiple-value-list (k))))))
		'(:VALUE (1 2 3))))

;; return-from unwinds unwind-protect forms
(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (block name
				   (v :start)
				   (unwind-protect
					(v (return-from name (v :returning)))
				     (v :cleanup))
				   (v :after))
				 (nreverse vs)))))


;; Labels
;; FLet
;; Eval-when
;; Macrolet
;; symbol-Macrolet
;; locally

;; tagbody/go
(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (tagbody (v :u1) (v :u2))
				 (nreverse vs)))))

(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (print (list 'pushing x)) (push x vs) (print (list 'vs-value vs)) x)
			   (list
			    (v 'before)
			    (tagbody
			       (v :ut1) (v :ut2)  
			     tag1 (v :t1) (v :t11)
			     tag2 (v :t2) (v :t22))
			    (v 'after))
			   (nreverse vs))))


(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list
			    (tagbody
			       (v :ut1) (go tag2) (v :ut2)  
			     tag1 (v :t1) (go end) (v :t11)
			     tag2 (v :t2) (go tag1) (v :t22)
			     end (v :end))
			    (nreverse vs)))))




(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (tagbody
				    (v :u1)
				  :t1
				    (v :t1)
				  :t2
				    (v :t2))
				 (nreverse vs)))))

(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (tagbody
				    (v :u1)
				    (go :t2)
				  :t1
				    (v :t1)
				    (go :end)
				  :t2
				    (v :t2)
				    (go :t1)
				  :end
				    (v :end))
				 (nreverse vs)))))

(assert (equal? (scm
		  (def vs ())
		  (def (v x) (push x vs) x)
		  (list (% (tagbody
			      (v :u1)
			      (go :t2)
			    :t1
			      (v :t1)
			      (go :end)
			    :t2
			      (v :t2)
			      (v (fcontrol :abort :value))
			      (go :t1)
			    :end
			      (v :end))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list v (k :resume) (k :resume))))
			(nreverse vs)))
		'((:VALUE NIL NIL) (:U1 :T2 :RESUME :T1 :END :RESUME :T1 :END))))

;; Nested tagbody
(assert (equal? (scm
		  (def vs ())
		  (def (v x) (push x vs) x)
		  (list (% (tagbody
			      (v :outer-u1)
			    :outer-t1
			      (v :outer-t1)
			      (tagbody
				 (v :inner-u1)
				 (go :inner-t2)
			       :inner-t1
				 (v :t1)
				 (go :outer-t2)
			       :inner-t2
				 (v :t2)
				 (v (fcontrol :abort :value))
				 (go :inner-t1))
			    :outer-t2
			      (v :outer-t2)
			      (go :end)
			    :end
			      (v :end))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list v (k :resume) (k :resume))))
			(nreverse vs)))
		'((:VALUE NIL NIL)
		  (:OUTER-U1 :OUTER-T1 :INNER-U1 :T2
		   :RESUME :T1 :OUTER-T2 :END
		   :RESUME :T1 :OUTER-T2 :END))))


;; progv

;; unwind-protect
(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (unwind-protect
				      (v :protected)
				   (v :cleanup))
				 (nreverse vs)))))

(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (catch :tag
				   (unwind-protect
					(progn
					  (v :protected)
					  (throw :tag :thrown-value)
					  (v :unreached))
				     (v :cleanup)))
				 (nreverse vs)))))


(assert (equal? (scm
		  (def vs ())
		  (def (v x) (push x vs) x)
		  (% (unwind-protect (progn (v 'protected) (values 1 2 3))
		       (v 'cleanup)
		       (fcontrol :escape-cleanup :value)
		       (v 'resume-cleanup))
		     :tag :escape-cleanup
		     :handler
		     (lambda (value k)
		       (v 'handler)
		       (list value
			     (multiple-value-list (k))
			     (multiple-value-list (k))
			     (nreverse vs)))))
		'(:VALUE (1 2 3) (1 2 3)
		  (PROTECTED CLEANUP HANDLER RESUME-CLEANUP RESUME-CLEANUP))))



;; Error, tried to re-eneter unwind-protect
(assert (equal? (scm
		  (def vs ())
		  (def (v x) (push x vs) x)
		  (ignore-errors
		   (% (unwind-protect
			   (progn
			     (v :protected)
			     (fcontrol :tag :thrown-value)
			     (v :unreached))
			(v :cleanup))
		      :tag :tag
		      :handler
		      (lambda (_ k)
			(k))))
		  (nreverse vs))
		'(:PROTECTED :CLEANUP)))

(assert (equal?
	 ;; Conditions unwind unwind-protect
	 (let (cleanup?)
	   (ignore-errors
	    (cps (unwind-protect
		      (error "error")
		   (set! cleanup? t))))
	   cleanup?)
	 (let (cleanup?)
	   (ignore-errors
	    (unwind-protect
		 (error "error")
	      (set! cleanup? t)))
	   cleanup?)))

;; GO's unwind
(assert (cps-form-equal?
	 (scm
	   (def vs ())
	   (def (v x) (push x vs) x)

	   (tagbody
	      (unwind-protect (progn (v :u1-protected) (go :t2))
		(v :u1-cleanup))
	    :t1 (unwind-protect (progn (v :t1-protected) (go :end))
		  (v :t1-cleanup))
	    :t2 (unwind-protect (progn (v :t2-protected) (go :t1))
		  (v :t2-cleanup))
	    :end)
	   (nreverse vs))))


;; catch/throw
(assert (cps-form-equal? (catch :tag (throw :tag (values 1 2 3)) (error "unreached"))))
(assert (cps-form-equal? (catch :tag (throw (throw :tag (values :one :two :three)) (values 1 2 3)) (error "unreached"))))
(assert (cps-form-equal? (catch :tag (catch (throw :tag (values :one :two :three)) (values 1 2 3)) (error "unreached"))))
(assert (cps-form-equal? (catch (values :outer :bouter)
			   (catch (values :inner :binner)
			     (throw :outer (values :one :two :three))
			     (error "unreached"))
			   (error "unreached"))))

(assert (equal? (scm
		  (% (progn
		       (catch :tag
			 (fcontrol :abort :value)
			 (print 'throwing)
			 (throw :tag :value))
		       (values 1 2 3))
		     :tag :abort
		     :handler (lambda (v k)
				(print 'catching)
				(list v (multiple-value-list (k))))))
		'(:VALUE (1 2 3))))

(assert (equal? (scm
		  (% (catch :outer
		       (let ((inner-results
			       (multiple-value-list
				(catch :inner
				  (fcontrol :abort :value)
				  (throw :inner (values 1 2 3))
				  (error "not reached")))))
			 (throw :outer inner-results)
			 (error "not reached")))
		     :tag :abort
		     :handler
		     (lambda (v k)
		       (list v (k)))))
		'(:VALUE (1 2 3))))

;; Error throwing to tag
#;
(cps (progn
       (catch :tag
	 (throw :tag :value))
       (throw :tag :value)))



;; load-time-value
;; multiple-value-prog1




(assert (equal? (% (list (fcontrol :abort :one) 2 3)
		   :tag :abort
		   :handler
		   (cl:lambda (value continuation)
		     (list value (funcall continuation 1))))
		'(:one (1 2 3))))

;; Re-establish inner prompts when continuing from an outer prompt:
(assert (equal? (% (% (list (fcontrol :outer-abort :one) (fcontrol :inner-abort :two) 3)
		      :tag :inner-abort
		      :handler
		      (cl:lambda (value continuation)
			(declare (ignore value))
			(funcall continuation 2)))
		   :tag :outer-abort
		   :handler
		   (cl:lambda (value continuation)
		     (declare (ignore value))
		     (funcall continuation 1)))
		'(1 2 3)))

;; Simple-exit example
(def (product . numbers)
  (% (let recurse ((numbers numbers))
       (cond
	 ((empty? numbers) 1)
	 (t (define-destructuring (number . rest-of-numbers) numbers)
	    ;; Short-circut if any number is 0
	    (cond
	      ((zero? number) (fcontrol :product :zero))
	      (t (* number (recurse rest-of-numbers)))))))
     :tag :product
     :handler
     (lambda (result _continuation)
       result)))

(assert (= (product 1 2 3 4 5)
	   (* 1 2 3 4 5)))
(assert (eq? :zero (product 0 1 2 3 4 5)))

;; Tree matching
(def ((make-fringe tree))
  (cps
   (progn
     (let recurse ((tree tree))
       (cond
	 ((pair? tree)
	  (recurse (car tree))
	  (recurse (cdr tree)))
	 ((empty? tree) :*)
	 (t
	  (fcontrol :yield tree))))
     (fcontrol :yield ()))))

(def (collect-fringe tree)
  (define leaves ())
  (let recurse ((fringe (make-fringe tree)))
    (% (fringe)
       :tag :yield
       :handler
       (lambda (leaf rest-of-fringe)
	 (cond
	   ((null? leaf) leaves)
	   (t
	    (push leaf leaves)
	    (recurse rest-of-fringe))))))
  leaves)

(def (same-fringe? tree1 tree2)
  (let recurse ((fringe1 (make-fringe tree1))
		(fringe2 (make-fringe tree2)))
    (% (fringe1)
       :tag :yield
       :handler
       (lambda (leaf1 rest-of-fringe1)
	 (% (fringe2)
	    :tag :yield
	    :handler
	    (lambda (leaf2 rest-of-fringe2)
	      (if (eq? leaf1 leaf2)
		  (if (null? leaf1)
		      t
		      (recurse rest-of-fringe1 rest-of-fringe2))
		  nil)))))))

(assert (equal? (collect-fringe '((1 2) ((3 (4 5)) (6 7))))
		'(7 6 5 4 3 2 1)))
(assert (same-fringe? '((1 2) ((3) (4 5)))
		      '((1 . 2) . ((3 . ()) . (4 . 5)))))
(assert (not (same-fringe? '((1 2) ((3) (4 5)))
			   '((1 2) ((3 4) (4 5))))))



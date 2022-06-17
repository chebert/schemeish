(in-package :schemeish.backend)

(install-syntax!)

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

(def (full-ordinary-lambda-list->function-argument-list-form full-lambda-list)
  (let (rest-provided?
	;; Required arguments are symbols
	required-arguments
	;; Optional arguments are lists
	optional-arguments)
    (map-ordinary-lambda-list
     (lambda (key parameter)
       (ecase key
	 (:positional (push parameter required-arguments))
	 (:optional
	  (define-destructuring (name default-value provided?) parameter)
	  (push `(when ,provided? (list ,name)) optional-arguments))
	 (:keyword)
	 (:key
	  (unless rest-provided?
	    (define-destructuring (name default-value provided?) parameter)
	    (push `(when ,provided? (list ,(make-keyword name) ,name)) optional-arguments)))
	 (:rest
	  (set! rest-provided? t)
	  (push parameter optional-arguments))
	 (:aux ())))
     full-lambda-list)
    `(list* ,@(nreverse required-arguments) (nconc ,@(nreverse optional-arguments)))))

(assert (equal? (with-readable-symbols
		  (full-ordinary-lambda-list->function-argument-list-form (full-ordinary-lambda-list '(p1 p2 p3 &optional o1 o2 o3 &key k1 k2 k3 &aux aux1 aux2 aux3))))
		'(list* P1 P2 P3
		  (NCONC (WHEN O1-PROVIDED? (LIST O1)) (WHEN O2-PROVIDED? (LIST O2))
		   (WHEN O3-PROVIDED? (LIST O3)) (WHEN K1-PROVIDED? (LIST :K1 K1))
		   (WHEN K2-PROVIDED? (LIST :K2 K2)) (WHEN K3-PROVIDED? (LIST :K3 K3))))))
(assert (equal? (with-readable-symbols
		  (full-ordinary-lambda-list->function-argument-list-form (full-ordinary-lambda-list '(p1 p2 p3 &optional o1 o2 o3 &rest rest &key k1 k2 k3 &aux aux1 aux2 aux3))))
		'(LIST* P1 P2 P3
		  (NCONC (WHEN O1-PROVIDED? (LIST O1)) (WHEN O2-PROVIDED? (LIST O2))
		   (WHEN O3-PROVIDED? (LIST O3)) REST))))

(for-macros
  (defvar *function->cps-function-table* (make-hash-table :weakness :key)))


(defmacro with-lexical-function-names (function-names &body body)
  `(let ((*lexical-context* (alist-update *lexical-context*
					  :lexical-function-names
					  (lambda (names) (append ,function-names names)))))
     ,@body))


(def (lexical-function-name? function-name)
  (member function-name (alist-ref *lexical-context* :lexical-function-names)))

(define (function->cps-function function)
  "Return the cps-function associated with function or the function itself if there is none."
  (hash-ref *function->cps-function-table* function))
(define (set-cps-function! function cps-function)
  "Associate the cps-function with the given function."
  (hash-set! *function->cps-function-table* function cps-function))

(for-macros
  (defvar *continuation* #'values))
(defmacro with-continuation (form continuation)
  "Evaluates form with *continuation* bound to the new continuation.
Under normal circumstances, this means the values of form will be passed to the lambda-list
before evaluating the continuation-body."
  `(cl:let ((*continuation* ,continuation))
     ,form))
(defmacro without-continuation (&body body)
  "Evaluates body with *CONTINUATION* bound to #'VALUES"
  `(cl:let ((*continuation* #'values))
     ,@body))
(defmacro save-continuation (name &body body)
  "Binds name to *CONTINUATION* in body"
  `(let ((,name *continuation*))
     ,@body))

(def (continue-with . values) (*continuation* . values))
(def (continue-with* values) (*continuation* . values))
(defmacro continue-with-values (values-form) `(multiple-value-call *continuation* ,values-form))

(for-macros
  (defvar *transform-cps-special-form-table* (make-hash-table)))
(define (register-transform-cps-special-form symbol transform)
  (hash-set! *transform-cps-special-form-table* symbol transform))

(for-macros
  (register-transformer 'cps
			(make-transformer *transform-cps-special-form-table*
					  'transform-cps-proper-list
					  (lambda (_ expression _) (error "Attempt to compile invalid dotted list: ~S" expression))
					  (lambda (_ expression _) (error "Attempt to compile invalid cyclic list: ~S" expression))
					  'transform-cps-atom)))

(define (transform-cps form)
  (transform 'cps form))
(define (transform-cps* forms)
  (transform* 'cps forms))

(defmacro cps (expr)
  "Transforms EXPR into continuation-passing-style."
  (transform-cps expr))

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

(def (atom->cps expression)
  `(continue-with ,expression))

(def (transform-cps-atom _ expression _)
  "Return a form that evaluates an atom in CPS."
  (atom->cps expression))

(defmacro with-return-to-saved-continuation (continuation &body body)
  "Evaluates from with *CONTINUATION* bound to continuation"
  `(cl:let ((*continuation* ,continuation))
     ,@body))
(defmacro with-primary-value-continuation ((primary-value-name form) &body continuation-body)
  "Evaluates form with *CONTINUATION* bound to continuation-body. 
Only the primary-value is passed to continuation body.
If no values are passed to the continuation, the primary-value is NIL."
  (let ((ignored-rest-of-values (unique-symbol 'ignored-rest-of-values)))
    `(with-continuation ,form
       (cl:lambda (&optional ,primary-value-name &rest ,ignored-rest-of-values)
	 (declare (ignore ,ignored-rest-of-values))
	 (declare (ignorable ,primary-value-name))
	 ,@continuation-body))))
(defmacro with-ignored-values-continuation (form &body continuation-body)
  "Evaluates form with *CONTINUATION* bound to continuation-body. Ignores values passed to the continuation."
  (let ((ignored-values (unique-symbol 'ignored-values)))
    `(with-continuation ,form
       (cl:lambda (&rest ,ignored-values)
	 (declare (ignore ,ignored-values))
	 ,@continuation-body))))
(defmacro with-values-continuation ((values-name form) &body continuation-body)
  "Evaluates form with *CONTINUATION* bound to continuation-body. Binds values passed to the continuation to VALUES-NAME."
  `(with-continuation ,form
     (cl:lambda (&rest ,values-name)
       ,@continuation-body)))

(def (eval-arguments->cps-form argument-forms form-proc)
  "Return a form that evalautes argument-forms from left to right in CPS,
before evaluating (form-proc arguments)"
  (let iterate ((arguments ())
		(argument-forms argument-forms))
    (cond
      ((empty? argument-forms) (form-proc (nreverse arguments)))
      (t
       (define-destructuring (argument-form . rest-argument-forms) argument-forms)
       (define-unique-symbols argument)
       (define eval-rest-of-arguments-form (iterate (cons argument arguments) rest-argument-forms))
       `(with-primary-value-continuation (,argument ,(transform-cps argument-form))
	  ,eval-rest-of-arguments-form)))))

(def (apply/cc function-designator arguments)
  (def function (if (symbol? function-designator)
		    (symbol-function function-designator)
		    function-designator))
  (def cps-function (function->cps-function function))
  (if cps-function
      ;; If the function has an associated cps-function, we can just call it.
      (apply cps-function arguments)
      ;; If the function is an ordinary function, we need to deliver its returned values
      ;; to the continuation
      (multiple-value-call *continuation* (apply function arguments))))
(def (funcall/cc function-designator . arguments)
  (apply/cc function-designator arguments))

(def (transform-cps-proper-list _ expression _)
  "Return a form that evaluates function-application in CPS.
Special cases currently exist for funcall and apply."
  (define-destructuring (function-name . argument-forms) expression)
  (define continue (make-symbol (format nil "CONTINUE-FROM ~S" function-name)))
  ;; Evaluate all arguments from left to right
  `(save-continuation ,continue
     ,(eval-arguments->cps-form
       argument-forms
       (cl:lambda (arguments)
	 ;; Set up a catch. If a CPS-FUNCTION is called, it will call the continuation and throw the results
	 ;; Otherwise, it will just return the values
	 (cond
	   ((lexical-function-name? function-name)
	    ;; This is a known cps function, we can call it with continue as the continuation
	    `(with-return-to-saved-continuation ,continue (,function-name ,@arguments)))

	   ;; TODO: Special case higher order functions. funcall/apply/mapcar et. al.
	   ((eq? function-name 'cl:funcall)
	    `(with-return-to-saved-continuation ,continue (funcall/cc ,@arguments)))
	   ((eq? function-name 'cl:apply)
	    `(with-return-to-saved-continuation ,continue (apply/cc ,(first arguments) (list* ,@(rest arguments)))))

	   ((eq? function-name 'dynamic-wind/cc)
	    `(dynamic-wind ,continue ,@arguments))
	   ((eq? function-name 'run/cc)
	    `(run ,continue ,@arguments))

	   ;; Special case values
	   ((eq? function-name 'cl:values)
	    `(funcall ,continue ,@arguments))

	   ;; TODO: Special case: functions which are known to not have CPS-FUNCTIONS
	   
	   (t
	    ;; Otherwise funcall it with continue
	    `(with-return-to-saved-continuation ,continue (funcall/cc ',function-name ,@arguments))))))))

(cps 1)
(cps (+ 1 2))
(cps (+ (print 1) (print 2)))
(cps (list (list 1 2 t 3)))

(define-transform-cps-special-form no-cps (expression environment)
  (second expression))
(defmacro no-cps (expr) expr)

(cps (list (no-cps (funcall *continuation* (list 1 2 t 3)))))

(define-transform-cps-special-form cl:quote (expression environment)
  (atom->cps expression))

(cps (list '(1 2 3)))

(def (progn->cps forms)
  (define-unique-symbols continue-from-progn)
  (def (progn->cps-iteration forms)
    ;; (progn . forms)
    (define-destructuring (cps-form . rest-of-forms) forms)
    (def form (transform-cps cps-form))
    (cond
      ;; (progn form) => form
      ((empty? rest-of-forms)
       `(with-return-to-saved-continuation ,continue-from-progn ,form))
      ;; (progn form . rest-of-forms)
      (t
       `(with-ignored-values-continuation ,form ,(progn->cps-iteration rest-of-forms)))))
  (cond
    ;; (progn) => nil
    ((empty? forms) (atom->cps nil))
    ;; (progn form) => form
    ((empty? (rest forms)) (transform-cps (first forms)))
    ;; (progn . forms)
    (t `(save-continuation ,continue-from-progn
	  ,(progn->cps-iteration forms)))))

(define-transform-cps-special-form cl:progn (expression environment)
  (define forms (rest expression))
  (progn->cps forms))

(cps (progn 1 2 3))
(cps (print (progn 1 2 3)))
(cps (progn (print 1) (print 2) (print 3)))

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
	  (with-return-to-saved-continuation ,continue-from-let
	    ,(progn->cps forms))))
      (t
       (define-destructuring (binding . rest-of-bindings) bindings)
       (define value-form (let-binding-value binding))
       (define binding-value(unique-symbol (format nil "~S ~S " 'let-binding-value-for (let-binding-name binding))))
       ;; Bind value to a unique name
       `(with-primary-value-continuation (,binding-value ,(transform-cps value-form))
	  ,(iterate rest-of-bindings (cons binding-value binding-values))))))
  `(save-continuation ,continue-from-let
     ,(iterate bindings ())))

(cps (let ((a (values 1 2 3))) a))

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
       `(cl:locally ,@declarations
	  (with-continuation
	      ,(progn->cps forms)
	    ,continue-from-let*)))
      ;; Iteration case: evaluate value of next binding, and bind it.
      (t (define-destructuring (binding . rest-of-bindings) bindings)
	 (define name (let-binding-name binding))
	 (define value (let-binding-value binding))
	 ;; Evaluate the next binding's value, binding it to name.
	 `(with-primary-value-continuation (,name ,(transform-cps value))
	    ,(iterate rest-of-bindings)))))
  `(save-continuation ,continue-from-let*
     ,(iterate bindings)))

(cps (let* ((a (values 1 2 3))) a))

(def (function-binding->cps name full-lambda-list body)
  (define default-parameter-assignments ())
  (define lambda-list (map-ordinary-lambda-list
		       (lambda (key parameter)
			 (ecase key
			   (:positional parameter)
			   ((:optional :key)
			    (push `(unless ,(third parameter)
				     (setq ,(first parameter)
					   ,(second parameter)))
				  default-parameter-assignments)
			    (list (first parameter) nil (third parameter)))
			   (:keyword parameter)
			   (:rest parameter)))
		       full-lambda-list))
  (define-values (declarations forms) (parse-declarations body))
  `(,name ,lambda-list
	  ,@declarations
	  ,(progn->cps
	    (append (nreverse default-parameter-assignments) forms))))

(def (lambda->cps ordinary-lambda-list body)
  (define-unique-symbols ordinary-function cps-function)
  (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
  (def cps-function-form (function-binding->cps 'cl:lambda full-lambda-list body))

  (def ordinary-function-form `(cl:lambda ,full-lambda-list
				 ;; Call the underlying cps-function, returning the result to the caller, instead of the continuation.
				 (without-continuation
				   (apply/cc ,cps-function ,(full-ordinary-lambda-list->function-argument-list-form full-lambda-list)))))
  `(cl:let* ((,cps-function ,cps-function-form)
	     ;; The continuation function curried with #'values as the continuation
	     (,ordinary-function ,ordinary-function-form))
     ;; Transform the lambda expression and register it as the CPS-function of the function
     (set-cps-function! ,ordinary-function ,cps-function)
     ;; Return an ordinary function.
     ,ordinary-function))


(define-transform-cps-special-form cl:lambda (expression environment)
  (define-destructuring (ordinary-lambda-list . body) (rest expression))
  `(continue-with ,(lambda->cps ordinary-lambda-list body)))

(cps (funcall (cl:lambda (a b c) (values a b c)) :a :b :c))
(funcall (cps (cl:lambda (a b c) (values a b c))) :a :b :c)


(define-transform-cps-special-form cl:setq (expression environment)
  (def pairs (setq-pairs expression))
  (define-unique-symbols continue-from-setq)
  (define (pairs->cps pairs)
    
    (define (pair->cps pair rest-of-pairs last-pair?)
      (define-destructuring (name value-form) pair)
      (def value-name (unique-symbol (symbolicate name '-value)))
      `(with-primary-value-continuation (,value-name ,(transform-cps value-form))
	 (setq ,name ,value-name)
	 ,(if last-pair?
	      ;; If this is the last pair, call the continuation with the value.
	      `(with-return-to-saved-continuation ,continue-from-setq
		 ,(atom->cps value-name))
	      ;; Otherwise, continue setting pairs.
	      (pairs->cps rest-of-pairs))))

    (define-destructuring (pair . rest-of-pairs) pairs)
    (cond
      ;; Base case: 1 pair remaining
      ((empty? rest-of-pairs) (pair->cps pair rest-of-pairs t))
      ;; Iteration: (setq pair pairs...)
      (t (pair->cps pair rest-of-pairs nil))))

  (cond
    ;; (setq) => nil
    ((empty? pairs) (atom->cps nil))
    ;; (setq pair . pairs...)
    (t `(save-continuation ,continue-from-setq ,(pairs->cps pairs)))))

(let (a b c)
  (list
   (cps (setq))
   (cps (setq a 1))
   a
   (cps (setq b (list a a)
	      c (list b b)))
   (list a b c)))

(define-transform-cps-special-form cl:if (expression environment)
  (define-destructuring (test-form then-form &optional else-form) (rest expression))
  (define-unique-symbols continue-from-if if-test-result)
  `(save-continuation ,continue-from-if
     (with-primary-value-continuation (,if-test-result ,(transform-cps test-form))
       (with-return-to-saved-continuation ,continue-from-if
	 (cl:if ,if-test-result
		,(transform-cps then-form)
		,(transform-cps else-form))))))

(cps (if (print nil) (print 1) (print 2)))

(define-transform-cps-special-form cl:the (expression environment)
  (define-destructuring (type-form value-form) (rest expression))
  (define-unique-symbols results continue-from-the)
  `(save-continuation ,continue-from-the
     (with-values-continuation (,results ,(transform-cps value-form))
       (with-return-to-saved-continuation ,continue-from-the
	 (continue-with-values (the ,type-form (values-list ,results)))))))

(cps (the (values number &optional) 1))
(cps (the (values number string &optional) (values 1 "string")))

(define-transform-cps-special-form cl:function (expression environment)
  (def function-name (second expression))
  (cond
    ((lexical-function-name? function-name)
     ;; If its a lexical function name we need to return an ordinary function, with #'function
     ;; as an associated cps-function. This means we'll want the full lambda list associated
     ;; with defining the function. Also, if the ordinary funciton already exists we should
     ;; grab that.
     ;; For now though
     (error "TODO"))
    ;; Otherwise it's an ordinary atom
    (t (atom->cps expression))))

(define-transform-cps-special-form cl:multiple-value-call (expression environment)
  (define-destructuring (function-form . arguments) (rest expression))
  (define-unique-symbols continue-from-multiple-value-call multiple-value-call-function)

  (def (eval-arguments->cps-form argument-forms form-proc)
    "Return a form that evalautes argument-forms from left to right in CPS,
before evaluating (form-proc argument-lists)"
    (let iterate ((argument-lists ())
		  (argument-forms argument-forms)
		  (index 0))
      (cond
	((empty? argument-forms) (form-proc (nreverse argument-lists)))
	(t
	 (define-destructuring (argument-form . rest-argument-forms) argument-forms)
	 (def argument-list (unique-symbol (format nil "MULTIPLE-VALUE-CALL-ARGUMENT-LIST-~S-" index)))
	 `(with-values-continuation (,argument-list ,(transform-cps argument-form))
	    ,(iterate (cons argument-list argument-lists) rest-argument-forms (1+ index)))))))

  `(save-continuation ,continue-from-multiple-value-call
     ;;  evalaute the primary value of function-form and capture it in FUNCTION
     (with-primary-value-continuation (,multiple-value-call-function ,(transform-cps function-form))
       ;; evaluate the arguments...
       ,(eval-arguments->cps-form
	 arguments
	 (cl:lambda (argument-lists)
	   ;; ...capturing the values of each argument as a list
	   `(with-return-to-saved-continuation ,continue-from-multiple-value-call
	      ;; Apply the function to the appended argument-lists
	      (apply/cc ,multiple-value-call-function (append ,@argument-lists))))))))

(cps (multiple-value-call #'list (values 1 2 3) (values 4 5 6)))

(define-transform-cps-special-form cl:eval-when (expression environment)
  (define-destructuring ((&rest situations) &body body) (rest expression))
  ;; if eval-when is called within a CPS form, then it isn't at the top level
  ;; so :load-toplevel and :compile-toplevel are irrelevant.
  ;; therefore we only need to handle the :execute case.
  (cond
    ((member :execute situations) (progn->cps body))
    (t `(continue-with nil))))

(cps (progn
       (eval-when () (print 'hi))
       (print 'after)))
(cps (progn
       (eval-when (:execute) (print 'hi))
       (print 'after)))

;; Macrolet
(define-transform-cps-special-form cl:macrolet (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  `(cl:macrolet ,definitions
     ,@declarations
     ,(progn->cps body)))

;; symbol-Macrolet
(define-transform-cps-special-form cl:symbol-macrolet (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  `(cl:symbol-macrolet ,definitions
     ,@declarations
     ,(progn->cps body)))

;; locally
(define-transform-cps-special-form cl:locally (expression environment)
  (def body (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  `(cl:locally ,@declarations
     ,(progn->cps forms)))

;; load-time-value
;; continuation barrier around load-time-value's form, since it is evaluated in a different context
(define-transform-cps-special-form cl:load-time-value (expression environment)
  (define-destructuring (form &optional read-only?) (rest expression))
  ;; cl:load-time-value only returns the primary value
  (atom->cps `(cl:load-time-value (without-continuation ,(transform-cps form)) ,read-only?)))

;; multiple-value-prog1: why is this a special form?
(define-transform-cps-special-form cl:multiple-value-prog1 (expression environment)
  (define-destructuring (values-form . forms) (rest expression))
  (define-unique-symbols continue-from-multiple-value-prog1 multiple-value-prog1-results)
  `(save-continuation ,continue-from-multiple-value-prog1
     ;; Evaluate the values form first, saving the results
     (with-values-continuation (,multiple-value-prog1-results ,(transform-cps values-form))
       (with-ignored-values-continuation ,(progn->cps forms)
	 (with-return-to-saved-continuation ,continue-from-multiple-value-prog1
	   (continue-with* ,multiple-value-prog1-results))))))

(define-transform-cps-special-form cl:labels (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))

  (define (labels-binding->cps binding)
    (define-destructuring (name ordinary-lambda-list . body) binding)
    (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
    (function-binding->cps name full-lambda-list body))

  (def function-names (map first definitions))
  (with-lexical-function-names function-names
    `(cl:labels ,(map labels-binding->cps definitions)
       ,@declarations
       ,(progn->cps forms))))

(define-transform-cps-special-form cl:flet (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  
  (define (flet-binding->cps binding)
    (define-destructuring (name ordinary-lambda-list . body) binding)
    (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
    (function-binding->cps name full-lambda-list body))

  (def function-names (map first definitions))
  `(cl:flet ,(map flet-binding->cps definitions)
     ,@declarations
     ,(with-lexical-function-names function-names
	(progn->cps forms))))

;; Dynamic forms

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
with the rest-of-thunk dynamically embedded in the prompt."
  ;; Execute the thunk, catching fcontrol-signals.
  (def thunk-result (let ((*prompt-established?* t))
		      (catch *fcontrol-tag*
			;; Call thunk returning results as a list.
			(multiple-value-list (without-continuation (funcall/cc thunk))))))
  (cond
    ;; If the result is an fcontrol-signal, it means an (FCONTROL value) form was encountered.
    ((fcontrol-signal? thunk-result)
     ;; TODO: Destructure structures
     (define fcontrol-tag (fcontrol-signal-tag thunk-result))
     (define value (fcontrol-signal-value thunk-result))
     (define rest-of-thunk (fcontrol-signal-continuation thunk-result))
     (cond
       ((eq? tag fcontrol-tag)
	;; This is the tag we are trying to catch. Invoke the handler (which may or may not be in CPS).
	;; on the signal's value and continuation
	;; Return the results to whoever called run.
	(with-return-to-saved-continuation continue-from-run
	  (funcall/cc handler value rest-of-thunk)))
       (*prompt-established?*
	;; If a prompt is established, then tag may be meant for an outer run.
	;; re-throw with setting the prompt up again as part of the continuation
	(throw *fcontrol-tag*
	  (make-fcontrol-signal
	   fcontrol-tag
	   value
	   ;; Return a modified continuation
	   (lambda arguments
	     ;; When resumed, set up a prompt around the rest-of-thunk.
	     (run continue-from-run
		  tag
		  (lambda () (apply rest-of-thunk arguments))
		  handler)))))
       ;; If this is not our tag, and there is no outer prompt established, then we have an error.
       (t (error "Outermost prompt ~S: No enclosing prompt found for fcontrol signal with tag ~S" tag fcontrol-tag))))
    ;; Otherwise, we encountered a normal exit: return the results to whoever called run.
    (t (apply continue-from-run thunk-result))))

(def (default-prompt-handler value _continuation) value)
(defvar *default-prompt-tag* (unique-symbol 'default-prompt-tag))


(defmacro % (expr &key (tag '*default-prompt-tag*) (handler '(function default-prompt-handler)))
  "Sets up a prompt with the given tag"
  `(no-cps (cps (run/cc ,tag (cl:lambda () ,expr) ,handler))))
(defmacro catch/cc ((&key (tag '*default-prompt-tag*) (handler '(function default-prompt-handler))) &body body)
  "Equivalent to (% (progn body...) :tag tag :handler handler)"
  `(% (progn ,@body) :tag ,tag :handler ,handler))

;; (fcontrol tag values-form)
;; Evaluates tag, then value.
;; throws to tag with (make-fcontrol-signal tag value current-continuation), aborting the current continuation
(define-transform-cps-special-form fcontrol (expression environment)
  (define-unique-symbols continue-from-fcontrol)
  (define arguments (rest expression))
  `(save-continuation ,continue-from-fcontrol
     ,(eval-arguments->cps-form
       arguments
       (lambda (argument-names)
	 (define-destructuring (tag value) argument-names)
	 `(cond
	    (*prompt-established?*
	     ;; Throw to the nearest established prompt, if one is established
	     (throw *fcontrol-tag* (make-fcontrol-signal ,tag ,value ,continue-from-fcontrol)))
	    (t (error "Attempt to (FCONTROL ~S ~S) without an established prompt. See %, RUN." ,tag ,value)))))))
(defmacro fcontrol (tag value)
  "Evaluates tag, then value, throwing tag, value, and the current continuation
to the dynamically nearest established RUN, aborting the current continuation.
If FCONTROL is evaluated in a non-CPS function, it issues a warning and evaluates to VALUE."
  (declare (ignore tag value))
  `(error "Attempt to FCONTROL in a non-CPS environment."))
(defmacro throw/cc (&optional value (tag '*default-prompt-tag*))
  "Equivalent to (FCONTROL TAG VALUE)."
  `(fcontrol ,tag ,value))

(define-transform-cps-special-form cl:block (expression environment)
  (define-destructuring (name . forms) (rest expression))
  (define-unique-symbols block-tag)
  (define lexical-context
    (alist-update *lexical-context*
		  :block-tag-alist
		  (cut (alist-set _ name block-tag))))
  (let ((*lexical-context* lexical-context))
    (transform-cps
     `(run/cc ',block-tag
	      (cl:lambda () ,@forms)
	      (cl:lambda (results k)
		(declare (ignore k))
		(values-list results))))))


(define-transform-cps-special-form cl:return-from (expression environment)
  (define-destructuring (name &optional values-form) (rest expression))
  (define block-tag (alist-ref (alist-ref *lexical-context* :block-tag-alist) name))
  (define-unique-symbols return-from-values)
  (unless block-tag
    (error "Could not find BLOCK named ~S in the current lexical environment." name))
  `(with-values-continuation (,return-from-values ,(transform-cps values-form))
     ,(transform-cps `(fcontrol ',block-tag ,return-from-values))))


(define-transform-cps-special-form cl:catch (expression environment)
  (define-destructuring (tag . forms) (rest expression))
  (transform-cps
   `(run/cc ,tag
	    (cl:lambda () ,@forms)
	    (cl:lambda (results k)
	      (declare (ignore k))
	      (values-list results)))))

(define-transform-cps-special-form cl:throw (expression environment)
  (define-destructuring (tag-form results-form) (rest expression))
  (define-unique-symbols tag results)
  `(with-primary-value-continuation (,tag ,(transform-cps tag-form))
     (with-values-continuation (,results ,(transform-cps results-form))
       ;; Return results to the prompt
       ,(transform-cps `(fcontrol ,tag ,results)))))

(def (rethrow-if-fcontrol-signal signal modify-continuation)
  "If signal is an fcontrol-signal, re-throw it with modify-continuation applied to the signal-continuation."
  (when (fcontrol-signal? signal)
    ;; Rethrow it, but with a modified continuation that calls dynamic-wind around the rest-of-thunk
    (def tag (fcontrol-signal-tag signal))
    (def value (fcontrol-signal-value signal))
    (def rest-of-thunk (fcontrol-signal-continuation signal))
    (throw *fcontrol-tag*
      (make-fcontrol-signal tag value (modify-continuation rest-of-thunk)))))

(def (dynamic-wind continue-from-dynamic-wind before-thunk thunk after-thunk)
  ;; evaluate before-thunk, ignoring results
  (with-ignored-values-continuation (funcall/cc before-thunk)
    ;; A normal-exit occurs when a thunk evaluates without encountering an fcontrol/condition
    (let (thunk-had-normal-exit? thunk-results after-thunk-had-normal-exit?)
      (unwind-protect
	   ;; PROTECTED FORM
	   ;; evaluate thunk
	   (let ((result (catch *fcontrol-tag*
			   ;; evaluate thunk, saving the returned values
			   (set! thunk-results (multiple-value-list (without-continuation (funcall/cc thunk)))))))
	     ;; If we caught an fcontrol-tag from within the thunk, rethrow with a modified continuation.
	     (rethrow-if-fcontrol-signal
	      result
	      ;; Modify rest-of-thunk by wrapping it in an identical dynamic-wind
	      (lambda (rest-of-thunk)
		;; modified continuation:
		(cl:lambda (&rest arguments)
		  (dynamic-wind continue-from-dynamic-wind
				before-thunk
				;; Rest of thunk
				(cl:lambda ()
				  (with-return-to-saved-continuation rest-of-thunk
				    (continue-with* arguments)))
				after-thunk))))
	     
	     ;; if we made it here, thunk had a normal-exit
	     (set! thunk-had-normal-exit? t))

	;; CLEANUP
	;; evaluate after-thunk
	(let ((result (catch *fcontrol-tag* (without-continuation (funcall/cc after-thunk)))))
	  ;; If we caught an fcontrol-signal in the after-thunk
	  (rethrow-if-fcontrol-signal
	   result
	   ;; modify the rest of the after-thunk
	   ;; return-to rest-of-thunk, before returning-to continue-from-dynamic-wind
	   (lambda (rest-of-after-thunk)
	     ;; Modified continuation:
	     (cl:lambda (&rest arguments)
	       (run (if thunk-had-normal-exit?
			(lambda _ (apply continue-from-dynamic-wind thunk-results))
			values)
		    (unique-symbol 'run)
		    (cl:lambda () (apply rest-of-after-thunk arguments))
		    default-prompt-handler)
	       ;; Call the rest of thunk with the arguments
	       #;
	       (with-ignored-values-continuation (apply/cc rest-of-after-thunk arguments)
		 ;; If thunk exited normally, we can return the results to whoever called dynamic-wind.
		 (print 'continuing-from-rest-of-after-thunk)
		 (when thunk-had-normal-exit?
		   (print 'thunk-had-normal-exit)
		   ;; IF thunk had a normal-exit
		   ;; then continue from dynamic-wind with the thunk-results
		   (with-return-to-saved-continuation continue-from-dynamic-wind
		     (print 'returning-from-dynamic-wind)
		     (continue-with* thunk-results)))
		 (when (not thunk-had-normal-exit?)
		   (print 'thunk-did-not-have-normal-exit))))))
	  
	  ;; IF we made it here, after-thunk had a normal-exit
	  (set! after-thunk-had-normal-exit? t)))
      
      ;; If we had a normal exit from the thunk and the after-thunk, we need to call the continuation
      ;; with the thunk-results
      (when (and thunk-had-normal-exit? after-thunk-had-normal-exit?)
	(apply continue-from-dynamic-wind thunk-results)))))

(define-transform-cps-special-form cl:unwind-protect (expression environment)
  (define-destructuring (protected &body cleanup) (rest expression))
  ;; Don't allow re-entry into protected forms.
  (transform-cps
   `(let ((ok? t))
      (dynamic-wind/cc
       (cl:lambda ()
	 (if ok?
	     (set! ok? nil)
	     (error "Attempt to re-enter the protected form of an unwind-protect.")))
       (cl:lambda () ,protected)
       (cl:lambda () ,@cleanup)))))


(defvar *tagbody-go-tag* (unique-symbol 'tagbody-go-tag))
;; Tagbody:
;; tags have lexical scope and dynamic extent
;; if there is no matching tag visible to go, results are undefined.
(define-transform-cps-special-form cl:tagbody (expression environment)
  (define-destructuring (untagged-statements . tagged-forms) (parse-tagbody (rest expression)))
  (define-unique-symbols continue-from-tagbody thunk tagbody-prompt-tag tag-thunk)
  (define tags (map first tagged-forms))

  (define (thunk-form statements)
    "Return a thunk that calls (progn statements...) in continuation-passing style."
    `(cl:lambda () (without-continuation ,(progn->cps statements))))
  
  (define (tag-thunk-form statements next-tag)
    "Return a thunk that evaluates a tag's statements before calling the next-tag's thunk."
    (thunk-form (append statements (list `(funcall ,(tag->function-name next-tag))))))

  (define (last-tag-thunk-form statements)
    "Return a thunk that evaluates a tag's statements."
    (thunk-form statements))

  (define tag->function-name-alist
    (map (lambda (tag) (cons tag (unique-symbol (symbolicate tag '-function))))
	 tags))
  (define (tag->function-name tag)
    (alist-ref tag->function-name-alist tag))
  (define function-names (map tag->function-name tags))
  
  (define (tag->statements tag)
    (alist-ref tagged-forms tag))

  ;; GO needs to throw to a specific tagbody, and the name of a tag-thunk to throw
  (define (extend-lexical-context alist)
    (alist-update alist :tagbody-context-alist
		  (lambda (alist)
		    (append (map (lambda (tag)
				   (cons tag (list tagbody-prompt-tag (tag->function-name tag))))
				 tags)
			    alist))))

  (let* ((*lexical-context* (extend-lexical-context *lexical-context*)))
    (define untagged-thunk-form
      (if (empty? tags)
	  (last-tag-thunk-form untagged-statements)
	  (tag-thunk-form untagged-statements (first tags))))

    (define function-name-assignments
      (append
       (map (lambda (tag next-function-name)
	      `(setq ,(tag->function-name tag) ,(tag-thunk-form (tag->statements tag) next-function-name)))
	    tags
	    (rest tags))
       (map (lambda (tag)
	      `(setq ,(tag->function-name tag) ,(last-tag-thunk-form (tag->statements tag))))
	    (last tags))))
    
    `(let (,@function-names)
       ,@function-name-assignments
       (save-continuation ,continue-from-tagbody
	 (let run-tagbody ((,thunk ,untagged-thunk-form))
	   (let (encountered-go?)
	     (with-primary-value-continuation (,tag-thunk (run *continuation*
							       ',tagbody-prompt-tag
							       ,thunk
							       (cl:lambda (tag-thunk _continue-from-go)
								 (declare (ignore _continue-from-go))
								 (set! encountered-go? t)
								 tag-thunk)))
	       (if encountered-go?
		   (run-tagbody ,tag-thunk)
		   (with-return-to-saved-continuation ,continue-from-tagbody
		     (continue-with nil))))))))))

(define-transform-cps-special-form cl:go (expression environment)
  (define-destructuring (tag) (rest expression))
  (define tag-data (alist-ref (alist-ref *lexical-context* :tagbody-context-alist) tag))
  (cond
    (tag-data
     (define-destructuring (tagbody-prompt-tag function-name) tag-data)
     (transform-cps `(fcontrol ',tagbody-prompt-tag ,function-name)))
    (t (error "Could not find TAG ~S in lexical-context of GO." tag))))


;; progv
;; Progv forms can be re-entered, but the dynamic bindings will no longer be in effect.

;; TODO: It might be nice if we could resume with the dynamic bindings intact.
;; The issue is that we need to grab the current bindings right before exiting the continuation.
;; So that we can re-establish when resuming.
;; Since we only get the continuation after exiting the dynamic context, it's too late.
;; we would need to modify FCONTROL within cps progv forms to grab the current dynamic bindings
;; To do this, each fcontrol signal would need to have a list of dynamic bindings, as which
;; prompt they came from.
;; The alternative is to let dynamic bindings go, and instead rely on new forms that
;; establish fluid bindings.
;; It's hard to say which is the right default, but since I don't use progv that often anyways,
;; I don't have a problem with just dropping them for now.
(define-transform-cps-special-form cl:progv (expression environment)
  (define-destructuring (vars-form vals-form &body forms) (rest expression))
  (define-unique-symbols continue-from-progv vars vals progv-prompt-tag)
  `(save-continuation ,continue-from-progv
     (with-primary-value-continuation (,vars ,(transform-cps vars-form))
       (with-primary-value-continuation (,vals ,(transform-cps vals-form))
	 (with-return-to-saved-continuation ,continue-from-progv
	   ,(transform-cps
	     `(run/cc ',progv-prompt-tag
		      (cl:lambda () (no-cps (progv ,vars ,vals ,(progn->cps forms))))
		      #'default-prompt-handler)))))))

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
(assert (cps-form-equal? (funcall (cl:lambda (a b c) (values a b c)) 1 2 3)))
(assert (equal? (multiple-value-list
		 (funcall (cps (funcall (cl:lambda (f) (cl:lambda (&rest args) (apply f args)))
					(cl:lambda (&rest args) (apply #'values args))))
			  1 2 3))
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

(assert (equal? (scm
		  (% (funcall (cl:lambda (p1 p2 &optional (o1 (fcontrol :abort :o1)) (o2 (fcontrol :abort :o2)))
				(list p1 p2 o1 o2))
			      :p1 :P2 :o1)
		     :tag :abort
		     :handler
		     (cl:lambda (v k)
		       (list v (funcall k :o2) (funcall k :o2-again)))))
		'(:O2 (:P1 :P2 :O1 :O2) (:P1 :P2 :O1 :O2-AGAIN))))


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
(cps (scm ((block name
	     (let ((f (lambda () (return-from name (lambda () :ok)))))
	       f)))))

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

(assert (cps-form-equal? (list
			  (labels ()
			    :value)
			  :after)))

(assert (cps-form-equal? (list
			  (flet ()
			    :value)
			  :after)))


(assert (equal? (% (labels ((f1 (&optional (o1 (fcontrol :abort :value)))
			      (f2 `(f1 ,o1)))
			    (f2 (p)
			      `(f2 ,p)))
		     (f1))
		   :tag :abort
		   :handler (lambda (v k)
			      (list v (funcall k :o1))))
		'(:VALUE (F2 (F1 :O1)))))
(assert (equal? (% (flet ((f1 (&optional (o1 (fcontrol :abort :value)))
			    `(f1 ,o1))
			  (f2 (v) `(f2 ,v)))
		     (list (f1) (f2 :v)))
		   :tag :abort
		   :handler (lambda (v k)
			      (list v (funcall k :o1))))
		'(:VALUE ((F1 :O1) (F2 :V)))))


;; Eval-when
;; Eval-when will never appear as a top-level-form if it is part of a CPS expression
;; Therefore we only test the :execute
(assert (cps-form-equal? (list (eval-when (:execute) (list 1 2 3)) 2 3)))
(assert (cps-form-equal? (list (eval-when (:compile-toplevel :load-toplevel) (list 1 2 3)) 2 3)))


;; Macrolet
(assert (scm
	  (cps-form-equal?
	   (let ((f (cl:lambda (x flag)
		      (macrolet ((fudge (z)
				   `(if flag (list '* ,z ,z) ,z)))
			`(+ ,x
			    ,(fudge x)
			    ,(fudge `(+ ,x 1)))))))
	     (list (f :x nil)
		   (f :x :flag))))))

;; symbol-Macrolet
(assert (cps-form-equal?
	 (list (symbol-macrolet ((garner `(list :attention)))
		 garner)
	       'garner)))

;; locally
(assert (cps-form-equal?
	 (funcall (cl:lambda (y)
		    (declare (special y))
		    (let ((y t))
		      (list y
			    (locally (declare (special y)) y))))
		  nil)))


;; tagbody/go
(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
			   (list (tagbody (v :u1) (v :u2))
				 (nreverse vs)))))

(assert (cps-form-equal? (scm
			   (def vs ())
			   (def (v x) (push x vs) x)
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
		  (list (% (list
			    (v :before)
			    (tagbody
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
			    (v :after))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list v (k :resume) (k :resume))))
			(nreverse vs)))
		'((:VALUE (:BEFORE NIL :AFTER) (:BEFORE NIL :AFTER))
		  (:BEFORE :U1 :T2 :RESUME :T1 :END :AFTER :RESUME :T1 :END :AFTER))))

;; Nested tagbody
(assert (equal? (scm
		  (def vs ())
		  (def (v x) (push x vs) x)
		  (list (% (list
			    (v :before)
			    (tagbody
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
			    (v :after))
			   :tag :abort
			   :handler
			   (lambda (v k)
			     (list v (k :resume) (k :resume))))
			(nreverse vs)))
		'((:VALUE (:BEFORE NIL :AFTER) (:BEFORE NIL :AFTER))
		  (:BEFORE
		   :OUTER-U1 :OUTER-T1 :INNER-U1 :T2
		   :RESUME :T1 :OUTER-T2 :END
		   :AFTER
		   :RESUME :T1 :OUTER-T2 :END
		   :AFTER))))


(assert (cps-form-equal? (scm
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
			   (nreverse vs))))

;; progv
(assert (cps-form-equal? (let ((x 3))
			   (LIST
			    (progv '(x) '(4)
			      (list x (symbol-value 'x)))
			    (list x (boundp 'x))))))

(assert (equal? (scm
		  (% (let ((x 3))
		       (list
			(progv '(x) '(4)
			  (list x (symbol-value 'x)
				(setf (symbol-value 'x) 2)
				(fcontrol :abort :value)
				(boundp 'x)))
			(list x (boundp 'x))))
		     :tag :abort
		     :handler (lambda (v k) (list v (k :resume)))))
		'(:VALUE ((3 4 2 :RESUME NIL) (3 NIL)))))


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

(def (run-thunk-until-normal-exit thunk)
  "Run thunk calling (k v) until a normal exit occurs.
Useful for testing."
  (cps (let recurse ((thunk thunk))
	 (run/cc *default-prompt-tag*
		 thunk
		 (lambda (v k)
		   (recurse (lambda () (k v))))))))

(defvar *vs*)
(def (trace-v x) (push x *vs*) x)
(defmacro with-v-tracing (&body body)
  `(let ((*vs* ()))
     (list ,@body
	   (reverse *vs*))))

;; Attempt to re-enter a protected form
(assert (null (ignore-errors (with-v-tracing
			       (scm (run-thunk-until-normal-exit
				     (cps (lambda ()
					    (trace-v :before)
					    (unwind-protect (trace-v (throw/cc (trace-v :protected)))
					      (trace-v :cleanup))
					    (trace-v :after)))))))))

;; Resume cleanup with abnormal exit
(assert (equal? (ignore-errors (with-v-tracing
				 (scm (run-thunk-until-normal-exit
				       (cps (lambda ()
					      (trace-v :before)
					      (unwind-protect (error "error")
						(trace-v (throw/cc :cleanup1))
						(trace-v (throw/cc :cleanup2)))
					      (trace-v :after)))))))
		'(:CLEANUP2 (:BEFORE :CLEANUP1 :CLEANUP2))))

;; Resume cleanup with normal exit
(assert (equal? (with-v-tracing
		  (scm (run-thunk-until-normal-exit
			(cps (lambda ()
			       (trace-v :before)
			       (trace-v (unwind-protect (trace-v :protected)
					  (trace-v (throw/cc :cleanup1))
					  (trace-v (throw/cc :cleanup2))))
			       (trace-v :after))))))
		'(:AFTER (:BEFORE :PROTECTED :CLEANUP1 :CLEANUP2 :PROTECTED :AFTER))))

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
;; Error, tried to re-eneter unwind-protect
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
(assert (cps
	 (scm
	   (def (rnd) (list (load-time-value (random 17)) 2))
	   (equal? (rnd) (rnd)))))

;; multiple-value-prog1
(assert (cps-form-equal? (let (temp)
			   (setq temp '(1 2 3))
			   (list (multiple-value-list
				  (multiple-value-prog1
				      (values-list temp)
				    (setq temp nil)
				    (values-list temp)))
				 temp)))) 



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
(def (make-fringe tree)
  (cps
   (lambda ()
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

;; TODO: fluid-let
;; TODO: optimize in-place expressions that are known to not signal controls
;; TODO: Special case higher order functions. funcall/apply/mapcar et. al.
;; TODO: special-case no-cps functions

;; An expression N is non-signaling if, cps-expansion, it is a(n):
;;   N := atom
;;   N := (non-signaling-function arguments...)
;;   N := (cl:quote expr)
;;   N := (cl:function name)
;;   N := (cl:load-time-value expr read-only-p)
;;   N := (cl:lambda parameters body)
;;   N := (cl:progn M)
;;   N := (cl:let bindings declarations... M)
;;   N := (cl:let* bindings declarations... M)
;;   N := (cl:the value-type N)
;;   N := (cl:multiple-value-call non-signaling-function arguments...)
;;   N := (cl:setq pairs...)
;;   N := (cl:if test N_then N_else)
;;   N := (cl:eval-when () body...) | (cl:eval-when (:execute) M)
;;   N := (cl:macrolet bindings M)
;;   N := (cl:symbol-macrolet bindings M)
;;   N := (cl:locally declarations... M)
;;   N := (cl:multiple-value-prog1 M)
;;   N := (cl:labels bindings M)
;;   N := (cl:flet bindings M)
;;   N := (cl:block name M)
;;   N := (cl:catch tag M)
;;   N := (cl:unwind-protect N M)
;;   N := (cl:tagbody M_1 tag1 M_tag1 tag2 M_tag2 ... tagn M_tagn)
;;   N := (cl:progv variables values M)
;;   where M := N_1 N_2 ... N_n
;; Corrollary An expression S signals if, after macro-expansion, it is a(n):
;;   S := (fcontrol tag value)
;;   S := (signaling-function arguments...)


;; Transformation on output rather than special forms
;; (with-ignored-values-continuation N body) => (progn N body)
;; (save-continuation name (with-return-to-saved-continuation name body)) => body
;; (with-primary-value-continuation (name N) body) => (let ((name N)) body)
;; (with-values-continuation (name N) body) => (let ((name (multiple-value-list N))) body)

;; Multi-pass lisp compilation
;;  1st pass: expand all macros with special cases
;;  Subsequent pass: code-transformer


(hash-keys *transform-cps-special-form-table*)


(def (augment-environment-with-macro-bindings bindings environment)
  (trivial-cltl2:augment-environment
   environment :macro
   (map (lambda (binding)
	  (def-destructuring (name lambda-list . body) binding)
	  (list name (trivial-cltl2:enclose
		      (trivial-cltl2:parse-macro name lambda-list body environment)
		      environment)))
	bindings)))
(def (augment-environment-with-symbol-macro-bindings bindings environment)
  (trivial-cltl2:augment-environment environment :symbol-macro bindings))

(def (macroexpand-progn-forms forms environment)
  (map (lambda (form) (macroexpand-all-cps form environment)) forms))
(def (macroexpand-body body environment)
  (def-values (declarations forms) (parse-declarations body))
  (append declarations (macroexpand-progn-forms forms environment)))

(def (expanded-body body environment)
  (def-values (declarations forms) (parse-declarations body))
  (def expanded-body (macroexpand-body body environment))
  (cond
    ((empty? declarations)
     (cond ((empty? forms) nil)
	   ((empty? (rest forms)) (first expanded-body))
	   (t `(progn ,@expanded-body))))
    (t `(locally ,@expanded-body))))


(defvar *cps-special-forms*
  '(NO-CPS FCONTROL
    
    cl:QUOTE cl:FUNCTION cl:PROGN COMMON-LISP:LET cl:LET*
    cl:MACROLET cl:SYMBOL-MACROLET cl:LOCALLY
    COMMON-LISP:LAMBDA

    cl:SETQ cl:IF cl:THE
    cl:MULTIPLE-VALUE-CALL cl:EVAL-WHEN 
    cl:LOAD-TIME-VALUE cl:MULTIPLE-VALUE-PROG1 cl:LABELS cl:FLET cl:BLOCK
    cl:RETURN-FROM cl:CATCH cl:THROW cl:UNWIND-PROTECT cl:TAGBODY cl:GO cl:PROGV))

(def (macroexpand-all-cps cps-form environment)
  "Macroexpand-all for CPS-FORM, respecting Common Lisp special forms as well as CPS special forms.
See *CPS-SPECIAL-FORMS* for a full list of special forms recognized by CPS. 
The resulting expansion will not have any macros remaining. 
Any cl:macrolet and cl:symbol-macrolet will be removed after their expansions have been applied."
  (cond
    ;; CPS-FORM is either, NIL, a symbol or a symbol-macro
    ((symbol? cps-form)
     (def-values (expanded-cps-form macro?) (macroexpand-1 cps-form environment))
     (if macro?
	 ;; If it was a symbol macro, we may need to expand again.
	 (macroexpand-all-cps expanded-cps-form environment)
	 ;; otherwise, we reached a termination point
	 expanded-cps-form))

    ;; CPS-FORM is a non-symbol atom
    ((atom cps-form) cps-form)

    ;; Otherwise cps-form-is a pair
    (t
     (def symbol (first cps-form))
     (cond
       ((member symbol '(cl:quote cl:function)) cps-form)
       ((eq? symbol 'cl:progn)
	`(cl:progn ,@(macroexpand-progn-forms (progn-forms cps-form) environment)))
       ((member symbol '(cl:let cl:let*))
	`(,symbol ,(map (lambda (binding)
			  (list (let-binding-name binding) (macroexpand-all-cps (let-binding-value binding) environment)))
			(let-bindings cps-form))
		  ,@(macroexpand-body (let-body cps-form) environment)))

       ;; Macrolets and symbol-macrolets are replaced with locally since the bindings are no longer relevant after expansion
       ((eq? symbol 'cl:macrolet)
	(let ((environment (augment-environment-with-macro-bindings (macrolet-bindings cps-form) environment)))
	  (expanded-body (macrolet-body cps-form) environment)))
       ((eq? symbol 'cl:symbol-macrolet)
	(let ((environment (augment-environment-with-symbol-macro-bindings (symbol-macrolet-bindings cps-form) environment)))
	  (expanded-body (symbol-macrolet-body cps-form) environment)))
       ((eq? symbol 'cl:locally)
	(expanded-body (locally-body cps-form) environment))
       ((eq? symbol 'cl:lambda)
	;; NOTE: cl-lambda list is a full lambda list after this transformation. kinda nice.
	`(cl:lambda ,(map-ordinary-lambda-list
		      (lambda (key parameter)
			(ecase key
			  ((:positional :rest) parameter)
			  (:keyword parameter)
			  ((:optional :key)
			   (cond
			     ((pair? parameter)
			      (define-destructuring (name &optional default-value provided?) parameter)
			      (list name (macroexpand-all-cps default-value environment)
				    (or provided? (unique-symbol (symbolicate name '-provided?)))))
			     (t (list parameter nil (unique-symbol (symbolicate parameter '-provided?))))))
			  (:aux
			   (cond
			     ((pair? parameter)
			      (define-destructuring (name &optional default-value) parameter)
			      (list name (macroexpand-all-cps default-value environment)))
			     (t (list parameter nil))))))
		      (lambda-parameters cps-form))
	   ,@(macroexpand-body (lambda-body cps-form) environment)))

       ((eq? symbol 'cl:setq)
	`(cl:setq ,@(append-map
		     (lambda (pair)
		       (def-destructuring (name value) pair)
		       (list name (macroexpand-all-cps value environment)))
		     (setq-pairs cps-form))))

       ((eq? symbol 'cl:if)
	`(cl:if ,(macroexpand-all-cps (if-test cps-form) environment)
		,(macroexpand-all-cps (if-then cps-form) environment)
		,(macroexpand-all-cps (if-else cps-form) environment)))

       ((eq? symbol 'cl:the)
	`(cl:the ,(the-value-type cps-form) ,(macroexpand-all-cps (the-form cps-form) environment)))

       ;; TODO
       ((eq? symbol 'cl:multiple-value-call))
       ((eq? symbol 'cl:eval-when))
       ((eq? symbol 'cl:load-time-value))
       ((eq? symbol 'cl:multiple-value-prog1))
       ((eq? symbol 'cl:labels))
       ((eq? symbol 'cl:flet))
       ((eq? symbol 'cl:block))
       ((eq? symbol 'cl:return-from))
       ((eq? symbol 'cl:catch))
       ((eq? symbol 'cl:throw))
       ((eq? symbol 'cl:tagbody))
       ((eq? symbol 'cl:go))
       ((eq? symbol 'cl:progv))
       ((eq? symbol 'cl:unwind-protect))

       ((eq? symbol 'no-cps))
       ((eq? symbol 'fcontrol))

       ;; If it isn't a special form, then it's either a function call or a macro expansion.
       (t
	(def-values (expanded-cps-form macro?) (macroexpand-1 cps-form environment))
	(if macro?
	    (macroexpand-all-cps expanded-cps-form environment)
	    ;; If it's a function call, we need to expand the arguments
	    (progn
	      (def-destructuring (name . arguments) expanded-cps-form)
	      `(,name ,@(map (lambda (form) (macroexpand-all-cps form environment)) arguments)))))))))

(macroexpand-all-cps '(symbol-macrolet ((a :a))
		       'b
		       (macrolet ((m (a) `(list ,a)))
			 (cl:lambda (&optional a (m (m a)))
			   (locally a (m a))
			   (if (setq a (m 1))
			       (the (m 1) (m 2))
			       (m 3)))
			 a))
		     nil)
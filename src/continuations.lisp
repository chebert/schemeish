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
  (def arguments ())
  
  (let (rest-provided?)
    (map-ordinary-lambda-list
     (lambda (key parameter)
       (ecase key
	 (:positional (push `(list ,parameter) arguments))
	 (:optional
	  (define-destructuring (name default-value provided?) parameter)
	  (push `(when ,provided? (list ,name)) arguments))
	 (:keyword)
	 (:key
	  (unless rest-provided?
	    (define-destructuring (name default-value provided?) parameter)
	    (push `(when ,provided? (list (make-keyword ',name) ,name)) arguments)))
	 (:rest
	  (set! rest-provided? t)
	  (push parameter arguments))
	 (:aux ())))
     full-lambda-list))
  (if (empty? arguments)
      ()
      `(nconc ,@(nreverse arguments))))
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
(defmacro save-continuation (name &body body)
  "Binds name to *CONTINUATION* in body"
  `(let ((,name *continuation*))
     ,@body))

(def (continue-to continuation . values) (continuation . values))
(def (continue-with . values) (*continuation* . values))

;; Anytime a continuation is created (lambda (&rest results) ...) the *continuation* needs to be set/updated
;; Anytime a continuation is passed, *continuation* needs to be (let ((*continuation* ,continuation)) ...) instead
;; Anytime a continuation is recieved (lambda (,continuation) ...), (let ((,continuation *continuation*)) ...) instead
;; If a function is called, the continuation argument is now implicit, so what now?
;;    Function style | call style | expected behavior
;;    CPS            | CPS        | results of function are passed to *continuation*, signaled continuation returns to *continuation*
;;    CPS            | non-cps    | same as ^
;;    non-cps        | cps        | results of function are passed to *continuation*


;; Difference:
;; Cps-function will call *continuation* with its results, we should assume that *continuation* is #'values in a non-cps call context
;;(cps-function a b c)
;; when calling regular-function from CPS code, results need to be passed to the continuation.
;;(multiple-value-call *continuation* (regular-function a b c))


;; would this work?
#;
(catch :return-values (multiple-value-call *continuation* (funcall
							   (cl:lambda (a b c)
							     (throw :return-values
							       (funcall *continuation* a b c)))
							   :a :b :c)))
#;
(catch :return-values (multiple-value-call *continuation* (funcall
							   (cl:lambda (a b c)
							     (values a b c))
							   :a :b :c)))

;; Ok now calling functions from a non-cps context
;; (funcall (cl:lambda (a b c) (values a b c)) :a :b :c) ;; no problem, obviously


;;(funcall (cl:lambda (a b c) (throw :return-values (funcall *continuation* a b c))) :a :b :c)
;; 2 problems, no return-values set up, and no *continuation* set up.
;; if nested within a cps context, this will short-circuit the rest of the non-cps code: BAD
;; if not nested within a cps context, this will be an error (no catch for :return-values): BAD 


#;
(let ((fn (let* ((cps-function (cl:lambda (a b c)
				 (throw :return-values
				   (funcall *continuation* a b c))))
		 (fn (cl:lambda (a b c)
		       (let ((*continuation* #'values))
			 (catch :return-values
			   (funcall cps-function a b c))))))
	    (set-cps-function! fn cps-function)
	    fn)))
  (list
   ;; called from ordinary context
   (multiple-value-list (funcall fn :a :b :c))
   ;; called from cps context
   (let ((*continuation* #'list))
     (catch :return-values
       (funcall (function->cps-function fn) :a :b :c)))))

#;
(cps
 (flet ((cps-function (a b c)
	  (values a b c)))
   (declare foo)
   (list
    (no-cps
     (multiple-value-list (cps-function :a :b :c)))
    (no-cps
     (multiple-value-list (funcall (function cps-function) :a :b :c)))
    (multiple-value-list (cps-function :a :b :c))
    (multiple-value-list (funcall (function cps-function) :a :b :c)))))

#;
(flet ((cps-function (a b c)
	 (throw :return-values
	   (funcall *continuation* a b c))))
  (declare foo)
  (let ((ordinary-function (cl:lambda (a b c)
			     (let ((*continuation* #'values))
			       (catch :return-values
				 (funcall cps-function a b c))))))
    (set-cps-function! ordinary-function (function cps-function))
    ;; in cps, setup (function cps-function to return ordinary-function in the rest of the body
    
    (list
     ;; It's not easy to go back and forth between cps and no-cps code lexically.
     ;; honestly it probably shouldn't even be allowed.
     (flet ((cps-function (&rest any) (error "Attempt to call locally defined CPS function CPS-FUNCTION after lexically escaping (via NO-CPS).")))
       (multiple-value-list (cps-function :a :b :c)))
     (flet ((cps-function (&rest any) (error "Attempt to call locally defined CPS function CPS-FUNCTION after lexically escaping (via NO-CPS).")))
       (multiple-value-list (funcall (function cps-function) :a :b :c)))

     (multiple-value-list (cps-function :a :b :c))
     (multiple-value-list (funcall (function->cps-function (function cps-function)) :a :b :c)))))


;; in order to do the right thing, we need to know if we are being called in a CPS call context.
;; how is this determined?


;; so the best we can do is have a lookup table for cps-functions.
;; UNLESS we can assume that cps style functions WILL NOT be called from outside a CPS context.
;; IF WE CAN assume ^ above, is that really a big deal? you can just call (CPS (function-application))
;; and avoid the hash table lookup. It's a slight burden to users of the cps functions,
;; and it will likely be a difficult-to-detect error if they forget to do it: which would be a huge burden to debug.

;; favor correctness over efficiency!

#||
(define (funcall/c continuation function . arguments)
"Call function with arguments, passing the resulting values to continuation. 
If function has an associated CPS-FUNCTION, call it with the given continuation and arguments."
(let ((cps-function (function->cps-function function)))
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
||#

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

(defmacro cps (expr &environment env)
  "Transforms EXPR into continuation-passing-style."
  (macroexpand-1 (transform-cps expr) env))

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
  expression)

(def (transform-cps-atom _ expression _)
  "Return a form that evaluates an atom in CPS."
  expression)

(def (expand-cps environment cps-form)
  "Returns (values expanded-form simple?). Transforms cps-form, macroexpands it in the given environment.
The result is simple if the result is an atom or quote."
  (let ((form (macroexpand (transform-cps cps-form) environment)))
    (values form
	    (or (atom form)
		(and (pair? form) (member (first form) '(cl:quote cl:function)))))))

(def (cps->primary-value environment cps-form primary-value-name continuation-form-proc)
  "Form that evaluates cps-form, passing the results to a continuation
that can accept multiple values. Binds name to the primary value for continuation-form."
  (define-unique-symbols ignored-rest-of-values)
  (define-values (form simple?) (expand-cps environment cps-form))
  (if simple?
      (continuation-form-proc form)
      `(with-continuation ,form
	   (cl:lambda (&optional ,primary-value-name &rest ,ignored-rest-of-values)
	     (declare (ignore ,ignored-rest-of-values))
	     (declare (ignorable ,primary-value-name))
	     ,(continuation-form-proc primary-value-name)))))

(def (eval-arguments->cps-form env argument-forms form-proc)
  "Return a form that evalautes argument-forms from left to right in CPS,
before evaluating (form-proc arguments)"
  (let iterate ((arguments ())
		(argument-forms argument-forms))
    (cond
      ((empty? argument-forms) (form-proc (nreverse arguments)))
      (t
       (define-destructuring (argument-form . rest-argument-forms) argument-forms)
       (cps->primary-value env argument-form
			   (unique-symbol 'argument)
			   (lambda (argument)
			     (iterate (cons argument arguments) rest-argument-forms)))))))

(def (apply/c continuation function-designator arguments)
  (def function (if (symbol? function-designator)
		    (symbol-function function-designator)
		    function-designator))
  (def cps-function (function->cps-function function))
  (if cps-function
      ;; If the function has an associated cps-function, we need to set the *continuation*,
      ;; so it can return to it. 
      (with-continuation (apply cps-function arguments) continuation)
      ;; If the function is an ordinary function, we need to deliver its returned values
      ;; to the continuation
      (multiple-value-call continuation (apply function arguments))))
(def (funcall/c continuation function-designator . arguments)
  (apply/c continuation function-designator arguments))

(def (transform-cps-proper-list _ expression environment)
  "Return a form that evaluates function-application in CPS.
Special cases currently exist for funcall and apply."
  (define-destructuring (function-name . argument-forms) expression)
  (define continue (make-symbol (format nil "CONTINUE-FROM ~S" function-name)))
  ;; Evaluate all arguments from left to right
  `(save-continuation ,continue
     ,(eval-arguments->cps-form
       environment
       argument-forms
       (cl:lambda (arguments)
	 ;; Set up a catch. If a CPS-FUNCTION is called, it will call the continuation and throw the results
	 ;; Otherwise, it will just return the values
	 (cond
	   ((lexical-function-name? function-name)
	    ;; This is a known cps function, we can call it with continue as the continuation
	    `(with-continuation (,function-name ,@arguments) ,continue))

	   ((eq? function-name 'cl:funcall)
	    `(funcall/c ,continue ,@arguments))
	   ((eq? function-name 'cl:apply)
	    `(apply/c ,continue ,(first arguments) (list* ,@(rest arguments))))
	   ((eq? function-name 'cl:values)
	    `(funcall ,continue ,@arguments))
	   (t
	    ;; Otherwise funcall it with continue
	    `(funcall/c ,continue ',function-name ,@arguments)))))))

;; maybe an Intermediate representation would be best. or maybe. it was alright before

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

(def (progn->cps environment forms)
  (define-unique-symbols continue)
  (def (progn->cps-iteration forms)
    (cond
      ;; (progn) => NIL
      ((empty? forms) (atom->cps nil))
      ;; (progn . forms)
      (t
       (define-destructuring (form . rest-of-forms) forms)
       (cond
	 ;; (progn form) => form
	 ((empty? rest-of-forms)
	  (define-values (expanded-form simple?) (expand-cps environment form))
	  `,(if simple?
		`(continue-to ,continue ,expanded-form)
		`(with-continuation ,expanded-form ,continue)))
	 ;; (progn form . rest-of-forms)
	 (t
	  (define-unique-symbols ignored)
	  (define-values (expanded-form simple?) (expand-cps environment form))
	  (if simple?
	      `(progn ,expanded-form ,(progn->cps-iteration rest-of-forms))
	      `(with-continuation ,expanded-form
		 (cl:lambda (&rest ,ignored)
		   (declare (ignore ,ignored))
		   ,(progn->cps-iteration rest-of-forms)))))))))
  `(save-continuation ,continue
     ,(progn->cps-iteration forms)))

(define-transform-cps-special-form cl:progn (expression environment)
  (define forms (rest expression))
  (progn->cps environment forms))


;; TODO: reduce number of progns in output
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
	  (with-continuation
	      ,(progn->cps environment forms)
	      ,continue-from-let)))
      (t
       (define-destructuring (binding . rest-of-bindings) bindings)
       (define value-form (let-binding-value binding))
       ;; Bind value to a unique name
       (cps->primary-value
	environment
	value-form
	(unique-symbol 'binding-value)
	(lambda (binding-value)
	  (iterate rest-of-bindings (cons binding-value binding-values)))))))
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
	      ,(progn->cps environment forms)
	      ,continue-from-let*)))
      ;; Iteration case: evaluate value of next binding, and bind it.
      (t (define-destructuring (binding . rest-of-bindings) bindings)
	 (define name (let-binding-name binding))
	 (define value (let-binding-value binding))
	 ;; Evaluate the next binding's value, binding it to name.
	 (cps->primary-value
	  environment
	  value
	  name
	  (lambda (_)
	    (iterate rest-of-bindings))))))
  `(save-continuation ,continue-from-let*
     ,(iterate bindings)))

(cps (let* ((a (values 1 2 3))) a))

(def (function-binding->cps environment name full-lambda-list body)
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
  (define-values (expanded-forms simple?) (expand-cps
					   environment
					   `(progn
					      ,@(nreverse default-parameter-assignments)
					      ,@forms)))
  `(,name ,lambda-list
	  ,@declarations
	  ,(if simple?
	       `(funcall *continuation* ,expanded-forms)
	       expanded-forms)))

(define-transform-cps-special-form cl:lambda (expression environment)
  (define-destructuring (ordinary-lambda-list . body) (rest expression))
  (define-unique-symbols ordinary-function cps-function)
  (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
  (def cps-function-form (function-binding->cps environment 'cl:lambda full-lambda-list body))
  (def ordinary-function-form `(cl:lambda ,full-lambda-list
				 ;; Call the underlying cps-function, returning the result to the caller.
				 (apply/c #'values ,cps-function ,(full-ordinary-lambda-list->function-argument-list-form full-lambda-list))))
  `(cl:let* ((,cps-function ,cps-function-form)
	     ;; The continuation function curried with #'values as the continuation
	     (,ordinary-function ,ordinary-function-form))
     ;; Transform the lambda expression and register it as the CPS-function of the function
     (set-cps-function! ,ordinary-function ,cps-function)
     ;; Return an ordinary function.
     (continue-with ,ordinary-function)))

(with-continuation
    (cps (funcall (cl:lambda (a b c) (values a b c)) :a :b :c))
  #'list)
(funcall (cps (cl:lambda (a b c) (values a b c))) :a :b :c)


(define-transform-cps-special-form cl:setq (expression environment)
  (def pairs (setq-pairs expression))
  (define-unique-symbols continue-from-setq)
  (define (pairs->cps pairs)
    (define (pair->cps pair rest-of-pairs last-pair?)
      (define-destructuring (name value-form) pair)
      (cps->primary-value
       environment
       value-form
       (unique-symbol (symbolicate name '-value))
       (cl:lambda (value)
	 `(progn
	    (setq ,name ,value)
	    ,(if last-pair?
		 ;; If this is the last pair, call the continuation with the value.
		 `(continue-to ,continue-from-setq ,value)
		 ;; Otherwise, continue setting pairs.
		 (pairs->cps rest-of-pairs))))))

    (cond
      ;; Base case: 0 pairs
      ((empty? pairs) (atom->cps nil))
      (t
       (define-destructuring (pair . rest-of-pairs) pairs)
       (cond
	 ;; Base case: 1 pair remaining
	 ((empty? rest-of-pairs) (pair->cps pair rest-of-pairs t))
	 (t (pair->cps pair rest-of-pairs nil))))))
  (cond
    ((empty? pairs) (atom->cps nil))
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
  (define-unique-symbols continue-from-if)
  `(save-continuation ,continue-from-if
     ,(cps->primary-value
       environment
       test-form
       (unique-symbol 'test-result)
       (cl:lambda (test)
	 `(with-continuation
	      (cl:if ,test
		     ,(transform-cps then-form)
		     ,(transform-cps else-form))
	      ,continue-from-if)))))

(cps (if (print nil) (print 1) (print 2)))

(define-transform-cps-special-form cl:the (expression environment)
  (define-destructuring (type-form value-form) (rest expression))
  (define-unique-symbols results continue-from-the)
  (define-values (expanded-value simple?) (expand-cps environment value-form))
  (if simple?
      `(the ,type-form ,expanded-value)
      `(let ((,continue-from-the *continuation*))
	 (with-continuation ,(transform-cps value-form)
	   (cl:lambda (&rest ,results)
	     (multiple-value-call ,continue-from-the (the ,type-form (values-list ,results))))))))

(cps (the number 1))
(cps (the (values number string &optional) (values 1 "string")))

(define-transform-cps-special-form cl:function (expression environment)
  (def name (second expression))
  (define-unique-symbols continue-from-function function cps-function)
  (if (lexical-function-name? name)
      `(cps-lambda ,continue-from-function
		   (let* ((,cps-function (function ,name))
			  (,function (cl:lambda (&rest arguments) (apply ,cps-function #'values arguments))))
		     (set-cps-function! ,function ,cps-function)
		     (continue-to ,continue-from-function ,function)))
      expression))

(define-transform-cps-special-form cl:multiple-value-call (expression environment)
  (define-destructuring (function-form . arguments) (rest expression))
  (define-unique-symbols continue-from-multiple-value-call)

  (def (eval-arguments->cps-form argument-forms form-proc)
    "Return a form that evalautes argument-forms from left to right in CPS,
before evaluating (form-proc arguments)"
    (let iterate ((argument-lists ())
		  (argument-forms argument-forms))
      (cond
	((empty? argument-forms) (form-proc (nreverse argument-lists)))
	(t
	 (define-destructuring (argument . rest-argument-forms) argument-forms)
	 (define-unique-symbols argument-list)
	 (define-values (form simple?) (expand-cps environment argument))
	 `(with-continuation ,(if simple?
				  `(continue-with ,form)
				  form)
	    (cl:lambda (&rest ,argument-list)
	      ,(iterate (cons argument-list argument-lists) rest-argument-forms)))))))

  `(let ((,continue-from-multiple-value-call *continuation*))
     ;;  evalaute the primary value of function-form and capture it in FUNCTION
     ,(cps->primary-value
       environment
       function-form
       (unique-symbol 'function)
       (cl:lambda (function)
	 ;; evaluate the arguments...
	 (eval-arguments->cps-form
	  arguments
	  (cl:lambda (argument-lists)
	    ;; ...capturing the values of each argument as a list
	    ;; Apply the function to the appended argument-lists
	    `(apply/c ,continue-from-multiple-value-call ,function (nconc ,@argument-lists))))))))

(cps (multiple-value-call #'list (values 1 2 3) (values 4 5 6)))

(define-transform-cps-special-form cl:eval-when (expression environment)
  (define-destructuring ((&rest situations) &body body) (rest expression))
  ;; if eval-when is called within a CPS form, then it isn't at the top level
  ;; so :load-toplevel and :compile-toplevel are irrelevant.
  ;; therefore we only need to handle the :execute case.
  (cond
    ((member :execute situations) (transform-cps `(progn ,@body)))
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
     ,(transform-cps `(progn ,@body))))

;; symbol-Macrolet
(define-transform-cps-special-form cl:symbol-macrolet (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  `(cl:symbol-macrolet ,definitions
     ,@declarations
     ,(transform-cps `(progn ,@body))))

;; locally
(define-transform-cps-special-form cl:locally (expression environment)
  (def body (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  `(cl:locally ,@declarations
     ,(transform-cps `(progn ,@forms))))

;; load-time-value
;; continuation barrier around load-time-value's form, since it is evaluated at a different time
(define-transform-cps-special-form cl:load-time-value (expression environment)
  (define-destructuring (form &optional read-only?) (rest expression))
  `(cl:load-time-value (with-continuation ,(transform-cps form) #'values) ,read-only?))

;; multiple-value-prog1: why is this a special form?
(define-transform-cps-special-form cl:multiple-value-prog1 (expression environment)
  (define-destructuring (values-form . forms) (rest expression))
  (define-unique-symbols continue-from-multiple-value-prog1 results ignored)
  `(save-continuation ,continue-from-multiple-value-prog1
     ;; Evaluate the values form first, saving the results
     (with-continuation ,(transform-cps values-form)
	 (cl:lambda (&rest ,results)
	   (with-continuation ,(transform-cps `(progn ,@forms))
	       (cl:lambda (&rest ,ignored)
		 (declare (ignore ,ignored))
		 (continue-to ,continue-from-multiple-value-prog1 ,results)))))))


(define-transform-cps-special-form cl:labels (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))

  (define (labels-binding->cps binding)
    (define-destructuring (name ordinary-lambda-list . body) binding)
    (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
    (function-binding->cps environment name full-lambda-list body))

  (def function-names (map first definitions))
  (with-lexical-function-names function-names
    `(cl:labels ,(map labels-binding->cps definitions)
       ,@declarations
       ,(transform-cps `(progn ,@forms)))))

(define-transform-cps-special-form cl:flet (expression environment)
  (define-destructuring (definitions &body body) (rest expression))
  (define-values (declarations forms) (parse-declarations body))
  
  (define (flet-binding->cps binding)
    (define-destructuring (name ordinary-lambda-list . body) binding)
    (def full-lambda-list (full-ordinary-lambda-list ordinary-lambda-list))
    (function-binding->cps environment name full-lambda-list body))

  (def function-names (map first definitions))
  `(cl:flet ,(map flet-binding->cps definitions)
     ,@declarations
     ,(with-lexical-function-names function-names
	(transform-cps `(progn ,@forms)))))


;; Dynamic forms

(define-struct fcontrol-signal
    (tag value continuation)
    :opaque)

(for-macros (defvar *fcontrol-tag* (unique-symbol "FCONTROL-TAG")))
(for-macros (defvar *prompt-established?* nil))

(def (run continue-from-run tag thunk handler (modify-suspended-continuation identity))
  "Perform THUNK in a dynamic context that catches all tags (cons tag *current-run-tags*).
Return the results to continue-from-run.
If tag is caught because of (fcontrol tag value), the (handler value rest-of-thunk) is invoked with
the rest-of-thunk.
If a different tag is caught because of (fcontrol another-tag value), the control is re-signaled
with a continuation: (modify-suspended-continuation rest-of-thunk) in the re-established dynamic context before continuing to continue-from-run."
  ;; Execute the thunk, catching fcontrol-signals.
  (def thunk-result (let ((*prompt-established?* t))
		      (catch *fcontrol-tag*
			;; Call thunk returning results as a list.
			(funcall/c #'list thunk))))
  (cond
    ;; If the result is an fcontrol-signal, it means an (FCONTROL value) form was encountered.
    ((fcontrol-signal? thunk-result)
     ;; TODO: Destructure structures
     (define fcontrol-tag (fcontrol-signal-tag thunk-result))
     (define value (fcontrol-signal-value thunk-result))
     (define rest-of-thunk (fcontrol-signal-continuation thunk-result))
     (cond
       ((eq? tag fcontrol-tag)
	;; This is the tag we are trying to catch. Invoke the handler.
	;; on the signal's value and continuation
	;; Return the results to whoever called run.
	(funcall/c continue-from-run handler value rest-of-thunk))
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
		  ;; TODO: Is modify-suspended-continuation necessary?
		  (lambda () (apply (modify-suspended-continuation rest-of-thunk) arguments))
		  handler
		  modify-suspended-continuation)))))
       ;; If this is not our tag, and there is no outer prompt established, then we have an error.
       (t (error "Outermost prompt ~S: No enclosing prompt found for fcontrol signal with tag ~S" tag fcontrol-tag))))
    ;; Otherwise, we encountered a normal exit: return the results to whoever called run.
    (t (apply continue-from-run thunk-result))))

(def (default-prompt-handler value _continuation) value)
(defvar *default-prompt-tag* (unique-symbol 'default-prompt-tag))

(defmacro % (expr &key (tag '*default-prompt-tag*) (handler '(function default-prompt-handler)) &environment environment)
  "Sets up a prompt with the given tag"
  (multiple-value-bind (expanded-expr simple?) (expand-cps environment expr)
    (if simple?
	expanded-expr
	(let ((continue-from-% (unique-symbol 'continue-from-%)))
	  `(save-continuation ,continue-from-%
	     (cps (run ,continue-from-% ,tag (cl:lambda () (no-cps ,expanded-expr)) ,handler)))))))
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
       environment
       arguments
       (lambda (argument-names)
	 (define-destructuring (tag value) argument-names)
	 `(progn
	    (unless *prompt-established?*
	      (error "Attempt to (FCONTROL ~S ~S) without an established prompt. See %, RUN." ,tag ,value))
	    (throw *fcontrol-tag* (make-fcontrol-signal ,tag ,value ,continue-from-fcontrol)))))))
(defmacro fcontrol (tag value)
  "Evaluates tag, then value, throwing tag, value, and the current continuation
to the dynamically nearest established RUN, aborting the current continuation.
If FCONTROL is evaluated in a non-CPS function, it issues a warning and evaluates to VALUE."
  (declare (ignore tag value))
  `(error "Attempt to FCONTROL in a non-CPS environment."))
(defmacro throw/cc (value &optional (tag '*default-prompt-tag*))
  "Equivalent to (FCONTROL TAG VALUE) but with an optional default tag."
  `(fcontrol ,tag ,value))

(define-transform-cps-special-form cl:block (expression environment)
  (define-destructuring (name . forms) (rest expression))
  (define-unique-symbols block-tag)
  (define lexical-context
    (alist-update *lexical-context*
		  :block-tag-alist
		  (cut (alist-set _ name block-tag))))
  (let ((*lexical-context* lexical-context))
    (transform-cps `(run *continuation*
			 ',block-tag
			 (cl:lambda () ,@forms)
			 (cl:lambda (results k)
			   (declare (ignore k))
			   (values-list results))))))


(define-transform-cps-special-form cl:return-from (expression environment)
  (define-destructuring (name &optional values-form) (rest expression))
  (define block-tag (alist-ref (alist-ref *lexical-context* :block-tag-alist) name))
  (define-unique-symbols results)
  (unless block-tag
    (error "Could not find BLOCK named ~S in the current lexical environment." name))
  `(with-continuation ,(transform-cps values-form)
       (cl:lambda (&rest ,results)
	 ,(transform-cps `(fcontrol ',block-tag ,results)))))


(define-transform-cps-special-form cl:catch (expression environment)
  (define-destructuring (tag-form . forms) (rest expression))
  (define-unique-symbols continue-from-catch tag)
  `(save-continuation ,continue-from-catch
     ,(cps->primary-value
       environment tag-form tag
       (cl:lambda (tag)
	 ;; Create a prompt with the given tag.
	 ;; TODO: put these tags in a different namespace from other prompt tags.
	 (transform-cps
	  `(run ,continue-from-catch
		,tag
		(cl:lambda () ,@forms)
		(cl:lambda (results k)
		  (declare (ignore k))
		  (values-list results))))))))

(define-transform-cps-special-form cl:throw (expression environment)
  (define-destructuring (tag-form results-form) (rest expression))
  (define-unique-symbols tag results)
  (cps->primary-value
   environment tag-form tag
   (cl:lambda (tag)
     `(with-continuation ,(transform-cps results-form)
	  (cl:lambda (&rest ,results)
	    ;; Return to the prompt
	    ,(transform-cps `(fcontrol ,tag ,results)))))))

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


;; TODO: Below

(define-transform-cps-special-form cl:unwind-protect (expression environment)
  (define-destructuring (protected &body cleanup) (rest expression))
  (define-unique-symbols results normal-exit?
    continue-from-unwind-protect result arguments finished-cleanup?)
  (define-values (protected-expanded protected-simple?) (expand-cps environment protected))
  (define-values (cleanup-expanded cleanup-simple?) (expand-cps environment `(progn ,@cleanup)))
  `(save-continuation ,continue-from-unwind-protect
     (no-cps
      (let (,normal-exit? ,results ,finished-cleanup?)
	(unwind-protect
	     ;; Protected form
	     (let ((,result
		     (catch *fcontrol-tag*
		       ;; evaluate protected, setting results to the returned values
		       (set! ,results ,(if protected-simple?
					   `(list ,protected-expanded)
					   `(with-continuation ,protected-expanded #'list))))))
	       ;; Caught an fcontrol-tag from within the protected form?
	       (when (fcontrol-signal? ,result)
		 ;; Rethrow it, but don't allow re-entry
		 (throw *fcontrol-tag*
		   (make-fcontrol-signal
		    (fcontrol-signal-tag ,result)
		    (fcontrol-signal-value ,result)
		    ;; Don't allow re-entry into the protected froms
		    (cl:lambda (&rest ,arguments)
		      (declare (ignore ,arguments))
		      (error "Attempt to re-enter a protected form.")))))
	       
	       ;; if we made it here, we have had a normal-exit (no throw)
	       (set! ,normal-exit? t))
	  ;; Cleanup forms
	  (let ((,result
		  (catch *fcontrol-tag*
		    ,(if cleanup-simple?
			 cleanup-expanded
			 `(with-continuation ,cleanup-expanded #'values)))))
	    ;; If we caught an fcontrol-signal in the cleanup forms
	    (when (fcontrol-signal? ,result)
	      ;; re-throw it, with a modified continuation
	      (throw *fcontrol-tag*
		(make-fcontrol-signal
		 (fcontrol-signal-tag ,result)
		 (fcontrol-signal-value ,result)
		 ;; Return a continuation that calls the rest of the cleanup forms,
		 ;; followed by, if we had a normal exit, the a call to the continuation from the unwind-protect. 
		 (cl:lambda (&rest ,arguments)
		   ;; Call the rest of the cleanup-forms with the arguments
		   (apply (fcontrol-signal-continuation ,result) ,arguments)
		   ;; IF we had a normal-exit
		   (when ,normal-exit?
		     ;; then continue from unwind-protect with the protected-form's results
		     (apply ,continue-from-unwind-protect ,results))
		   ;; If we didn't have a normal-exit? then we are at a dead end.
		   ))))
	    ;; IF we made it here, we had a normal exit from the cleanup forms.
	    (set! ,finished-cleanup? t)))
	
	;; If we had a normal exit from the protected form and the cleanup forms, we need to call the continuation
	;; with the results of the protected form.
	(when (and ,normal-exit? ,finished-cleanup?)
	  (apply ,continue-from-unwind-protect ,results))))))

(catch/cc (:handler (cl:lambda (v k) (funcall k :resume-cleanup)))
  (throw/cc :abort)
  :result)

(catch/cc (:handler (lambda (v k) (funcall k :resume-cleanup)))
  (let ((result (unwind-protect (print :protected)
		  (print (throw/cc :abort-cleanup)))))
    (print :after)
    result)
  :result)
;; Should return (:cleanup :protected)




;; TODO: Below

(defvar *tagbody-go-tag* (unique-symbol 'tagbody-go-tag))
;; Tagbody:
;; tags have lexical scope and dynamic extent
;; if there is no matching tag visible to go, results are undefined.
(define-transform-cps-special-form cl:tagbody (expression environment)
  (define-destructuring (untagged-statements . tagged-forms) (parse-tagbody (rest expression)))
  (define-unique-symbols continue-from-tagbody recurse thunk tagbody-prompt-tag results)
  (define tags (map first tagged-forms))

  (define (thunk-form statements)
    "Return a thunk that calls (progn statements...) in continuation-passing style."
    `(cl:lambda () (funcall ,(transform-cps `(progn ,@statements)) #'values)))
  
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
    
    `(cps-lambda ,continue-from-tagbody
		 (no-cps
		  (let (,@function-names)
		    ,@function-name-assignments
		    (let ,recurse ((,thunk ,untagged-thunk-form))
		      (let (encountered-go?)
			(run (cl:lambda (&rest ,results)
			       ;; If a go was encountered loop until no more go forms are encountered
			       (if encountered-go?
				   ;; a go was encountered, loop with the tag-thunk returned from the handler
				   (,recurse (first ,results))
				   ;; Otherwise, return nil from the tagbody
				   (funcall ,continue-from-tagbody nil)))
			     ',tagbody-prompt-tag
			     ,thunk
			     (lambda (tag-thunk _continue-from-go)
			       ;; Encountered a go, we will continue looping
			       (set! encountered-go? t)
			       ;; return the tag-thunk to the continuation
			       tag-thunk)))))))))

(define-transform-cps-special-form cl:go (expression environment)
  (define-unique-symbols continue-from-go)
  (define-destructuring (tag) (rest expression))
  (define tag-data (alist-ref (alist-ref *lexical-context* :tagbody-context-alist) tag))
  (cond
    (tag-data
     (define-destructuring (tagbody-prompt-tag function-name) tag-data)
     `(cps-lambda ,continue-from-go
		  (no-cps (funcall ,(transform-cps `(fcontrol ',tagbody-prompt-tag ,function-name)) ,continue-from-go))))
    (t (error "Could not find TAG ~S in lexical-context of GO." tag))))


;; progv
;; Progv forms can be re-entered, but the dynamic bindings will no longer be in effect.
(define-transform-cps-special-form cl:progv (expression environment)
  (define-destructuring (vars-form vals-form &body forms) (rest expression))
  (define-unique-symbols continue-from-progv vars vals tag value ignored-continuation)
  `(cps-lambda ,continue-from-progv
	       ,(cps->primary-value
		 vars-form vars
		 (list (cps->primary-value
			vals-form vals
			`((run ,continue-from-progv
			       ',tag
			       (cl:lambda () (progv ,vars ,vals (funcall ,(transform-cps `(progn ,@forms)) #'values)))
			       (cl:lambda (,value ,ignored-continuation)
				 (declare (ignore ,ignored-continuation))
				 ,value)
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
			       )))))))




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
(assert (equal? (funcall (function->cps-function (cps (cl:lambda (a b c) (values a b c)))) #'list 1 2 3)
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

(assert (equal? (cps
		 (scm
		   (% (funcall (cl:lambda (p1 p2 &optional (o1 (fcontrol :abort :o1)) (o2 (fcontrol :abort :o2)))
				 (list p1 p2 o1 o2))
			       :p1 :P2 :o1)
		      :tag :abort
		      :handler
		      (cl:lambda (v k)
			(list v (funcall k :o2) (funcall k :o2-again))))))
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
(assert (cps-form-equal? (cps
			  (scm
			    (def vs ())
			    (def (v x) (push x vs) x)
			    (list (block name
				    (v :start)
				    (unwind-protect
					 (v (return-from name (v :returning)))
				      (v :cleanup))
				    (v :after))
				  (nreverse vs))))))


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
		  (cps
		   (% (let ((x 3))
			(list
			 (progv '(x) '(4)
			   (list x (symbol-value 'x)
				 (setf (symbol-value 'x) 2)
				 (fcontrol :abort :value)
				 (boundp 'x)))
			 (list x (boundp 'x))))
		      :tag :abort
		      :handler (lambda (v k) (list v (k :resume))))))
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


;;(cps (progn a b c))

;; CURRENT State
#;
(FUNCALL
 (COMMON-LISP:LAMBDA (#:CONTINUE1725)
   (FUNCALL
    (COMMON-LISP:LAMBDA (#:CONTINUE-FROM-ATOM1729)
      (FUNCALL #:CONTINUE-FROM-ATOM1729 A))
    (COMMON-LISP:LAMBDA (&REST #:IGNORED1726)
      (DECLARE (IGNORE #:IGNORED1726))
      (FUNCALL
       (COMMON-LISP:LAMBDA (#:CONTINUE1727)
         (FUNCALL
          (COMMON-LISP:LAMBDA (#:CONTINUE-FROM-ATOM1730)
            (FUNCALL #:CONTINUE-FROM-ATOM1730 B))
          (COMMON-LISP:LAMBDA (&REST #:IGNORED1728)
            (DECLARE (IGNORE #:IGNORED1728))
            (FUNCALL
             (COMMON-LISP:LAMBDA (#:CONTINUE-FROM-ATOM1731)
               (FUNCALL #:CONTINUE-FROM-ATOM1731 C))
             #:CONTINUE1727))))
       #:CONTINUE1725))))
 #'VALUES)
;; vs: 7 funcalls vs 3, 7 lambdas vs 2, 6 bindings vs. 6
#;
(COMMON-LISP:LET ((#:CONTINUE-FROM-PROGN1744 *CONTINUATION*))
  (COMMON-LISP:LET ((*CONTINUATION*
                      (COMMON-LISP:LAMBDA (&REST #:RESULTS1743)
			(DECLARE (IGNORE #:RESULTS1743))
			(COMMON-LISP:LET ((*CONTINUATION*
                                            (COMMON-LISP:LAMBDA
						(&REST #:RESULTS1743)
                                              (DECLARE (IGNORE #:RESULTS1743))
                                              (COMMON-LISP:LET ((*CONTINUATION*
								  #:CONTINUE-FROM-PROGN1744))
						(FUNCALL *CONTINUATION*
							 (PRINT C))))))
                          (FUNCALL *CONTINUATION* B)))))
    (FUNCALL *CONTINUATION* A)))


;; GOAL:


(let ((continue-from-progn *continuation*))
  (with-continuation
      (cl:lambda (&rest results)
	(declare (ignore results))
	(with-continuation (cl:lambda (&rest results)
			     (declare (ignore results))
			     (funcall continue-from-progn (print 'c)))
	  (funcall *continuation* (print 'b))))
    (funcall *continuation* (print 'a))))

(def (reduce-atom expression)
  `(funcall *continuation* ,expression))
(def (reduce-progn expression)
  (def forms (rest expression))
  (define-unique-symbols results continue-from-progn)
  (def (reduce-progn-forms forms)
    (cond
      ((empty? forms) (reduce-atom nil))
      (t
       (define-destructuring (form . rest-of-forms) forms)
       (cond
	 ((empty? rest-of-forms) `(let ((*continuation* ,continue-from-progn))
				    ,(reduce-atom form)))
	 (t
	  `(with-continuation
	       (cl:lambda (&rest ,results)
		 (declare (ignore ,results))
		 ,(reduce-progn-forms rest-of-forms))
	     ,(reduce-atom form)))))))
  `(let ((,continue-from-progn *continuation*))
     ,(reduce-progn-forms forms)))

(reduce-progn '(progn (print 'a) (print 'b) (print 'c)))
#;
(LET ((#:CONTINUE-FROM-PROGN1744 *CONTINUATION*))
  (WITH-CONTINUATION (COMMON-LISP:LAMBDA (&REST #:RESULTS1743)
                       (DECLARE (IGNORE #:RESULTS1743))
                       (WITH-CONTINUATION (COMMON-LISP:LAMBDA
                                              (&REST #:RESULTS1743)
                                            (DECLARE (IGNORE #:RESULTS1743))
                                            (LET
                                                ((*CONTINUATION*
                                                   #:CONTINUE-FROM-PROGN1744))
                                              (FUNCALL *CONTINUATION*
                                                       (PRINT 'C))))
                         (FUNCALL *CONTINUATION* (PRINT 'B))))
    (FUNCALL *CONTINUATION* (PRINT 'A))))

;; A Cps expression would go from
;;   (funcall cps-expr continuation) -> cps-expr

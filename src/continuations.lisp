(unless (cl:find-package :schemeish.continuations)
  (cl:make-package :schemeish.continuations :use '(:schemeish)))

(cl:in-package :schemeish.continuations)

;; CONTINUATIONS is not in a usable state yet.

;; For continuations to be complete, they should respect
;; dynamic special forms (rather than replacing them with lexical forms)
;; such as TAGBODY, BLOCK.

;; In order to do this properly, Kent Pitman's Unwind-Protect vs. Continuations
;; suggests a second version of call/cc which can escape the dynamic context.

;; This could be implemented in terms of DYNAMIC-WIND. But then I'd need to
;; implement DYNAMIC-WIND.
;; References:
;; Racket docs:
;;   https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._dynamic-wind%29%29
;; Matthias Blume functional implementation:
;;   https://groups.google.com/g/comp.lang.scheme/c/kvtSI43Nbfo/m/ESnQ3U1xV2kJ?hl=en


;; Notes about the current version:
;; TAGBODY and BLOCK forms are replaced with lexical/non-dynamic (and therefore re-entrant) LABELS forms
;; CATCH and UNWIND-PROTECT are tricky to implement correctly in the presence of continuations. see above.
;; I'm not sure about the behavior of PROGV.

;; Issues in the current version:
;; Dynamic-wind
;; Need to iron out things like accepting multiple values, but only using the primary value.
;; Code-walker:
;;   Split out the code-walker
;;   Need better error reporting for the code walker.
;; Need to be able to add new implementation-specific special forms (like 'SB-INT:NAMED-LAMBDA) 
;; Need more tests, and more deep testing
;; Handle if CALL/CC is used outside of a continuation region

(install-syntax!)

(define (declare? form) (and (pair? form) (eq? (first form) 'cl:declare)))

(define (body-declarations body)
  (takef body #'declare?))
(define (body-forms body)
  (dropf body #'declare?))

(define (function-body-declarations body)
  (if (and (string? (first body)) (not (empty? (rest body))))
      (cons (first body) (body-declarations (rest body)))
      (body-declarations body)))
(define (function-body-forms body)
  (if (and (string? (first body)) (not (empty? (rest body))))
      (body-forms (rest body))
      (body-forms body)))

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
  (if (empty? arguments)
      (apply #'funcall/c continuation function argument)
    (apply #'funcall/c continuation function argument (nconc (butlast arguments) (first (last arguments))))))

(define (list-type list)
  "Returns one of (:proper :cyclic :dotted (values :dotted :cons))"
  #g((list? list))
  ;; Field is a list, list*, cons, or a cycle
  (let recurse ((xs list)
		(visited ())
		(result ()))
       (cond
	((empty? xs) :proper)
	((member xs visited) :cyclic)
	((pair? xs)
	 ;; In the middle of the list, keep looking.
	 (recurse (rest xs) (cons xs visited) (cons (first xs) result)))
	(t
	 (cond
	  ;; Dotted-lists
	  ((pair? (rest list)) :dotted)
	  (t (values :dotted :cons)))))))

(define (proper-list? list)
  (and (list? list)
       (eq? :proper (list-type list))))

;; TODO: fix these so they are more helpful
#;(progn
    (define (progn-valid? expr)
      (let ((forms (progn-forms expr)))
	(unless (proper-list? forms)
	  (return-from progn-valid? (format nil "Forms are not a proper-list ~S" forms)))
	t))

    (define (let-valid? expr)
      (when (or (not (proper-list? expr))
		(< (length expr) 2))
	(return-from let-valid? (format nil "Malformed LET: ~S" expr)))

      (let ((bindings (let-bindings expr)))
	(unless (proper-list? bindings)
	  (return-from let-valid? (format nil "Malformed bindings ~S in ~S" bindings expr)))
	(let ((invalid-binding (find-if-not #'let-binding? bindings)))
	  (when invalid-binding
	    (return-from let-valid? (format nil "Malformed binding ~S in bindings ~S in ~S" invalid-binding bindings expr))))
	t))

    (define (let*-valid? expr) (let-valid? expr))

    (define (block-valid? expr)
      (unless (proper-list? expr)
	(return-from block-valid? (format nil "Block should be a proper-list: ~S" expr)))
      (unless (> (length expr) 1)
	(return-from block-valid? (format nil "Empty block: ~S" expr)))
      (unless (symbol? (block-name expr))
	(return-from block-valid? (format nil "Name should be a symbol: ~S in ~S" (block-name expr) expr)))
      t)

    (define (return-from-valid? expr)
      (unless (and (proper-list? expr)
		   (member (length expr) '(2 3)))
	(return-from return-from-valid? (format nil "Malformed RETURN-FROM: ~S" expr)))
      (unless (symbol? (return-from-name expr))
	(return-from return-from-valid? (format nil "Name should be a symbol: ~S in ~S" (return-from-name expr) expr)))
      t)

    (define (labels-valid? expr)
      (unless (and (proper-list? expr)
		   (>= (length expr) 2))
	(return-from labels-valid? (format nil "Malformed: ~S" expr)))
      (let ((bindings (labels-bindings expr)))
	(unless (proper-list? bindings)
	  (return-from labels-valid? (format nil "Malformed bindings: ~S in ~S" bindings expr)))
	(let ((invalid-binding (find-if-not #'function-binding? bindings)))
	  (unless (null? invalid-binding)
	    (return-from labels-valid? (format nil "Malformed binding: ~S in bindings ~S in ~S" invalid-binding bindings expr))))))

    (define (flet-valid? expr)
      (labels-valid? expr))

    (define (function-valid? expr)
      (unless (and (proper-list? expr)
		   (= (length expr) 2))
	(return-from function-valid? (format nil "Malformed FUNCTION: ~S" expr)))
      t)

    (define (quote-valid? expr)
      (unless (and (proper-list? expr)
		   (= (length expr) 2))
	(return-from quote-valid? (format nil "Malformed QUOTE: ~S" expr)))
      t)

    (define (eval-when-valid? expr)
      (unless (and (proper-list? expr)
		   (> (length expr) 1))
	(return-from eval-when-valid? (format nil "Malformed EVAL-WHEN: ~S" expr)))
      (unless (proper-list? (eval-when-situations expr))
	(return-from eval-when-valid? (format nil "Malformed EVAL-WHEN situations: ~S in ~S" (eval-when-situations expr) expr)))
      t)

    (define (setq-valid? expr)
      (unless (and (proper-list? expr)
		   (>= (length expr) 3))
	(return-from setq-valid? (format nil "Malformed SETQ: ~S" expr)))
      (unless (odd? (length expr))
	(return-from setq-valid? (format nil "Expected pairs in ~S" expr)))
      (let ((invalid-pair (find-if-not (lambda (pair) (symbol? (first pair))) (setq-pairs expr))))
	(when invalid-pair
	  (return-from setq-valid? (format nil "Invalid name ~S in pair ~S in ~S" (first invalid-pair) invalid-pair expr))))
      t)

    (define (if-valid? expr)
      (unless (and (proper-list? expr)
		   (member (length expr) '(3 4)))
	(return-from if-valid? (format nil "Malformed IF: ~S" expr)))
      t))

'((define (macrolet-valid? expr))
  (define (symbol-macrolet-valid? expr))
  (define (symbol-macrolet-valid? expr))
  (define (locally-valid? expr))
  (define (tagbody-valid? expr))
  (define (go-valid? expr))
  (define (the-valid? expr))
  (define (progv-valid? expr))
  (define (unwind-protect-valid? expr))
  (define (catch-valid? expr))
  (define (throw-valid? expr))
  (define (load-time-value-valid? expr))
  (define (multiple-value-call-valid? expr))
  (define (multiple-value-prog1-valid? expr))
  (define (lambda-valid? expr)))

(for-macros
 (defvar *special-form->cps-table* (make-hash-table)))
(define (register-special-form->cps symbol special-form->cps)
  (hash-set! *special-form->cps-table* symbol special-form->cps))
(define (special-form->cps expr)
  "Converts the given special form expr to continuation-passing style. 
The resulting expression will take a continuation as its first argument,
and call that continuation with its results."
  #g((pair? expr) (symbol? (first expr)))
  [(hash-ref *special-form->cps-table* (first expr)) expr])

(define (expr->cps expr (environment))
  "Return the form of a function that takes a continuation, 
and calls that continuation with the values of expr."
  (cond
   ;; Special Forms
   ((progn? expr) (progn->cps (progn-forms expr)))
   ((let? expr) (let->cps (let-bindings expr) (let-body expr)))
   ((let*? expr) (let*->cps (let*-bindings expr) (let*-body expr)))
   ((block? expr) (block->cps (block-name expr) (block-body expr)))
   ((return-from? expr) (return-from->cps (return-from-name expr) (return-from-value expr)))
   ((labels? expr) (labels->cps (labels-bindings expr) (labels-body expr)))
   ((flet? expr) (flet->cps (flet-bindings expr) (flet-body expr)))
   ((function? expr) (function->cps (function-name expr)))
   ((quote? expr) (quote->cps (quote-expr expr)))
   ((eval-when? expr) (eval-when->cps (eval-when-situations expr) (eval-when-forms expr)))
   ((setq? expr) (setq->cps (setq-pairs expr)))
   ((if? expr) (if->cps (if-test expr) (if-then expr) (if-else expr)))
   ((call/cc? expr) (call/cc->cps (call/cc-function expr)))
   ((macrolet? expr) (macrolet->cps (macrolet-bindings expr) (macrolet-body expr)))
   ((symbol-macrolet? expr) (symbol-macrolet->cps (symbol-macrolet-bindings expr) (symbol-macrolet-body expr)))
   ((locally? expr) (locally->cps (locally-body expr)))
   ((tagbody? expr) (tagbody->cps (tagbody-tags-and-statements expr)))
   ((go? expr) (go->cps (go-tag expr)))
   ((the? expr) (the->cps (the-value-type expr) (the-form expr)))
   ((progv? expr) (progv->cps (progv-vars expr) (progv-vals expr) (progv-forms expr)))
   ((unwind-protect? expr) (unwind-protect->cps (unwind-protect-protected expr) (unwind-protect-cleanup expr)))
   ((catch? expr) (catch->cps (catch-tag expr) (catch-forms expr)))
   ((throw? expr) (throw->cps (throw-tag expr) (throw-result expr)))
   ((load-time-value? expr) (load-time-value->cps (load-time-value-form expr) (load-time-value-read-only-p expr)))
   ((multiple-value-call? expr) (multiple-value-call->cps (multiple-value-call-function expr) (multiple-value-call-arguments expr)))
   ((multiple-value-prog1? expr) (multiple-value-prog1->cps (multiple-value-prog1-values-form expr) (multiple-value-prog1-forms expr)))
   
   ((lambda? expr) (lambda->cps (lambda-parameters expr) (lambda-body expr)))

   ((macro-function-application? expr environment) (macro-function-application->cps expr environment))
   ((function-application? expr) (function-application->cps (function-application-function expr) (function-application-arguments expr)))
   (t (atom->cps expr))))

(defmacro expr->cps-in-current-environment (&environment environment expr block-table lexical-cps-function-names tag->function-name-table)
  (let ((*block-table* block-table)
	(*tag->function-name-table* tag->function-name-table)
	(*lexical-cps-function-names* lexical-cps-function-names))
    (expr->cps expr environment)))

(define (expr->cps-form expr)
  "Return a form that, when evaluted, converts expr to cps in the current lexical environment."
  `(expr->cps-in-current-environment ,expr ,*block-table* ,*lexical-cps-function-names* ,*tag->function-name-table*))

(define (check-cps-for-expr expr)
  (let ((cps-result (funcall (eval (expr->cps expr)) #'list))
	(result (multiple-value-list (eval expr))))
    (unless (equal? result cps-result)
      (error "CPS-RESULT ~S not the same as RESULT ~S" cps-result result))
    result))

(define (macro-function-application? expr environment)
  (and (pair? expr)
       (macro-function (first expr) environment)))
(define (macro-function-application->cps expr environment)
  (expr->cps-form (macroexpand expr environment)))

(defmacro foob (a b c)
  `(+ ,a ,b ,c))
(check-cps-for-expr '(+ 1 (foob 1 (foob 2 3 4) 3)))

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
       (funcall ,(expr->cps-form function)
		(cl:lambda (,function-name)
		  (multiple-value-call ,continuation (funcall ,function-name ,continuation)))))))

(progn
  (defvar *the-continuation*)
  (funcall (eval (expr->cps '(progn
			      (print 'before)
			      (print (call/cc (cl:lambda (k) (setq *the-continuation* k) 'during)))
			      (print 'after)
			      (values))))
	   #'values)
  (funcall [*the-continuation* :again] #'values))

(define (quote? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:quote)
       (= 2 (length expr))))
(define (quote-expr expr)
  (second expr))
(define (quote->cps expr)
  (atom->cps `',expr))

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
	   `(funcall ,(expr->cps-form argument)
		     (cl:lambda (,name)
		       ,(recurse (rest arguments) (cons name names) (1+ n))))))))

  (recurse arguments () 0))

(check-cps-for-expr '(values 1 2 3))
(check-cps-for-expr '(funcall #'values 1 2 3))

(check-cps-for-expr '(funcall (cl:lambda (&rest args) (values-list args)) 1 2 3))
(check-cps-for-expr '(apply (cl:lambda (&rest args) (values-list args)) 1 2 '(3 4)))
(check-cps-for-expr '(apply (cl:lambda (&rest args) (values-list args)) 1 2 3 ()))

(define (atom->cps atom)
  (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-ATOM"))))
    `(cl:lambda (,continuation) (multiple-value-call ,continuation ,atom))))

(check-cps-for-expr '1)

(define (progn? expr)
  (and (pair? expr)
       (eq? (car expr) 'cl:progn)))
(define (progn-forms expr)
  (rest expr))
(define (progn->cps forms)
  (cond
    ;; Base case: (progn) => NIL
    ((empty? forms) (atom->cps nil))
    ;; Base case: (progn FORM) => FORM
    ((empty? (rest forms)) (expr->cps-form (first forms)))
    ;; Iteration: (progn form forms...)
    (t (let ((continuation (unique-symbol (format nil "CONTINUE-FROM-PROGN")))
	     (ignored (unique-symbol 'ignored)))
	 `(cl:lambda (,continuation)
	    ;; Evaluate first form, discarding result
	    (funcall ,(expr->cps-form (first forms))
		     (cl:lambda (&rest ,ignored)
		       (declare (ignore ,ignored))
		       ;; Pass the continuation to the rest of the forms
		       (funcall ,(progn->cps (rest forms)) ,continuation))))))))

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
  (define declarations (function-body-declarations body))
  (define forms (function-body-forms body))
  (define cps-lambda
    (let ((continuation (unique-symbol 'continue-from-lambda)))
      `(cl:lambda ,(cons continuation parameters)
	 ,@declarations
	 (funcall ,(progn->cps forms) ,continuation))))
  
  (atom->cps `(cps-function->function ,cps-lambda)))

(check-cps-for-expr '(funcall (cl:lambda (&rest args) (declare (ignorable args)) args) 1 2 3))

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
  (and (not (null? expr))
       (or (and (proper-list? expr)
		(or (= (length expr) 1)
		    (= (length expr) 2)))
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
	      (new-bindings (map #'list binding-names (nreverse names)))
	      (declarations (body-declarations body))
	      (forms (body-forms body)))
	 `(cl:lambda (,continuation)
	    ;; Use LET to assign bindings so that we automatically handle dynamic variables.
	    (cl:let ,new-bindings
	      ,@declarations
	      (funcall ,(progn->cps forms) ,continuation)))))
      ;; Iteration case: evaluate value of next binding, and store its name
      (t (let* ((binding (first bindings))
		(name (unique-symbol (format nil "LET-BINDING-VALUE-~S-" (let-binding-name binding))))
		(value (let-binding-value binding)))
	   ;; Evaluate the next binding's value
	   `(funcall ,(expr->cps-form value)
		     (cl:lambda (,name)
		       ,(recurse (rest bindings) (cons name names))))))))

  (recurse bindings ()))

(check-cps-for-expr '(cl:let ((x 1)
			      y
			      (z))
		      (declare (ignorable x))
		      (declare (ignorable z))
		      (print x)
		      (print y)
		      (print z)
		      (values x 2 3)))

(define (let*->cps bindings body)
  (define binding-names (map #'let-binding-name bindings))
  (define (recurse bindings)
    (cond
      ;; Base case: Evaluate body
      ;; Unfortunately declarations can only apply to the body.
      ;; This is inevitable, since each binding depends on the previous binding.
      ((empty? bindings)
       ;; Create a let that binds (name name), so that we can add declarations.
       (let* ((continuation (unique-symbol (format nil "CONTINUE-FROM-LET")))
	      (new-bindings (map #'list binding-names binding-names))
	      (declarations (body-declarations body))
	      (forms (body-forms body)))
	 `(cl:lambda (,continuation)
	    (cl:let ,new-bindings
	      ,@declarations
	      (funcall ,(progn->cps forms) ,continuation)))))
      ;; Iteration case: evaluate value of next binding, and bind it.
      (t (let* ((binding (first bindings))
		(name (let-binding-name binding))
		(value (let-binding-value binding)))
	   ;; Evaluate the next binding's value, binding it to name.
	   `(funcall ,(expr->cps-form value)
		     (cl:lambda (,name) ,(recurse (rest bindings))))))))
  (recurse bindings))

(check-cps-for-expr '(cl:let* ((x 1)
			       (y (1+ x))
			       (z (1+ y)))
		      (declare (type number y))
		      (values x z)))

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


(define (block->cps name body)
  (let* ((continuation (unique-symbol (format nil "CONTINUE-FROM-BLOCK-~S-" name)))
	 (*block-table* (alist-set *block-table* name continuation)))
    `(lambda (,continuation)
       (funcall ,(progn->cps body) ,continuation))))

(define (return-from->cps name value)
  (define continuation (alist-ref *block-table* name))
  (define ignored-continuation (unique-symbol 'ignored-continuation-from-return-from))
  (define values-name (unique-symbol 'return-from-value))
  (unless continuation
    (error "RETURN-FROM: No block named ~S" name))
  `(cl:lambda (,ignored-continuation)
     (declare (ignore ,ignored-continuation))
     (funcall ,(expr->cps-form value)
	      (cl:lambda (&rest ,values-name)
		(apply ,continuation ,values-name)))))

(define (flet? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:flet)
       (or (pair? (flet-bindings expr))
	   (error "Malformed FLET bindings: ~S" expr))
       (or (for-all #'function-binding? (flet-bindings expr))
	   (error "Malformed FLET bindings: ~S" expr))
       (or (list? (flet-body expr))
	   (error "Malformed FLET body: ~S" expr))))
(define (flet-bindings expr)
  (second expr))
(define (flet-body expr)
  (cddr expr))

(define (function-binding? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (and (not (null? (function-binding-name expr)))
	    (symbol? (function-binding-name expr)))
       (list? (function-binding-parameters expr))
       (list? (function-binding-body expr))))
(define (function-binding-name expr)
  (first expr))
(define (function-binding-parameters expr)
  (second expr))
(define (function-binding-body expr)
  (cddr expr))


(define (function-binding->cps binding)
  (define name (function-binding-name binding))
  (define continuation (unique-symbol (format nil "CONTINUE-FROM-LOCAL-FUNCTION-~S-" name)))
  ;; Take continuation as the first parameter
  (define parameters (cons continuation (function-binding-parameters binding)))
  (define body (function-binding-body binding))
  (define declarations (function-body-declarations body))
  (define forms (function-body-forms body))
  `(,name ,parameters
	  ,@declarations
	  (funcall ,(block->cps name forms) ,continuation)))

(define (flet->cps bindings body)
  (define names (map #'function-binding-name bindings))
  (define continuation (unique-symbol 'continue-from-flet))
  (define declarations (body-declarations body))
  (define forms (body-forms body))
  `(cl:lambda (,continuation)
     (cl:flet ,(map #'function-binding->cps bindings)
       ,@declarations
       (funcall
	;; Lexically bind names for local functions in body only. 
	,(with-lexical-cps-function-names names (progn->cps forms))
	,continuation))))

(check-cps-for-expr '(flet ((foo (x y z)
			     "docstring"
			     (declare (ignorable y))
			     (values x y z)))
		      (declare (ignorable (function foo)))
		      (foo 1 2 3)))

(define (labels? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:labels)
       (or (pair? (labels-bindings expr))
	   (error "Malformed LABELS bindings: ~S" expr))
       (or (for-all #'function-binding? (labels-bindings expr))
	   (error "Malformed LABELS bindings: ~S" expr))
       (or (list? (labels-body expr))
	   (error "Malformed LABELS body: ~S" expr))))
(define (labels-bindings expr)
  (second expr))
(define (labels-body expr)
  (cddr expr))

(define (labels->cps bindings body)
  (define names (map #'function-binding-name bindings))
  (define continuation (unique-symbol 'continue-from-labels))
  (define declarations (body-declarations body))
  (define forms (body-forms body))
  ;; Lexically bind names in body AND in bindings.
  (with-lexical-cps-function-names names
    `(cl:lambda (,continuation)
       (cl:labels ,(map #'function-binding->cps bindings)
	 ,@declarations
	 (funcall ,(progn->cps forms) ,continuation)))))

(check-cps-for-expr '(labels ((foo (x y z) "docstring" (declare (ignorable x z)) (foo2 x y z))
			      (foo2 (&rest args) "docstring" (declare (ignorable args)) (values-list args)))
		      (declare (ignorable (function foo2)))
		      (foo 1 2 3)))

(define (eval-when? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:eval-when)
       (>= (length expr) 2)
       (or (list? (eval-when-situations expr))
	   (error "Badly-formed eval-when situations: ~S" expr))))
(define (eval-when-situations expr)
  (second expr))
(define (eval-when-forms expr)
  (cddr expr))
(define (eval-when->cps situations forms)
  `(eval-when ,situations
     ,(progn->cps forms)))

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
	   (funcall ,(expr->cps-form value)
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
       (funcall ,(expr->cps-form test)
		(cl:lambda (,test-result)
		  (funcall (cl:if ,test-result
				  ,(expr->cps-form then)
				  ,(expr->cps-form else))
			   ,continuation))))))

(check-cps-for-expr (if (or t nil)
			:true
			:false))
(check-cps-for-expr (if (and t nil)
			:true
			:false))

(define (macrolet? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:macrolet)
       (or (list? (macrolet-bindings expr))
	   (error "Badly formed bindings for macrolet: ~S" expr))))
(define (macrolet-bindings expr)
  (second expr))
(define (macrolet-body expr)
  (cddr expr))
(define (macrolet->cps bindings body)
  (define continuation (unique-symbol 'continue-from-macrolet))
  (define declarations (body-declarations body))
  (define forms (body-forms body))
  `(cl:lambda (,continuation)
     (cl:macrolet ,bindings
       ,@declarations
       (funcall ,(progn->cps forms) ,continuation))))

(check-cps-for-expr '(macrolet ((foo (x y z) `(values ,x ,y ,z)))
		      (foo 1 2 3)))

(define (symbol-macrolet? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:symbol-macrolet)
       (or (list? (symbol-macrolet-bindings expr))
	   (error "Badly formed bindings for macrolet: ~S" expr))))
(define (symbol-macrolet-bindings expr)
  (second expr))
(define (symbol-macrolet-body expr)
  (cddr expr))
(define (symbol-macrolet->cps bindings body)
  (define continuation (unique-symbol 'continue-from-symbol-macrolet))
  (define declarations (body-declarations body))
  (define forms (body-forms body))
  `(cl:lambda (,continuation)
     (cl:symbol-macrolet ,bindings
       ,@declarations
       (funcall ,(progn->cps forms) ,continuation))))

(check-cps-for-expr '(symbol-macrolet ((x 'foo))
		      (list x (let ((x 'bar)) x))))


(define (locally? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:locally)))
(define (locally-body expr)
  (cdr expr))
(define (locally->cps body)
  (define continuation (unique-symbol 'continue-from-locally))
  (define declarations (body-declarations body))
  (define forms (body-forms body))
  `(cl:lambda (,continuation)
     (cl:locally ,@declarations (funcall ,(progn->cps forms) ,continuation))))

(check-cps-for-expr '(locally
		      (declare (special *lexical-cps-function-names*))
		      (values 1 2 3)))


(define (tagbody? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:tagbody)))
(define (tagbody-tags-and-statements expr)
  (cdr expr))


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

(define (alist-union alist new-alist)
  (foldl (lambda (pair alist)
	   (alist-set alist (car pair) (cdr pair)))
	 alist new-alist))

(define (tagbody->cps tags-and-statements)
  (define parsed (parse-tagbody tags-and-statements))
  (define untagged-statements (first parsed))
  (define tagged-forms (rest parsed))

  (define (tagged-form->name form) (first form))
  (define (tagged-form->statements form) (rest form))

  (define untagged-function-name (unique-symbol 'untagged-statements))
  (define tagged-form-names (map tagged-form->name tagged-forms))
  (define tagged-form-statements (map tagged-form->statements tagged-forms))

  (define tagged-function-names
    (map (lambda (name) (unique-symbol (format nil "TAG-~S-" name))) tagged-form-names))

  ;; The continuation passed to the tagbody
  (define tagbody-continuation (unique-symbol 'continue-from-tagbody))
  ;; Continues from tagbody-statements, calls tagbody-continuation with nil.
  (define tagbody-statements-continuation (unique-symbol 'continue-from-tagbody-statements))

  ;; The untagged-form continues to the first tagged-form,
  ;; or if there are no tagged-forms, it continues to the tagbody-statements-continuation.
  (define untagged-form-continuation
    (if (empty? tagged-function-names)
	tagbody-statements-continuation
	`(function ,(first tagged-function-names))))
  ;; Each tagged-form continues to the next tagged-form,
  ;; except the last tagged-form continues to the tagbody-statements-continuation.
  (define tagged-form-continuations
    (append (map (lambda (name) `(function ,name)) (rest tagged-function-names))
	    (list tagbody-statements-continuation)))

  (define (continuation-function-binding-form function-name statements continuation)
    "Creates a function-binding in the form of a continuation whose results are ignored."
    (define ignored-parameter (unique-symbol 'ignored))
    `(,function-name (&rest ,ignored-parameter)
		     (declare (ignore ,ignored-parameter))
		     (funcall ,(progn->cps statements) ,continuation)))

  ;; Add the tags from the tagged-forms to the *tag->function-name-table* for (GO ...) forms
  (let ((*tag->function-name-table* (alist-union *tag->function-name-table*
						 (map #'cons tagged-form-names tagged-function-names))))

    ;; Function-binding definitions of continuations for each tagged-form.
    (define tagged-function-bindings (map continuation-function-binding-form
					  tagged-function-names tagged-form-statements tagged-form-continuations))

    ;; Function-binding definitions of continuations for the untagged and tagged-forms.
    (define function-bindings
      (cond
	((empty? untagged-statements) tagged-function-bindings)
	(t (cons (continuation-function-binding-form untagged-function-name
						     untagged-statements
						     untagged-form-continuation)
		 tagged-function-bindings))))

    ;; The first continuation to call: Either the untagged form, or the first tagged form.
    (define start-function-name
      (cond
	((empty? untagged-statements) (first tagged-function-names))
	(t untagged-function-name)))
    
    (cond
      ;; Empty tagbody:
      ((and (empty? untagged-statements) (empty? tagged-forms)) (atom->cps nil))
      (t (let ((ignored-parameter (unique-symbol 'ignored)))
	   `(cl:lambda (,tagbody-continuation)
	      ;; Create an intermediate continuation that calls the tagbody-continuation with NIL.
	      (cl:let ((,tagbody-statements-continuation
			 (cl:lambda (&rest ,ignored-parameter)
			   (declare (ignore ,ignored-parameter))
			   (funcall ,tagbody-continuation nil))))
		;; Establish the continuations as function bindings.
		(cl:labels ,function-bindings
		  ;; Call the first continuation.
		  (,start-function-name)))))))))

(define (go? expr)
  (and (pair? expr)
       (eq? (first expr) 'cl:go)
       (or (= 2 (length expr))
	   (error "Improperly formed GO: ~S" expr))))
(define (go-tag expr)
  (second expr))
(define (go->cps tag)
  (unless (alist-has-key? *tag->function-name-table* tag)
    (error "TAG ~S not found" tag))
  (let ((function-name (alist-ref *tag->function-name-table* tag))
	(ignored-continuation (unique-symbol 'ignored-continuation-from-go)))
    `(cl:lambda (,ignored-continuation)
       (declare (ignore ,ignored-continuation))
       (,function-name))))

(check-cps-for-expr '(tagbody))
(check-cps-for-expr '(let (vals)
		      (tagbody
		       point-a
			 (setq vals '(1 2 3)))
		      vals))
(check-cps-for-expr '(tagbody (values 1 2 3)))

(check-cps-for-expr '(let (vals)
		      (tagbody
			 (push 1 vals)
		       point-a
			 (push 2 vals)
		       point-b
			 (go point-d)
		       point-c
			 (push 'do-not-push vals)
		       point-d
			 (push 3 vals))
		      (nreverse vals)))

(check-cps-for-expr '(cl:let (val)
		      (tagbody
			 (setq val 1)
			 (go point-a)
			 (incf val 16)
		       point-c
			 (incf val 04)
			 (go point-b)
			 (incf val 32)
		       point-a
			 (incf val 02)
			 (go point-c)
			 (incf val 64)
		       point-b
			 (incf val 08))
		      val))

(define (the? expr)
  (and (pair? expr)
       (= 3 (length expr))
       (eq? (first expr) 'cl:the)))
(define (the-value-type expr)
  (second expr))
(define (the-form expr)
  (third expr))
(define (the->cps value-type form)
  (define continuation (unique-symbol 'continue-from-the))
  (define form-values (unique-symbol 'the-form-values))
  `(cl:lambda (,continuation)
     (funcall ,(expr->cps-form form)
	      (cl:lambda (&rest ,form-values)
		(multiple-value-call ,continuation (the ,value-type (values-list ,form-values)))))))
(check-cps-for-expr '(the fixnum (+ 5 7)))
(check-cps-for-expr '(the (values integer float symbol t null list) (truncate 3.2 2)))
(check-cps-for-expr '(let* ((x (list 'a 'b 'c))
			    (y 5))
		      (setf (the fixnum (car x)) y)
		      x))

(define (multiple-value-prog1? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:multiple-value-prog1)))
(define (multiple-value-prog1-values-form expr)
  (second expr))
(define (multiple-value-prog1-forms expr)
  (cddr expr))
(define (multiple-value-prog1->cps values-form forms)
  (define continuation (unique-symbol 'continue-from-multiple-value-prog1))
  (define results (unique-symbol 'multiple-value-results))
  (define ignored (unique-symbol 'ignored))
  `(cl:lambda (,continuation)
     ;; Evaluate the values form...
     (funcall ,(expr->cps-form values-form)
	      (cl:lambda (&rest ,results)
		;; ...saving the results
		;; evalute forms from left to right
		(funcall ,(progn->cps forms)
			 (cl:lambda (&rest ,ignored)
			   (declare (ignore ,ignored))
			   ;; Return the saved results
			   (apply ,continuation ,results)))))))

(check-cps-for-expr '(let (temp)
		      (setq temp '(1 2 3))
		      (multiple-value-prog1
			  (values-list temp)
			(setq temp nil)
			(values-list temp))))

(define (multiple-value-call? expr)
  (and (pair? expr)
       (>= (length expr) 3)
       (eq? (first expr) 'cl:multiple-value-call)))
(define (multiple-value-call-function expr)
  (second expr))
(define (multiple-value-call-arguments expr)
  (cddr expr))
(define (multiple-value-call->cps function arguments)
  ;; evaluate function
  ;; evaluate each argument
  ;; all values from each argument are gathered together and given to function
  (define continuation (unique-symbol 'continue-from-multiple-value-call))
  (define function-result (unique-symbol 'multiple-value-call-function))
  (define arguments-results (unique-symbol 'multiple-value-call-arguments))

  (define (arguments->cps arguments)
    "Return a function that takes a continuation and calls that continuation 
with a list of all values of each argument in arguments."
    (define (arguments->cps-iter arguments n)
      (cond
	;; Base Case: no arguments, just return nil.
	((empty? arguments) (atom->cps nil))
	(t (let ((continuation (unique-symbol (format nil "~S~S-" 'continue-from-multiple-value-call-argument- n)))
		 (argument-values (unique-symbol (format nil "~S~S-" 'argument-values n)))
		 (arguments-values (unique-symbol (format nil "~S~S+-" 'arguments-values (1+ n)))))
	     `(cl:lambda (,continuation)
		;; evaluate the next argument...
		(funcall ,(expr->cps-form (first arguments))
			 (cl:lambda (&rest ,argument-values)
			   ;; ...capturing the values in argument-values
			   ;; Then evaluate the rest of the argument-values...
			   (funcall ,(arguments->cps-iter (rest arguments) (1+ n))
				    (cl:lambda (,arguments-values)
				      ;; ...capturing the values in arguments-values
				      ;; Return the first argument-values appended to the rest of the arguments-values 
				      (funcall ,continuation (append ,argument-values ,arguments-values)))))))))))
    (arguments->cps-iter arguments 0))
  
  `(cl:lambda (,continuation)
     ;; Evalute the function argument...
     (funcall ,(expr->cps-form function)
	      ;; TODO: handle multiple values for explicit continuations
	      (cl:lambda (,function-result)
		;; ...capturing the value in function-result
		;; evaluate all arguments
		(funcall ,(arguments->cps arguments)
			 (cl:lambda (,arguments-results)
			   ;; capturing the values in arguments-results
			   ;; Apply the function to the arguments-results with the given continuation.
			   (apply/c ,continuation ,function-result ,arguments-results)))))))

(check-cps-for-expr '(multiple-value-call #'list (values 1 2) (values 3 4)))
(check-cps-for-expr '(multiple-value-call #'+ (floor 5 3) (floor 19 4)))

(define (load-time-value? expr)
  (and (pair? expr)
       (member (length expr) '(2 3))
       (eq? (first expr) 'cl:load-time-value)))
(define (load-time-value-form expr)
  (second expr))
(define (load-time-value-read-only-p expr)
  (third expr))
(define (load-time-value->cps form read-only-p)
  (atom->cps `(cl:load-time-value (funcall ,(expr->cps-form form) #'values) ,read-only-p)))

;; Check load-time-value
(assert (let* ((form '(cl:lambda (n) (+ n (load-time-value (random 42) nil))))
	       (fn [(eval (expr->cps form)) #'values]))
	  (for-all (lambda (n) (= n [fn 0]))
		   (list [fn 0]
			 [fn 0]
			 [fn 0]
			 [fn 0]))))

(define (catch? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:catch)))
(define (catch-tag expr)
  (second expr))
(define (catch-forms expr)
  (cddr expr))
(define (catch->cps tag forms)
  (define continuation (unique-symbol 'continue-from-catch))
  (define tag-value (unique-symbol 'catch-tag-value))
  `(cl:lambda (,continuation)
     ;; Evaluate tag...
     (funcall ,(expr->cps-form tag)
	      (cl:lambda (,tag-value)
		;; ...capturing the result in tag-value
		(multiple-value-call ,continuation
		  ;; Return the results of the catch to the continuation.
		  (cl:catch ,tag-value
		    ;; Convert FORMS to a progn, but only return to catch once.
		    (funcall ,(progn->cps forms) #'values)))))))

(define (throw? expr)
  (and (pair? expr)
       (= (length expr) 3)
       (eq? (first expr) 'cl:throw)))
(define (throw-tag expr)
  (second expr))
(define (throw-result expr)
  (third expr))
(define (throw->cps tag result)
  (define continuation (unique-symbol 'ignored-continue-from-throw))
  (define tag-value (unique-symbol 'throw-tag-value))
  (define result-values (unique-symbol 'throw-result-values))
  `(cl:lambda (,continuation)
     (declare (ignore ,continuation))
     ;; Evaluate tag...
     (funcall ,(expr->cps-form tag)
	      (cl:lambda (,tag-value)
		;; ...capturing the result in tag-value
		;; Evaluate result...
		(funcall ,(expr->cps-form result)
			 (cl:lambda (&rest ,result-values)
			   ;; Capturing the results in result-values
			   ;; Evaluate throw, ignoring the continuation.
			   (throw ,tag-value (values-list ,result-values))))))))

(check-cps-for-expr '(catch 'dummy-tag 1 2 (throw 'dummy-tag (values 3 3 3)) 4))
(check-cps-for-expr '(catch 'dummy-tag 1 2 3 (values 4 4 4 4)))
(defun test-throw-back (tag) (throw tag t))
(check-cps-for-expr '(catch 'dummy-tag (test-throw-back 'dummy-tag) 2))
(check-cps-for-expr '(catch 'c
		      (flet ((c1 () (throw 'c 1)))
			(catch 'c (c1) (print 'unreachable))
			2)))

(define (unwind-protect? expr)
  (and (pair? expr)
       (>= (length expr) 2)
       (eq? (first expr) 'cl:unwind-protect)))
(define (unwind-protect-protected expr)
  (second expr))
(define (unwind-protect-cleanup expr)
  (cddr expr))
(define (unwind-protect->cps protected-form cleanup-forms)
  (define continuation (unique-symbol 'continue-from-unwind-protect))
  `(cl:lambda (,continuation)
     ;; Call continuation with the results of unwind-protect.
     (apply ,continuation
	    (unwind-protect
		 ;; Evaluate the protected form, but only return to unwind-protect once.
		 (funcall ,(expr->cps-form protected-form) #'values)
	      ;; Evaluate the cleanup forms, but only return to unwind-protect once.
	      (funcall ,(progn->cps cleanup-forms) #'values)))))

(check-cps-for-expr '(catch 'bar
		      (catch 'foo
			(unwind-protect (throw 'foo 3)
			  (throw 'bar 4)
			  (error "UNREACHED")))))

(check-cps-for-expr '(catch 'foo
		      (format t "The inner catch returns ~s.~%"
		       (catch 'foo
			 (unwind-protect (throw 'foo :first-throw)
			   (throw 'foo :second-throw))))
		      :outer-catch))

(check-cps-for-expr '(catch nil 
		      (unwind-protect (throw nil 1)
			(throw nil 2))))
(check-cps-for-expr (block nil   
		      (unwind-protect (progn
					(return 1)
					(return 2))
			(return 3))))
(check-cps-for-expr '(cl:let (state)
		      (flet ((dummy-function (x)
			       (setq state 'running)
			       (unless (numberp x) (throw 'abort 'not-a-number))
			       (setq state (1+ x))))
			(list
			 (catch 'abort (dummy-function 1))
			 state

			 (catch 'abort (dummy-function 'trash))
			 state

			 (catch 'abort (unwind-protect (dummy-function 'trash) 
					 (setq state 'aborted)))
			 state))))
(check-cps-for-expr '(block nil
		      (let ((x 5))
			(declare (special x))
			(unwind-protect (return)
			  (print x)))))

;; NOTE: Not possible to goto tags from within unwind-protect or catch
#;(check-cps-for-expr '(cl:let (results)
			       (tagbody
				(cl:let ((x 3))
					(unwind-protect
					    (when (numberp x) (go out))
					  (push x results)))
				out (push :out results))
			       (nreverse results)))

(define (progv? expr)
  (and (pair? expr)
       (>= (length expr) 3)
       (eq? (first expr) 'cl:progv)
       (list? (progv-vars expr))
       (list? (progv-vals expr))))
(define (progv-vars expr) (second expr))
(define (progv-vals expr) (third expr))
(define (progv-forms expr) (cdddr expr))
(define (progv->cps vars vals forms)
  (atom->cps `(cl:progv ,vars ,vals ,@forms)))

(defmacro with-continuations (&body body)
  "Convert body into a similar expression that uses continuation-passing-style.
The values of body will be returned.

Be careful with dynamic forms such as CATCH and UNWIND-PROTECT.
Throwing should work correctly, but RETURN-FROM and GO will both exhibit
different behavior from CL when RETURNing-to or GOing-to a form outside of
the CATCH/UNWIND-PROTECT. This is because within a WITH-CONTINUATIONS form,
TAGBODY and BLOCK are lexical rather than dynamic.

TAGBODY and BLOCK are no longer dynamic-extent. Because of continuations
they can be re-entered at any time. RETURN-FROM can therefore be called any number of times."
  `(funcall ,(progn->cps body) #'values))

(define (simple-test)
  (define the-continuation nil)
  
  (define (test)
    (with-continuations
     (let ((i 0))
       (call/cc (lambda (k) (set! the-continuation k)))
       (set! i (1+ i))
       i)))

  (define another-continuation nil)
  (list (test)
	(the-continuation)
	(the-continuation)
	(progn
	  (set! another-continuation the-continuation)
	  (test))
	(the-continuation)
	(another-continuation)))
(simple-test)
;; => (1 2 3 1 2 4)

(with-continuations
 (define (coroutine-test)
   (define queue ())
   (define (empty-queue?)
     (empty? queue))
   (define (enqueue x)
     (set! queue (append queue (list x))))
   (define (dequeue)
     (let ((x (first queue)))
       (set! queue (rest queue))
       x))

   (define (fork proc)
     (call/cc (lambda (k) (enqueue k) [proc])))

   (define (yield)
     (call/cc (lambda (k) (enqueue k) [(dequeue)])))

   (define (thread-exit)
     (if (empty-queue?)
	 (return-from coroutine-test)
       [(dequeue)]))

   (define (do-stuff-n-print str)
     (lambda ()
       (let recurse ((n 0))
	    (cond
	     ((> n 10) (thread-exit))
	     (t (format t "~A ~A~%" str n)
		(yield)
		(recurse (+ n 1)))))))

   (fork (do-stuff-n-print "This is AAA"))
   (fork (do-stuff-n-print "Hello from BBB"))
   (thread-exit)))

(uninstall-syntax!)

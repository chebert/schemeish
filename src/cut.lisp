(in-package #:schemeish.cut)

(install-syntax!)

(for-macros
  (define (cut-form cut-spec id eval-once?)
    (define (lambda-form arg-spec apply-form bindings)
      "Construct the lambda form."
      (let ((form `(lambda ,arg-spec ,apply-form)))
	(if bindings
	    `(let* ,bindings ,form)
	    form)))
    
    (define (placeholder? argument)
      "Argument matches id"
      (eq? argument id))
    (define (make-argument-name num-args)
      "unique symbol of the form ARG<n> where n is the 0-based index."
      (unique-symbol (string-append "ARG" (number->string num-args))))
    (define (make-binding-name num-bindings)
      "unique symbol of the form BINDING<n> where n is the 0-based index."
      (unique-symbol (string-append "BINDING" (number->string num-bindings))))
    (define (make-rest-argument-name)
      "unique symbol of the form REST-ARG"
      (unique-symbol :rest-arg))
    
    (define (process-argument spec arg-spec apply-form num-args bindings)
      "Spec is of the form: (provided-argument . ...)"
      (cond
	(eval-once?
	 (let ((binding-name (make-binding-name (length bindings))))
	   (process-spec (rest spec)
			 arg-spec
			 (cons binding-name apply-form)
			 num-args
			 (cons (list binding-name (first spec)) bindings))))
	(t (process-spec (rest spec)
			 arg-spec
			 (cons (first spec) apply-form)
			 num-args
			 bindings))))
    (define (process-placeholder spec arg-spec apply-form num-args bindings)
      "Spec is of the form: (? . ...)"
      (let ((argument-name (make-argument-name num-args)))
	(process-spec (rest spec)
		      (cons argument-name arg-spec)
		      (cons argument-name apply-form)
		      (1+ num-args)
		      bindings)))

    (define (process-end-of-proper-list arg-spec apply-form bindings)
      "Spec is the empty list"
      (lambda-form (nreverse arg-spec)
		   (nreverse (cons () apply-form))
		   (nreverse bindings)))

    (define (process-rest-placeholder arg-spec apply-form bindings)
      "Spec is the rest placeholder: e.g the ? in (list 1 2 . ?)"
      (let ((argument-name (make-rest-argument-name)))
	(lambda-form (append (nreverse arg-spec) argument-name)
		     (nreverse (cons argument-name apply-form))
		     (nreverse bindings))))

    (define (process-rest-argument spec arg-spec apply-form bindings)
      "Spec is a rest argument: e.g the '(3 4) in (list 1 2 . '(3 4))"
      (cond
	(eval-once?
	 (let ((binding-name (unique-symbol :rest-binding)))
	   (lambda-form (nreverse arg-spec)
			(append (nreverse apply-form) binding-name)
			(nreverse (cons (list binding-name spec) bindings)))))
	(t (lambda-form (nreverse arg-spec)
			(append (nreverse apply-form) spec)
			(nreverse bindings)))))

    (define (process-spec spec arg-spec apply-form num-args bindings)
      "Process a spec."
      (cond
	;; spec is a list starting with a placeholder: (? . ...)
	((and (pair? spec) (placeholder? (first spec)))
	 (process-placeholder spec arg-spec apply-form num-args bindings))
	;; spec is a list starting with a provided argument: (value . ...) 
	((pair? spec)
	 (process-argument spec arg-spec apply-form num-args bindings))
	;; spec is empty list
	((null? spec) (process-end-of-proper-list arg-spec apply-form bindings))
	;; spec is a placeholder at the end of a dotted list
	((placeholder? spec) (process-rest-placeholder arg-spec apply-form bindings))
	;; spec is a rest argument
	(t (error "Expected NIL or placeholder ~S in dotted cut-spec ~S: got ~S" id cut-spec spec))))

    (define (initial-apply-form)
      "initial apply form is (apply #'fn) but reversed."
      (nreverse (list 'apply `(function ,(first cut-spec)))))
    
    (unless (not (pair? cut-spec))
      (process-spec (rest cut-spec)
		    ()
		    (initial-apply-form)
		    0
		    ()))))

(assert (equal? (with-readable-symbols
		  (cut-form '(list) '? nil))
		'(LAMBDA NIL
		  (APPLY #'LIST ()))))
(assert (equal? (with-readable-symbols
		  (cut-form '() '? nil))
		'()))
(assert (equal? (with-readable-symbols
		  (cut-form '(list 1 ? 3 ? . ?) '? nil))
		'(LAMBDA (ARG0 ARG1 . rest-arg)
		  (APPLY #'LIST 1 ARG0 3 ARG1 REST-ARG))))
(assert (not (ignore-errors (with-readable-symbols
			      (cut-form '(list ? 3 . 4) '? nil)))))

(assert (equal? (with-readable-symbols
		  (cut-form '(list 1 ? 3 . ?) '? t))
		'(LET* ((BINDING0 1) (BINDING1 3))
		  (LAMBDA (ARG0 . REST-ARG)
		    (APPLY #'LIST BINDING0 ARG0 BINDING1 REST-ARG)))))


(defmacro cut ((&rest cut-spec) &key (placeholder-id '?) (eval-once? t))
  "Creates a 'curried' function using cut-spec and placeholder-id.
Examples:
  [(cut (list 1 2 ? ?)) 3 4] => (1 2 3 4)
  [(cut (+ 5 . ?)) 1 2 3] => 11
  [(cut [(compose (cut (* 2 ?)) (cut (+ 2 ? ?))) 3 ?]) 3] => 16

If eval-once? is true, provided arguments will be evaluated once when the function is created."
  (cut-form cut-spec placeholder-id eval-once?))

(assert (equal? [(cut (list 1 ? 3 . ?)) 2 4 5 6]
		'(1 2 3 4 5 6)))

(uninstall-syntax!)

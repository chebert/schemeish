(in-package #:schemeish.cut)

(install-syntax!)

(for-macros
  (define (cut-form cut-spec id)
    (define (lambda-form arg-spec apply-form)
      `(lambda ,arg-spec ,apply-form))

    (define (placeholder? argument)
      (eq? argument id))
    (define (make-argument-name num-args)
      (unique-symbol (string-append "ARG" (number->string num-args))))
    (define (make-rest-argument-name)
      (unique-symbol :rest-arg))
    
    (define (process-argument spec arg-spec apply-form num-args)
      "Spec is of the form: (provided-argument . ...)"
      (process-spec (rest spec)
		    arg-spec
		    (cons (first spec) apply-form)
		    num-args))
    (define (process-placeholder spec arg-spec apply-form num-args)
      "Spec is of the form: (? . ...)"
      (let ((argument-name (make-argument-name num-args)))
	(process-spec (rest spec)
		      (cons argument-name arg-spec)
		      (cons argument-name apply-form)
		      (1+ num-args))))

    (define (process-end-of-proper-list arg-spec apply-form)
      "Spec is ()"
      (lambda-form (nreverse arg-spec)
		   (nreverse (cons () apply-form))))

    (define (process-rest-placeholder arg-spec apply-form)
      "Spec is the rest placeholder: e.g the ? in (list 1 2 . ?)"
      (let ((argument-name (make-rest-argument-name)))
	(lambda-form (append (nreverse arg-spec) argument-name)
		     (nreverse (cons argument-name apply-form)))))

    (define (process-spec spec arg-spec apply-form num-args)
      (cond
	;; spec is a list starting with a placeholder: (? . ...)
	((and (pair? spec) (placeholder? (first spec))) (process-placeholder spec arg-spec apply-form num-args))
	;; spec is a list starting with a provided argument: (value . ...) 
	((pair? spec) (process-argument spec arg-spec apply-form num-args))
	;; spec is empty list
	((null? spec) (process-end-of-proper-list arg-spec apply-form))
	;; spec is a placeholder at the end of a dotted list
	((placeholder? spec) (process-rest-placeholder arg-spec apply-form))
	(t (error "Invalid cut-spec: ~S. Expected placeholder: ~S or NIL." cut-spec id))))

    (define (initial-apply-form)
      (list (list 'function (first cut-spec)) 'apply))
    
    (unless (not (pair? cut-spec))
      (process-spec (rest cut-spec)
		    ()
		    (initial-apply-form)
		    0))))

(assert (equal? (with-readable-symbols
		  (cut-form '(list) '?))
		'(LAMBDA NIL
		  (APPLY #'LIST ()))))
(assert (equal? (with-readable-symbols
		  (cut-form '() '?))
		'()))
(assert (equal? (with-readable-symbols
		  (cut-form '(list 1 ? 3 ? . ?) '?))
		'(LAMBDA (ARG0 ARG1 . rest-arg)
		  (APPLY #'LIST 1 ARG0 3 ARG1 REST-ARG))))
(assert (not (ignore-errors (with-readable-symbols
			      (cut-form '(list 1 2 . 3) '?)))))


(defmacro cut ((&rest cut-spec) &optional (placeholder-id '?))
  "Creates a 'curried' function using cut-spec and placeholder-id.
Examples:
  [(cut (list 1 2 ? ?)) 3 4] => (1 2 3 4)
  [(cut (+ 5 . ?)) 1 2 3] => 11
  [(cut [(compose (cut (* 2 ?)) (cut (+ 2 ? ?))) 3 ?]) 3] => 16"
  (cut-form cut-spec placeholder-id))

(assert (equal? [(cut (list ? ? ? . ?)) 1 2 3 4 5 6]
		'(1 2 3 4 5 6)))


(uninstall-syntax!)

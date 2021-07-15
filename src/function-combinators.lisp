(defpackage #:function-combinators
  (:use :schemeish.schemeish))

(in-package #:function-combinators)

(shadow '(schemeish.schemeish:compose* schemeish.schemeish:compose))

(install-syntax!)

(defparameter *arity-table* (make-hash-table :weakness :key))

(export
 (define (restrict-arity proc arity)
   "Restricts the arity (see GET-ARITY) for proc."
   (hash-set! *arity-table* proc arity)
   proc))

(export
 (define (get-arity proc)
   "Gets the procedure-arity or restricted arity for proc."
   (or (hash-ref *arity-table* proc)
       (procedure-arity proc))))

(export
 (define (compose* procs)
   "Return the composition of procs with restricted arity."
   (foldr (lambda (f g)
	    (restrict-arity
	     (lambda args
	       (multiple-value-call f (apply g args)))
	     (get-arity g)))
	  #'values procs)))

(export
 (define (compose . procs)
   "Return the composition of procs with restricted arity."
   (compose* procs)))

(define (match-arity-specs spec-a spec-b)
  "Returns an arity-spec that matches spec-a and spec-b, or NIL if they cannot match."
  (define (rest-spec? spec)
    (and (list? spec) (eq? (first spec) :*)))
  (define (other-keys-spec? spec)
    (and (list? spec) (eq? (first spec) :**)))

  (define (match-other-keys-and-number other-keys number)
    (let ((n (cdr other-keys)))
      (when (and (eq? (even? n) (even? number))
		 (>= number n))
	number)))
  (define (match-rest-and-number rest number)
    (let ((n (cdr rest)))
      (when (>= number n)
	number)))
  (define (match-other-keys-and-rest other-keys rest)
    (let ((n-other (cdr other-keys))
	  (n-rest (cdr rest)))
      (cond
	((<= n-rest n-other) other-keys)
	((eq? (even? n-rest) (even? n-other))
	 (cons :** n-rest))
	(t
	 (cons :** (1+ n-rest))))))
  
  (cond
    ((and (number? spec-a) (number? spec-b) (= spec-a spec-b))
     spec-a)
    ((and (rest-spec? spec-a) (rest-spec? spec-b))
     (let ((na (cdr spec-a))
	   (nb (cdr spec-b)))
       (cons :* (max na nb))))
    ((and (other-keys-spec? spec-a) (other-keys-spec? spec-b))
     (let ((na (cdr spec-a))
	   (nb (cdr spec-b)))
       (when (eq? (even? na) (even? nb))
	 (cons :** (max na nb)))))

    ((and (rest-spec? spec-a) (number? spec-b))
     (match-rest-and-number spec-a spec-b))
    ((and (number? spec-a) (rest-spec? spec-b))
     (match-rest-and-number spec-b spec-a))

    ((and (other-keys-spec? spec-a) (number? spec-b))
     (match-other-keys-and-number spec-a spec-b))
    ((and (number? spec-a) (other-keys-spec? spec-b))
     (match-other-keys-and-number spec-b spec-a))

    ((and (other-keys-spec? spec-a) (rest-spec? spec-b))
     (match-other-keys-and-rest spec-a spec-b))
    ((and (rest-spec? spec-a) (other-keys-spec? spec-b))
     (match-other-keys-and-rest spec-b spec-a))))

(assert (equal? (match-arity-specs 2 4)
		'NIL))
(assert (equal? (match-arity-specs 2 2)
		'2))

(assert (equal? (match-arity-specs '(:* . 3) 2)
		NIL))
(assert (equal? (match-arity-specs '(:* . 3) 3)
		'3))
(assert (equal? (match-arity-specs '(:* . 3) 4)
		'4))

(assert (equal? (match-arity-specs '(:** . 4) 3)
		'NIL))
(assert (equal? (match-arity-specs '(:** . 4) 4)
		'4))
(assert (equal? (match-arity-specs '(:** . 4) 5)
		NIL))

(assert (equal? (match-arity-specs '(:* . 2) '(:* . 3))
		'(:* . 3)))

(assert (equal? (match-arity-specs '(:** . 2) '(:** . 3))
		'nil))
(assert (equal? (match-arity-specs '(:** . 5) '(:** . 3))
		'(:** . 5)))

(assert (equal? (match-arity-specs '(:** . 3) '(:* . 3))
		'(:** . 3)))
(assert (equal? (match-arity-specs '(:** . 3) '(:* . 4))
		'(:** . 5)))
(assert (equal? (match-arity-specs '(:** . 3) '(:* . 6))
		'(:** . 7)))

(define (matching-arity arity-a arity-b)
  "Returns an arity that matches both a and b or NIL if no arities match."
  (define (loop-b result spec-a a b)
    (cond ((null? b) (loop-a result (rest a)))
	  (t (let ((spec-b (first b)))
	       (let ((match (match-arity-specs spec-a spec-b)))
		 (loop-b (if match
			     (adjoin match result :test #'equal?)
			     result)
			 spec-a
			 a
			 (rest b)))))))

  (define (loop-a result a)
    (cond ((null? a) result)
	  (t (loop-b result (first a) a arity-b))))

  (nreverse (loop-a () arity-a)))

(assert (equal? (matching-arity '(2 4 (:** . 6))
				'((:* . 3)))
		'(4 (:** . 6))))


(define (fixed-arity? arity)
  "True if arity has a single, numerical arity."
  (and (pair? arity) (null? (rest arity)) (number? (first arity))))
(define (fixed-arity-nargs arity)
  "Returns number of arguments in the fixed-arity."
  (first arity))

(export
 (define (parallel-apply f g)
   "return a function with an arity that matches f and g and applies args to both f and g and returns the values of f and g."
   (define (the-combination . args)
     (multiple-value-call #'values (apply f args) (apply g args)))
   (let ((arity (matching-arity (get-arity f) (get-arity g))))
     (assert (not (null? arity)))
     (restrict-arity the-combination arity))))

(export
 (define (parallel-combine h f g)
   "return a function with an arity that matches f and g and applies args to both f and g and calls h with the results of f then g."
   (compose h (parallel-apply f g))))

(export
 (define (spread-apply f g)
   "Return a function with a fixed-arity which applies the first arguments to f and the rest of the arguments to g. Returns the values of f then g."
   (let ((arity-f (get-arity f))
         (arity-g (get-arity g)))
     (assert (fixed-arity? arity-f))
     (assert (fixed-arity? arity-g))

     (let ((n (fixed-arity-nargs arity-f))
	   (m (fixed-arity-nargs arity-g)))

       (let ((o (+ n m)))
	 (restrict-arity
	  (lambda args
	    (assert (= o (length args)))
	    (multiple-value-call #'values
	      (apply f (take args n))
	      (apply g (drop args n))))
	  (list o)))))))
(export
 (define (spread-combine h f g)
   "Return a function with a fixed-arity which applies the first arguments to f and the rest of the arguments to g. Applies h to the return values of f then g."
   (compose h (spread-apply f g))))

(export
 (define (permute-arguments . permspec)
   "Return a function f, with arguments permuted according to permspec."
   (let ((permute (make-permutation permspec)))
     (lambda (f)
       (define (the-combination . args)
         (apply f [permute args]))
       (let ((n (get-arity f)))
         (assert (= n (length permspec)))
         (restrict-arity the-combination n))))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p))
         permspec))
  the-permuter)

(define (add-arity arity-a nargs)
  (map (lambda (spec)
	 (cond ((number? spec) (+ spec nargs))
	       (t (cons (car spec) (+ nargs (cdr spec))))))
       arity-a))
(define (arity-has-nargs? arity nargs)
  (there-exists (lambda (spec)
		  (cond ((number? spec) (= spec nargs))
			((eq? (car spec) :*)
			 (<= (cdr spec) nargs))
			((eq? (car spec) :**)
			 (let ((n (cdr spec)))
			   (and (<= n nargs)
				(eq? (even? n) (even? nargs)))))))
		arity))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (assert (pair? lst))
    (if (= index 0)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cons value lst)
        (progn
          (assert (pair? lst))
          (cons (car lst) (lp (cdr lst) (- index 1)))))))

(define (make-discarder indices)
  (let ((sorted-indices (sort indices #'>)))
    (lambda (args)
      (foldl (lambda (index result)
	       (list-remove result index))
	     args
	     sorted-indices))))

(define (make-curryer indices args)
  (lambda (f-args)
    (foldl (lambda (index value result)
	     (list-insert result index value))
	   args
	   indices
	   f-args)))

(assert (equal? [(make-discarder '(1 2)) '(:a :b :c :d)]
		'(:A :D)))
(assert (equal? [(make-curryer '(1 3) '(:a :c)) '(:b :d)]
		'(:A :B :C :D)))


(export
 (define (discard-arguments . indices)
   (let ((discard (make-discarder indices))
	 (max-i (apply #'max indices)))
     (lambda (f)
       (let ((m (add-arity (get-arity f) (length indices))))
	 (assert (arity-has-nargs? m (1+ max-i)))
	 (restrict-arity
	  (lambda args
	    (assert (arity-has-nargs? m (length args)))
	    (apply f [discard args]))
	  m))))))

(assert (equal? [[(discard-arguments 1 3) #'list] :a :b :c :d]
		'(:A :C)))

(export
 (define ((curry-arguments . indices) . args)
   (let ((curry (make-curryer indices args))
	 (n (+ (length indices) (length args))))
     (lambda (f)
       (assert (arity-has-nargs? (get-arity f) n))
       (lambda f-args
	 (apply f [curry f-args]))))))

(assert (equal? [[[(curry-arguments 1 3) :a :c] #'list] :b :d]
		'(:A :B :C :D)))

(uninstall-syntax!)

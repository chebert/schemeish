(in-package #:schemeish.backend)

(install-syntax!)

;;; Procedures

(export
 (define procedure? #'functionp))

(export
 (define (compose* procs)
   "Function compositions. Mulitple values of one function are used as arguments to the next."
   (foldr (lambda (proc result)
	    (lambda args
	      (multiple-value-call proc (apply result args))))
	  (lambda args (values-list args))
	  procs)))

(export
 (define (compose . procs)
   "Function compositions. Mulitple values of one function are used as arguments to the next."
   (compose* procs)))


(assert (equal (multiple-value-list [(compose) :x :y :z])
	       '(:x :y :z)))

(assert (equal [(compose (lambda (x y z) (list 'f x y z))) 'x 'y 'z]
	       '(f x y z)))

(assert (equal
	 [(compose (lambda (a b c) (list 'f a b c))
		   (lambda (x y) (values (list 'g x) (list 'g y) (list 'g 'c))))
	  'x 'y]

	 '(f (g x) (g y) (g c))))



(define (remove-indices indices list)
  "Remove all 0-based indices from list."
  (nreverse (foldl (lambda (arg index result)
		     (if (member index indices)
			 result
			 (cons arg result)))
		   ()
		   list
		   (range (length list)))))

(assert (equal (remove-indices '(1 3) '(a b c d e f)) '(a c e f)))

(export
 (define ((ignore-args . indices) f)
   "Return a function, which when applied to a function F ignores positional arguments matching the 0-based indices."
   (lambda args
     (apply f (remove-indices indices args)))))

(assert (equal [[(ignore-args 1 4) (lambda (a c d) (list a c d))]
		:a :b :c :d :e]
	       '(:A :C :D)))


(export
 (define (lcurry proc . left-args)
   "Return a procedure waiting for the right-args."
   (lambda right-args
     (apply proc (append left-args right-args)))))

(assert (= [(lcurry '- 5 4) 3]
	   (- 5 4 3)))

(export
 (define (rcurry proc . right-args)
   "Return a procedure waiting the left-args."
   (lambda left-args
     (apply proc (append left-args right-args)))))

(assert (= [(rcurry '- 4 3) 5]
	   (- 5 4 3)))

(export
 (define (swap-args proc)
   "Swap args of 2-argument procedure proc."
   (lambda (x y) [proc y x])))

(assert (equal [(swap-args 'cons) 1 2]
	       (cons 2 1)))

(export
 (define (memo-proc proc)
   "Memoize procedure proc of no arguments."
   (let ((run? ())
	 (result-values))
     (lambda args
       (unless run?
	 (setq result-values (multiple-value-list (apply proc args))
	       run? t)
	 result-values)
       (values-list result-values)))))

(export
 (define (disjoin* predicates)
   "Return a predicate equivalent to predicates joined together with or."
   (lambda (x)
     (let rec ((result nil)
	       (predicates predicates))
       (if (or result (null? predicates))
	   result
	   (rec [(first predicates) x]
		(rest predicates)))))))
(export
 (define (disjoin . predicates)
   "Return a predicate equivalent to predicates joined together with or."
   (disjoin* predicates)))

(assert (equal (map (disjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(t t nil t)))


(export
 (define (conjoin* predicates)
   "Return a predicate equivalent to predicates joined together with and."
   (lambda (x)
     (let rec ((result t)
	       (predicates predicates))
       (if (or (not result) (null? predicates))
	   result
	   (rec [(first predicates) x]
		(rest predicates)))))))
(export
 (define (conjoin . predicates)
   "Return a predicate equivalent to predicates joined together with and."
   (conjoin* predicates)))

(export
 (define (for-all* predicate lists)
   (apply #'every predicate lists)))
(export
 (define (for-all predicate . lists)
   (for-all* predicate lists)))

(export
 (define (there-exists* predicate lists)
   (apply #'some predicate lists)))
(export
 (define (there-exists predicate . lists)
   (there-exists* predicate lists)))

(assert (equal (map (conjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(nil t nil nil)))

(export
 (define (const v)
   "Return a procedure of 0+ args that always returns v"
   (lambda args
     (declare (ignore args))
     v)))

(assert (= [(const 3) 1 2 3] 3))

(uninstall-syntax!)

(in-package #:schemeish.backend)

(install-syntax!)

;; TODO: Replace these with map-ordinary-lambda-list
(defparameter *lambda-list-keywords*
  '(&optional &rest &key &allow-other-keys &aux))
(define (parse-lambda-list-arguments argument-list)
  (define (parse-arg-sublist args result result-key arg-proc (parsed-args))
    (cond ((or (empty? args)
	       (member (first args) *lambda-list-keywords*))
	   ;; Reached the end of this sublist.
	   ;; Add parsed-args to the result alist and continue parsing from parse-args
	   (parse-args args (alist-set result result-key (nreverse parsed-args))))
	  (t
	   ;; Add this argument to the parsed-arg-list and continue parsing the sublist.
	   (parse-arg-sublist (rest args)
			      result
			      result-key
			      arg-proc
			      (cons [arg-proc (first args)]
				    parsed-args)))))
  
  (define (parse-required-args args result)
    (parse-arg-sublist args result :required #'identity))
  
  (define (parse-optional-args args result)
    (parse-arg-sublist args result :optional (lambda (arg) (first (flatten arg)))))

  (define (parse-key-args args result)
    (parse-arg-sublist args result :key (lambda (arg) (first (flatten arg)))))
  
  (define (parse-args args (result))
    (cond
      ;; No more arguments. Return the result.
      ((empty? args) (nreverse result))
      (t
       (let ((arg (first args)))
	 (cond
	   ((eq? arg '&optional) (parse-optional-args (rest args) result))
	   ((eq? arg '&rest) (parse-args (drop args 2) (alist-set result :rest (second args))))
	   ((eq? arg '&key) (parse-key-args (rest args) result))
	   ((eq? arg '&allow-other-keys) (parse-args (rest args) (alist-set result :allow-other-keys? t)))
	   ((eq? arg '&aux) (parse-args () result))
	   (t (parse-required-args args result)))))))
  (parse-args argument-list))

(export
 (define (procedure-arguments procedure)
   "Returns the procedure's argument list in the form of an alist with the following keys (in order):
    (:required . required-arguments)
    (:optional . optional-arguments)
    (:rest . rest-arg-name)
    (:key . keyword-arguments) 
    (:allow-other-keys? . t/nil)"
   (parse-lambda-list-arguments (arg:arglist procedure))))

(assert (equal? (procedure-arguments (cl:lambda (a b c) a b c))
		'((:REQUIRED A B C))))

(assert (equal? (procedure-arguments (cl:lambda (a b c &optional d (e 1) (f 2 f-provided?))
				       a b c d e f f-provided?))
		'((:REQUIRED A B C) (:OPTIONAL D E F))))

(assert (equal? (procedure-arguments (cl:lambda (a b c &optional d (e 1) (f 2 f-provided?) &rest rest)
				       a b c d e f f-provided? rest))
		'((:REQUIRED A B C) (:OPTIONAL D E F) (:REST . REST))))

;; Disable tests that involve &optional and &key arguments

#+nil
(assert (equal? (procedure-arguments (cl:lambda (a b c &optional d (e 1) (f 2 f-provided?) &rest rest &key g (h 1) (i 2 i-provided?))
				       a b c d e f f-provided? rest g h i i-provided?))
		'((:REQUIRED A B C) (:OPTIONAL D E F) (:REST . REST) (:KEY G H I))))

#+nil
(assert (equal? (procedure-arguments (cl:lambda (a b c &optional d (e 1) (f 2 f-provided?) &rest rest &key g (h 1) (i 2 i-provided?) &allow-other-keys)
				       a b c d e f f-provided? rest g h i i-provided?))
		'((:REQUIRED A B C) (:OPTIONAL D E F) (:REST . REST) (:KEY G H I)
		  (:ALLOW-OTHER-KEYS? . T))))

#+nil
(assert (equal?
	 (procedure-arguments (cl:lambda (a b c &optional d (e 1) (f 2 f-provided?) &rest rest &key g (h 1) (i 2 i-provided?) &allow-other-keys &aux j (k 1))
				a b c d e f f-provided? rest g h i i-provided? j k))
	 '((:REQUIRED A B C) (:OPTIONAL D E F) (:REST . REST) (:KEY G H I)
	   (:ALLOW-OTHER-KEYS? . T))))


[(lcurry (lambda (:k1) k1) :k1) 'keywords-have-arity=2-or-0]
;; => KEYWORDS-HAVE-ARITY=2-OR-0

(null? (ignore-errors [(lcurry (lambda (:k1) k1) :k1)]))
;; => T

[(lambda (:k1) k1)]
;; => NIL

[(lambda ((optional)) optional)]
;; => NIL
[(lambda ((optional)) optional) :optionals-have-arity=1-or-0]
;; => :OPTIONALS-HAVE-ARITY=1-OR-0

[(cl:lambda (&key &allow-other-keys) :allow-other-keys-have-infinite-even-arity) 1 2 3 4 5 6 7 8]
;; Disable tests that pass improper arities

#+nil
(null? (ignore-errors [(cl:lambda (&key &allow-other-keys) :allow-other-keys-have-infinite-even-arity) 1 2 3 4 5 6 7 8 9]))
;; => T

[(cl:lambda (&rest rest) rest) :rest :has :infinite :arity]
;; => (:REST :HAS :INFINITE :ARITY)

[(cl:lambda (&rest rest &key &allow-other-keys) rest) :rest :and :allow-other-keys :have :infitite :arity]
;; => (:REST :AND :ALLOW-OTHER-KEYS :HAVE :INFITITE :ARITY)
#+nil
(null? (ignore-errors [(cl:lambda (&rest rest &key &allow-other-keys) rest) :even :arity :only!]))
;; => T

(export
 (define (procedure-arguments-required-arguments arguments)
   (alist-ref arguments :required ())))
(export
 (define (procedure-arguments-optional-arguments arguments)
   (alist-ref arguments :optional ())))
(export
 (define (procedure-arguments-key-arguments arguments)
   (alist-ref arguments :key ())))
(export
 (define (procedure-arguments-rest-argument arguments)
   (alist-ref arguments :rest ())))
(export
 (define (procedure-arguments-allow-other-keys? arguments)
   (alist-ref arguments :allow-other-keys? ())))

(export
 (define (procedure-arity procedure)
   "Returns an arity of the form '(n1 n2 n3 ...) where n is one of:
  an integer representing an exact number of arguments
  a pair '(:* . X) representing an indefinite number of arguments following x number of arguments,
  or a pair '(:** . X) representing an indefinite number of key-argument pairs following x number of arguments.

  Examples:
   (procedure-arity (cl:lambda (fixed1 fixed2 &optional opt1 opt2 &rest rest &key key1 key2) ...)) ;; => '(2 3 4 6 (:* . 8))
   (procedure-arity (cl:lambda (&rest rest &key k1 k2 &allow-other-keys) ...)) ;; => '(2 (:** . 4))
"
   (define arguments (procedure-arguments procedure))
   (define required-arity (list (length (procedure-arguments-required-arguments arguments))))

   (define (extend-arity base-arity arity-proc)
     (cons [arity-proc (first base-arity)] base-arity))
   
   (define (arity-extended-by-optional-like base-arity num arg-arity-n)
     (cond ((= 0 num) base-arity)
	   (t (arity-extended-by-optional-like
	       (extend-arity base-arity (lambda (n) (+ arg-arity-n n)))
	       (1- num)
	       arg-arity-n))))

   (define (arity-extended-by-optionals base-arity)
     (arity-extended-by-optional-like base-arity
				      (length (procedure-arguments-optional-arguments arguments))
				      1))
   (define (arity-extended-by-keys base-arity)
     (arity-extended-by-optional-like base-arity
				      (length (procedure-arguments-key-arguments arguments))
				      2))

   (define (arity-extended-by-indefinite base-arity rest? allow-other-keys?)
     (cond
       (allow-other-keys? (cons (cons :** (first base-arity)) (rest base-arity)))
       (rest? (cons (cons :* (first base-arity)) (rest base-arity)))
       (t base-arity)))

   (define (arity-finished base-arity)
     (nreverse base-arity))
   
   (arity-finished
    (arity-extended-by-indefinite
     (arity-extended-by-keys (arity-extended-by-optionals required-arity))
     (not (null? (procedure-arguments-rest-argument arguments)))
     (procedure-arguments-allow-other-keys? arguments)))))

(assert (equal? (procedure-arity (cl:lambda ()))
		'(0)))
(assert (equal? (procedure-arity (cl:lambda (a b c) a b c))
		'(3)))
(assert (equal? (procedure-arity (cl:lambda (a b &optional c d) a b c d))
		'(2 3 4)))
(assert (equal? (procedure-arity (cl:lambda (a b &optional c d &rest rest) a b c d rest))
		'(2 3 (:* . 4))))

#+nil
(assert (equal? (procedure-arity (cl:lambda (a b &optional c d &rest rest &key e f) a b c d e f rest))
		'(2 3 4 6 (:* . 8))))
#+nil
(assert (equal? (procedure-arity (cl:lambda (a b &optional c d &rest rest &key e f &allow-other-keys) a b c d e f rest))
		'(2 3 4 6 (:** . 8))))
(assert (equal? (procedure-arity (cl:lambda (&rest rest) rest))
		'((:* . 0))))
(assert (equal? (procedure-arity (cl:lambda (&key &allow-other-keys)))
		'((:** . 0))))

(export
 (define (has-specific-arity? arity-list fixed-arity-n)
   "Returns true if an arity-list (retrieved from procedure-arity) has the specific fixed arity."
   (assert (or (zero? fixed-arity-n) (positive? fixed-arity-n)))
   (cond ((empty? arity-list) nil)
	 (t
	  (let ((arity (first arity-list)))
	    (cond
	      ((and (number? arity) (= fixed-arity-n arity)) t)
	      ((pair? arity)
	       (let ((type (car arity))
		     (value (cdr arity)))
		 (cond
		   ((eq? type :*) (<= value fixed-arity-n))
		   ((eq? type :**) (and (<= value fixed-arity-n)
					(eq? (even? fixed-arity-n) (even? value)))))))
	      (t (has-specific-arity? (rest arity-list) fixed-arity-n))))))))

(assert (has-specific-arity? '(2 3 4) 3))
(assert (not (has-specific-arity? '(2 3 4) 5)))
(assert (has-specific-arity? '(2 3 (:* . 4)) 5))
(assert (not (has-specific-arity? '(2 3 (:** . 4)) 5)))
(assert (has-specific-arity? '(2 3 (:** . 4)) 6))

;; TODO: Arity-table
;; TODO: Restrict arity when creating higher order functions. COMPOSE, etc.
;; TODO: Generics

(uninstall-syntax!)

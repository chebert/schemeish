(in-package #:schemeish.base)

(for-macros (install-syntax!))

(define (make-keyword symbol)
  (intern (symbol-name symbol) :keyword))

(define (symbol->string symbol) (symbol-name symbol))

(define (symbol? datum) (symbolp datum))


(define (procedure? datum) (functionp datum))

(define (document! proc docstring)
  "Attach documentation to proc before returning it."
  (setf (documentation proc 'function) docstring)
  proc)


(define (eq? obj1 obj2) (eq obj1 obj2))

(defgeneric equal? (object1 object2)
  (:documentation "Provides a generic interface to EQUAL."))
(defmethod equal? (object1 object2) (equal object1 object2))

(define (map function list &rest more-lists)
  (apply #'mapcar function list more-lists))

(define (append* lists)
  (apply #'append lists))

(define (empty? datum) (null datum))

(define (for-each proc . lists)
  "Apply proc to each element of lists. Arity of proc should match the number of lists."
  (let rec ((lists lists))
    (unless (member nil lists)
      (apply proc (map 'first lists))
      (rec (map 'rest lists)))))

(assert (equal?
	 (with-output-to-string (s)
	   (for-each (lambda (x y) (format s "~S" (list x y)))
		     '(a b c)
		     '(1 2 3)))
	 "(A 1)(B 2)(C 3)"))

(define (filter predicate list)
  "Keep elements of list that satisfy predicate."
  (remove-if-not predicate list))

(define (pair? datum) "T if datum is a cons." (consp datum))
(define (null? datum) "T if datum is nil." (null datum))
(define (list? datum) "Alias for (listp datum)." (listp datum))



(define (list-ref list pos)
  "Return the value of list at pos."
  (nth pos list))

(define (list-tail list pos)
  "Return the sublist of list starting at pos."
  (nthcdr pos list))

(define (foldl proc init . lists)
  "Fold (proc e1 e2 ... result) across lists starting from the start of the lists."
  (let rec ((result init)
	    (lists lists))
    (if (or (empty? lists) (member nil lists))
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldl 'cons () '(1 2 3 4))
	       '(4 3 2 1)))

(assert (= (foldl (lambda (a b result)
		    (* result (- a b)))
		  1
		  '(1 2 3)
		  '(4 5 6))
	   -27))

(define (foldr proc init . lists)
  "Fold (proc e1 e2 ... result) across lists starting from the end of the lists."
  (let rec ((result init)
	    (lists (map 'reverse lists)))
    (if (or (empty? lists) (member nil lists))
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldr 'cons '() '(1 2 3 4))
	       '(1 2 3 4)))
(assert (equal (foldr (lambda (v l) (cons (1+ v) l)) '() '(1 2 3 4))
	       '(2 3 4 5)))

(define (negative? num) (minusp num))
(define (positive? num) (plusp num))

(define (andmap proc . lists)
  "Return the last non-nil result of mapping proc across lists, or nil if some result is nil."
  (let rec ((result t)
	    (lists lists))
    (if (or (not result) (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))

(assert (andmap 'positive? '(1 2 3)))
;; (andmap 'positive? '(1 2 a)) => error
(assert (not (andmap 'positive? '(1 -2 a))))
(assert (= 9 (andmap '+ '(1 2 3) '(4 5 6))))

(define (ormap proc . lists)
  "Return the first non-nil result of mapping proc across lists."
  (let rec ((result ())
	    (lists lists))
    (if (or result (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))


(assert (ormap 'eq? '(a b c) '(a b c)))
(assert (ormap 'positive? '(1 2 a)))
(assert (= 5 (ormap '+ '(1 2 3) '(4 5 6))))


(define (remq v list)
  "Remove using eq? as a test."
  (remove v list :test #'eq))

(define (remove* v-list list (test #'equal?))
  "Removes all elements in v-list from list."
  (foldl (lambda (v result) (remove v result :test test)) list v-list))

(assert (equal
	 (remove* (list 1 2) (list 1 2 3 2 4 5 2))
	 '(3 4 5)))

(define (remq* v-list list) (remove* v-list list #'eq?))

(define (sort list less-than? (:extract-key (lambda (x) x)))
  "Returns a sorted list."
  (cl:sort (copy-list list) less-than? :key extract-key))

(assert (equal (let* ((original-list '(1 3 4 2))
		      (sorted-list (sort original-list '<)))
		 (assert (equal '(1 3 4 2) original-list))
		 sorted-list)
	       '(1 2 3 4)))


(define (memf proc list)
  "Returns the first sublist of list whose first element satisfies predicate proc."
  (let rec ((list list))
    (if (or (null? list) [proc (first list)])
	list
	(rec (rest list)))))

(assert (equal (memf (lambda (arg) (> arg 9)) '(7 8 9 10 11))
	       '(10 11)))

(define (findf proc list)
  "Finds the first element in list that satisfies predicate proc."
  (let ((found (memf proc list)))
    (if (list? found)
	(first found)
	())))

(assert (= (findf (lambda (arg) (> arg 9)) '(7 8 9 10 11))
	   10))

(define (list-update list pos updater)
  "Returns a list with (updater value) to the value at pos in list."
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (cond
      ((null? list) (nreverse result))
      (t
       (rec (rest list)
	    (1+ current-pos)
	    (cons (let ((x (first list)))
		    (if (= current-pos pos) [updater x] x))
		  result))))))

(assert (equal (list-update '(zero one two) 1 'symbol->string)
	       '(ZERO "ONE" TWO)))


(define (list-set list pos value)
  "Return a list with the value at pos replaced."
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (cond
      ((null? list) (nreverse result))
      (t
       (rec (rest list)
	    (1+ current-pos)
	    (cons (let ((x (first list)))
		    (if (= current-pos pos) value x))
		  result))))))

(assert (equal (list-set '(zero one two) 2 "two")
	       '(zero one "two")))

(define (take list n)
  "Takes the first n elements from list"
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (if (or (= n current-pos) (empty? list))
	(nreverse result)
	(rec (rest list)
	     (1+ current-pos)
	     (cons (first list) result)))))

(assert (equal (take '(1 2 3 4 5) 2)
	       '(1 2)))

(define (drop list n)
  "Drops the first n elements from list"
  (list-tail list n))

(define (split-at list pos)
  "Returns (list (take list pos) (drop list pos))"
  (list (take list pos) (drop list pos)))

(define (even? x) (evenp x))
(define (odd? x) (oddp x))
(define (zero? x) (zerop x))


(define (compose . procs)
  "Function compositions. Mulitple values of one function are used as arguments to the next."
  (foldr (lambda (proc result)
	   (lambda args
	     (multiple-value-call proc (apply result args))))
	 (lambda args (values-list args))
	 procs))

(assert (equal (multiple-value-list [(compose) :x :y :z])
	       '(:x :y :z)))

(assert (equal [(compose (lambda (x y z) (list 'f x y z))) 'x 'y 'z]
	       '(f x y z)))

(assert (equal
	 [(compose (lambda (a b c) (list 'f a b c))
		   (lambda (x y) (values (list 'g x) (list 'g y) (list 'g 'c))))
	  'x 'y]

	 '(f (g x) (g y) (g c))))

(define (filter-map proc . lists)
  "Remove nil from the result of mapping proc over lists."
  (remove nil (apply 'map proc lists)))

(define (range end (start 0) (step 1))
  "Return a list of elements from [start,end) using step as the stepsize."
  (if (<= start end)
      (loop for i from start below end by step collecting i)
      (loop for i from start downto end by step collecting i)))

(define (append-map proc . lists)
  "Append the results of mapping procedure across lists."
  (append* (apply 'map proc lists)))

(define (map-successive n f list)
  "Maps f over successive groups of size n in list."
  (let rec ((list list)
	    (length (length list))
	    (result ()))
    (cond
      ((< length n) (nreverse result))
      (t (rec (rest list)
	      (1- length)
	      (cons (apply f (subseq list 0 n)) result))))))

(assert (equal? (map-successive 3 'list (list 1 2 3 4))
		'((1 2 3) (2 3 4))))

(define (filter-not pred list)
  "Returns a list of elements that don't satisfy predicate pred."
  (filter (compose 'not pred) list))

(define (partition pred list)
  "Returns (list elements-satisfying-pred elements-not-satisfying-pred)"
  (list (filter pred list)
	(filter-not pred list)))

(assert (equal (partition 'even? '(1 2 3 4 5 6))
	       '((2 4 6) (1 3 5))))

(define (lcurry proc . left-args)
  "Return a procedure waiting for the right-args."
  (lambda right-args
    (apply proc (append left-args right-args))))

(assert (= [(lcurry '- 5 4) 3]
	   (- 5 4 3)))

(define (rcurry proc . right-args)
  "Return a procedure waiting the left-args."
  (lambda left-args
    (apply proc (append left-args right-args))))

(assert (= [(rcurry '- 4 3) 5]
	   (- 5 4 3)))

(define (swap-args proc)
  "Swap args of 2-argument procedure proc."
  (lambda (x y) [proc y x]))

(assert (equal [(swap-args 'cons) 1 2]
	       (cons 2 1)))

(define (memo-proc proc)
  "Memoize procedure proc of no arguments."
  (let ((run? ())
	(result-values))
    (lambda args
      (unless run?
	(setq result-values (multiple-value-list (apply proc args))
	      run? t)
	result-values)
      (values-list result-values))))


(define (alist-ref alist key (failure-result))
  "Rerturns the value associated with key in alist, else the failure-result."
  (let ((pair (assoc key alist :test #'equal?)))
    (if (pair? pair)
	(cdr pair)
	failure-result)))
(define (alist-remove alist key)
  "Returns an alist with key removed."
  (remove key alist :test #'equal? :key #'car))
(define (alist-set alist key value)
  "Returns an alist with key set to value."
  (acons key value (alist-remove alist key)))

(define (alist-update alist key updater (failure-result))
  "Applies updater to the value associated with key and updates the result in alist.
Applies updater to failure-result if key is not present."
  (alist-set alist key [updater (alist-ref alist key failure-result)]))

(define (alist-map alist proc)
  "Alist with proc applied to all values of alist."
  (map (lambda (binding) [proc (car binding) (cdr binding)]) alist))

(define (alist-keys alist)
  "A list of all keys in alist."
  (alist-map alist (lambda (key value) (declare (ignore value)) key)))
(define (alist-values alist)
  "A list of all of the values in alist."
  (alist-map alist (lambda (key value) (declare (ignore key)) value)))

(define (alist-has-key? alist key)
  "T if the key is present in alist"
  (let ((no-key (gensym)))
    (not (eq? no-key (alist-ref alist key no-key)))))

(define (alist-set* alist . keys-and-values)
  "Update all of the values in alist with pairs of key value ..."
  (let rec ((keys-and-values keys-and-values)
	    (alist alist))
    (cond
      ((empty? keys-and-values) alist)
      ((empty? (rest keys-and-values)) (error "badly formed arguments."))
      (t (let ((key (first keys-and-values))
	       (value (second keys-and-values)))
	   (rec
	    (drop keys-and-values 2)
	    (alist-set alist key value)))))))

(define (alist . keys-and-values)
  "Constructs an alist from pairs of key value ..."
  (nreverse (apply #'alist-set* () keys-and-values)))

(define (disjoin* predicates)
  "Return a predicate equivalent to predicates joined together with or."
  (lambda (x)
    (let rec ((result nil)
	      (predicates predicates))
      (if (or result (null? predicates))
	  result
	  (rec [(first predicates) x]
	       (rest predicates))))))
(define (disjoin . predicates)
  "Return a predicate equivalent to predicates joined together with or."
  (disjoin* predicates))

(assert (equal (map (disjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(t t nil t)))


(define (conjoin* predicates)
  "Return a predicate equivalent to predicates joined together with and."
  (lambda (x)
    (let rec ((result t)
	      (predicates predicates))
      (if (or (not result) (null? predicates))
	  result
	  (rec [(first predicates) x]
	       (rest predicates))))))
(define (conjoin . predicates)
  "Return a predicate equivalent to predicates joined together with and."
  (conjoin* predicates))

(define (for-all* predicate list)
  (every predicate list))
(define (for-all predicate . list)
  (every predicate list))

(define (there-exists* predicate list)
  (some predicate list))
(define (there-exists predicate . list)
  (some predicate list))

(assert (equal (map (conjoin 'negative? 'even?)
		    '(-1 -2 1 2))
	       '(nil t nil nil)))

(define (const v)
  "Return a procedure of 0+ args that always returns v"
  (lambda args
    (declare (ignore args))
    v))

(assert (= [(const 3) 1 2 3] 3))


(defmacro nand (&rest expressions)
  "The same as (not (and expressions...))"
  `(not (and ,@expressions)))

(defmacro nor (&rest expressions)
  "The same as (not (or expressions...))"
  `(not (or ,@expressions)))

(define (xor b1 b2)
  "Logical xor of booleans b1 and b2."
  (or (and b1 (not b2))
      (and b2 (not b1))))

(define (quotient n m)
  "Trunacate n/m"
  (truncate n m))

(define (number->string number (radix 10))
  "Convert number to string using radix as the base."
  (let ((*print-base* radix))
    (format nil "~S" number)))

(define (degr1ees->radians deg)
  "convert degrees to radians"
  (/ (* pi deg) 180))
(define (radians->degrees rads)
  "convert radians to degrees."
  (/ (* 180 rads) pi))

(define (sqr n)
  "n*n"
  (* n n))

(define (sgn x)
  "Return the sign of x: 1,-1, or 0"
  (cond
    ((positive? x) 1)
    ((negative? x) -1)
    ((zero? x) 0)))

(define (number? datum) (numberp datum))


(define (set-member? set value)
  "True if value is a member of set."
  (member value set :test #'equal?))
(define (set-add set value)
  "Adds value to set."
  (if (set-member? set value)
      set
      (cons value set)))
(define (set-remove set value)
  "Removes value from set."
  (remove set value :test #'equal?))
(define (set-empty? set)
  "True if set is empty."
  (empty? set))
(define (set-count set)
  "Number of elements in set."
  (length set))
(define (set->stream set)
  "Returns the elements of set as a stream."
  (list->stream set))
(define (set-union . sets)
  "Returns the union of all sets."
  (foldl (lambda (set result)
	   (union set result :test #'equal?))
	 ()
	 sets))
(define (set-intersect set . sets)
  "Return a set with all elements in set that are also in all sets."
  (foldl (lambda (set result)
	   (intersection result set :test #'equal?))
	 set
	 sets))
(define (set-subtract set . sets)
  "Return a set with all elements in set that are not in any of sets."
  (foldl (lambda (set result)
	   (set-difference result set :test #'equal?))
	 set
	 sets))
(define (subset? set1 set2)
  "True if set1 is a subset of set2"
  (set-empty? (set-subtract set1 set2)))
(define (set=? set1 set2)
  "True if set1 = set2"
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (flatten tree)
  (cond
    ((null? tree) ())
    ((pair? tree) (append (flatten (car tree)) (flatten (cdr tree))))
    (t (list tree))))



(define (string-append . strings)
  (apply 'concatenate 'string strings))

(define (string? datum) (stringp datum))

(define (string-starts-with? string sub-string)
  (string= (subseq string 0 (length sub-string))
	   sub-string))

(define (newline (out *standard-output*)) (format out "~%"))
(define (display datum (out *standard-output*)) (format out "~A" datum))
(define (displayln datum (out *standard-output*))
  (display datum out)
  (newline out))


(defmacro set! (id expression)
  `(setq ',id ,expression))
(define (set-car! pair value) (setf (car pair) value))
(define (set-cdr! pair value) (setf (cdr pair) value))

(defmacro delay (&body body)
  "Delays body."
  `(memo-proc (lambda () ,@body)))
(define (force promise)
  "Evaluates promise."
  [promise])

(define *the-empty-stream* ())
(defmacro stream-cons (first rest)
  "Construct a stream from an element and a delayed stream."
  `(cons ,first (delay ,rest)))
(define (stream-car stream)
  "First element of stream."
  (car stream))
(define (stream-cdr stream)
  "Forces the rest of the stream."
  (force (cdr stream)))

(define (stream-empty? stream)
  "T if the stream is *the-empty-stream*"
  (eq *the-empty-stream* stream))

(define (stream-for-each stream proc)
  "Applies proc to each element of stream, discarding results"
  (let rec ((stream stream))
    (unless (stream-empty? stream)
      [proc (stream-car stream)]
      (rec (stream-cdr stream)))))

(define (stream-length stream)
  "The length of the stream."
  (let ((count 0))
    (stream-for-each stream (lambda (x) (declare (ignore x)) (incf count)))
    count))

(define (stream->list stream)
  "A list of all of the elements in stream."
  (let ((xs ()))
    (stream-for-each stream (lambda (x) (push x xs)))
    (nreverse xs)))

(define (stream-first stream)
  "The first element of a stream."
  (stream-car stream))
(define (stream-rest stream)
  "Forces the rest of the stream."
  (stream-cdr stream))

(define (stream? datum)
  "True if datum is a stream-like object."
  (or (eq? datum *the-empty-stream*)
      (and (pair? datum)
	   (procedure? (cdr datum)))))

(define (list->stream list)
  "Constructs a stream from a list of values."
  (if (empty? list)
      *the-empty-stream*
      (stream-cons (first list) (list->stream (rest list)))))

(define (stream . list)
  "Constructs a stream from a list of values."
  (list->stream list))

(define *test-stream* (stream 1 2 3))

(assert (equal (stream->list *test-stream*)
	       '(1 2 3)))

(assert (equal (let* ((one 0) (two 1) (three 2)
		      (stream (stream-cons (incf one) (stream-cons (incf two) (stream-cons (incf three) *the-empty-stream*)))))
		 (stream->list stream)
		 (stream->list stream))
	       '(1 2 3)))

(define (stream-map proc stream)
  "A stream which has proc applied to each element."
  (if (stream-empty? stream)
      *the-empty-stream*
      (stream-cons [proc (stream-first stream)] (stream-map proc (stream-rest stream)))))

(assert (equal (stream->list (stream-map (lcurry '* 5) *test-stream*))
	       '(5 10 15)))

(define (stream-fold proc init stream)
  "A stream which applies (proc accumulated-value element) to successive elements of stream."
  (cond
    ((stream-empty? stream) init)
    (t
     (let ((first (stream-first stream))
	   (rest (stream-rest stream)))
       (cond
	 ((stream-empty? rest) [proc init first])
	 (t (stream-fold proc [proc init (stream-first stream)] rest)))))))

(assert (eq :init (stream-fold t :init *the-empty-stream*)))
(assert (equal (stream-fold (swap-args 'cons) () *test-stream*)
	       '(3 2 1)))

(define (stream-filter predicate stream)
  "A stream with only the elements which satisfy predicate."
  (cond
    ((stream-empty? stream) stream)
    (t
     (let ((x (stream-first stream)))
       (if [predicate x]
	   (stream-cons x (stream-filter predicate (stream-rest stream)))
	   (stream-filter predicate (stream-rest stream)))))))

(assert (equal (stream->list (stream-filter 'odd? (stream 1 2 3)))
	       '(1 3)))

(define (stream-drop stream n)
  "A stream without the first n elements of stream."
  (let rec ((stream stream)
	    (n n))
    (if (or (stream-empty? stream) (<= n 0))
	stream
	(rec (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-drop (stream 1 2 3) 2))
	       '(3)))

(define (stream-take stream n)
  "A stream with up to the first n elements of stream."
  (if (or (stream-empty? stream) (<= n 0))
      *the-empty-stream*
      (stream-cons (stream-first stream)
		   (stream-take (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-take (stream 1 2 3) 2))
	       '(1 2)))

(define (stream-ref stream i)
  "Returns the i-th element (0-based indexing) of stream."
  (stream-first (stream-drop stream i)))

(assert (= (stream-ref (stream 0 1 2 3) 1)
	   1))

(define (stream-append . streams)
  "A stream in which combines streams to follow one after the other."
  (cond
    ((null? streams) *the-empty-stream*)
    (t
     (let ((stream (first streams)))
       (cond
	 ((stream-empty? stream)
	  (apply 'stream-append (rest streams)))
	 (t
	  (stream-cons (stream-first stream)
		       (apply 'stream-append
			      (stream-rest stream)
			      (rest streams)))))))))

(assert (equal
	 (stream->list (stream-append (stream 1 2 3) (stream 4 5 6) (stream 7 8 9)))
	 '(1 2 3 4 5 6 7 8 9)))

(define (stream-flatten stream-of-streams)
  "A stream which combines a stream of streams into a single stream using stream-append."
  (stream-fold 'stream-append
	       *the-empty-stream*
	       stream-of-streams))

(assert (equal
	 (stream->list (stream-flatten (stream (stream 1 2 3)
					       (stream 4 5 6)
					       (stream 7 8 9))))
	 '(1 2 3 4 5 6 7 8 9)))


(define (stream-range start end)
  "A stream of integers from start up to (1- end)."
  (cond
    ((> start end) *the-empty-stream*)
    (t
     (stream-cons start
		  (stream-range (1+ start) end)))))

(assert (equal (stream->list (stream-range 4 8))
	       '(4 5 6 7 8)))

(define (stream-flatmap proc s)
  "Stream-flatten the result of mapping proc across stream s."
  (stream-flatten (stream-map proc s)))

(assert (equal (stream->list (stream-flatmap
			      (lambda (i)
				(stream-map
				 (lambda (j) (list i j))
				 (stream 4 5)))
			      (stream 1 2)))
	       '((1 4) (1 5) (2 4) (2 5))))


(define (stream-map-successive n f stream)
  "Apply f to successive groups of size n in stream."
  (let ((group (stream->list (stream-take stream n))))
    (cond ((< (length group) n)
	   *the-empty-stream*)
	  (t (stream-cons (apply f group)
			  (stream-map-successive n f (stream-rest stream)))))))

(assert (equal? (stream->list (stream-map-successive 3 'list (stream 1 2 3 4)))
		'((1 2 3) (2 3 4))))


(define (random-stream limit)
  "Return a stream of random numbers below limit.
If limit is an integer, returns integers.
If limit is a float returns floats.
Does not affect the random-state."
  (define (%random-stream rs)
    (stream-cons (random limit rs)
		 (%random-stream rs)))
  (%random-stream (make-random-state)))

;; Random-stream does not affect the random-state
(assert (equal (stream->list (stream-take (random-stream 1.0) 10))
	       (stream->list (stream-take (random-stream 1.0) 10))))

(assert (stream-empty? (stream-filter (lambda (x) (not (<= 0.0 x 1.0)))
				      (stream-take (random-stream 1.0) 10))))




(for-macros (uninstall-syntax!))
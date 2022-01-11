(in-package #:schemeish.base)

(install-syntax!)

(define (make-keyword symbol)
  (intern (symbol-name symbol) :keyword))

(define symbol->string #'symbol-name)
(define symbol? #'symbolp)
(define procedure? #'functionp)

(define (parameter? symbol)
  "Returns true if symbol is a parameter i.e. dynamically scoped."
  #+sbcl
  (eq? :special (sb-cltl2:variable-information symbol))
  #-sbcl
  (and (not (constantp symbol))
       ;; If we have an error, its because the parameter has a type
       ;; associated with it. Therefore we know its a parameter.
       (eval `(not (ignore-errors
		    (let (,symbol)
		      (let ((f (lambda () ,symbol)))
			(let ((,symbol t))
			  (not (eq? [f] t))))))))))

(define (document-proc proc docstring)
  "Attach documentation to proc before returning it."
  (setf (documentation proc 'function) docstring)
  proc)


(define eq? #'eq)

(defgeneric equal? (object1 object2)
  (:documentation "Provides a generic interface to EQUAL."))
(defmethod equal? (object1 object2) (equal object1 object2))

(define (map function list &rest more-lists)
  (apply #'mapcar function list more-lists))

(define (append* lists)
  (apply #'append lists))

(define empty? #'null)

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

(define (repeat fn count)
  "Repeatedly call fn count times."
  (assert (not (negative? count)))
  (for-each (lambda (_) [fn]) (range count)))

(define (filter predicate list)
  "Keep elements of list that satisfy predicate."
  (remove-if-not predicate list))

(define pair? "T if datum is a cons." #'consp)
(define null? "T if datum is nil." #'null)
(define list? "Alias for (listp datum)." #'listp)

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

(define negative? #'minusp)
(define positive? #'plusp)

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

(define (sort list less-than? (:extract-key #'identity))
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


(define (intersperse element list)
  "Return list with element placed between every other element."
  (define (intersperse-loop list result)
    (cond
      ((empty? list) (nreverse result))
      (t (intersperse-loop (rest list)
			   (list* (first list) element result)))))

  (assert (list? list))
  (cond
    ((empty? list) ())
    (t
     (cons (first list) (intersperse-loop (rest list) ())))))

(assert (equal? (intersperse :a ())
		()))
(assert (equal? (intersperse :a '(:b))
		'(:b)))
(assert (equal? (intersperse :b '(:a :c))
		'(:a :b :c)))
(assert (equal? (intersperse :i '(:e :e :o))
		'(:e :i :e :i :o)))

(define even? #'evenp)
(define odd? #'oddp)
(define zero? #'zerop)

(define (compose* procs)
  "Function compositions. Mulitple values of one function are used as arguments to the next."
  (foldr (lambda (proc result)
	   (lambda args
	     (multiple-value-call proc (apply result args))))
	 (lambda args (values-list args))
	 procs))

(define (compose . procs)
  "Function compositions. Mulitple values of one function are used as arguments to the next."
  (compose* procs))



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

(define ((ignore-args . indices) f)
  "Return a function, which when applied to a function F ignores positional arguments matching the 0-based indices."
  (lambda args
    (apply f (remove-indices indices args))))

(assert (equal [[(ignore-args 1 4) (lambda (a c d) (list a c d))]
		:a :b :c :d :e]
	       '(:A :C :D)))


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

(define (alist-for-each alist proc)
  "Proc applied to all values of alist."
  (for-each (lambda (binding) [proc (car binding) (cdr binding)]) alist))

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

(define (for-all* predicate lists)
  (apply #'every predicate lists))
(define (for-all predicate . lists)
  (for-all* predicate lists))

(define (there-exists* predicate lists)
  (apply #'some predicate lists))
(define (there-exists predicate . lists)
  (there-exists* predicate lists))

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

(define (degrees->radians deg)
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

(define number? #'numberp)

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

(define (string-append* strings)
  "Applies string-append to strings."
  (apply #'string-append strings))

(define string? #'stringp)

(define (string-starts-with? string sub-string)
  (and (>= (length string) (length sub-string))
       (string= (subseq string 0 (length sub-string))
		sub-string)))

(define (chars-string char count)
  "Return a string with char repeated count times."
  (make-string count :initial-element char))

(define (join-strings strings separator)
  "Joins strings with the character separator in between each pair of strings."
  (string-append* (intersperse (chars-string separator 1) strings)))

(define (string-empty? string)
  "True if string is empty."
  (zero? (length string)))

(define (split-string-if string split-char?)
  "Return a list of strings that have been split whenever split-char? is true.
Chars that satisfy split-char? will be removed, and empty strings will not be returned."
  (define (not-split-char? char) (not [split-char? char]))
  (define (first-char string) (aref string 0))

  (define (split-string-iter string result)
    (cond
      ((string-empty? string) result)
      ([split-char? (first-char string)]
       ;; Remove initial split-char?
       (let ((start (position-if not-split-char? string)))
	 (cond
	   ((null? start) result)
	   (t (split-string-iter (subseq string start) result)))))
      (t
       (let ((end (position-if split-char? string)))
	 (cond
	   ((null? end) (cons string result))
	   (t (split-string-iter (subseq string (1+ end)) (cons (subseq string 0 end) result))))))))

  (nreverse (split-string-iter string ())))

(define (split-string string split-char)
  "Splits the string on each occurrence of split-char in string."
  (split-string-if string (lcurry #'char= split-char)))

(assert (equal? (split-string-if "     the three     wise    men   joined hands in holy matrimony.    " (lcurry #'char= #\space))
		'("the" "three" "wise" "men" "joined" "hands" "in" "holy" "matrimony.")))


(define (string-for-each proc string)
  "Apply proc to each character in string."
  (define end (length string))
  (define (iter index)
    (when (< index end)
      [proc (aref string index)]
      (iter (1+ index))))
  (iter 0))
(define (string-map proc string)
  "Applies proc to each character in string, returning a new string 
of the results appended together. Proc is expected to return a character or string"
  (with-output-to-string (s)
    (string-for-each (lambda (char)
		       (format s "~A" [proc char]))
		     string)))

(define (string->list string)
  (coerce string 'list))
(define (list->string list)
  (coerce list 'string))

(define (newline (out *standard-output*)) (format out "~%"))
(define (display datum (out *standard-output*)) (format out "~A" datum))
(define (displayln datum (out *standard-output*))
  (display datum out)
  (newline out))

(define (symbolicate . things)
  (intern (apply 'string-append (map 'string things))))

(defmacro set! (id expression)
  `(setq ,id ,expression))
(define (set-car! pair value) (setf (car pair) value))
(define (set-cdr! pair value) (setf (cdr pair) value))

(defmacro delay (&body body)
  "Delays body."
  `(memo-proc (lambda () ,@body)))
(define (force promise)
  "Evaluates promise."
  [promise])

(defparameter *the-empty-stream* ())
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

(defparameter *test-stream* (stream 1 2 3))

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


(unexport
 (define (random-stream limit)
   "Return a stream of random numbers below limit.
If limit is an integer, returns integers.
If limit is a float returns floats.
Does not affect the random-state."
   (define (%random-stream rs)
     (stream-cons (random limit rs)
		  (%random-stream rs)))
   (%random-stream (make-random-state))))

;; Random-stream does not affect the random-state
(assert (equal (stream->list (stream-take (random-stream 1.0) 10))
	       (stream->list (stream-take (random-stream 1.0) 10))))

(assert (stream-empty? (stream-filter (lambda (x) (not (<= 0.0 x 1.0)))
				      (stream-take (random-stream 1.0) 10))))

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

(define (procedure-arguments procedure)
  "Returns the procedure's argument list in the form of an alist with the following keys (in order):
    (:required . required-arguments)
    (:optional . optional-arguments)
    (:rest . rest-arg-name)
    (:key . keyword-arguments) 
    (:allow-other-keys? . t/nil)"
  (parse-lambda-list-arguments (arg:arglist procedure)))

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

(define (procedure-arguments-required-arguments arguments)
  (alist-ref arguments :required ()))
(define (procedure-arguments-optional-arguments arguments)
  (alist-ref arguments :optional ()))
(define (procedure-arguments-key-arguments arguments)
  (alist-ref arguments :key ()))
(define (procedure-arguments-rest-argument arguments)
  (alist-ref arguments :rest ()))
(define (procedure-arguments-allow-other-keys? arguments)
  (alist-ref arguments :allow-other-keys? ()))

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
    (procedure-arguments-allow-other-keys? arguments))))

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
	     (t (has-specific-arity? (rest arity-list) fixed-arity-n)))))))

(assert (has-specific-arity? '(2 3 4) 3))
(assert (not (has-specific-arity? '(2 3 4) 5)))
(assert (has-specific-arity? '(2 3 (:* . 4)) 5))
(assert (not (has-specific-arity? '(2 3 (:** . 4)) 5)))
(assert (has-specific-arity? '(2 3 (:** . 4)) 6))

;; TODO: Arity-table
;; TODO: Restrict arity when creating higher order functions. COMPOSE, etc.
;; TODO: Generics


(define (group key-fn list)
  "Groups elements of list that have the same key-fn into an alist."
  (define (rec list result)
    (cond
      ((null? list)
       (alist-map result
		  (lambda (key list)
		    (cons key (nreverse list)))))
      (t (let ((item (first list)))
	   (let ((key [key-fn item]))
	     (rec (rest list)
		  (alist-update result key (lambda (vals) (cons item vals)) ())))))))
  (rec list ()))

(assert (equal? (group 'car '((0 a b c)
			      (1 a b c)
			      (0 d e f)
			      (2 a b c)
			      (1 d e f)))
		'((1 (1 A B C) (1 D E F)) (2 (2 A B C)) (0 (0 A B C) (0 D E F)))))

(define (hash-ref table key (failure-result))
  "Returns the value associated with key in the hash-table table or failure-result."
  (multiple-value-bind (value present?) (gethash key table)
    (if present?
	value
	failure-result)))
(define (hash-set! table key value)
  "Sets the value associated with key in hash-table table to value. Returns the value."
  (setf (gethash key table) value))
(define (hash-find-keyf table predicate (failure-result))
  "Returns the first key that satisfies [predicate key] in table."
  (loop for key being the hash-keys in table
	do (when [predicate key]
	     (return-from hash-find-keyf key)))
  failure-result)

(define (hash-ref-default table key delayed-value)
  "Return the value associated with key in table.
If there is no value, computes [delayed-value] and stores it in the table
before returning it."
  (let* ((no-value (gensym))
	 (value (hash-ref table key no-value)))
    (if (eq? no-value value)
	(hash-set! table key [delayed-value])
	value)))

(define (hash-update! table key updater (failure-result))
  "Updates the value in table associated with key using [updater value].
If no value is associated with key, failure-result is used instead."
  (hash-set! table key [updater (hash-ref table key failure-result)]))

(define (hash-map table proc)
  "Maps [proc key value] over the keys and values of table, producing a list as a result."
  (loop for key being the hash-keys of table using (hash-value value)
	collecting [proc key value]))

(define (hash-keys table)
  "Returns a list of all of the keys in table."
  (loop for key being the hash-keys of table collecting key))

(define (hash-values table)
  "Returns a list of all of the values in table."
  (loop for value being the hash-values of table collecting value))

(define (hash->alist table)
  "Return an alist representation of the key/value pairs in table."
  (hash-map table #'cons))

(define (hash-for-each table proc)
  "Apply [proc key value] to each key/value pair in table."
  (maphash proc table))

(define (hash-remove! table key)
  "Removes key and associated value from table"
  (remhash key table))
(define (hash-clear! table)
  "Remvoes all keys and values from table."
  (clrhash table))


(define (vector-ref vector index)
  (aref vector index))
(define (vector-set! vector index value)
  (setf (aref vector index) value))

(define (safe-vector-ref vector index out-of-bounds-result)
  "Returns the out-of-bounds-result if index is out of bounds."
  (if (>= index (length vector))
      out-of-bounds-result
      (aref vector index)))

(define (vector->list vector)
  (coerce vector 'list))
(define (list->vector list)
  (coerce list 'vector))

(uninstall-syntax!)

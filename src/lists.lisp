(in-package #:schemeish.backend)

(install-syntax!)

;;; Lists
(export
 (define (map function list &rest more-lists)
   (apply #'mapcar function list more-lists)))

(export
 (define (append* lists)
   (apply #'append lists)))

(export
 (define empty? #'null))

(export
 (define (for-each proc . lists)
   "Apply proc to each element of lists. Arity of proc should match the number of lists."
   (let rec ((lists lists))
     (unless (member nil lists)
       (apply proc (map 'first lists))
       (rec (map 'rest lists))))))

(assert (equal?
	 (with-output-to-string (s)
	   (for-each (lambda (x y) (format s "~S" (list x y)))
		     '(a b c)
		     '(1 2 3)))
	 "(A 1)(B 2)(C 3)"))

(export
 (define (repeat fn count)
   "Repeatedly call fn count times."
   (assert (not (negative? count)))
   (for-each (lambda (_) [fn]) (range count))))

(export
 (define (filter predicate list)
   "Keep elements of list that satisfy predicate."
   (remove-if-not predicate list)))

(export
 (define (filter-not pred list)
   "Returns a list of elements that don't satisfy predicate pred."
   (filter (lambda (x) (not [pred x])) list)))

(export
 (define (partition pred list)
   "Returns (list elements-satisfying-pred elements-not-satisfying-pred)"
   (list (filter pred list)
	 (filter-not pred list))))

(assert (equal (partition 'even? '(1 2 3 4 5 6))
	       '((2 4 6) (1 3 5))))

(export
 (define (filter-map proc . lists)
   "Remove nil from the result of mapping proc over lists."
   (remove nil (apply 'map proc lists))))

(export
 (define (range end (start 0) (step 1))
   "Return a list of elements from [start,end) using step as the stepsize."
   (if (<= start end)
       (loop for i from start below end by step collecting i)
       (loop for i from start downto end by step collecting i))))


(export
 (define (append-map proc . lists)
   "Append the results of mapping procedure across lists."
   (append* (apply 'map proc lists))))

(export
 (define (map-successive n f list)
   "Maps f over successive groups of size n in list."
   (let rec ((list list)
	     (length (length list))
	     (result ()))
     (cond
       ((< length n) (nreverse result))
       (t (rec (rest list)
	       (1- length)
	       (cons (apply f (subseq list 0 n)) result)))))))

(assert (equal? (map-successive 3 'list (list 1 2 3 4))
		'((1 2 3) (2 3 4))))

(export
 (define pair? "T if datum is a cons." #'consp))
(export
 (define null? "T if datum is nil." #'null))
(export
 (define list? "Alias for (listp datum)." #'listp))

(export
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
	  (t (values :dotted :cons))))))))

(export
 (define (proper-list? list)
   (and (list? list)
	(eq? :proper (list-type list)))))


(export
 (define (list-ref list pos)
   "Return the value of list at pos."
   (nth pos list)))

(export
 (define (list-tail list pos)
   "Return the sublist of list starting at pos."
   (nthcdr pos list)))

(export
 (define (foldl proc init . lists)
   "Fold (proc e1 e2 ... result) across lists starting from the start of the lists."
   (let rec ((result init)
	     (lists lists))
     (if (or (empty? lists) (member nil lists))
	 result
	 (rec (apply proc (append (map #'first lists) (list result)))
	      (map #'rest lists))))))

(assert (equal (foldl 'cons () '(1 2 3 4))
	       '(4 3 2 1)))

(assert (= (foldl (lambda (a b result)
		    (* result (- a b)))
		  1
		  '(1 2 3)
		  '(4 5 6))
	   -27))

(export
 (define (foldr proc init . lists)
   "Fold (proc e1 e2 ... result) across lists starting from the end of the lists."
   (let rec ((result init)
	     (lists (map 'reverse lists)))
     (if (or (empty? lists) (member nil lists))
	 result
	 (rec (apply proc (append (map #'first lists) (list result)))
	      (map #'rest lists))))))

(assert (equal (foldr 'cons '() '(1 2 3 4))
	       '(1 2 3 4)))
(assert (equal (foldr (lambda (v l) (cons (1+ v) l)) '() '(1 2 3 4))
	       '(2 3 4 5)))

(export
 (define (andmap proc . lists)
   "Return the last non-nil result of mapping proc across lists, or nil if some result is nil."
   (let rec ((result t)
	     (lists lists))
     (if (or (not result) (member nil lists))
	 result
	 (rec (apply proc (map 'first lists))
	      (map 'rest lists))))))

(assert (andmap 'positive? '(1 2 3)))
;; (andmap 'positive? '(1 2 a)) => error
(assert (not (andmap 'positive? '(1 -2 a))))
(assert (= 9 (andmap '+ '(1 2 3) '(4 5 6))))

(export
 (define (ormap proc . lists)
   "Return the first non-nil result of mapping proc across lists."
   (let rec ((result ())
	     (lists lists))
     (if (or result (member nil lists))
	 result
	 (rec (apply proc (map 'first lists))
	      (map 'rest lists))))))


(assert (ormap 'eq? '(a b c) '(a b c)))
(assert (ormap 'positive? '(1 2 a)))
(assert (= 5 (ormap '+ '(1 2 3) '(4 5 6))))


(export
 (define (remq v list)
   "Remove using eq? as a test."
   (remove v list :test #'eq)))

(export
 (define (remove* v-list list (test #'equal?))
   "Removes all elements in v-list from list."
   (foldl (lambda (v result) (remove v result :test test)) list v-list)))

(assert (equal
	 (remove* (list 1 2) (list 1 2 3 2 4 5 2))
	 '(3 4 5)))

(export
 (define (remq* v-list list) (remove* v-list list #'eq?)))

(export
 (define (sort list less-than? (:extract-key #'identity))
   "Returns a sorted list."
   (cl:sort (copy-list list) less-than? :key extract-key)))

(assert (equal (let* ((original-list '(1 3 4 2))
		      (sorted-list (sort original-list '<)))
		 (assert (equal '(1 3 4 2) original-list))
		 sorted-list)
	       '(1 2 3 4)))


(export
 (define (memf proc list)
   "Returns the first sublist of list whose first element satisfies predicate proc."
   (let rec ((list list))
     (if (or (null? list) [proc (first list)])
	 list
	 (rec (rest list))))))

(assert (equal (memf (lambda (arg) (> arg 9)) '(7 8 9 10 11))
	       '(10 11)))

(export
 (define (findf proc list)
   "Finds the first element in list that satisfies predicate proc."
   (let ((found (memf proc list)))
     (if (list? found)
	 (first found)
	 ()))))

(assert (= (findf (lambda (arg) (> arg 9)) '(7 8 9 10 11))
	   10))

(export
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
		   result)))))))

(assert (equal (list-update '(zero one two) 1 'symbol->string)
	       '(ZERO "ONE" TWO)))


(export
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
		   result)))))))

(assert (equal (list-set '(zero one two) 2 "two")
	       '(zero one "two")))

(export
 (define (take list n)
   "Takes the first n elements from list"
   (let rec ((list list)
	     (current-pos 0)
	     (result '()))
     (if (or (= n current-pos) (empty? list))
	 (nreverse result)
	 (rec (rest list)
	      (1+ current-pos)
	      (cons (first list) result))))))

(assert (equal (take '(1 2 3 4 5) 2)
	       '(1 2)))

(export
 (define (drop list n)
   "Drops the first n elements from list"
   (list-tail list n)))

(export
 (define (split-at list pos)
   "Returns (list (take list pos) (drop list pos))"
   (list (take list pos) (drop list pos))))


(export
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
      (cons (first list) (intersperse-loop (rest list) ()))))))

(assert (equal? (intersperse :a ())
		()))
(assert (equal? (intersperse :a '(:b))
		'(:b)))
(assert (equal? (intersperse :b '(:a :c))
		'(:a :b :c)))
(assert (equal? (intersperse :i '(:e :e :o))
		'(:e :i :e :i :o)))

(uninstall-syntax!)

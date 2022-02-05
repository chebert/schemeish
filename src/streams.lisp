(in-package #:schemeish.backend)

(install-syntax!)

;;; Streams

(export
 (defvar *the-empty-stream* ()))
(defmacro stream-cons (first rest)
  "Construct a stream from an element and a delayed stream."
  `(cons ,first (delay ,rest)))
(export 'stream-cons)
(export
 (define (stream-car stream)
   "First element of stream."
   (car stream)))
(export
 (define (stream-cdr stream)
   "Forces the rest of the stream."
   (force (cdr stream))))

(export
 (define (stream-empty? stream)
   "T if the stream is *the-empty-stream*"
   (eq *the-empty-stream* stream)))

(export
 (define (stream-for-each stream proc)
   "Applies proc to each element of stream, discarding results"
   (let rec ((stream stream))
     (unless (stream-empty? stream)
       [proc (stream-car stream)]
       (rec (stream-cdr stream))))))

(export
 (define (stream-length stream)
   "The length of the stream."
   (let ((count 0))
     (stream-for-each stream (lambda (x) (declare (ignore x)) (incf count)))
     count)))

(export
 (define (stream->list stream)
   "A list of all of the elements in stream."
   (let ((xs ()))
     (stream-for-each stream (lambda (x) (push x xs)))
     (nreverse xs))))

(export
 (define (stream-first stream)
   "The first element of a stream."
   (stream-car stream)))
(export
 (define (stream-rest stream)
   "Forces the rest of the stream."
   (stream-cdr stream)))

(export
 (define (stream? datum)
   "True if datum is a stream-like object."
   (or (eq? datum *the-empty-stream*)
       (and (pair? datum)
	    (procedure? (cdr datum))))))

(export
 (define (list->stream list)
   "Constructs a stream from a list of values."
   (if (empty? list)
       *the-empty-stream*
       (stream-cons (first list) (list->stream (rest list))))))

(export
 (define (stream . list)
   "Constructs a stream from a list of values."
   (list->stream list)))

(defparameter *test-stream* (stream 1 2 3))

(assert (equal (stream->list *test-stream*)
	       '(1 2 3)))

(assert (equal (let* ((one 0) (two 1) (three 2)
		      (stream (stream-cons (incf one) (stream-cons (incf two) (stream-cons (incf three) *the-empty-stream*)))))
		 (stream->list stream)
		 (stream->list stream))
	       '(1 2 3)))

(export
 (define (stream-map proc stream)
   "A stream which has proc applied to each element."
   (if (stream-empty? stream)
       *the-empty-stream*
       (stream-cons [proc (stream-first stream)] (stream-map proc (stream-rest stream))))))

(assert (equal (stream->list (stream-map (lcurry '* 5) *test-stream*))
	       '(5 10 15)))

(export
 (define (stream-fold proc init stream)
   "A stream which applies (proc accumulated-value element) to successive elements of stream."
   (cond
     ((stream-empty? stream) init)
     (t
      (let ((first (stream-first stream))
	    (rest (stream-rest stream)))
	(cond
	  ((stream-empty? rest) [proc init first])
	  (t (stream-fold proc [proc init (stream-first stream)] rest))))))))

(assert (eq :init (stream-fold t :init *the-empty-stream*)))
(assert (equal (stream-fold (swap-args 'cons) () *test-stream*)
	       '(3 2 1)))

(export
 (define (stream-filter predicate stream)
   "A stream with only the elements which satisfy predicate."
   (cond
     ((stream-empty? stream) stream)
     (t
      (let ((x (stream-first stream)))
	(if [predicate x]
	    (stream-cons x (stream-filter predicate (stream-rest stream)))
	    (stream-filter predicate (stream-rest stream))))))))

(assert (equal (stream->list (stream-filter 'odd? (stream 1 2 3)))
	       '(1 3)))

(export
 (define (stream-drop stream n)
   "A stream without the first n elements of stream."
   (let rec ((stream stream)
	     (n n))
     (if (or (stream-empty? stream) (<= n 0))
	 stream
	 (rec (stream-rest stream) (1- n))))))

(assert (equal (stream->list (stream-drop (stream 1 2 3) 2))
	       '(3)))

(export
 (define (stream-take stream n)
   "A stream with up to the first n elements of stream."
   (if (or (stream-empty? stream) (<= n 0))
       *the-empty-stream*
       (stream-cons (stream-first stream)
		    (stream-take (stream-rest stream) (1- n))))))

(assert (equal (stream->list (stream-take (stream 1 2 3) 2))
	       '(1 2)))

(export
 (define (stream-ref stream i)
   "Returns the i-th element (0-based indexing) of stream."
   (stream-first (stream-drop stream i))))

(assert (= (stream-ref (stream 0 1 2 3) 1)
	   1))

(export
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
			       (rest streams))))))))))

(assert (equal
	 (stream->list (stream-append (stream 1 2 3) (stream 4 5 6) (stream 7 8 9)))
	 '(1 2 3 4 5 6 7 8 9)))

(export
 (define (stream-flatten stream-of-streams)
   "A stream which combines a stream of streams into a single stream using stream-append."
   (stream-fold 'stream-append
		*the-empty-stream*
		stream-of-streams)))

(assert (equal
	 (stream->list (stream-flatten (stream (stream 1 2 3)
					       (stream 4 5 6)
					       (stream 7 8 9))))
	 '(1 2 3 4 5 6 7 8 9)))


(export
 (define (stream-range start end)
   "A stream of integers from start up to (1- end)."
   (cond
     ((> start end) *the-empty-stream*)
     (t
      (stream-cons start
		   (stream-range (1+ start) end))))))

(assert (equal (stream->list (stream-range 4 8))
	       '(4 5 6 7 8)))

(export
 (define (stream-flatmap proc s)
   "Stream-flatten the result of mapping proc across stream s."
   (stream-flatten (stream-map proc s))))

(assert (equal (stream->list (stream-flatmap
			      (lambda (i)
				(stream-map
				 (lambda (j) (list i j))
				 (stream 4 5)))
			      (stream 1 2)))
	       '((1 4) (1 5) (2 4) (2 5))))


(export
 (define (stream-map-successive n f stream)
   "Apply f to successive groups of size n in stream."
   (let ((group (stream->list (stream-take stream n))))
     (cond ((< (length group) n)
	    *the-empty-stream*)
	   (t (stream-cons (apply f group)
			   (stream-map-successive n f (stream-rest stream))))))))

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

(uninstall-syntax!)

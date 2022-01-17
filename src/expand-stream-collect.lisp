(in-package #:schemeish.expand-stream-collect)

(install-syntax!)

(define (stream-collect-bindings-fn binding-names body)
  (let ((arg-name (gensym)))
    `(lambda (,arg-name)
       (destructuring-bind ,binding-names ,arg-name
	 (declare (ignorable ,@binding-names))
	 ,@body))))

(define (stream-collect-filter-form binding-names test-form stream-form)
  `(stream-filter
    ,(stream-collect-bindings-fn binding-names (list test-form))
    ,stream-form))

(stream-collect-filter-form '(i j) '(even? (+ i j)) :stream)
'(STREAM-FILTER
  (LAMBDA (#:G586)
    (DESTRUCTURING-BIND (I J) #:G586 (DECLARE (IGNORABLE I J)) (EVEN? (+ I J))))
  :STREAM)

(define (stream-collect-inner-map-form binding binding-names)
  `(stream-map (lambda (,(first binding))
		 (list ,@binding-names))
	       ,(second binding)))

(assert (equal? (stream-collect-inner-map-form '(j (stream-range 1 (1- i)))
					       '(i j))
		'(STREAM-MAP
		  (LAMBDA (J)
		    (LIST I J))
		  (STREAM-RANGE 1 (1- I)))))

(define (stream-collect-flatmap-form binding body)
  `(stream-flatmap (lambda (,(first binding))
		     ,@body)
		   ,(second binding)))

(assert (equal? (stream-collect-flatmap-form '(i (stream-range 1 n)) '(:body))
		'(STREAM-FLATMAP
		  (LAMBDA (I)
		    :BODY)
		  (STREAM-RANGE 1 N))))

(define (stream-collect-outer-map binding-names form stream)
  `(stream-map
    ,(stream-collect-bindings-fn binding-names (list form))
    ,stream))

(stream-collect-outer-map '(i j) '(list i j (+ i j)) :stream)
'(STREAM-MAP
  (LAMBDA (#:G588)
    (DESTRUCTURING-BIND
	(I J)
	#:G588
      (DECLARE (IGNORABLE I J))
      (LIST I J (+ I J))))
  :STREAM)

(define (stream-collect-inner-flatmaps bindings)
  (when (null? bindings)
    (error "stream-collect: requires at least one binding."))
  (let ((binding-names (map 'car bindings))
	(bindings (reverse bindings)))
    (let rec ((result (stream-collect-inner-map-form (first bindings)
						     binding-names))
	      (bindings (rest bindings)))
      (if (null? bindings)
	  result
	  (rec
	   (stream-collect-flatmap-form (first bindings) (list result))
	   (rest bindings))))))

(assert (equal? (stream-collect-inner-flatmaps '((i (stream-range 1 n))
						 (j (stream-range 1 (1- i)))))
		'(STREAM-FLATMAP
		  (LAMBDA (I)
		    (STREAM-MAP
		     (LAMBDA (J)
		       (LIST I J))
		     (STREAM-RANGE 1 (1- I))))
		  (STREAM-RANGE 1 N))))

(define (stream-collect-form map-form bindings filter-form)
  (let ((binding-names (map 'car bindings)))
    (stream-collect-outer-map
     binding-names
     map-form
     (stream-collect-filter-form
      binding-names
      filter-form
      (stream-collect-inner-flatmaps bindings)))))


(stream-collect-form '(list i j (+ i j))
		     '((i (stream-range 1 n))
		       (j (stream-range 1 (1- i))))
		     '(even? (+ i j)))
'(STREAM-MAP
  (LAMBDA (#:G594)
    (DESTRUCTURING-BIND
	(I J)
	#:G594
      (DECLARE (IGNORABLE I J))
      (LIST I J (+ I J))))
  (STREAM-FILTER
   (LAMBDA (#:G593)
     (DESTRUCTURING-BIND
         (I J)
         #:G593
       (DECLARE (IGNORABLE I J))
       (EVEN? (+ I J))))
   (STREAM-FLATMAP
    (LAMBDA (I)
      (STREAM-MAP
       (LAMBDA (J)
         (LIST I J))
       (STREAM-RANGE 1 (1- I))))
    (STREAM-RANGE 1 N))))


(uninstall-syntax!)

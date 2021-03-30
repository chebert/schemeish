;;;; schemeish.lisp

(in-package #:schemeish)

(defmacro for-macros (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(for-macros
  (defun map (function list &rest more-lists)
    (apply #'mapcar function list more-lists)))

;; Named let.
(defmacro let (&whole whole &rest rest)
  (declare (ignore rest))
  (if (symbolp (second whole))
      (destructuring-bind (name bindings &rest body) (rest whole)
	`(labels ((,name ,(map #'car bindings)
		    ,@body))
	   (,name ,@(map #'second bindings))))
      `(cl:let ,@(rest whole))))

(for-macros
  (defun read-left-bracket (stream char)
    (declare (ignore char))
    (let ((form (read-delimited-list #\] stream t)))
      (if (null form)
	  '()
	  `(funcall ,@form))))
  (defun read-right-bracket (stream char)
    (declare (ignore stream char))
    (error "read: unmatched ]"))

  (defun install-syntax! ()
    (set-macro-character #\[ #'read-left-bracket)
    (set-macro-character #\] #'read-right-bracket)))

(for-macros
  (install-syntax!))

(for-macros
  (defun ensure-bool (datum) (not (not datum)))
  (defun group (predicate xs)
    (cond ((empty? xs) '())
	  (t
	   (let rec ((xs (rest xs))
		     (current-group-name (ensure-bool [predicate (first xs)]))
		     (groups (list (list (first xs)))))
	     (cond
	       ((empty? xs) (map #'reverse (reverse groups)))
	       (t (let* ((x (first xs))
			 (x-group-name (ensure-bool [predicate x])))
		    (rec (rest xs)
			 x-group-name
			 (if (eq x-group-name current-group-name)
			     (cons (cons x (first groups)) (rest groups))
			     (cons (list x) groups))))))))))

  (defun flatten (v)
    (cond
      ((consp v) (append (flatten (car v))
			 (flatten (cdr v))))
      ((null v) ())
      (t (list v)))))

(assert (equal
	 (group #'listp '((1 2 3)
			  (a b c)
			  expr
			  expr2
			  (d e f)
			  expr3))
	 '(((1 2 3) (A B C)) (EXPR EXPR2) ((D E F)) (EXPR3))))

(for-macros
  (defun define-form-name (form)
    (first (flatten form)))
  (defun arg-list->lambda-list (args)
    (cond
      ((consp args)
       (cons (first args) (arg-list->lambda-list (rest args))))
      ((null args) '())
      ((symbolp args) `(&rest ,args))
      (t (error "bad thing to be in an arglist: ~S" args))))


  (defmacro λ (arg-list &body body)
    `(lambda ,(arg-list->lambda-list arg-list) ,@body))
  (defun define-form->lambda-list (form)
    (assert (consp form))
    (arg-list->lambda-list (cdr form)))
  (defun define? (form) (and (consp form) (eq (first form) 'define)))
  (defun define-procedure? (form)
    (and (define? form) (consp (second form))))
  (defun define-name (form)
    (let ((name-form (second form)))
      (first (flatten name-form))))

  (defun append* (lists)
    (apply #'append lists))

  (defun nested-define-name-and-arglists (form)
    (let rec ((form form)
	      (result '()))
      (if (not (listp form))
	  (cons form result)
	  (rec (car form)
	       (cons (cdr form) result)))))
  (defun expand-nested-define (form body)
    (let ((name-and-arglists (nested-define-name-and-arglists form)))
      `(,(first name-and-arglists)
	,(arg-list->lambda-list (second name-and-arglists))
	,@(let rec ((arglists (reverse (cddr name-and-arglists)))
		    (result body))
	    (if (null arglists)
		result
		(rec (rest arglists)
		     `((λ ,(first arglists)
			 ,@result))))))))
  
  (defun expand-local-defines (defines body)
    (let ((names (map #'define-name defines))
	  (function-definitions (map (λ (define)
				       (if (nested-define? (second define))
					   (expand-nested-define (second define) (cddr define))
					   `(,(define-name define)
					     ,(define-form->lambda-list (second define))
					     ,@(define-body->body (cddr define)))))
				     (remove-if-not #'define-procedure? defines))))
      `(let ,names
	 (labels ,function-definitions
	   (setq ,@(append* (map
			     (λ (name define)
			       (list name
				     (if (define-procedure? define)
					 `(function ,name)
					 (third define))))
			     names defines)))
	   ,@body))))

  (defun nested-define? (name-or-form)
    (and (listp name-or-form) (listp (first name-or-form))))
  
  (defun expand-define (name-or-form body)
    (cond
      ((nested-define? name-or-form)
       (cons 'defun (expand-nested-define name-or-form body)))
      ((listp name-or-form)
       `(defun ,(define-form-name name-or-form)
	    ,(define-form->lambda-list name-or-form)
	  ,@(define-body->body body)))
      ((symbolp name-or-form)
       (assert (= 1 (length body)))
       `(defparameter ,name-or-form ,@body))
      (t
       (error "badly formed define."))))
  
  (defun empty? (datum) (null datum))
  (defun define-body->body (body)
    (cond
      ((empty? body) (error "define: empty definition"))
      (t
       (let* ((groups (group #'define? body))
	      (count (length groups)))
	 (cond ((= 1 count)
		(if (define? (first (first groups)))
		    (error "define: missing body")
		    ;; no local definitions
		    body))
	       ((= 2 count)
		(list (expand-local-defines (first groups) (second groups))))
	       (t (error "define: local defines intermixed with code."))))))))

(assert (equal
	 (define-form->lambda-list '(name))
	 '()))
(assert (equal
	 (define-form->lambda-list '(name . args))
	 '(&rest args)))
(assert (equal
	 (define-form->lambda-list '(name arg1 arg2 . args))
	 '(arg1 arg2 &rest args)))
(assert (equal
	 (define-form->lambda-list '(name arg1 arg2))
	 '(arg1 arg2)))

(defmacro define (name-or-form &body body)
  (expand-define name-or-form body))

(define (((test-nested-defines x) y . yargs) . zargs)
  `(,x ,y ,@yargs ,@zargs))

(assert (equal [[(test-nested-defines :x) :y :z] :a :b :c]
	       '(:x :y :z :a :b :c)))

(define (test-inner-nested-defines)
  (define ((inner-nested x) y)
    (list x y))
  inner-nested)

(assert (equal [[(test-inner-nested-defines) :x] :y]
	       '(:x :y)))

(assert (equal
	 (macroexpand-1 '(define *hello* 'algebra))
	 '(defparameter *hello* 'algebra)))

(assert (equal
	 (macroexpand-1
	  '(define (name arg1 . args)
	    (print arg1)
	    (print args)))
	 '(defun name (arg1 &rest args)
	   (print arg1)
	   (print args))))
;; (define (name arg1 . args) body...) =>
;; (defun name (arg1 &rest args) body...)

(assert (equal
	 (macroexpand-1
	  '(define (top arg1 . args)
	    (define local-var 1)
	    (define (local-fn arg1 . args)
	      (define (local-local-fn arg)
		'body)
	      'body)
	    local-var))
	 '(DEFUN TOP (ARG1 &REST ARGS)
	   (LET (LOCAL-VAR LOCAL-FN)
	     (LABELS ((LOCAL-FN (ARG1 &REST ARGS)
			(LET (LOCAL-LOCAL-FN)
			  (LABELS ((LOCAL-LOCAL-FN (ARG)
				     'BODY))
			    (SETQ LOCAL-LOCAL-FN #'LOCAL-LOCAL-FN)
			    'BODY))))
	       (SETQ LOCAL-VAR 1
		     LOCAL-FN #'LOCAL-FN)
	       LOCAL-VAR)))))

(define *get-bundle-type-predicate* (gensym))
(define (make-bundle-predicate name)
  (let (self)
    (setq self
	  (λ args
	    (if (empty? args)
		`(make-bundle-predicate ,name)
		(let ((data (first args)))
		  (and (functionp data)
		       (eq self [data *get-bundle-type-predicate*]))))))))

(define *name?* (make-bundle-predicate :bundle))
(assert [*name?* (λ (arg)
		   (cond
		     ((eq *get-bundle-type-predicate* arg) *name?*)))])

(define *get-bundle-list* (gensym))
(for-macros
  (define (make-keyword symbol)
    (intern (symbol-name symbol) :keyword)))

(define *get-bundle-permissions* (gensym))

(defmacro bundle (type-predicate bundle-list-form &rest fn-names)
  `(λ (arg)
     (cond
       ((and ,type-predicate (eq *get-bundle-type-predicate* arg))
	,type-predicate)
       ((eq *get-bundle-list* arg)
	,(if bundle-list-form
	     `(list ',(first bundle-list-form) ,@(rest bundle-list-form))
	     '()))
       ((eq *get-bundle-permissions* arg) ',(map (λ (name) (make-keyword name)) fn-names))
       ,@ (map (λ (name) `((eq ,(make-keyword name) arg) ,name)) fn-names))))

(defun bundle-permissions (bundle)
  [bundle *get-bundle-permissions*])
(defun bundle-list (bundle)
  [bundle *get-bundle-list*])

(define *point?* (make-bundle-predicate :point))
(define (make-point x y)
  (define (get-x) x)
  (define (get-y) y)
  (define (set-x! new-x) (setq x new-x))
  (define (set-y! new-y) (setq y new-y))

  (bundle *point?* (make-point x y) get-x get-y set-x! set-y!))

#+nil
(sb-introspect:function-lambda-list [(make-point 3 4) :set-x!])
;; => (NEW-X)


(let ((point (make-point 3 4)))
  (assert (= 3 [[point :get-x]]))
  [[point :set-x!] 32]
  (assert (= 32 [[point :get-x]]))
  (assert [*point?* point]))

(define (filter predicate list)
  (remove-if-not predicate list))

(for-macros
  (define (pair? datum) (consp datum))
  (define (null? datum) (null datum))
  (define (list? datum) (listp datum)))

(define (list-ref list pos)
  (nth pos list))

(define (list-tail list pos)
  (nthcdr pos list))

(define (foldl proc init . lists)
  (let rec ((result init)
	    (lists lists))
    (if (member nil lists)
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldl 'cons () '(1 2 3 4))
	       '(4 3 2 1)))

(assert (= (foldl (λ (a b result)
		    (* result (- a b)))
		  1
		  '(1 2 3)
		  '(4 5 6))
	   -27))

(define (foldr proc init . lists)
  (let rec ((result init)
	    (lists (map 'reverse lists)))
    (if (member nil lists)
	result
	(rec (apply proc (append (map #'first lists) (list result)))
	     (map #'rest lists)))))

(assert (equal (foldr 'cons '() '(1 2 3 4))
	       '(1 2 3 4)))
(assert (equal (foldr (λ (v l) (cons (1+ v) l)) '() '(1 2 3 4))
	       '(2 3 4 5)))


(define (negative? num) (minusp num))
(define (positive? num) (plusp num))

(define (andmap proc . lists)
  (let rec ((result t)
	    (lists lists))
    (if (or (not result) (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))

(assert (andmap 'positive? '(1 2 3)))
;; (andmap 'positive? '(1 2 a))
(assert (not (andmap 'positive? '(1 -2 a))))
(assert (= 9 (andmap '+ '(1 2 3) '(4 5 6))))

(define (ormap proc . lists)
  (let rec ((result ())
	    (lists lists))
    (if (or result (member nil lists))
	result
	(rec (apply proc (map 'first lists))
	     (map 'rest lists)))))

(define (eq? obj1 obj2) (eq obj1 obj2))
(define (equal? obj1 obj2) (equal obj1 obj2))

(assert (ormap 'eq? '(a b c) '(a b c)))
(assert (ormap 'positive? '(1 2 a)))
(assert (= 5 (ormap '+ '(1 2 3) '(4 5 6))))

(define (for-each proc . lists)
  (let rec ((lists lists))
    (unless (member nil lists)
      (apply proc (map 'first lists))
      (rec (map 'rest lists)))))

(assert (equal?
	 (with-output-to-string (s)
	   (for-each (λ (x y) (format s "~S" (list x y)))
		     '(a b c)
		     '(1 2 3)))
	 "(A 1)(B 2)(C 3)"))

(define (remq v list)
  (remove v list :test #'eq))

(defun remove* (v-list list &optional (test #'equal?))
  (foldl (λ (v result) (remove v result :test test)) list v-list))

(assert (equal
	 (remove* (list 1 2) (list 1 2 3 2 4 5 2))
	 '(3 4 5)))

(define (remq* v-list list) (remove* v-list list #'eq?))

(defun sort (list less-than? &key (extract-key (λ (x) x)))
  (cl:sort (copy-list list) less-than? :key extract-key))

(assert (equal (let ((original-list '(1 3 4 2)))
		 (sort original-list '<))
	       '(1 2 3 4)))

(define (memf proc list)
  (let rec ((list list))
    (if (or (null? list) [proc (first list)])
	list
	(rec (rest list)))))

(assert (equal (memf (λ (arg) (> arg 9)) '(7 8 9 10 11))
	       '(10 11)))

(define (findf proc list)
  (let ((found (memf proc list)))
    (if (list? found)
	(first found)
	())))

(assert (= (findf (λ (arg) (> arg 9)) '(7 8 9 10 11))
	   10))

(define (list-update list pos updater)
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

(define (symbol->string symbol) (symbol-name symbol))

(assert (equal (list-update '(zero one two) 1 'symbol->string)
	       '(ZERO "ONE" TWO)))

(define (list-set list pos value)
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

(define (take list pos)
  (let rec ((list list)
	    (current-pos 0)
	    (result '()))
    (if (or (= pos current-pos) (empty? list))
	(nreverse result)
	(rec (rest list)
	     (1+ current-pos)
	     (cons (first list) result)))))

(assert (equal (take '(1 2 3 4 5) 2)
	       '(1 2)))

(define (drop list pos) (list-tail list pos))

(define (split-at list pos)
  (list (take list pos) (drop list pos)))

(define (takef list predicate)
  (let rec ((list list)
	    (result '()))
    (if (or (empty? list) (not [predicate (first list)]))
	(nreverse result)
	(rec
	 (rest list)
	 (cons (first list) result)))))

(define (dropf list predicate)
  (let rec ((list list))
    (if (or (empty? list) (not [predicate (first list)]))
	list
	(rec (rest list)))))

(define (even? x) (evenp x))
(define (odd? x) (oddp x))

(assert (equal (takef '(2 4 5 8) 'even?)
	       '(2 4)))

(assert (equal (dropf '(2 4 5 8) 'even?)
	       '(5 8)))

(define (splitf-at list predicate)
  (list (takef list predicate) (dropf list predicate)))



(assert (equal (flatten '((a) b (c (d) . e) ()))
	       '(a b c d e)))

(define (compose . procs)
  (foldr (λ (proc result)
	   (λ args
	     (multiple-value-call proc (apply result args))))
	 (λ args (values-list args))
	 procs))

(assert (equal (multiple-value-list [(compose) :x :y :z])
	       '(:x :y :z)))

(assert (equal [(compose (λ (x y z) (list 'f x y z))) 'x 'y 'z]
	       '(f x y z)))

(assert (equal
	 [(compose (λ (a b c) (list 'f a b c))
		   (λ (x y) (values (list 'g x) (list 'g y) (list 'g 'c))))
	  'x 'y]

	 '(f (g x) (g y) (g c))))

(define (filter-map proc . lists)
  (remove nil (apply 'map proc lists)))

(defun range (end &optional (start 0) (step 1))
  (if (<= start end)
      (loop for i from start below end by step collecting i)
      (loop for i from start downto end by step collecting i)))

(define (append-map proc . lists)
  (append* (apply 'map proc lists)))

(define (filter-not pred list)
  (filter (compose 'not pred) list))

(define (partition pred list)
  (list (filter pred list)
	(filter-not pred list)))

(assert (equal (partition 'even? '(1 2 3 4 5 6))
	       '((2 4 6) (1 3 5))))

(define (procedure? datum) (functionp datum))

(define (lcurry proc . left-args)
  (λ right-args
    (apply proc (append left-args right-args))))

(assert (= [(lcurry '- 5 4) 3]
	   (- 5 4 3)))

(define (rcurry proc . right-args)
  (λ left-args
    (apply proc (append left-args right-args))))

(assert (= [(rcurry '- 4 3) 5]
	   (- 5 4 3)))

(define (swap-args proc)
  (λ (x y) [proc y x]))

(assert (equal [(swap-args 'cons) 1 2]
	       (cons 2 1)))

(define (memo-proc proc)
  (let ((run? ())
	(result-values))
    (λ args
      (unless run?
	(setq result-values (multiple-value-list (apply proc args))
	      run? t)
	result-values)
      (values-list result-values))))

(defmacro delay (&body body)
  `(memo-proc (λ () ,@body)))
(define  (force promise) [promise])

(define *the-empty-stream* ())
(defmacro stream-cons (first rest)
  `(cons ,first (delay ,rest)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-empty? stream) (eq *the-empty-stream* stream))

(define (stream-for-each stream proc)
  (let rec ((stream stream))
    (unless (stream-empty? stream)
      [proc (stream-car stream)]
      (rec (stream-cdr stream)))))

(define (stream-length stream)
  (let ((count 0))
    (stream-for-each stream (λ (x) (declare (ignore x)) (incf count)))
    count))

(define (stream->list stream)
  (let ((xs ()))
    (stream-for-each stream (λ (x) (push x xs)))
    (nreverse xs)))

(define (stream-first stream) (stream-car stream))
(define (stream-rest stream) (stream-cdr stream))

(define (stream? datum)
  (or (eq? datum *the-empty-stream*)
      (and (pair? datum)
	   (procedure? (cdr datum)))))

(define (list->stream list)
  (if (empty? list)
      *the-empty-stream*
      (stream-cons (first list) (list->stream (rest list)))))

(define (stream . list)
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
  (if (stream-empty? stream)
      *the-empty-stream*
      (stream-cons [proc (stream-first stream)] (stream-map proc (stream-rest stream)))))

(assert (equal (stream->list (stream-map (lcurry '* 5) *test-stream*))
	       '(5 10 15)))

(define (stream-fold proc init stream)
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
  (let rec ((stream stream)
	    (n n))
    (if (or (stream-empty? stream) (<= n 0))
	stream
	(rec (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-drop (stream 1 2 3) 2))
	       '(3)))

(define (stream-take stream n)
  (if (or (stream-empty? stream) (<= n 0))
      *the-empty-stream*
      (stream-cons (stream-first stream)
		   (stream-take (stream-rest stream) (1- n)))))

(assert (equal (stream->list (stream-take (stream 1 2 3) 2))
	       '(1 2)))

(define (stream-ref stream i)
  (stream-first (stream-drop stream i)))

(assert (= (stream-ref (stream 0 1 2 3) 1)
	   1))

(define (stream-append . streams)
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
  (stream-fold 'stream-append
	       *the-empty-stream*
	       stream-of-streams))

(assert (equal
	 (stream->list (stream-flatten (stream (stream 1 2 3)
					       (stream 4 5 6)
					       (stream 7 8 9))))
	 '(1 2 3 4 5 6 7 8 9)))


(define (stream-range start end)
  (cond
    ((> start end) *the-empty-stream*)
    (t
     (stream-cons start
		  (stream-range (1+ start) end)))))

(assert (equal (stream->list (stream-range 4 8))
	       '(4 5 6 7 8)))

(define (stream-flatmap proc s)
  (stream-flatten (stream-map proc s)))

(assert (equal (stream->list (stream-flatmap
			      (λ (i)
				(stream-map
				 (λ (j) (list i j))
				 (stream 4 5)))
			      (stream 1 2)))
	       '((1 4) (1 5) (2 4) (2 5))))


(for-macros
  (define (stream-collect-bindings-fn binding-names body)
    (let ((arg-name (gensym)))
      `(λ (,arg-name)
	 (destructuring-bind ,binding-names ,arg-name
	   (declare (ignorable ,@binding-names))
	   ,@body)))))

(for-macros
  (define (stream-collect-filter-form binding-names test-form stream-form)
    `(stream-filter
      ,(stream-collect-bindings-fn binding-names (list test-form))
      ,stream-form)))

(stream-collect-filter-form '(i j) '(even? (+ i j)) :stream)

(for-macros
  (define (stream-collect-inner-map-form binding binding-names)
    `(stream-map (λ (,(first binding))
		   (list ,@binding-names))
		 ,(second binding))))

(stream-collect-inner-map-form '(j (stream-range 1 (1- i)))
			       '(i j))
(for-macros
  (define (stream-collect-flatmap-form binding body)
    `(stream-flatmap (λ (,(first binding))
		       ,@body)
		     ,(second binding))))

(stream-collect-flatmap-form '(i (stream-range 1 n)) '(:body))

(for-macros
  (define (stream-collect-outer-map binding-names form stream)
    `(stream-map
      ,(stream-collect-bindings-fn binding-names (list form))
      ,stream)))

(stream-collect-outer-map '(i j) '(list i j (+ i j)) :stream)

(for-macros
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
	     (rest bindings)))))))

(stream-collect-inner-flatmaps '((i (stream-range 1 n))
				 (j (stream-range 1 (1- i)))))

(for-macros
  (define (stream-collect-form map-form bindings filter-form)
    (let ((binding-names (map 'car bindings)))
      (stream-collect-outer-map
       binding-names
       map-form
       (stream-collect-filter-form
	binding-names
	filter-form
	(stream-collect-inner-flatmaps bindings))))))


(stream-collect-form '(list i j (+ i j))
		     '((i (stream-range 1 n))
		       (j (stream-range 1 (1- i))))
		     '(even? (+ i j)))

(defmacro stream-collect (map-form bindings filter-form)
  (stream-collect-form map-form bindings filter-form))

(define (prime? num)
  (let ((root (floor (sqrt num))))
    (not (find-if (λ (div) (zerop (rem num div))) (range (1+ root) 2)))))

(define (prime-sum-pairs n)
  (stream-collect
   (list i j (+ i j))
   ((i (stream-range 1 n))
    (j (stream-range 1 (1- i))))
   (prime? (+ i j))))

(assert (equal (stream->list (prime-sum-pairs 6))
	       '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))))

(defun alist-ref (alist key &optional failure-result)
  (let ((pair (assoc key alist :test #'equal?)))
    (if (pair? pair)
	(cdr pair)
	failure-result)))
(define (alist-remove alist key)
  (remove key alist :test #'equal? :key #'car))
(define (alist-set alist key value)
  (acons key value (alist-remove alist key)))

(defun alist-update (alist key updater &optional failure-result)
  (alist-set alist key [updater (alist-ref alist key failure-result)]))
(define (alist-map alist proc)
  (map (λ (binding) [proc (car binding) (cdr binding)]) alist))

(define (alist-keys alist)
  (alist-map alist (λ (key value) (declare (ignore value)) key)))
(define (alist-values alist)
  (alist-map alist (λ (key value) (declare (ignore key)) value)))

(define (alist-has-key? alist key)
  (let ((no-key (gensym)))
    (eq? no-key (alist-ref alist key no-key))))

(define (alist-set* alist . keys-and-values)
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
  (apply #'alist-set* () keys-and-values))

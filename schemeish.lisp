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
      (when (null form)
	(error "Empty [], expected function combination."))
      `(funcall ,@form)))
  (defun read-right-bracket (stream char)
    (declare (ignore stream char))
    (error "read: unmatched ]"))
  
  (defun install-syntax! ()
    (set-macro-character #\[ #'read-left-bracket)
    (set-macro-character #\] #'read-right-bracket)))

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
			     (cons (list x) groups)))))))))))

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
    (first form))
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
      (if (consp name-form)
	  (car name-form)
	  name-form)))

  (defun append* (lists)
    (apply #'append lists))
  
  (defun expand-local-defines (defines body)
    (let ((names (map #'define-name defines))
	  (function-definitions (map (λ (define)
				       `(,(define-name define)
					 ,(define-form->lambda-list (second define))
					 ,@(define-body->body (cddr define))))
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

  (defun expand-define (name-or-form body)
    (cond
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
[*name?* (λ (arg)
	   (cond
	     ((eq *get-bundle-type-predicate* arg)
	      *name?*)))]

(define *get-bundle-list* (gensym))
(define (make-keyword symbol)
  (intern (symbol-name symbol) :keyword))

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

(let ((point (make-point 3 4)))
  (print [[point :get-x]])
  [[point :set-x!] 32]
  (print [[point :get-x]]))

(define (filter predicate list)
  (remove-if-not predicate list))
(define (pair? datum) (consp datum))
(define (null? datum) (null datum))
(define (list? datum) (listp datum))
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

(assert (equal (takef '(2 4 5 8) 'even?)
	       '(2 4)))

(assert (equal (dropf '(2 4 5 8) 'even?)
	       '(5 8)))

(define (splitf-at list predicate)
  (list (takef list predicate) (dropf list predicate)))

(define (flatten v)
  (cond
    ((pair? v) (append (flatten (car v)) (flatten (cdr v))))
    ((null? v) ())
    (t (list v))))

(assert (equal (flatten '((a) b (c (d) . e) ()))
	       '(a b c d e)))

(define (compose . procs)
  (foldl (λ (proc result)
	   (λ (x)
	     [result [proc x]]))
	 (λ (x) x)
	 procs))

(define (filter-map proc . lists)
  (remove nil (apply 'map proc lists)))

(define (partition pred list)
  (list (filter pred list)
	(filter (compose 'not pred) list)))

(assert (equal (partition 'even? '(1 2 3 4 5 6))
	       '((2 4 6) (1 3 5))))

(defun range (end &optional (start 0) (step 1))
  (if (<= start end)
      (loop for i from start below end by step collecting i)
      (loop for i from start downto end by step collecting i)))

(define (append-map proc . lists)
  (append* (apply 'map proc lists)))

(define (filter-not pred list)
  (filter (compose 'not pred) list))

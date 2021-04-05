;;;; schemeish.lisp

(in-package #:schemeish)

(for-macros
  (install-syntax!))

(for-macros
  ;; Re-export functions which have parameters bound to them as just functions.
  (setf (symbol-function '+) (symbol-function 'cl:+)
	(symbol-function '-) (symbol-function 'cl:-)
	(symbol-function '*) (symbol-function 'cl:*)
	(symbol-function '/) (symbol-function 'cl:/)))

(defclass struct () ())
(define (struct? datum)
  (typep datum 'struct))
(defgeneric struct-copy (struct)
  (:documentation "Returns a shallow copy of struct."))
(defmethod struct-copy (struct)
  (error "Struct ~S is not a known structure type." struct))
(defgeneric struct->list (transparent-struct)
  (:documentation "Returns a list of the form '(constructor-name field-values) for the transparent structure."))
(defmethod struct->list (struct)
  (error "Struct ~S is not a transparent structure." struct))
(defgeneric struct-accessors (transparent-struct)
  (:documentation "Returns a list of accessor symbols for the transparent structure."))
(defmethod struct-accessors (struct)
  (error "Struct ~S is not a transparent structure." struct))

(for-macros
  (define (make-struct-info type-name super-type-name field-names)
    (alist :type-name type-name
	   :super-type-name super-type-name
	   :field-names field-names)))
(for-macros
  (define (struct-info-type-name si) (alist-ref si :type-name))
  (define (struct-info-super-type-name si) (alist-ref si :super-type-name))
  (define (struct-info-field-names si) (alist-ref si :field-names))

  (defvar *struct-info-table*
    (make-hash-table :test #'eq)
    "Hash Table from structure type-name->struct-info")

  (define (get-struct-info type-name)
    (gethash type-name *struct-info-table* nil))
  (define (set-struct-info! info)
    (let* ((type-name (struct-info-type-name info))
	   (existing-info (get-struct-info type-name)))
      (when (and existing-info
		 (or (not (equal? (struct-info-super-type-name info)
				  (struct-info-super-type-name existing-info)))
		     (not (equal? (struct-info-field-names info)
				  (struct-info-field-names existing-info)))))
	(warn "Modifying structure ~S. Any sub-classed structures need to be recompiled." type-name))
      (setf (gethash type-name *struct-info-table*) info))))

(for-macros
  (define (struct-info-ancestor-fields info)
    "Returns an alist of ((ancestor . fields) ... (parent . fields) (me . fields)) From oldest generation to youngest."
    (let ((super-type-name (struct-info-super-type-name info)))
      (cond
	((null? super-type-name)
	 (list (cons (struct-info-type-name info) (struct-info-field-names info))))
	(t
	 (let ((super-struct-info (get-struct-info super-type-name)))
	   (cond
	     ((null? super-struct-info)
	      (error "The super type ~S does not exist in the *struct-info-table*" super-type-name))
	     (t  (append
		  (struct-info-ancestor-fields super-struct-info)
		  (list (cons (struct-info-type-name info) (struct-info-field-names info))))))))))))

(for-macros
  (define (struct-defclass-slot-name type-name field-name)
    (intern (string-append (symbol->string type-name) "-" (symbol->string field-name)))))

(for-macros
  (define (struct-defclass-slot-names type-name field-names)
    (map (lambda (field-name) (struct-defclass-slot-name type-name field-name))
	 field-names)))

(assert (equal? (struct-defclass-slot-names 'point '(x y))
		'(point-x point-y)))
(for-macros
  (define (ancestor-fields->field-names ancestor-fields)
    (append-map 'cdr ancestor-fields))
  (define (ancestor-fields->slot-names ancestor-fields)
    (append* (alist-map ancestor-fields
			(lambda (type-name field-names)
			  (struct-defclass-slot-names type-name field-names))))))

(let ((*struct-info-table* (make-hash-table :test #'eq)))
  (set-struct-info! (make-struct-info 'grandpa () '(father)))
  (set-struct-info! (make-struct-info 'father 'grandpa '(son)))
  (set-struct-info! (make-struct-info 'son 'father '(grandpa)))

  (let ((ancestor-fields (struct-info-ancestor-fields (get-struct-info 'son))))
    (assert (equal? (ancestor-fields->field-names ancestor-fields)
		    '(father son grandpa)))
    (assert (equal? (ancestor-fields->slot-names ancestor-fields)
		    '(grandpa-father father-son son-grandpa)))))


(for-macros
  (define (parse-struct-field-spec field-spec)
    (cond
      ((symbol? field-spec) (cons field-spec :immutable))
      ((and (pair? field-spec)
	    (symbol? (first field-spec)))
       (cond
	 ((equal? (rest field-spec) '(:mutable))
	  (cons (first field-spec) :mutable))
	 (t (error "Unknown field-option(s): ~S" (rest field-spec)))))
      (t (error "bad thing to be a field-spec: ~S" field-spec)))))

(assert (equal (parse-struct-field-spec 'field-name)
	       '(FIELD-NAME . :IMMUTABLE)))
(assert (equal (parse-struct-field-spec '(field-name :mutable))
	       '(FIELD-NAME . :MUTABLE)))

(for-macros
  (define (parse-struct-options struct-options)
    (cond
      ((empty? struct-options) ())
      (t
       (let ((opt (first struct-options)))
	 (cond
	   ((or (eq? :transparent opt)
		(eq? :mutable opt))
	    (cons (cons opt ()) (parse-struct-options (rest struct-options))))
	   ((or (eq? :guard opt)
		(eq? :super opt))
	    (cond
	      ((or (null? (rest struct-options))
		   (keywordp (second struct-options)))
	       (error "Expected form for struct-option ~S" opt))
	      (t
	       (cons (cons opt (eval (second struct-options))) (parse-struct-options (cddr struct-options))))))
	   (t (error "Bad thing to be a struct-option ~S" opt))))))))


#+nil(assert (equal (parse-struct-options '(:transparent :mutable :guard (lambda (x y z) (values x y z)) :super 'point))
		    '((:TRANSPARENT) (:MUTABLE)
		      (:GUARD LAMBDA NIL
		       (LAMBDA (X Y Z)
			 (VALUES X Y Z)))
		      (:SUPER LAMBDA NIL 'POINT))))

(for-macros
  (define (struct-constructor-name type-name)
    (intern (string-append (symbol->string 'make-) (symbol->string type-name)))))

(assert (eq? (struct-constructor-name 'point)
	     'make-point))



(for-macros
  (define (struct-defclass-form type-name field-names super-type-name)
    (let ((supers (cond ((null? super-type-name) '(struct))
			(t `(,super-type-name)))))
      `(defclass ,type-name ,supers
	 ,(struct-defclass-slot-names type-name field-names)))))

(assert (equal? (struct-defclass-form 'point '(x y) ())
		'(DEFCLASS POINT (struct) (point-x point-y))))

(assert (equal? (struct-defclass-form 'point3 '(z) 'point)
		'(DEFCLASS POINT3 (point) (point3-z))))

(for-macros
  (define (struct-define-constructor-form type-name constructor-name field-names super-type-name)
    (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	   (super-field-names (ancestor-fields->field-names ancestor-fields))
	   (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
      `(define (,constructor-name ,@(append super-field-names field-names))
	 (let ((struct (make-instance ',type-name)))
	   ,@(map (lambda (slot-name value-name)
		    `(setf (slot-value struct ',slot-name) ,value-name))
		  super-slot-names
		  super-field-names)
	   ,@(map (lambda (slot-name value-name)
		    `(setf (slot-value struct ',slot-name) ,value-name))
		  (struct-defclass-slot-names type-name field-names)
		  field-names)
	   struct)))))

(assert (equal? (struct-define-constructor-form 'point 'make-point '(x y) '())
		'(DEFINE (MAKE-POINT X Y)
		  (LET ((STRUCT (MAKE-INSTANCE 'POINT)))
		    (SETF (SLOT-VALUE STRUCT 'POINT-X) X)
		    (SETF (SLOT-VALUE STRUCT 'POINT-Y) Y)
		    STRUCT))))

(let ((*struct-info-table* (make-hash-table)))
  (set-struct-info! (make-struct-info 'point () '(x y)))
  
  (assert (equal? (struct-define-constructor-form 'point3 'make-point3 '(z) 'point)
		  '(DEFINE (MAKE-POINT3 X Y Z)
		    (LET ((STRUCT (MAKE-INSTANCE 'POINT3)))
		      (SETF (SLOT-VALUE STRUCT 'POINT-X) X)
		      (SETF (SLOT-VALUE STRUCT 'POINT-Y) Y)
		      (SETF (SLOT-VALUE STRUCT 'POINT3-Z) Z)
		      STRUCT)))))

(for-macros
  (define (struct-define-struct-copy-form type-name field-names super-type-name)
    (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	   (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
      `(defmethod struct-copy ((struct ,type-name))
	 (let ((copy (make-instance ',type-name)))
	   ,@(map (lambda (slot-name)
		    `(setf (slot-value copy ',slot-name) (slot-value struct ',slot-name)))
		  super-slot-names)
	   ,@(map (lambda (slot-name)
		    `(setf (slot-value copy ',slot-name) (slot-value struct ',slot-name)))
		  (struct-defclass-slot-names type-name field-names))
	   copy)))))

(for-macros
  (define (struct-define-struct->list-form type-name field-names super-type-name)
    (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	   (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
      `(defmethod struct->list ((struct ,type-name))
	 (list
	  ',(struct-constructor-name type-name)
	  ,@(map (lambda (slot-name) `(slot-value struct ',slot-name)) super-slot-names)
	  ,@(map (lambda (slot-name) `(slot-value struct ',slot-name)) (struct-defclass-slot-names type-name field-names)))))))

(for-macros
  (define (struct-define-accessor-form type-name slot-name)
    `(define (,slot-name ,type-name)
       (slot-value ,type-name ',slot-name))))

(assert (equal? (struct-define-accessor-form 'point 'point-x)
		'(DEFINE (POINT-X POINT)
		  (SLOT-VALUE POINT 'POINT-X))))

(for-macros
  (define (struct-define-struct-accessors-form type-name field-names super-type-name)
    (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	   (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
      `(defmethod struct-accessors ((struct ,type-name))
	 '(,@(append
	      (map (lambda (slot-name) slot-name) super-slot-names)
	      (map (lambda (slot-name) slot-name) (struct-defclass-slot-names type-name field-names))))))))

(for-macros
  (define (struct-define-field-setter-forms type-name field-name)
    (let ((setter-name (intern (string-append "SET-" (symbol->string type-name) "-" (symbol->string field-name) "!")))
	  (slot-name (struct-defclass-slot-name type-name field-name)))
      `(progn
	 (define (,setter-name ,type-name value)
	   (setf (slot-value ,type-name ',slot-name) value)
	   value)
	 (defsetf ,slot-name ,setter-name)))))

(for-macros
  (define (struct-define-equal?-form type-name field-names super-type-name)
    (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	   (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
      `(defmethod equal? ((object1 ,type-name) (object2 ,type-name))
	 (and 
	  ,@(append
	     (map (lambda (slot-name)
		    `(equal? (slot-value object1 ',slot-name)
			     (slot-value object2 ',slot-name)))
		  super-slot-names)
	     (map (lambda (slot-name)
		    `(equal? (slot-value object1 ',slot-name)
			     (slot-value object2 ',slot-name)))
		  (struct-defclass-slot-names type-name field-names))))))))

(for-macros
  (define (struct-define-print-object-form type-name)
    `(defmethod print-object ((struct ,type-name) stream)
       (print-object (struct->list struct) stream))))


(for-macros
  (define (struct-define-type-predicate-form type-name)
    (let ((predicate-name (intern (string-append (symbol->string type-name) "?"))))
      `(define (,predicate-name datum)
	 (typep datum ',type-name)))))

(assert (equal? (struct-define-type-predicate-form 'point)
		'(DEFINE (POINT? DATUM)
		  (TYPEP DATUM 'POINT))))

(for-macros
  (define (struct-form type-name field-specs struct-options)
    (let* ((parsed-field-specs (map 'parse-struct-field-spec field-specs))
	   (field-names (map 'car parsed-field-specs))
	   (slot-names (struct-defclass-slot-names type-name field-names))
	   (parsed-struct-options (parse-struct-options struct-options))
	   (super-type-name (alist-ref parsed-struct-options :super nil)))
      `(progn
	 (set-struct-info! (make-struct-info ',type-name ',super-type-name ',field-names))
	 ,(struct-defclass-form type-name field-names super-type-name)
	 ,(struct-define-struct-copy-form type-name field-names super-type-name)
	 ,@(cond ((alist-has-key? parsed-struct-options :transparent)
		  (list
		   (struct-define-struct->list-form type-name field-names super-type-name)
		   (struct-define-struct-accessors-form type-name field-names super-type-name)
		   (struct-define-equal?-form type-name field-names super-type-name)
		   (struct-define-print-object-form type-name)))
		 (t ()))
	 ,@(cond ((alist-has-key? parsed-struct-options :mutable)
		  (map (lambda (field-name)
			 (struct-define-field-setter-forms type-name field-name))
		       field-names))
		 (t
		  (map (lambda (field-spec)
			 (struct-define-field-setter-forms type-name (car field-spec)))
		       (filter (lambda (field-spec) (eq? (cdr field-spec) :mutable))
			       parsed-field-specs))))
	 ,(struct-define-constructor-form type-name (struct-constructor-name type-name)
					  field-names
					  super-type-name)
	 ,@(map (lambda (slot-name) (struct-define-accessor-form type-name slot-name))
		slot-names)
	 ,(struct-define-type-predicate-form type-name)))))

(struct-form 'point '(x y) '())
(struct-form 'point3 '(z) '(:super 'point))
(struct-form 'tpoint '(x y) '(:transparent))
(struct-form 'mpoint '(x y) '(:mutable))
(struct-form 'mypoint '(x (y :mutable)) '())

(defmacro define-struct (type-name (&rest field-specs) &rest struct-options)
  "A structure is a record object with a CLOS class type, 
automatically generated constructor of the form (MAKE-<type-name> field-args...)
field accessors of the form (<type-name>-<field-name> struct-arg),
and a type predicate of the form (<type-name>? datum). 
It takes takes the form
    (define-struct type-name (field-specs...) struct-options...)
Where a field-spec is either FIELD-NAME or (FIELD-NAME :MUTABLE)
and a struct-option is one of:
  :mutable
  :transparent
  :super super-struct-type-name-form

If mutable is provided for fields or the whole structure, 
setters are generated of the form SET-<type-name>-<field-name>!
and setf forms are generated for (setf (<type-name>-<field-name> struct-arg) value).

If transparent is provided:
- a recursive EQUAL? test is generated to test equality of each field. Otherwise only identity is tested.
- (struct->list p) creates a list that looks like a constructor call. This is used when printing the object.
- (struct-accessors p) returns a list of all of the accessors associated with transparent structure p. 

If a super-type symbol is specified, this structure will inherit all of the accessors, setters, and predicates from
the super classes in addition to the fields provided by field-specs."
  ;; TODO: issue when a transparent object inherits from an opaque object
  ;; TODO: guard clauses
  `(for-macros
     ,(struct-form type-name field-specs struct-options)))

;; Struct form
'(define-struct <type-name>
  (field-specs...)
  struct-options...)

(define (string-starts-with? string sub-string)
  (string= (subseq string 0 (length sub-string))
	   sub-string))

(define-struct point (x y))
(let ((p (make-point 3 4)))
  (assert (equal? (list (point-x p)	   ;; 3
			(point-y p)	   ;; 4
			(point? p)	   ;; t
			(struct? p)	   ;; t
			(not (equal? (make-point 3 4) (make-point 3 4))) ;; t
			(not (equal? (struct-copy p) p)) ;; t
			(equal? p p))  ;; t
		  (list 3 4 t t t t t)))
  (assert (string-starts-with? (format nil "~S" p) "#<POINT")))

;; Super types
(define-struct point3d (z)
	       :super 'point)
(let ((p3d (make-point3d 3 4 5)))
  (assert (and-let* ((copy (struct-copy p3d))
		     ((equal? (point-x p3d) (point-x copy)))
		     ((equal? (point3d-z p3d) (point3d-z copy))))
	    (not (equal? p3d copy))))
  (assert (equal? (list (point? p3d)	;; t
			(point3d? p3d)	;; t
			(point-x p3d)	;; 3
			(point-y p3d)	;; 4
			(point3d-z p3d) ;; 5
			(and-let* ((copy (struct-copy p3d))
				   ((equal? (point-x p3d) (point-x copy)))
				   ((equal? (point3d-z p3d) (point3d-z copy))))
			  (not (equal? p3d copy))) ;;t
			(string-starts-with? (format nil "~S" p3d) "#<POINT3D")) ;; #<struct point3d>
		  (list t t 3 4 5 t t))))

;; Super-duper types
(define-struct point4d (w)
	       :super 'point3d)
(let ((p4d (make-point4d 'x 'y 'z 'w)))
  (assert [(conjoin 'struct? 'point? 'point3d? 'point4d?) p4d])
  (list (point-x p4d)
	(point-y p4d)
	(point3d-z p4d)
	(point4d-w p4d)))

;; Transparent structures
(define-struct tpoint (x y) :transparent)
(let ((p (make-tpoint 3 4)))
  (assert (every 'identity
		 (list
		  (equal? (struct->list p) '(make-tpoint 3 4))
		  (equal? (struct-accessors p) '(tpoint-x tpoint-y))
		  (string= (format nil "~S" '(make-tpoint 3 4)) (format nil "~S" p))
		  (equal? p p)
		  (equal? (make-tpoint 3 4) (make-tpoint 3 4))))))

;; Mutable structures
(define-struct mpoint (x y) :mutable :transparent)
(let ((p (make-mpoint 3 4)))
  (setf (mpoint-x p) 5)
  (setf (mpoint-y p) 6)
  (assert (equal? (struct->list p) '(make-mpoint 5 6)))
  (set-mpoint-x! p :x)
  (set-mpoint-y! p :y)
  (assert (equal? (struct->list p) '(make-mpoint :x :y))))

;; Mutable fields
(define-struct mpoint3 (x y (z :mutable)) :transparent)
(let ((p (make-mpoint3 3 4 5)))
  (setf (mpoint3-z p) 20)
  (assert (equal? p (make-mpoint3 3 4 20))))

;; TODO: Guard-expressions
#+nil
(struct ipoint (x y)
	:guard (lambda (x y)
		 (if (not (and (integerp x) (integerp y)))
		     (error "ipoints require integer arguments. got: X=~S Y=~S" x y)
		     (values x y))))


(define *queue?* (make-bundle-predicate :queue))
(define (make-queue (front-ptr ()))
  (define rear-ptr (last front-ptr))
  (define (empty?) (null? front-ptr))
  (define (front)
    (cond
      ((empty?)
       (error "Cannot get the front of an empty queue."))
      (t (car front-ptr))))
  (define (insert! item)
    (let ((new-pair (cons item '())))
      (cond
	((empty?)
	 (setq front-ptr new-pair)
	 (setq rear-ptr new-pair))
	(t
	 (set-cdr! rear-ptr new-pair)
	 (setq rear-ptr new-pair)))))
  (define (delete!)
    (cond
      ((empty?)
       (error "Cannot delete from an empty queue."))
      (t
       (setq front-ptr (cdr front-ptr)))))

  (bundle *queue?* (make-queue front-ptr)
	  empty?
	  front
	  insert!
	  delete!))

(define (queue? v) [*queue?* v])
(define (queue-empty? q) [[q :empty?]])
(define (queue-front q) [[q :front]])
(define (queue-insert! q item)
  [[q :insert!] item]
  q)
(define (queue-delete! q)
  [[q :delete!]]
  q)

(assert (queue? (make-queue)))
(assert (queue-empty? (make-queue)))
(let ((q (make-queue)))
  (assert (eq? q (queue-insert! q 1)))
  (assert (= 1 (queue-front q)))
  (queue-insert! q 2)
  (assert (= 1 (queue-front q)))
  (assert (eq? q (queue-delete! q)))
  (assert (= 2 (queue-front q)))
  (assert (queue-empty? (queue-delete! q)))

  (assert (null (ignore-errors (queue-front q))))
  (assert (null (ignore-errors (queue-delete! q)))))

(define (serialize datum)
  "Recursively serializes structs using struct->list, bundles using bundle-list, and lists
into a list form that can be EVAL'd.
Bundles will no longer share identity after EVAL."
  (cond
    ((struct? datum) (let ((list (struct->list datum)))
		       (cons (first list) (map 'serialize (rest list)))))
    ((bundle? datum) (let ((list (bundle-list datum)))
		       (cons (first list) (map 'serialize (rest list)))))
    ((null? datum) ())
    ((pair? datum) `(cons ,(serialize (car datum)) ,(serialize (cdr datum))))
    (t `',datum)))

(assert (equal? (eval (serialize (list 1 2 3 4)))
		(list 1 2 3 4)))

(let* ((point (make-tpoint 3 4)))
  (assert (equal? (eval (serialize point)) point)))
(let* ((qpoint (make-tpoint (make-queue) (make-queue (list 1 2 3)))))
  (assert (equal? (serialize (eval (serialize qpoint)))
		  (serialize qpoint))))

(define *lambda-list-keywords*
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
    (let ((arity (nreverse base-arity)))
      arity))
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

(define (definition-with-definitions-nested-inside-let)
  (define a 1)
  (define steak :sauce)
  (let ((here :marker))
    (define (okay? var)
      (if (eq? var :sauce)
	  :yeah-its-kay))
    (let* ((uh-oh :hi))
      (define (steak-sauce) steak)
      (okay? (and here uh-oh (steak-sauce))))))

(assert (eq? :yeah-its-kay (definition-with-definitions-nested-inside-let)))

(for-macros
  (define (lexical-name->parameter-name symbol)
    (intern (string-append "*" (symbol->string symbol) "*"))))


(for-macros
  (define (special? symbol)
    "True if symbol is marked as special."
    #+sbcl
    (eq? :special (sb-cltl2:variable-information symbol))
    #-sbcl
    (let ((f (gensym)))
      (null?
       (ignore-errors
	(eval `(let ((,symbol 1))
		 (let ((,f (lambda () ,symbol)))
		   (let ((,symbol 2))
		     (not (eql 2 (funcall ,f))))))))))))

(for-macros
  (define (special-form? symbol)
    (member symbol '(cl:block      cl:let*                  cl:return-from
		     cl:catch      cl:load-time-value       cl:setq
		     cl:eval-when  cl:locally               cl:symbol-macrolet
		     cl:flet       cl:macrolet              cl:tagbody
		     cl:function   cl:multiple-value-call   cl:the
		     cl:go         cl:multiple-value-prog1  cl:throw
		     cl:if         cl:progn                 cl:unwind-protect
		     cl:labels     cl:progv
		     cl:let        cl:quote))))

(for-macros
  (define (parameter-name? symbol)
    "True if the symbol has a *NAME* naming convention"
    (and-let*
	((str (symbol->string symbol))
	 (length (length str))
	 ((>= length 3))
	 ((char= #\* (aref str 0)))
	 ((char= #\* (aref str (1- length))))
	 ((find-if (lambda (c) (not (char= #\* c))) (subseq str 1 (1- length)))))
      t)))

(assert (parameter-name? '*get-bundle-type-predicate*))
(assert (not (parameter-name? '***)))

(for-macros
  (define (parameter-name->lexical-name symbol (wrap-str "/"))
    "Given a symbol with a special naming convention (like *NAME*), 
return a symbol which follows the naming convetion /NAME/ (wrapped with whatever wrap-str is),
The returned symbol will be in the current package.
This operation fails if the resulting lexical-name is declared to be special."
    (let ((str (symbol->string symbol)))
      (let ((result (intern (string-append wrap-str (subseq str 1 (1- (length str))) wrap-str))))
	(assert (not (special? result)))
	result))))

(assert (eq? (parameter-name->lexical-name '*get-bundle-type-predicate*)
	     '/get-bundle-type-predicate/))

(assert (eq? (parameter-name->lexical-name '*terminal-io*)
	     '/terminal-io/))



;; TODO: combine docs/set arities for compose et al.
;; TODO: more packages (for macros)

(for-macros
  (define (lexical-bindings parameter-wrap-string special-fn-append-string)
    (let (package-symbols fn-bindings special-bindings)
      (do-symbols (sym)
	(cond
	  ;; Ignore duplicate symbols.
	  ((member sym package-symbols) t)
	  ((and (fboundp sym)
		(not (special-form? sym))
		(not (macro-function sym)))
	   (if (special? sym)
	       (let ((lexical-sym (intern (string-append (symbol-name sym) special-fn-append-string))))
		 (assert (not (special? lexical-sym)))
		 (push `(,lexical-sym #',sym) fn-bindings))
	       (push `(,sym #',sym) fn-bindings))
	   (push sym package-symbols))
	  ((and (special? sym) (parameter-name? sym))
	   (push (list (parameter-name->lexical-name sym parameter-wrap-string) sym) special-bindings)
	   (push sym package-symbols))))
      (nconc fn-bindings special-bindings))))

(defmacro lexically ((&key (parameter-wrap-sym '/) (special-fn-append-sym 'f)) &body body)
  "Evaluate body in a lexical scope, expanding defines as if inside of a define or lambda.
Establishes lexical-bindings for all normal functions and parameters in the current package.
Effectively all functions can be called with [], while all special-forms and macros would use (). 
Use in conjunction with EXPOSE.

Lexical bindings for the current package are of the form (fn #'fn) if fn is not any of:
  - a special form
  - a macro-function
  - a special variable (such as /)
If fn is a special variable it is appended with special-fn-append. 
E.g. since / is a special variable, the binding is (/f #'/). 
and (/parameter/ *parameter*) if parameter is all of
  - a special variable?
  - named the special variable naming convention *NAME*
Parameter-wrap-string is used to determine the string that wraps the lexical parameter name.

Note: Since the parameter name is declared special, the lexical name must be different.
There are cases where there are functions named SAME-NAME and parameters named *SAME-NAME*,
so we can't simply omit the *'s."
  (let ((lexical-bindings (lexical-bindings (symbol->string parameter-wrap-sym) (symbol->string special-fn-append-sym))))
    `(let ,lexical-bindings
       (declare (ignorable ,@(map #'first lexical-bindings)))
       ,@(expand-function-body body))))

(defmacro expose ((&rest fn-specs) &rest var-specs)
  "Define var-specs as parameters in the global scope.
Define fn-specs as functions in the global scope.

Fn-specs are one of the following forms:
  fn-name: (fdefinition 'fn-name) is set in the global environment to the value of fn-name
  (global-fn-name fn-name): (fdefinition 'global-fn-name) is set in the the global environment to the value of fn-name

Var-specs are of the following forms:
  VAR-NAME: *VAR-NAME* is defined/set as a special variable in the global environment with its initial value as VAR-NAME
  (global-special-name var-name): GLOBAL-SPECIAL-NAME is defined/set as a special variable in the global environment with its initial value as VAR-NAME
     It is STRONGLY recommended that you use *'s to wrap global-special-name.

The return value is a list of '(PARAMETER-NAMES... GLOBAL-FN-NAMES ...)
Used in conjunction with LEXICALLY you can do something like:
  (export (lexically () ... (expose ...)))"
  (let ((var-names (map (lambda (spec)
			  (cond ((pair? spec) (second spec))
				(t spec)))
			var-specs))
	(parameter-names (map (lambda (spec)
				(cond ((pair? spec) (first spec))
				      (t (lexical-name->parameter-name spec))))
			      var-specs))
	(fn-names (map (lambda (spec)
			 (cond ((pair? spec) (second spec))
			       (t spec)))
		       fn-specs))
	(global-fn-names (map (lambda (spec)
				(cond ((pair? spec) (first spec))
				      (t spec)))
			      fn-specs)))
    `(progn
       ,@(map (lambda (parameter-name var-name) `(defparameter ,parameter-name ,var-name))
	      parameter-names var-names)
       ,@(map (lambda (fn-name global-fn-name) `(setf (fdefinition ',global-fn-name) ,fn-name))
	      fn-names global-fn-names)
       ',(append parameter-names global-fn-names))))

(progn
  (assert (equal? (lexically ()
		    (define test-x 1 "test-x")
		    (define (test-y) "test-y" [+ test-x 2])
		    (define (lexical-test-z) "tests z" [+ [test-y] test-x])
		    (define lexical-test-w 1)
		    
		    (expose ((lexical-test-y test-y)
			     lexical-test-z)
			    (*lexical-test-x* test-x)
			    lexical-test-w))
		  '(*LEXICAL-TEST-X* *lexical-test-w* LEXICAL-TEST-Y LEXICAL-TEST-Z)))

  (assert (string= (documentation 'lexical-test-y 'function)
		   "test-y"))

  (assert (= 1 *lexical-test-x*))
  (assert (= 1 *lexical-test-w*))
  (assert (= 3 (lexical-test-y)))
  (assert (= 4 (lexical-test-z))))

(for-macros (uninstall-syntax!))

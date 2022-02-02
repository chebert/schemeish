(in-package #:schemeish.backend)

(install-syntax!)

(defclass struct () ()
  (:documentation "The base type for structures defined using DEFINE-STRUCT."))
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
(export '(struct struct-copy struct->list struct-accessors struct?))

(define (make-struct-info type-name super-type-name field-names)
  (alist :type-name type-name
	 :super-type-name super-type-name
	 :field-names field-names))
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
    (setf (gethash type-name *struct-info-table*) info)))

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
		(list (cons (struct-info-type-name info) (struct-info-field-names info)))))))))))

(define (struct-defclass-slot-name type-name field-name)
  (intern (string-append (symbol->string type-name) "-" (symbol->string field-name))))

(define (struct-defclass-slot-names type-name field-names)
  (map (lambda (field-name) (struct-defclass-slot-name type-name field-name))
       field-names))

(assert (equal? (struct-defclass-slot-names 'point '(x y))
		'(point-x point-y)))
(define (ancestor-fields->field-names ancestor-fields)
  (append-map 'cdr ancestor-fields))
(define (ancestor-fields->slot-names ancestor-fields)
  (append* (alist-map ancestor-fields
		      (lambda (type-name field-names)
			(struct-defclass-slot-names type-name field-names)))))

(let ((*struct-info-table* (make-hash-table :test #'eq)))
  (set-struct-info! (make-struct-info 'grandpa () '(father)))
  (set-struct-info! (make-struct-info 'father 'grandpa '(son)))
  (set-struct-info! (make-struct-info 'son 'father '(grandpa)))

  (let ((ancestor-fields (struct-info-ancestor-fields (get-struct-info 'son))))
    (assert (equal? (ancestor-fields->field-names ancestor-fields)
		    '(father son grandpa)))
    (assert (equal? (ancestor-fields->slot-names ancestor-fields)
		    '(grandpa-father father-son son-grandpa)))))


(define (parse-struct-field-spec field-spec)
  (cond
    ((symbol? field-spec) (cons field-spec :immutable))
    ((and (pair? field-spec)
	  (symbol? (first field-spec)))
     (cond
       ((equal? (rest field-spec) '(:mutable))
	(cons (first field-spec) :mutable))
       (t (error "Unknown field-option(s): ~S" (rest field-spec)))))
    (t (error "bad thing to be a field-spec: ~S" field-spec))))

(assert (equal (parse-struct-field-spec 'field-name)
	       '(FIELD-NAME . :IMMUTABLE)))
(assert (equal (parse-struct-field-spec '(field-name :mutable))
	       '(FIELD-NAME . :MUTABLE)))

(define (parse-struct-options struct-options)
  (cond
    ((empty? struct-options) ())
    (t
     (let ((opt (first struct-options)))
       (cond
	 ((or (eq? :opaque opt)
	      (eq? :mutable opt))
	  (cons (cons opt ()) (parse-struct-options (rest struct-options))))
	 ((or (eq? :documentation opt)
	      (eq? :super opt))
	  (cond
	    ((or (null? (rest struct-options))
		 (keywordp (second struct-options)))
	     (error "Expected form for struct-option ~S" opt))
	    (t
	     (cons (cons opt (eval (second struct-options))) (parse-struct-options (cddr struct-options))))))
	 (t (error "Bad thing to be a struct-option ~S" opt)))))))


(assert (equal? (parse-struct-options '(:opaque :mutable :super 'point :documentation "docs"))
		'((:OPAQUE) (:MUTABLE) (:SUPER . POINT) (:DOCUMENTATION . "docs"))))

(define (struct-constructor-name type-name)
  (intern (string-append (symbol->string 'make-) (symbol->string type-name))))

(assert (eq? (struct-constructor-name 'point)
	     'make-point))

(define (struct-defclass-form type-name field-names super-type-name documentation)
  (let ((supers (cond ((null? super-type-name) '(struct))
		      (t `(,super-type-name)))))
    `(defclass ,type-name ,supers
       ,(struct-defclass-slot-names type-name field-names)
       ,@(when documentation `((:documentation ,documentation))))))

(assert (equal? (struct-defclass-form 'point '(x y) () ())
		'(DEFCLASS POINT (struct) (point-x point-y))))

(assert (equal? (struct-defclass-form 'point3 '(z) 'point ())
		'(DEFCLASS POINT3 (point) (point3-z))))

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
	 struct))))

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
	 copy))))

(define (struct-define-struct->list-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod struct->list ((struct ,type-name))
       (list
	',(struct-constructor-name type-name)
	,@(map (lambda (slot-name) `(slot-value struct ',slot-name)) super-slot-names)
	,@(map (lambda (slot-name) `(slot-value struct ',slot-name)) (struct-defclass-slot-names type-name field-names))))))

(define (struct-define-accessor-form type-name slot-name)
  `(define (,slot-name ,type-name)
     (slot-value ,type-name ',slot-name)))

(assert (equal? (struct-define-accessor-form 'point 'point-x)
		'(DEFINE (POINT-X POINT)
		  (SLOT-VALUE POINT 'POINT-X))))

(define (struct-define-struct-accessors-form type-name field-names super-type-name)
  (let* ((ancestor-fields (struct-info-ancestor-fields (get-struct-info super-type-name)))
	 (super-slot-names (ancestor-fields->slot-names ancestor-fields)))
    `(defmethod struct-accessors ((struct ,type-name))
       '(,@(append
	    (map (lambda (slot-name) slot-name) super-slot-names)
	    (map (lambda (slot-name) slot-name) (struct-defclass-slot-names type-name field-names)))))))

(define (struct-define-field-setter-forms type-name field-name)
  (let ((setter-name (intern (string-append "SET-" (symbol->string type-name) "-" (symbol->string field-name) "!")))
	(slot-name (struct-defclass-slot-name type-name field-name)))
    `(progn
       (define (,setter-name ,type-name value)
	 (setf (slot-value ,type-name ',slot-name) value)
	 value)
       (defsetf ,slot-name ,setter-name))))

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
		(struct-defclass-slot-names type-name field-names)))))))

(defparameter *self-evaluating-symbols* '(t nil))
(define (printable-field field)
  "Returns symbols as '(quote symbol), lists as '(list ...),
dotted-lists as '(dotted-list ...) and conses as '(cons ...).
For lists containing a cycle, just returns the list as is."
  (cond ((and (symbol? field)
	      (not (keywordp field))
	      (not (member field *self-evaluating-symbols*)))
	 `',field)
	((pair? field)
	 ;; Field is a list, list*, cons, or a cycle
	 (let recurse ((xs field)
		       (visited ())
		       (result ()))
	   (cond
	     ((empty? xs)
	      ;; We are in a proper list
	      `(list ,@(map #'printable-field field)))
	     ((member xs visited)
	      ;; We are in a cycle, just return the field
	      field)
	     ((pair? xs)
	      ;; In the middle of the list, keep looking.
	      (recurse (rest xs) (cons xs visited) (cons (first xs) result)))
	     (t
	      ;; xs is not empty or a list, we are in a dotted list or cons.
	      (cond
		;; Dotted-list
		((pair? (rest field)) `(list* ,@(nreverse (map #'printable-field (cons xs result)))))
		(t `(cons ,(printable-field (car field)) ,(printable-field (cdr field)))))))))
	;; Field is something else. Just print it.
	(t field)))

(assert (equal? (printable-field :a) :a))
(assert (equal? (printable-field 'a) '(quote a)))
(assert (equal? (printable-field 1) 1))
(assert (equal? (printable-field (list 1 2 3))
		'(list 1 2 3)))
(assert (equal? (printable-field (cons 1 (cons 2 3)))
		'(list* 1 2 3)))
(assert (equal? (printable-field (cons 1 2))
		'(cons 1 2)))
(assert (equal? (printable-field (list (list 1) (list 2 (list 3))))
		'(LIST (LIST 1) (LIST 2 (LIST 3)))))

(define (print-transparent-struct struct stream)
  (let ((list (struct->list struct)))
    (print-object (cons (first list) (map #'printable-field (rest list)))
		  stream)))

(define (struct-define-print-object-form type-name)
  `(defmethod print-object ((struct ,type-name) stream)
     (print-transparent-struct struct stream)))

(define (struct-define-type-predicate-form type-name predicate-name)
  `(define (,predicate-name datum)
     (typep datum ',type-name)))

(assert (equal? (struct-define-type-predicate-form 'point 'point?)
		'(DEFINE (POINT? DATUM)
		  (TYPEP DATUM 'POINT))))

(define (struct-form type-name field-specs struct-options)
  (let* ((parsed-field-specs (map 'parse-struct-field-spec field-specs))
	 (field-names (map 'car parsed-field-specs))
	 (slot-names (struct-defclass-slot-names type-name field-names))
	 (parsed-struct-options (parse-struct-options struct-options))
	 (super-type-name (alist-ref parsed-struct-options :super nil))
	 (constructor-name (struct-constructor-name type-name))
	 (predicate-name (intern (string-append (symbol->string type-name) "?"))))
    `(progn
       (set-struct-info! (make-struct-info ',type-name ',super-type-name ',field-names))
       ,(struct-defclass-form type-name field-names super-type-name (alist-ref parsed-struct-options :documentation))
       ,(struct-define-struct-copy-form type-name field-names super-type-name)
       ,@(cond ((not (alist-has-key? parsed-struct-options :opaque))
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
       ,(struct-define-constructor-form type-name constructor-name
					field-names
					super-type-name)
       ,@(map (lambda (slot-name) (struct-define-accessor-form type-name slot-name))
	      slot-names)
       ,(struct-define-type-predicate-form type-name predicate-name)
       '(,type-name ,constructor-name ,predicate-name ,@slot-names))))

(struct-form 'point '(x y) '())
(struct-form 'point3 '(z) '(:super 'point))
(struct-form 'tpoint '(x y) '(:opaque))
(struct-form 'mpoint '(x y) '(:mutable))
(struct-form 'mypoint '(x (y :mutable)) '())
(struct-form 'mypoint '(x y) '(:documentation "docstring"))

(uninstall-syntax!)

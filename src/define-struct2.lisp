(in-package #:schemeish.backend)

(install-syntax!)

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
  :opaque
  :super super-struct-type-name-form
  :documentation documentation-string

:MUTABLE
If mutable is provided for fields or the whole structure, 
setters are generated of the form SET-<type-name>-<field-name>!
and setf forms are generated for (setf (<type-name>-<field-name> struct-arg) value).

:OPAQUE
If opaque is NOT provided:
- a recursive EQUAL? test is generated to test equality of each field. Otherwise only identity is tested.
- (struct->list p) creates a list that looks like a constructor call. This is used when printing the object.
- (struct-accessors p) returns a list of all of the accessors associated with transparent structure p. 

:SUPER super-struct-type-name-form
If a super-type symbol is specified, this structure will inherit all of the accessors, setters, and predicates from
the super classes in addition to the fields provided by field-specs.

Returns a list of newly defined symbols."
  ;; TODO: issue when a transparent object inherits from an opaque object
  ;; TODO: documentation-tags
  ;; TODO: guard-tags
  `(for-macros
     ,(struct-form type-name field-specs struct-options)))
(export 'define-struct)

(define-struct point (x y) :opaque)
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
	       :super 'point
	       :opaque)
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
(define-struct tpoint (x y))
(let ((p (make-tpoint 3 4)))
  (assert (every 'identity
		 (list
		  (equal? (struct->list p) '(make-tpoint 3 4))
		  (equal? (struct-accessors p) '(tpoint-x tpoint-y))
		  (string= (format nil "~S" '(make-tpoint 3 4)) (format nil "~S" p))
		  (equal? p p)
		  (equal? (make-tpoint 3 4) (make-tpoint 3 4))))))

;; Mutable structures
(define-struct mpoint (x y) :mutable)
(let ((p (make-mpoint 3 4)))
  (setf (mpoint-x p) 5)
  (setf (mpoint-y p) 6)
  (assert (equal? (struct->list p) '(make-mpoint 5 6)))
  (set-mpoint-x! p :x)
  (set-mpoint-y! p :y)
  (assert (equal? (struct->list p) '(make-mpoint :x :y))))

;; Mutable fields
(define-struct mpoint3 (x y (z :mutable)))
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

(uninstall-syntax!)

(in-package #:schemeish.bundle)

(for-macros
  (install-syntax!))

(defvar *get-bundle-type-predicate* (gensym))
(defvar *get-bundle-predicate-symbol* (gensym))
(defvar *get-is-bundle-predicate?* (gensym))
(defvar *is-bundle-predicate* (gensym))

(define (make-bundle-predicate name)
  "Returns a predicate which, only evaluates to true 
when given a bundle with this type-predicate"
  (define (dispatch arg)
    (cond
      ((eq? *get-bundle-predicate-symbol* arg) name)
      ((eq? *get-is-bundle-predicate?* arg)
       *is-bundle-predicate*)
      ((procedure? arg)
       (eq? dispatch [arg *get-bundle-type-predicate*]))
      (t nil)))
  dispatch)

(define (bundle-predicate-symbol predicate)
  "Returns the debug symbol associated with predicate."
  [predicate *get-bundle-predicate-symbol*])
(define (bundle-predicate? datum)
  (and (procedure? datum)
       (eq? *is-bundle-predicate* [datum *get-is-bundle-predicate?*])))

(defvar *name?* (make-bundle-predicate :bundle))
(assert [*name?* (lambda (arg)
		   (cond
		     ((eq *get-bundle-type-predicate* arg) *name?*)))])

(defvar *get-bundle-list* (gensym))

(defparameter *get-bundle-permissions* (gensym))
(defparameter *bundle?* (make-bundle-predicate :bundle))

;; TODO: Remove bundle-list-form
(defmacro bundle (type-predicate bundle-list-form &rest fn-names)
  "Create a bundle of permissions for closure objects.
Type-predicate is nil or a predicate created by make-bundle-predicate.
Example:
    (defparameter *point?* (make-bundle-predicate :point))
    (define (make-point x y)
      (define (get-x) x)
      (define (get-y) y)
      (define (set-x! new-x) (setq x new-x))
      (define (set-y! new-y) (setq y new-y))
    
      (bundle *point?* (make-point x y) get-x get-y set-x! set-y!))
    
    (let ((point (make-point 3 4)))
      [point :get-x] ;; => closure of 0 arguments
      (assert (= 3 [[point :get-x]]))
      [point :set-x!] ;; => closure of 1 argument
      [[point :set-x!] 32]
      (assert (= 32 [[point :get-x]]))
      (assert [*point?* point])
      (bundle-permissions bundle) ; => '(:get-x :get-y :set-x! :set-y!)
      (bundle-list bundle) ; => '(make-point 32 4)
      )"
  `(lambda (arg)
     (cond
       ((eq *get-bundle-type-predicate* arg)
	(or ,type-predicate *bundle?*))
       ((eq *get-bundle-list* arg)
	,(if bundle-list-form
	     `(list ',(first bundle-list-form) ,@(rest bundle-list-form))
	     '()))
       ((eq *get-bundle-permissions* arg) ',(map (lambda (name) (make-keyword name)) fn-names))
       ,@ (map (lambda (name) `((eq ,(make-keyword name) arg) ,name)) fn-names))))

(define (bundle-documentation bundle)
  "Generates documentation for bundle and all of its permissions."
  (with-output-to-string (s)
    (format s "~S~%A bundle of type ~S with permissions:" (bundle-list bundle) (bundle-predicate-symbol [bundle *get-bundle-type-predicate*]))
    (for-each (lambda (permission)
		(let ((fn [bundle permission]))
		  (format s "~&  ~S: ~A" (cons (list 'bundle permission) (arg:arglist fn)) (documentation fn 'function))))
	      (bundle-permissions bundle))))

(define (bundle-permissions bundle)
  "Return a list of permissions to the bundle."
  [bundle *get-bundle-permissions*])
(define (bundle-list bundle)
  "Return the bundle as a '(constructor-name arg-values...)"
  [bundle *get-bundle-list*])
(define (bundle? bundle)
  (and (procedure? bundle)
       (bundle-predicate? (ignore-errors [bundle *get-bundle-type-predicate*]))))

(defvar *point?* (make-bundle-predicate :point))
(define (make-bundle-point x y)
  (define (get-x) "x-coord" x)
  (define (get-y) "y-coord" y)
  (define (set-x! new-x) "set x-coord to new-x" (setq x new-x))
  (define (set-y! new-y) "set y-coord to new-y" (setq y new-y))

  (bundle *point?* (make-bundle-point x y) get-x get-y set-x! set-y!))


(bundle-documentation (make-bundle-point 3 4))
"(MAKE-BUNDLE-POINT 3 4)
A bundle of type :POINT with permissions:
  ((BUNDLE :GET-X)): x-coord
  ((BUNDLE :GET-Y)): y-coord
  ((BUNDLE :SET-X!) NEW-X): set x-coord to new-x
  ((BUNDLE :SET-Y!) NEW-Y): set y-coord to new-y"

(let ((point (make-bundle-point 3 4)))
  (assert (bundle? point))
  (assert (= 3 [[point :get-x]]))
  [[point :set-x!] 32]
  (assert (= 32 [[point :get-x]]))
  (assert [*point?* point]))
#+nil
(sb-introspect:function-lambda-list [(make-bundle-point 3 4) :set-x!])
;; => (NEW-X)


(for-macros
  (uninstall-syntax!))

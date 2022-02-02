(in-package #:schemeish.backend)

(install-syntax!)

(defvar *get-bundle-type-predicate* (gensym))
(defvar *get-bundle-predicate-symbol* (gensym))

(define (make-bundle-predicate name)
  "Returns a predicate which, only evaluates to true 
when given a bundle with this type-predicate"
  (define (dispatch arg)
    (cond
      ((eq? *get-bundle-predicate-symbol* arg) name)
      ((procedure? arg)
       (eq? dispatch [arg *get-bundle-type-predicate*]))
      (t nil)))
  dispatch)
(export 'make-bundle-predicate)

(define (bundle-predicate-symbol predicate)
  "Returns the debug symbol associated with predicate."
  [predicate *get-bundle-predicate-symbol*])

(defvar *name?* (make-bundle-predicate :bundle))
(assert [*name?* (lambda (arg)
		   (cond
		     ((eq *get-bundle-type-predicate* arg) *name?*)))])

(defvar *get-bundle-permissions* (gensym "GET-BUNDLE-PERMISSIONS"))

(define (get-fn-identifier? fn-identifier)
  (and (eql (first fn-identifier) :get)
       (symbol? (second fn-identifier))
       (empty? (rest (rest fn-identifier)))))
(define (set-fn-identifier? fn-identifier)
  (and (eql (first fn-identifier) :set!)
       (symbol? (second fn-identifier))
       (or (empty? (rest (rest fn-identifier)))
	   (and (symbol? (third fn-identifier))
		(empty? (rest (rest (rest fn-identifier))))))))

(define (set-fn-identifier-setter-keyword fn-identifier)
  (make-keyword (make-symbol (or (and-let* ((name (third fn-identifier))) (symbol->string name))
				 (concatenate 'string "SET-" (symbol->string (second fn-identifier)) "!")))))

(define (fn-identifier->permission-name fn-identifier)
  (cond ((symbol? fn-identifier) (make-keyword fn-identifier))
	((get-fn-identifier? fn-identifier) (make-keyword (second fn-identifier)))
	((set-fn-identifier? fn-identifier)
	 (make-keyword (set-fn-identifier-setter-keyword fn-identifier)))))

;; TODO: switch to a case statement
(define (bundle-fn-identifier->permission-form arg-name fn-identifier)
  (let* ((permission-name (fn-identifier->permission-name fn-identifier))
	 (test-permission-form `(eq ,permission-name ,arg-name)))
    (cond ((symbol? fn-identifier)
	   `(,test-permission-form ,fn-identifier))
	  ((get-fn-identifier? fn-identifier)
	   `(,test-permission-form (lambda () ,(second fn-identifier))))
	  ((set-fn-identifier? fn-identifier)
	   (let ((value-name (unique-symbol 'value)))
	     `(,test-permission-form
	       (lambda (,value-name) (set! ,(second fn-identifier) ,value-name))))))))

(assert (equal? (bundle-fn-identifier->permission-form 'arg 'fn-name)
		'((EQ :FN-NAME ARG) FN-NAME)))
(assert (equal? (bundle-fn-identifier->permission-form 'arg '(:get variable-name))
		'((EQ :VARIABLE-NAME ARG)
		  (LAMBDA NIL
		    VARIABLE-NAME))))
(assert (equal? (with-readable-symbols
		  (bundle-fn-identifier->permission-form 'arg '(:set! variable-name)))
		'((EQ :SET-VARIABLE-NAME! ARG)
		  (LAMBDA (VALUE)
		    (SET! VARIABLE-NAME VALUE)))))
(assert (equal? (with-readable-symbols
		  (bundle-fn-identifier->permission-form 'arg '(:set! variable-name setter-name!)))
		'((EQ :setter-name! ARG)
		  (LAMBDA (VALUE)
		    (SET! VARIABLE-NAME VALUE)))))

(for-macros
  (defvar *bundles* (make-hash-table :weakness :key))

  (define (register-bundle! bundle)
    (setf (gethash bundle *bundles*) t)
    bundle))

(export
 (define (bundle? bundle)
   (gethash bundle *bundles*)))

(defmacro bundle (type-predicate &rest fn-identifiers)
  "Create a bundle of permissions for closure objects.
A bundle is a function (bundle-proc msg) => permission, where each permission
is meant to be a locally defined function described by fn-identifiers.
Each fn-identifier is one of:
   fn-name => a symbolic name which maps to a function value.
     Produces a :fn-name permission.
   (:get variable-name) => variable-name is a symbolic name which maps to a value.
     Produces a :variable-name permission which returns a function of zero arguments.
   (:set! variable-name <setter-name>) => variable-name is a symbolic name which maps to a value.
     Produces a :setter-name permission which returns a function of one argument.
     If setter-name defaults to :set-variable-name! if not provided.
                                         
Type-predicate is nil or a predicate created by make-bundle-predicate.
Example:
    (defparameter *point?* (make-bundle-predicate :point))
    (define (make-point x y)
      (define (get-x) x)
      (define (get-y) y)
      (define (set-x! new-x) (setq x new-x))
      (define (set-y! new-y) (setq y new-y))
    
      (bundle *point?* get-x get-y set-x! set-y!))
    
    (let ((point (make-point 3 4)))
      [point :get-x] ;; => closure of 0 arguments
      (assert (= 3 [[point :get-x]]))
      [point :set-x!] ;; => closure of 1 argument
      [[point :set-x!] 32]
      (assert (= 32 [[point :get-x]]))
      (assert [*point?* point])
      (bundle-permissions bundle) ; => '(:get-x :get-y :set-x! :set-y!))"
  (let* ((arg-name (unique-symbol 'arg))
	 (permission-forms (map (lcurry #'bundle-fn-identifier->permission-form arg-name) fn-identifiers))
	 (permission-names (map #'fn-identifier->permission-name fn-identifiers)))
    (assert (every #'identity permission-forms))
    `(register-bundle!
      (lambda (,arg-name)
	(cond
	  ((eq *get-bundle-type-predicate* ,arg-name)
	   ,(cond
	      ((null? type-predicate) '(constantly nil))
	      ((symbolp type-predicate) `(function ,type-predicate))
	      (t type-predicate)))
	  ((eq *get-bundle-permissions* ,arg-name) ',permission-names)
	  ;; TODO: switch to a case statement
	  ,@permission-forms
	  (t (error "Unrecognized permission ~S for bundle. Expected one of: ~S"
		    ,arg-name ',permission-names)))))))
(export 'bundle)

(defvar *bundle-print-object-table* (make-hash-table :weakness :key))

(export
 (define (define-bundle-print-object bundle print-object-proc)
   "Defines the print-object-proc for the given bundle. [print-object-proc stream] will be called
when print-object is called on the bundle."
   (setf (gethash bundle *bundle-print-object-table*) print-object-proc)
   bundle))
(export
 (define (undefine-bundle-print-object bundle)
   "Removes the print-object-proc for the given bundle."
   (remhash bundle *bundle-print-object-table*)
   bundle))
(define (bundle-print-object-proc bundle)
  (gethash bundle *bundle-print-object-table*))

(export
 (define (bundle-documentation bundle)
   "Generates documentation for bundle and all of its permissions."
   (with-output-to-string (s)
     (format s "~%A bundle of type ~S with permissions:" (bundle-predicate-symbol [bundle *get-bundle-type-predicate*]))
     (for-each (lambda (permission)
		 (let ((fn [bundle permission]))
		   (format s "~&  ~S: ~A" (cons (list 'bundle permission) (arg:arglist fn)) (documentation fn 'function))))
	       (bundle-permissions bundle)))))

(export
 (define (bundle-permissions bundle)
   "Return a list of permissions to the bundle."
   [bundle *get-bundle-permissions*]))

(define point? (make-bundle-predicate :point))
(define (make-bundle-point x y)
  (define (get-x) "x-coord" x)
  (define (get-y) "y-coord" y)
  (define (set-x! new-x) "set x-coord to new-x" (setq x new-x))
  (define (set-y! new-y) "set y-coord to new-y" (setq y new-y))
  
  (bundle #'point? get-x get-y set-x! set-y!))

(defmethod print-object :around ((object function) stream)
  (if (bundle? object)
      (let ((proc (bundle-print-object-proc object)))
	(if proc
	    [proc stream]
	    (print-unreadable-object (object stream :identity t)
	      (format stream "BUNDLE ~S" (bundle-predicate-symbol [object *get-bundle-type-predicate*])))))
      (call-next-method)))

(make-bundle-point 3 4)

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
  (assert (point? point)))
#+nil
(sb-introspect:function-lambda-list [(make-bundle-point 3 4) :set-x!])
;; => (NEW-X)


(uninstall-syntax!)

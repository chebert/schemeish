(in-package #:schemeish.internals)

(defstruct guard-tag
  "A guard-tag is set of guard clauses that have been tagged as a guard for use in DEFINE.
Each clause is an unevaluated form. When evaluated each form should return a boolean.
When SCHEMEISH syntax is installed, #g(clauses...) is equaivalent to #.(make-guard-tag :clauses clauses)"
  clauses)
(setf (fdefinition 'guard-tag?) #'guard-tag-p)
(export '(guard-tag make-guard-tag guard-tag-clauses guard-tag?))

(export
 (defun read-guard-tag (stream char n)
   "Dispatch-macro reader for a guard-tag. #g(clauses...) => #.(make-guard-tag :clauses clauses)"
   (declare (ignore char n))
   (let ((clauses (read stream t (values) t)))
     (unless (listp clauses)
       (error "Expected guard clauses to be a list of the form (guard-clause...). Got ~S" clauses))
     (make-guard-tag :clauses clauses))))

(defmethod print-object ((guard guard-tag) stream)
  (let ((clauses (guard-tag-clauses guard)))
    (if (null clauses)
	(format stream "#G()")
	(format stream "#G~S" clauses))))

(assert (string= (print-object (make-guard-tag :clauses ()) nil)
		 "#G()"))
(assert (string= (print-object (make-guard-tag :clauses '((numberp x) (listp xs))) nil)
		 "#G((NUMBERP X) (LISTP XS))"))

(defvar *guard-clauses-enabled?* t)
(export (defun guard-clauses-enabled? ()
	  "True if enforcement of guard-clauses are enabled in the current dynamic context."
	  *guard-clauses-enabled?*))
(export (defun enable-guard-clauses! ()
	  "Enables enforcement of guard-clauses in the current dynamic context."
	  (setq *guard-clauses-enabled?* t)))
(export (defun disable-guard-clauses! ()
	  "Disables enforcement of guard-clauses in the current dynamic context."
	  (setq *guard-clauses-enabled?* nil)))
(export (defmacro with-guard-clauses-enabled (&body body)
	  "Creates a dynamic context around body with guard-clauses enforced."
	  `(cl:let ((*guard-clauses-enabled?* t))
	     ,@body)))
(export (defmacro with-guard-clauses-disabled (&body body)
	  "Creates a dynamic context around body with guard-clauses not enforced."
	  `(cl:let ((*guard-clauses-enabled?* nil))
	     ,@body)))

(defvar *guard-clauses-table* (make-hash-table :weakness :key)
  "A table from function to a list of guard-clauses guarding that function.")

(defun register-guard-clauses (function guard-clauses)
  "Associates function with guard-clauses in the *guard-clauses-hash-table*"
  (assert (functionp function))
  (assert (listp guard-clauses))
  (setf (gethash function *guard-clauses-table*) guard-clauses)
  function)

(export
 (defun registered-guard-clauses (function)
   "Retrieves the guard-clauses associated with function, or NIL if not present."
   (assert (functionp function))
   (gethash function *guard-clauses-table*)))

(defun guard-clauses-documentation-string (guard-clauses &optional name)
  "Returns a string documenting guard-clauses for name."
  (concatenate
   'string
   (if name
       (format nil "~S has" name)
       "Has")
   (if guard-clauses
       (format nil " the following guard clauses:~%~S" guard-clauses)
       " no guard clauses.")))

(defun enforce-guard-clauses-form (guard-clauses parameter-bindings)
  "Return a form that processes guard-clauses, causing an error if any clause fails.
Checks if *guard-clauses-enabled?* is true before evaluating any guard clauses.
Parameter-bindings are a list of (parameter-name value) for parameters which will be provided in the error message."
  `(when *guard-clauses-enabled?*
     ,@(mapcar (cl:lambda (guard-clause)
		 `(unless ,guard-clause
		    (error "Failed function guard-clause: ~S with the given parameter bindings: ~S" ',guard-clause ,parameter-bindings)))
	       guard-clauses)))

(assert (equal (enforce-guard-clauses-form '((numberp x) (listp xs)) '((x 3) (xs (1 2 3))))
	       '(WHEN *GUARD-CLAUSES-ENABLED?*
		 (UNLESS (NUMBERP X)
		   (ERROR
		    "Failed function guard-clause: ~S with the given parameter bindings: ~S"
		    '(NUMBERP X) ((X 3) (XS (1 2 3)))))
		 (UNLESS (LISTP XS)
		   (ERROR
		    "Failed function guard-clause: ~S with the given parameter bindings: ~S"
		    '(LISTP XS) ((X 3) (XS (1 2 3))))))))

(in-package #:schemeish.syntax)

(defstruct documentation-tag
  "A documentation-tag is an form that has been tagged as documentation for use in DEFINE."
  form)

(defstruct guard-tag
  "A guard-tag is set of guard clauses that have been tagged as a guard for use in DEFINE.
Each clause is an unevaluated form. When evaluated each form should return a boolean."
  clauses)

(setf (fdefinition 'documentation-tag?) #'documentation-tag-p)
(setf (fdefinition 'guard-tag?) #'guard-tag-p)
(export '(documentation-tag? documentation-tag documentation-tag-form guard-tag? guard-tag guard-tag-clauses))

(defun read-documentation-tag (stream char n)
  (declare (ignore char n))
  (let ((form (read stream t (values) t)))
    (make-documentation-tag :form form)))
(defmethod print-object ((object documentation-tag) stream)
  (format stream "#D~S" (documentation-tag-form object)))

(defun read-guard-tag (stream char n)
  (declare (ignore char n))
  (let ((clauses (read stream t (values) t)))
    (unless (listp clauses)
      (error "Expected guard clauses to be a list of the form (guard-clause...). Got ~S" clauses))
    (make-guard-tag :clauses clauses)))
(defmethod print-object ((guard guard-tag) stream)
  (let ((clauses (guard-tag-clauses guard)))
    (if (null clauses)
	(format stream "#G()")
	(format stream "#G~S" clauses))))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((form (read-delimited-list #\] stream t)))
    (if (null form)
	'()
	`(funcall ,@form))))
(defun read-right-bracket (stream char)
  (declare (ignore stream char))
  (error "read: unmatched ]"))

(defun read-sharpsign-semicolon (stream char n)
  (declare (ignore char n))
  (read stream nil (values) t)
  (values))

(defmacro install-syntax! ()
  "Installs [] reader syntax: 
    [function-name arg1 ...] => (funcall function-name arg1 ...)
#; reader syntax for commenting out forms
    #;form => ignored
#g reader syntax for reading guard forms
    #g(guard-clauses...)
#d reader syntax for reading documentation forms
    #dDocumentation-object"
  `(for-macros
     (set-macro-character #\[ #'read-left-bracket)
     (set-macro-character #\] #'read-right-bracket)
     (set-dispatch-macro-character #\# #\; #'read-sharpsign-semicolon)
     (set-dispatch-macro-character #\# #\g #'read-guard-tag)
     (set-dispatch-macro-character #\# #\d #'read-documentation-tag)))

(defmacro uninstall-syntax! ()
  "Uninstalls [], #;, #g, #d reader syntaxes if they was installed using INSTALL-SYNTAX!."
  `(for-macros
     (when (eq (get-macro-character #\[) #'read-left-bracket)
       (set-macro-character #\[ nil))
     (when (eq (get-macro-character #\]) #'read-right-bracket)
       (set-macro-character #\] nil))
     (when (eq (get-dispatch-macro-character #\# #\;) #'read-sharpsign-semicolon)
       (set-dispatch-macro-character #\# #\; nil))
     (when (eq (get-dispatch-macro-character #\# #\g) #'read-guard-tag)
       (set-dispatch-macro-character #\# #\g nil))
     (when (eq (get-dispatch-macro-character #\# #\d) #'read-documentation-tag)
       (set-dispatch-macro-character #\# #\D nil))))

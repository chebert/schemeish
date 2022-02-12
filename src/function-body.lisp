(in-package #:schemeish.internals)

(defun declaration? (form)
  "True if form is (cl:declare ...)"
  (and (consp form)
       (eq (first form) 'cl:declare)))

(export
 (defun parse-declarations (body)
   "Returns (values declarations forms)"
   (splitf body #'declaration?)))

(defun parse-documentation-source (body)
  "Returns (values forms documentation-source).
Body is (documentation-string form forms...) or ([documentation-tag] forms...)
The documentation-source returned is either a DOCUMENTATION-TAG-FORM, a string, or nil.
For more information about documentation-tags, see DOCUMENTATION-TAG, DOCUMENTATION-STRING, and DOCUMENTATION-SOURCE?"
  (cond
    ((null body) (values body nil))
    ((documentation-tag? (first body))
     (values (rest body) (documentation-tag-form (first body))))
    ((and (not (null (rest body)))
	  (stringp (first body)))
     (values (rest body) (first body)))
    (t (values body nil))))

(defun parse-guard-clauses (body)
  "Returns (values body guard-clauses). Assumes body is ([guard-tag] forms...)"
  (if (and (not (null body))
	   (guard-tag? (first body)))
      (values (rest body) (guard-tag-clauses (first body)))
      (values body nil)))

(export
 (defun parse-metadata-from-function-body (function-body)
   "Return (values lexical-body documentation-source-form guard-clauses declarations). 
A function-body is ([documentation-source] [guard-tag] declarations... lexical-body...)
For more information about DOCUMENTATION-SOURCE, see PARSE-DOCUMENTATION-SOURCE.
For more information about guard-tags, see GUARD-TAG.
For more information about lexical-body, see LEXICALLY."
   (multiple-value-bind (body documentation-source) (parse-documentation-source function-body)
     (multiple-value-bind (body guard-clauses) (parse-guard-clauses body)
       (multiple-value-bind (declarations body) (parse-declarations body)
	 (values body documentation-source guard-clauses declarations))))))

(export
 (defun parse-function (scm-parameters function-body)
   "Returns (values ordinary-lambda-list ignorable-parameters body documentation-source guard-clauses declarations)
Converts scm-parameters to an ordinary-lambda-list, and parses the metadata from function-body.
For more information about scm-parameters, SCM-PARAMETERS->ORDINARY-LAMBDA-LIST.
For more information about function-body, see PARSE-METADATA-FROM-FUNCTION-BODY."
   (multiple-value-call #'values
     (scm-parameters->ordinary-lambda-list scm-parameters)
     (parse-metadata-from-function-body function-body))))

(defun declare-ignorable-forms (ignorable-names)
  "Returns a list of forms that declare ignorable-names to be ignorable"
  (when ignorable-names
    (list `(declare (ignorable ,@ignorable-names)))))
(defun enforce-guard-clauses-forms (guard-clauses ordinary-lambda-list)
  "Return a list of forms that enforce guard clauses."
  (when guard-clauses
    (list (enforce-guard-clauses-form guard-clauses (ordinary-lambda-list-parameter-bindings-form ordinary-lambda-list)))))

(defun documentation-string-for-scm-parameters (scm-parameters)
  "A string documenting scm-parameters."
  (format nil "Parameters: ~S" scm-parameters))

(defun documentation-string-for-lambda (scm-parameters guard-clauses documentation-string)
  "Adds documentation for the lambda from the scm-parameters, guard-clauses, and the documentation-string."
  (concatenate 'string
	       documentation-string
	       (format nil "~&~%~%")
	       (documentation-string-for-scm-parameters scm-parameters)
	       (format nil "~&~%~%")
	       (guard-clauses-documentation-string guard-clauses)))

(defun register-lambda-metadata (function scm-parameters guard-clauses documentation-source)
  "Registers documentation, documentation-source, and guard-clauses for function."
  (let ((documentation-string (if documentation-source
				  (documentation-string documentation-source)
				  "")))
    (setf (documentation function t)
	  (documentation-string-for-lambda scm-parameters guard-clauses documentation-string)))
  (when guard-clauses (register-guard-clauses function guard-clauses))
  (when documentation-source (set-object-documentation-source! function documentation-source))
  function)


(defun parsed-function->lambda-form (ordinary-lambda-list ignorable-parameters body guard-clauses declarations)
  "Return a lambda form that:
- declares ignorable-parameters alongside declarations
- enforces guard-clauses
- expands body as if it is a lisp-2 style lexical-body"
  `(cl:lambda ,ordinary-lambda-list
     ,@(declare-ignorable-forms ignorable-parameters)
     ,@declarations
     ,@(enforce-guard-clauses-forms guard-clauses ordinary-lambda-list)
     (lexically ,@body)))

(export
 (defun lambda-form (scm-parameters function-body)
   "Return a form that, when evaluated, creates a lambda defined using the scm-parameters and function-body.
See PARSE-FUNCTION and REGISTER-LAMBDA-METADATA."
   (multiple-value-bind (ordinary-lambda-list ignorable-parameters body documentation-source guard-clauses declarations)
       (parse-function scm-parameters function-body)
     `(register-lambda-metadata
       ,(parsed-function->lambda-form ordinary-lambda-list ignorable-parameters body guard-clauses declarations)
       ',scm-parameters ',guard-clauses ,documentation-source))))


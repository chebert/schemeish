(in-package #:schemeish.internals)


(defun lexical-name->parameter-name (symbol)
  "Adds *ear-muffs* to symbol to make it look like a parameter, interning it."
  (intern (concatenate 'string "*" (symbol-name symbol) "*")))

(defun check-expose-spec (spec name)
  "Error if EXPOSE SPEC is malformed."
  (unless (or (symbolp spec)
	      (and (= (length spec) 2)
		   (symbolp name)))
    (error "Malformed spec ~S: Expected NAME or (GLOBAL-NAME VALUE)" spec)))
(defun expose-function-form (fn-spec)
  "Returns a (setf fdefinition) form for fn-spec."
  (let* ((pair? (consp fn-spec))
	 (name (if pair? (first fn-spec) fn-spec))
	 (value (if pair? (second fn-spec) fn-spec)))
    (check-expose-spec fn-spec name)
    `(progn (setf (fdefinition ',name) ,value) ',name)))

(defun expose-variable-form (var-spec)
  "Returns a defparameter form for var-spec."
  ;; TODO: allow for documentation
  (let* ((pair? (consp var-spec))
	 (name (if pair? (first var-spec) (lexical-name->parameter-name var-spec)))
	 (value (if pair? (second var-spec) var-spec)))
    (check-expose-spec var-spec name)
    `(defparameter ,name ,value)))

(export
 (defmacro expose ((&rest fn-specs) (&rest var-specs))
   "Define var-specs as parameters in the global scope via DEFPARAMETER.
Define fn-specs as functions in the global scope via (SETF FDEFINITION).
Designed to be used within a lexical-body. See LEXICALLY, DEFINE.

Fn-spec is one of:
  fn-name: Expands to (setf (fdefinition 'fn-name) fn-name)
  (global-fn-name value): Expands to (setf (fdefinition 'global-fn-name) value)

Var-spec one of:
  VAR-NAME: *Ear-muffs* are added to symbol to create *VAR-NAME*. Expands to (defparameter *var-name* var-name).
  (*global-special-name* value): Expands to (defparameter *global-special-name* value).

The return value is (PARAMETER-NAMES... GLOBAL-FN-NAMES ...)"
   `(list ,@(mapcar #'expose-variable-form var-specs) ,@(mapcar #'expose-function-form fn-specs))))

(export
 (defmacro expose-functions (&rest fn-specs)
   "Expands to (EXPOSE (fn-specs...) ())"
   `(expose (,@fn-specs) ())))

(export
 (defmacro expose-variables (&rest var-specs)
   "Expands to (EXPOSE () (var-specs...))"
   `(expose () (,@var-specs))))


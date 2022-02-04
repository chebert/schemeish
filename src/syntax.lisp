(in-package #:schemeish.internals)

(defun read-left-bracket (stream char)
  "Implements reader-macro for [. Transforms [forms...] => (funcall forms...)"
  (declare (ignore char))
  (let ((form (read-delimited-list #\] stream t)))
    (if (null form)
	'()
	`(funcall ,@form))))
(defun read-right-bracket (stream char)
  "Implements reader-macro for ]. Throws an error if an ] is unmatched."
  (declare (ignore stream char))
  (error "read: unmatched ]"))

(defun read-commented-form (stream char n)
  "Implements reader-macro for #;FORM. Discards FORM."
  (declare (ignore char n))
  (read stream nil (values) t)
  (values))

(export
 (defmacro install-syntax! ()
   "Installs syntax in the current read table.
[] See READ-LEFT-BRACKET
#; See READ-COMMENTED-FORM
#g See READ-GUARD-TAG
#d See READ-DOCUMENTATION-TAG"
   `(for-macros
      (set-macro-character #\[ #'read-left-bracket)
      (set-macro-character #\] #'read-right-bracket)
      (set-dispatch-macro-character #\# #\; #'read-commented-form)
      (set-dispatch-macro-character #\# #\g #'read-guard-tag)
      (set-dispatch-macro-character #\# #\d #'read-documentation-tag))))

(export
 (defmacro uninstall-syntax! ()
   "Uninstalls [], #;, #g, #d reader syntaxes if they were installed using INSTALL-SYNTAX!."
   `(for-macros
      (when (eq (get-macro-character #\[) #'read-left-bracket)
	(set-macro-character #\[ nil))
      (when (eq (get-macro-character #\]) #'read-right-bracket)
	(set-macro-character #\] nil))
      (when (eq (get-dispatch-macro-character #\# #\;) #'read-commented-form)
	(set-dispatch-macro-character #\# #\; nil))
      (when (eq (get-dispatch-macro-character #\# #\g) #'read-guard-tag)
	(set-dispatch-macro-character #\# #\g nil))
      (when (eq (get-dispatch-macro-character #\# #\d) #'read-documentation-tag)
	(set-dispatch-macro-character #\# #\D nil)))))

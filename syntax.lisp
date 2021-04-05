(in-package #:schemeish.syntax)

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((form (read-delimited-list #\] stream t)))
    (if (null form)
	'()
	`(funcall ,@form))))
(defun read-right-bracket (stream char)
  (declare (ignore stream char))
  (error "read: unmatched ]"))

(defun install-syntax! ()
  "Installs [] reader syntax. 
    [function-name arg1 ...] => (funcall function-name arg1 ...)"
  (set-macro-character #\[ #'read-left-bracket)
  (set-macro-character #\] #'read-right-bracket))

(defun uninstall-syntax! ()
  "Uninstalls [] reader syntax if it was installed using INSTALL-SYNTAX!."
  (when (eq (get-macro-character #\[) #'read-left-bracket)
    (set-macro-character #\[ nil))
  (when (eq (get-macro-character #\]) #'read-right-bracket)
    (set-macro-character #\] nil)))

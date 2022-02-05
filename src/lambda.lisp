(in-package #:schemeish.backend)

(install-syntax!)

(export
 (defmacro lambda (scm-parameters &body function-body)
   "Expands to a form that creates a lambda defined using the scm-parameters and function-body.
See PARSE-FUNCTION and REGISTER-LAMBDA-METADATA."
   (lambda-form scm-parameters function-body)))

(uninstall-syntax!)

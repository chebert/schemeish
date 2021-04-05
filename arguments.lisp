(in-package #:schemeish.arguments)


(defun optional-arg-list->lambda-list (args)
  (cond
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ((consp arg) `(,arg ,@(optional-arg-list->lambda-list (rest args))))
	 (t (error "Expected optional argument ~S to be of the form (name default-value-form) or (name)." arg)))))
    ((null args) ())
    ((symbolp args) (error "Optional argument list cannot have a rest argument."))))


(assert (equal (optional-arg-list->lambda-list '((opt "option") (option)))
	       '((OPT "option") (OPTION))))
(assert (null (ignore-errors (optional-arg-list->lambda-list '(bad-argument)))))
(assert (null (ignore-errors (optional-arg-list->lambda-list 'bad-rest-argument))))

(defun keyword-arg->lambda-list-arg (arg)
  (if (consp arg)
      (cons (intern (symbol-name (first arg))) (rest arg))
      (intern (symbol-name arg))))

(assert (equal (keyword-arg->lambda-list-arg :keyword)
	       'keyword))
(assert (equal (keyword-arg->lambda-list-arg '(:keyword "default"))
	       '(keyword "default")))

(defun keyword-arg-list->lambda-list (args)
  (cond
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ((consp arg)
	  (let ((arg-name (first arg)))
	    (cond
	      ((keywordp arg-name) `(,(keyword-arg->lambda-list-arg arg) ,@(keyword-arg-list->lambda-list (rest args))))
	      (t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))
	 ((keywordp arg)
	  `(,(keyword-arg->lambda-list-arg arg) ,@(keyword-arg-list->lambda-list (rest args))))
	 (t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))
    ((null args) ())
    ((symbolp args) (error "Keyword argument list not compatible with rest argument."))))

(assert (null (keyword-arg-list->lambda-list ())))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(bad-argument)))))
(assert (equal (keyword-arg-list->lambda-list '(:k1))
	       '(k1)))
(assert (equal (keyword-arg-list->lambda-list '((:k1 "default")))
	       '((k1 "default"))))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(((:bad-keyword) "default"))))))

(defun arg-list->lambda-list (args)
  (cond
    ;; (arg . rest-args)
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ;; arg is (optional default) or (:keyword default)
	 ((consp arg)
	  (let ((arg-name (first arg)))
	    (cond
	      ((keywordp arg-name) `(&key ,@(keyword-arg-list->lambda-list args)))
	      ((symbolp arg-name) `(&optional ,@(optional-arg-list->lambda-list args)))
	      (t (error "bad thing to be an arg-name: ~S" arg-name)))))
	 ;; arg is :keyword
	 ((keywordp arg) `(&key ,@(keyword-arg-list->lambda-list args)))
	 ;; arg is positional
	 (t (cons arg (arg-list->lambda-list (rest args)))))))
    ;; args is empty
    ((null args) '())
    ;; args is a rest parameter
    ((symbolp args) `(&rest ,args))
    (t (error "bad thing to be in an arglist: ~S" args))))

;; Null arguments
(assert (null (arg-list->lambda-list ())))
;; position + rest
(assert (equal (arg-list->lambda-list '(arg1 arg2 . args))
	       '(arg1 arg2 &rest args)))
;; positional arguments
(assert (equal (arg-list->lambda-list '(arg1 arg2 arg3))
	       '(arg1 arg2 arg3)))
;; optional arugments
(assert (equal (arg-list->lambda-list '(arg1 (opt-arg "optional") (opt-arg2 "optional2") (null-arg)))
	       '(arg1 &optional (opt-arg "optional") (opt-arg2 "optional2") (null-arg))))

;; Can't mix positional and optional
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 (opt-arg "optional") bad-arg (opt-arg2 "optional2"))))))
;; Can't mix optional and rest (...well you *could*)
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 (opt-arg "optional") . bad-args)))))
;; Bad arg-name for optional args
(assert (null (ignore-errors (arg-list->lambda-list '(arg1 ((oops-arg) "form"))))))

;; Invalid argument lists
(assert (equal (arg-list->lambda-list '(a1 a2 :k1 :k2))
	       '(A1 A2 &KEY K1 K2)))
(assert (equal (arg-list->lambda-list '(:k1))
	       '(&key k1)))
(assert (equal (arg-list->lambda-list '(a1 a2 (:k1 "default") :k2 (:k3 "default2")))
	       '(A1 A2 &KEY (K1 "default") K2 (K3 "default2"))))
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") bad-positional)))))
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") (bad-optional))))))
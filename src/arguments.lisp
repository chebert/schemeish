(in-package #:schemeish.arguments)

(install-syntax!)

(defun ignore-arg? (symbol)
  (let* ((str (string symbol)))
    (and (= (length str) 1)
	 (char= #\_ (aref str 0)))))

(defun ignorable-arg? (symbol)
  (let* ((str (string symbol)))
    (and (not (ignore-arg? symbol))
	 (> (length str) 1)
	 (char= #\_ (aref str 0)))))

(defun rename-ignore-arg (iterate arg-name-fn rename-arg-fn args reversed-lambda-list ignorable-args continue)
  "Check to see if the next arg in args is an ignore-arg and rename it if it is. If the argument is ignored
or ignorable, adds it to the ignorable-args list.
Calls (iterate args reversed-lambda-list ignorable-args continue)."
  (let ((arg (first args)))
    (cond
      ((ignore-arg? [arg-name-fn arg])
       (let ((arg-name (unique-symbol 'ignore)))
	 [iterate
	   (rest args)
	   (cons [rename-arg-fn arg arg-name] reversed-lambda-list)
	   (cons arg-name ignorable-args)
	   continue]))
      ((ignorable-arg? [arg-name-fn arg])
       [iterate
	 (rest args)
	 (cons arg reversed-lambda-list)
	 (cons [arg-name-fn arg] ignorable-args)
	 continue])
      (t
       [iterate
	 (rest args)
	 (cons arg reversed-lambda-list)
	 ignorable-args
	 continue]))))

(defun optional-arg? (arg)
  (consp arg))
(defun optional-arg-name (arg)
  (first arg))

(defun optional-arg-list->lambda-list-iteration (args reversed-lambda-list ignorable-args continue)
  (cond
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ((optional-arg? arg)
	  (rename-ignore-arg
	   #'optional-arg-list->lambda-list-iteration
	   #'optional-arg-name
	   (lambda (arg arg-name) (cons arg-name (rest arg)))
	   args
	   reversed-lambda-list
	   ignorable-args
	   continue))
	 (t (error "Expected optional argument ~S to be of the form (name default-value-form) or (name)." arg)))))
    ;; BASE Case: no more arguments
    ((null args) [continue reversed-lambda-list ignorable-args])
    ((symbolp args) (error "Optional argument list cannot have a rest argument."))))

(defun optional-arg-list->lambda-list (args)
  (optional-arg-list->lambda-list-iteration args () () (lambda (reversed-lambda-list ignorable-args)
							 (values (nreverse reversed-lambda-list) ignorable-args))))

(assert (equal (optional-arg-list->lambda-list '((opt "option") (option)))
	       '((OPT "option") (OPTION))))
(assert (null (ignore-errors (optional-arg-list->lambda-list '(bad-argument)))))
(assert (null (ignore-errors (optional-arg-list->lambda-list 'bad-rest-argument))))

(assert (equal (with-readable-symbols
		 (optional-arg-list->lambda-list '((_ "option") (_))))
	       '((IGNORE "option") (IGNORE))))
(assert (equal (with-readable-symbols
		 (multiple-value-list (optional-arg-list->lambda-list '((_a "option") (_b)))))
	       '(((_a "option") (_b))
		 (_b _a))))

(defun keyword-arg->lambda-list-arg (arg)
  (if (consp arg)
      (cons (intern (symbol-name (first arg))) (rest arg))
      (intern (symbol-name arg))))

(assert (equal (keyword-arg->lambda-list-arg :keyword)
	       'keyword))
(assert (equal (keyword-arg->lambda-list-arg '(:keyword "default"))
	       '(keyword "default")))

(defun keyword-arg-list->lambda-list-iteration (args reversed-lambda-list continue)
  (cond
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ((consp arg)
	  (let ((arg-name (first arg)))
	    (cond
	      ((keywordp arg-name) (keyword-arg-list->lambda-list-iteration
				    (rest args)
				    (cons (keyword-arg->lambda-list-arg arg) reversed-lambda-list)
				    continue))
	      (t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))
	 ((keywordp arg)
	  (keyword-arg-list->lambda-list-iteration
	   (rest args)
	   (cons (keyword-arg->lambda-list-arg arg) reversed-lambda-list)
	   continue))
	 (t (error "Expected keyword argument ~S to be of the form :key or (:key) or (:key default-value-form)" arg)))))

    ;; BASE case: no more arguments.
    ((null args) [continue reversed-lambda-list])
    ((symbolp args) (error "Keyword argument list not compatible with rest argument."))))
(defun keyword-arg-list->lambda-list (args)
  (keyword-arg-list->lambda-list-iteration args () #'nreverse))

(assert (null (keyword-arg-list->lambda-list ())))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(bad-argument)))))
(assert (equal (keyword-arg-list->lambda-list '(:k1))
	       '(k1)))
(assert (equal (keyword-arg-list->lambda-list '((:k1 "default")))
	       '((k1 "default"))))
(assert (null (ignore-errors (keyword-arg-list->lambda-list '(((:bad-keyword) "default"))))))

(defun arg-rest->lambda-list (arg-name reversed-lambda-list ignorable-args continue)
  [continue (list* arg-name '&rest reversed-lambda-list)
	    ignorable-args])

(defun add-rest-arg (arg reversed-lambda-list)
  (list* arg '&rest reversed-lambda-list))

(defun arg-list->lambda-list-iteration (args reversed-lambda-list ignorable-args continue)
  (cond
    ;; (arg . rest-args)
    ((consp args)
     (let ((arg (first args)))
       (cond
	 ;; arg is (optional default) or (:keyword default)
	 ((consp arg)
	  (let ((arg-name (first arg)))
	    (cond
	      ((keywordp arg-name)
	       ;; The rest of the arg-list is a keyword-arg-list
	       (keyword-arg-list->lambda-list-iteration
		args
		(cons '&key reversed-lambda-list)
		(lambda (reversed-lambda-list)
		  ;; Continue with the final lambda-list and the ignorable-args
		  [continue reversed-lambda-list ignorable-args])))
	      ((symbolp arg-name)
	       ;; The rest of arg-list is an optional-arg-list
	       (optional-arg-list->lambda-list-iteration
		args
		(cons '&optional reversed-lambda-list)
		ignorable-args
		continue))
	      (t (error "bad thing to be an arg-name: ~S" arg-name)))))
	 ;; arg is :keyword
	 ((keywordp arg)
	  (keyword-arg-list->lambda-list-iteration
	   args
	   (cons '&key reversed-lambda-list)
	   (lambda (reversed-lambda-list)
	     [continue reversed-lambda-list ignorable-args])))

	 ;; arg is positional
	 (t
	  (rename-ignore-arg
	   #'arg-list->lambda-list-iteration
	   #'identity
	   (lambda (arg arg-name) (declare (ignore arg)) arg-name)
	   args
	   reversed-lambda-list
	   ignorable-args
	   continue)))))

    ;; BASE Case
    ;; args is empty
    ((null args)
     [continue reversed-lambda-list ignorable-args])
    ;; args is a rest parameter
    ((symbolp args)
     (cond
       ((ignore-arg? args)
	(let ((arg (unique-symbol 'ignore)))
	  [continue (add-rest-arg arg reversed-lambda-list) (cons arg ignorable-args)]))
       (t
	[continue (add-rest-arg args reversed-lambda-list)
		  (cond
		    ((ignorable-arg? args) (cons args ignorable-args))
		    (t ignorable-args))])))
    (t (error "bad thing to be in an arglist: ~S" args))))

(defun arg-list->lambda-list (args)
  (arg-list->lambda-list-iteration args () ()
				   (lambda (reversed-lambda-list ignorable-args)
				     (values (nreverse reversed-lambda-list) ignorable-args))))

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

(assert (equal (arg-list->lambda-list '(a1 a2 :k1 :k2))
	       '(A1 A2 &KEY K1 K2)))
(assert (equal (arg-list->lambda-list '(:k1))
	       '(&key k1)))
(assert (equal (arg-list->lambda-list '(a1 a2 (:k1 "default") :k2 (:k3 "default2")))
	       '(A1 A2 &KEY (K1 "default") K2 (K3 "default2"))))

;; Invalid argument lists
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") bad-positional)))))
(assert (null (ignore-errors (arg-list->lambda-list '(a1 a2 (:k1 "default") (bad-optional))))))


;; Ignore args.
(assert (equal (with-readable-symbols
		 (multiple-value-list (arg-list->lambda-list '(arg1 _ _ arg2 . _))))
	       '((ARG1 IGNORE IGNORE ARG2 &REST IGNORE)
		 (ignore ignore ignore))))
;; Ignorable args.
(assert (equal (with-readable-symbols
		 (multiple-value-list (arg-list->lambda-list '(arg1 _ignorable1 _ arg2 . _rest))))
	       '((ARG1 _IGNORable1 IGNORE ARG2 &REST _rest)
		 (_rest ignore _ignorable1))))

(uninstall-syntax!)

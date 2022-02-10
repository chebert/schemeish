(in-package #:schemeish.internals)

(defun scm-ignore-parameter? (symbol)
  "True if SYMBOL-NAME is \"_\", and should therefore be ignored."
  (let* ((str (string symbol)))
    (and (= (length str) 1)
	 (char= #\_ (aref str 0)))))

(defun scm-ignorable-parameter? (symbol)
  "True if SYMBOL-NAME starts with \"_\", and should therefore be ignorable."
  (let* ((str (string symbol)))
    (and (not (scm-ignore-parameter? symbol))
	 (> (length str) 1)
	 (char= #\_ (aref str 0)))))

(defun rename-ignore-parameter (parameter-name-fn rename-parameter-fn parameters reversed-ordinary-lambda-list ignorable-parameters)
  "Check to see if the next parameter in parameters is an ignore-parameter and rename it if it is. If the parameter is ignored
or ignorable, adds it to the ignorable-parameters list.
Returns (values rest-parameters reversed-ordinary-lambda-list ignorable-parameters)."
  (let* ((parameter (first parameters))
	 (parameter-name (funcall parameter-name-fn parameter)))
    (cond
      ((scm-ignore-parameter? parameter-name)
       (let ((parameter-name (unique-symbol 'ignore)))
	 (values
	  (rest parameters)
	  (cons (funcall rename-parameter-fn parameter parameter-name) reversed-ordinary-lambda-list)
	  (cons parameter-name ignorable-parameters))))
      ((scm-ignorable-parameter? parameter-name)
       (values
	(rest parameters)
	(cons parameter reversed-ordinary-lambda-list)
	(cons (funcall parameter-name-fn parameter) ignorable-parameters)))
      (t
       (values
	(rest parameters)
	(cons parameter reversed-ordinary-lambda-list)
	ignorable-parameters)))))

(defun scm-optional-parameter? (parameter)
  (consp parameter))
(defun scm-optional-parameter-name (parameter)
  (first parameter))

(defun scm-optional-parameters->ordinary-lambda-list-iteration (parameters reversed-ordinary-lambda-list ignorable-parameters)
  "Converts the list of optional parameters to an ordinary-lambda-list. Returns (values reversed-ordinary-lambda-list ignorable-parameters)"
  (cond
    ((consp parameters)
     (let ((parameter (first parameters)))
       (cond
	 ((scm-optional-parameter? parameter)
	  (multiple-value-call #'scm-optional-parameters->ordinary-lambda-list-iteration
	    (rename-ignore-parameter
	     #'scm-optional-parameter-name
	     (lambda (parameter parameter-name) (cons parameter-name (rest parameter)))
	     parameters
	     reversed-ordinary-lambda-list
	     ignorable-parameters)))
	 (t (error "Expected optional parameter ~S to be of the form (name default-value-form) or (name)." parameter)))))
    ;; BASE Case: no more parameters
    ((null parameters) (values reversed-ordinary-lambda-list ignorable-parameters))
    ((symbolp parameters) (error "Optional parameter list cannot have a rest parameter."))))

(defun scm-optional-parameters->ordinary-lambda-list (parameters)
  "Returns (values ordinary-lambda-list ignorable-parameters)"
  (multiple-value-bind (reversed-ordinary-lambda-list ignorable-parameters)
      (scm-optional-parameters->ordinary-lambda-list-iteration parameters () ())
    (values (nreverse reversed-ordinary-lambda-list) ignorable-parameters)))

(assert (equal (scm-optional-parameters->ordinary-lambda-list '((opt "option") (option)))
	       '((OPT "option") (OPTION))))
(assert (null (ignore-errors (scm-optional-parameters->ordinary-lambda-list '(bad-parameter)))))
(assert (null (ignore-errors (scm-optional-parameters->ordinary-lambda-list 'bad-rest-parameter))))

(assert (equal (with-readable-symbols
		 (scm-optional-parameters->ordinary-lambda-list '((_ "option") (_))))
	       '((IGNORE "option") (IGNORE))))
(assert (equal (with-readable-symbols
		 (multiple-value-list (scm-optional-parameters->ordinary-lambda-list '((_a "option") (_b)))))
	       '(((_a "option") (_b))
		 (_b _a))))

(defun scm-keyword-parameter->ordinary-lambda-list-parameter (parameter)
  "Converts :KEYWORD to KEYWORD.
Converts (:KEYWORD ...) to (KEYWORD ...)"
  (if (consp parameter)
      (cons (intern (symbol-name (first parameter))) (rest parameter))
      (intern (symbol-name parameter))))

(assert (equal (scm-keyword-parameter->ordinary-lambda-list-parameter :keyword)
	       'keyword))
(assert (equal (scm-keyword-parameter->ordinary-lambda-list-parameter '(:keyword "default"))
	       '(keyword "default")))

(defun scm-keyword-parameters->ordinary-lambda-list-iteration (parameters reversed-ordinary-lambda-list)
  "Converts a list of schemeish keyword parameters to ordinary-lambda-list keyword parameters.
Returns a reversed-ordinary-lambda-list."
  (cond
    ((consp parameters)
     (let ((parameter (first parameters)))
       (cond
	 ((consp parameter)
	  (let ((parameter-name (first parameter)))
	    (cond
	      ((keywordp parameter-name)
	       (scm-keyword-parameters->ordinary-lambda-list-iteration
		(rest parameters)
		(cons (scm-keyword-parameter->ordinary-lambda-list-parameter parameter) reversed-ordinary-lambda-list)))
	      (t (error "Expected keyword parameter ~S to be of the form :key or (:key) or (:key default-value-form)" parameter)))))
	 ((keywordp parameter)
	  (scm-keyword-parameters->ordinary-lambda-list-iteration
	   (rest parameters)
	   (cons (scm-keyword-parameter->ordinary-lambda-list-parameter parameter) reversed-ordinary-lambda-list)))
	 (t (error "Expected keyword parameter ~S to be of the form :key or (:key) or (:key default-value-form)" parameter)))))

    ;; BASE case: no more parameters.
    ((null parameters) reversed-ordinary-lambda-list)
    ((symbolp parameters) (error "Keyword parameter list not compatible with rest parameter."))))

(defun scm-keyword-parameters->ordinary-lambda-list (parameters)
  (nreverse (scm-keyword-parameters->ordinary-lambda-list-iteration parameters ())))

(assert (null (scm-keyword-parameters->ordinary-lambda-list ())))
(assert (null (ignore-errors (scm-keyword-parameters->ordinary-lambda-list '(bad-parameter)))))
(assert (equal (scm-keyword-parameters->ordinary-lambda-list '(:k1))
	       '(k1)))
(assert (equal (scm-keyword-parameters->ordinary-lambda-list '((:k1 "default")))
	       '((k1 "default"))))
(assert (null (ignore-errors (scm-keyword-parameters->ordinary-lambda-list '(((:bad-keyword) "default"))))))

(defun add-rest-parameter (parameter reversed-ordinary-lambda-list)
  (list* parameter '&rest reversed-ordinary-lambda-list))

(defun scm-parameters->ordinary-lambda-list-iteration (parameters reversed-ordinary-lambda-list ignorable-parameters)
  (cond
    ;; (parameter . rest-parameters)
    ((consp parameters)
     (let ((parameter (first parameters)))
       (cond
	 ;; parameter is (optional default) or (:keyword default)
	 ((consp parameter)
	  (let ((parameter-name (first parameter)))
	    (cond
	      ((keywordp parameter-name)
	       ;; The rest of the scm-parameters is a scm-keyword-parameters
	       (values (scm-keyword-parameters->ordinary-lambda-list-iteration
			parameters
			(cons '&key reversed-ordinary-lambda-list))
		       ignorable-parameters))
	      ((symbolp parameter-name)
	       ;; The rest of scm-parameters is an scm-optional-parameters
	       (scm-optional-parameters->ordinary-lambda-list-iteration
		parameters
		(cons '&optional reversed-ordinary-lambda-list)
		ignorable-parameters))
	      (t (error "bad thing to be an parameter-name: ~S" parameter-name)))))
	 ;; parameter is :keyword
	 ((keywordp parameter)
	  (values (scm-keyword-parameters->ordinary-lambda-list-iteration
		   parameters
		   (cons '&key reversed-ordinary-lambda-list))
		  ignorable-parameters))

	 ;; parameter is positional
	 (t
	  (multiple-value-call #'scm-parameters->ordinary-lambda-list-iteration
	    (rename-ignore-parameter
	     #'identity
	     (lambda (parameter parameter-name) (declare (ignore parameter)) parameter-name)
	     parameters
	     reversed-ordinary-lambda-list
	     ignorable-parameters))))))

    ;; BASE Case: parameters is empty
    ((null parameters)
     (values reversed-ordinary-lambda-list ignorable-parameters))
    ;; Base Case: parameters is a rest parameter
    ((symbolp parameters)
     (cond
       ((scm-ignore-parameter? parameters)
	;; Ignored rest parameter
	(let ((parameter (unique-symbol 'ignore)))
	  (values (add-rest-parameter parameter reversed-ordinary-lambda-list) (cons parameter ignorable-parameters))))
       (t	
	(values (add-rest-parameter parameters reversed-ordinary-lambda-list)
		(cond
		  ;; Ignorable rest parameter
		  ((scm-ignorable-parameter? parameters) (cons parameters ignorable-parameters))
		  ;; Normal rest parameter
		  (t ignorable-parameters))))))
    (t (error "bad thing to be in an scm-parameters: ~S" parameters))))

(export
 (defun scm-parameters->ordinary-lambda-list (parameters)
   (multiple-value-bind (reversed-ordinary-lambda-list ignorable-parameters)
       (scm-parameters->ordinary-lambda-list-iteration parameters () ())
     (values (nreverse reversed-ordinary-lambda-list) ignorable-parameters))))

;; Null parameters
(assert (null (scm-parameters->ordinary-lambda-list ())))
;; position + rest
(assert (equal (scm-parameters->ordinary-lambda-list '(parameter1 parameter2 . parameters))
	       '(parameter1 parameter2 &rest parameters)))
;; positional parameters
(assert (equal (scm-parameters->ordinary-lambda-list '(parameter1 parameter2 parameter3))
	       '(parameter1 parameter2 parameter3)))
;; optional arugments
(assert (equal (scm-parameters->ordinary-lambda-list '(parameter1 (opt-parameter "optional") (opt-parameter2 "optional2") (null-parameter)))
	       '(parameter1 &optional (opt-parameter "optional") (opt-parameter2 "optional2") (null-parameter))))

;; Can't mix positional and optional
(assert (null (ignore-errors (scm-parameters->ordinary-lambda-list '(parameter1 (opt-parameter "optional") bad-parameter (opt-parameter2 "optional2"))))))
;; Can't mix optional and rest (...well you *could*)
(assert (null (ignore-errors (scm-parameters->ordinary-lambda-list '(parameter1 (opt-parameter "optional") . bad-parameters)))))
;; Bad parameter-name for optional parameters
(assert (null (ignore-errors (scm-parameters->ordinary-lambda-list '(parameter1 ((oops-parameter) "form"))))))

(assert (equal (scm-parameters->ordinary-lambda-list '(a1 a2 :k1 :k2))
	       '(A1 A2 &KEY K1 K2)))
(assert (equal (scm-parameters->ordinary-lambda-list '(:k1))
	       '(&key k1)))
(assert (equal (scm-parameters->ordinary-lambda-list '(a1 a2 (:k1 "default") :k2 (:k3 "default2")))
	       '(A1 A2 &KEY (K1 "default") K2 (K3 "default2"))))

;; Invalid parameter lists
(assert (null (ignore-errors (scm-parameters->ordinary-lambda-list '(a1 a2 (:k1 "default") bad-positional)))))
(assert (null (ignore-errors (scm-parameters->ordinary-lambda-list '(a1 a2 (:k1 "default") (bad-optional))))))


;; Ignore parameters.
(assert (equal (with-readable-symbols
		 (multiple-value-list (scm-parameters->ordinary-lambda-list '(parameter1 _ _ parameter2 . _))))
	       '((PARAMETER1 IGNORE IGNORE PARAMETER2 &REST IGNORE)
		 (ignore ignore ignore))))
;; Ignorable parameters.
(assert (equal (with-readable-symbols
		 (multiple-value-list (scm-parameters->ordinary-lambda-list '(parameter1 _ignorable1 _ parameter2 . _rest))))
	       '((PARAMETER1 _IGNORable1 IGNORE PARAMETER2 &REST _rest)
		 (_rest ignore _ignorable1))))


(defparameter *ordinary-lambda-list-keywords* '(cl:&optional cl:&key cl:&rest cl:&aux cl:&allow-other-keys))
(defun map-lambda-list (proc lambda-list keywords)
  "Map over the lambda-list. Keywords are a list of accepted keywords.
Proc is called with either (funcall proc :keyword keyword) or (funcall proc group parameter), where
group is one of (:optional :key :aux :rest :positional :whole)."
  (labels ((map-keyword (keyword parameters result)
	     (let ((result (cons (funcall proc :keyword keyword) result)))
	       (ecase keyword
		 (cl:&optional (map-group :optional parameters result))
		 (cl:&key (map-group :key parameters result))
		 (cl:&allow-other-keys (map-group :key parameters result))
		 ((cl:&rest cl:&body) (map-group :rest parameters result))
		 (cl:&aux (map-group :aux parameters result)))))
	   (map-group (group-name parameters result)
	     (cond
	       ((null parameters) result)
	       ((symbolp parameters)
		;; End of a dotted list: Rest parameter
		(list* (funcall proc :rest parameters) (funcall proc :keyword 'cl:&rest) result))
	       (t (let ((parameter (first parameters))
			(rest-parameters (rest parameters)))
		    (cond
		      ((member parameter keywords) (map-keyword parameter rest-parameters result))
		      ((and (eq group-name :positional) (consp parameter))
		       ;; Positional parameter is a nested lambda-list.
		       (map-group group-name rest-parameters (append (nreverse (map-lambda-list proc parameter keywords)) result)))
		      (t (map-group group-name rest-parameters (cons (funcall proc group-name parameter) result)))))))))
    (nreverse
     (if (and (member 'cl:&whole keywords) (eq (first lambda-list) 'cl:&whole))
	 (map-group :positional (rest (rest lambda-list))
		    (let ((whole-keyword (funcall proc :keyword 'cl:&whole))
			  (whole (funcall proc :whole (second lambda-list))))
		      (list whole whole-keyword)))
	 (map-group :positional lambda-list ())))))

(export-definition
  (defun map-ordinary-lambda-list (proc ordinary-lambda-list)
    "Map over the ordinary-lambda-list. Accepted keywords are *ORDINARY-LAMBDA-LIST-KEYWORDS*.
Proc is called with either [proc :keyword keyword] or [proc group parameter], where
group is one of (:optional :key :aux :rest :positional)."
    (map-lambda-list proc ordinary-lambda-list *ordinary-lambda-list-keywords*)))

(defparameter *destructuring-lambda-list-keywords* '(cl:&whole cl:&optional cl:&rest cl:&body cl:&key cl:&allow-other-keys cl:&aux))
(export-definition
  (defun map-destructuring-lambda-list (proc destructuring-lambda-list)
    "Map over the destructuring-lambda-list. Accepted keywords are *DESTRUCTURING-LAMBDA-LIST-KEYWORDS*.
Proc is called with either (funcall procc :keyword keyword) or (funcall proc group parameter), where
group is one of (:optional :key :aux :rest :positional :whole)."
    (map-lambda-list proc destructuring-lambda-list *destructuring-lambda-list-keywords*)))


(assert (let ((parameters '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &key (key value) &aux (aux value) &rest rest)))
	  (equal (map-ordinary-lambda-list (cl:lambda (&rest args) (second args)) parameters)
		 parameters)))

(assert (let ((parameters '(&whole whole a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &body body
			    &key (key value) &aux (aux value))))
	  (equal (map-destructuring-lambda-list (cl:lambda (&rest args) (second args)) parameters)
		 parameters)))

(defun lambda-list-parameter-names (lambda-list keywords)
  "Returns an ordered list of names bound by the given lambda-list"
  (apply #'append (map-lambda-list
		   (cl:lambda (group parameter)
		     (ecase group
		       (:keyword ())
		       ((:positional :rest :whole) (list parameter))
		       ((:optional :key :aux)
			(cond
			  ((consp parameter)
			   (cond
			     ((= (length parameter) 3) (list (first parameter) (third parameter)))
			     (t (list (first parameter)))))
			  (t (list parameter))))))
		   lambda-list
		   keywords)))

(defun ordinary-lambda-list-parameter-names (ordinary-lambda-list)
  "Returns an ordered list of names bound by the given ordinary-lambda-list."
  (lambda-list-parameter-names ordinary-lambda-list *ordinary-lambda-list-keywords*))

(assert (equal (let ((oll '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
			    &key (key value) &aux (aux value) &rest rest)))
		 (ordinary-lambda-list-parameter-names oll))
	       '(A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? KEY AUX REST)))

(defun destructuring-lambda-list-parameter-names (destructuring-lambda-list)
  "Returns an ordered list of bound names in the given destructuring-lambda-list."
  (lambda-list-parameter-names destructuring-lambda-list *destructuring-lambda-list-keywords*))

(assert (equal (let ((parameters '(&whole whole a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
				   &body body
				   &key (key value) &aux (aux value))))
		 (destructuring-lambda-list-parameter-names parameters))
	       '(WHOLE A B C OPT1 OPT2 OPT3 OPT4 OPT4-PROVIDED? BODY KEY AUX)))

(defun ordinary-lambda-list-parameter-bindings-form (ordinary-lambda-list)
  "Returns a form (list (list 'name name)...) for all parameter names bound by the ordinary-lambda-list."
  `(list ,@(mapcar (cl:lambda (name) `(list ',name ,name)) (ordinary-lambda-list-parameter-names ordinary-lambda-list))))

(assert (equal (ordinary-lambda-list-parameter-bindings-form '(a b c &optional opt1 (opt2) (opt3 t) (opt4 t opt4-provided?)
							       &key (key value) &aux (aux value) &rest rest))
	       '(LIST (LIST 'A A) (LIST 'B B) (LIST 'C C) (LIST 'OPT1 OPT1) (LIST 'OPT2 OPT2)
		 (LIST 'OPT3 OPT3) (LIST 'OPT4 OPT4) (LIST 'OPT4-PROVIDED? OPT4-PROVIDED?)
		 (LIST 'KEY KEY) (LIST 'AUX AUX) (LIST 'REST REST))))

(in-package #:schemeish.named-let)

;; Named let.
(defmacro let (&whole whole &rest rest)
  "Extends CL:let by allowing for named let recursive forms. E.g.
  (let rec ((n 10)
            (result '()))
    (if (= n 0)
        result
        (rec (1- n) (cons n result))))
  ;; => '(1 2 3 4 5 6 7 8 9 10)"
  (declare (ignore rest))
  (if (symbolp (second whole))
      (destructuring-bind (name bindings &rest body) (rest whole)
	`(labels ((,name ,(mapcar #'car bindings)
		    ,@body))
	   (,name ,@(mapcar #'second bindings))))
      `(cl:let ,@(rest whole))))

(in-package #:schemeish.lambda)


(defmacro lambda (arg-list &body body)
  "A lambda with scheme style argument lists. Some examples:
  (lambda (arg1 arg2 arg3) (list arg1 arg2 arg3)) ; Arity: 3
  (lambda (arg1 . args) (list* arg1 args)) ; Arity: at least 1
  (lambda args args) ; Arity: at least 0

  (lambda args args) ; all arguments stored in args
  (lambda (p1 p2 . args) (list* p1 p2 args)) ; Rest arg
  (lambda (p1 p2 (o1 \"default\") (o2 \"default\")) (list p1 p2 o1 o2)) ; Optional args
  (lambda (p1 p2 :k1 (:k2 \"default\")) (list p1 p2 k1 k2)) ; keyword args
  Rest/Optional/Keyword arguments are not compatible with each other.
  See DEFINE for more information on argument lists.
"
  `(cl:lambda ,(arg-list->lambda-list arg-list) ,@(expand-function-body body)))

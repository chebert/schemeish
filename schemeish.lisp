;;;; schemeish.lisp

(in-package #:schemeish)

(for-macros (install-syntax!))

(for-macros
  ;; Re-export functions which have parameters bound to them as just functions.
  (setf (symbol-function '+) (symbol-function 'cl:+)
	(symbol-function '-) (symbol-function 'cl:-)
	(symbol-function '*) (symbol-function 'cl:*)
	(symbol-function '/) (symbol-function 'cl:/)))

(define (serialize datum)
  "Recursively serializes structs using struct->list, bundles using bundle-list, and lists
into a list form that can be EVAL'd.
Bundles will no longer share identity after EVAL."
  (cond
    ((struct? datum) (let ((list (struct->list datum)))
		       (cons (first list) (map 'serialize (rest list)))))
    ((bundle? datum) (let ((list (bundle-list datum)))
		       (cons (first list) (map 'serialize (rest list)))))
    ((null? datum) ())
    ((pair? datum) `(cons ,(serialize (car datum)) ,(serialize (cdr datum))))
    (t `',datum)))

(assert (equal? (eval (serialize (list 1 2 3 4)))
		(list 1 2 3 4)))

(define-struct tpoint (x y) :transparent)
(let* ((point (make-tpoint 3 4)))
  (assert (equal? (eval (serialize point)) point)))
(let* ((qpoint (make-tpoint (make-queue) (make-queue (list 1 2 3)))))
  (assert (equal? (serialize (eval (serialize qpoint)))
		  (serialize qpoint))))

(for-macros (uninstall-syntax!))

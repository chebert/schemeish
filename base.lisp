(in-package #:schemeish.base)

(for-macros (install-syntax!))

(define (make-keyword symbol)
  (intern (symbol-name symbol) :keyword))

(define (symbol->string symbol) (symbol-name symbol))

(define (symbol? datum) (symbolp datum))


(define (procedure? datum) (functionp datum))

(define (eq? obj1 obj2) (eq obj1 obj2))

(defgeneric equal? (object1 object2)
  (:documentation "Provides a generic interface to EQUAL."))
(defmethod equal? (object1 object2) (equal object1 object2))

(define (map function list &rest more-lists)
  (apply #'mapcar function list more-lists))

(define (append* lists)
  (apply #'append lists))

(define (empty? datum) (null datum))

(define (for-each proc . lists)
  "Apply proc to each element of lists. Arity of proc should match the number of lists."
  (let rec ((lists lists))
    (unless (member nil lists)
      (apply proc (map 'first lists))
      (rec (map 'rest lists)))))

(assert (equal?
	 (with-output-to-string (s)
	   (for-each (lambda (x y) (format s "~S" (list x y)))
		     '(a b c)
		     '(1 2 3)))
	 "(A 1)(B 2)(C 3)"))

(for-macros (uninstall-syntax!))

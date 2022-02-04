(in-package #:schemeish.backend)

(install-syntax!)

;;; Logic
(export
 (define eq? #'eq))

(defgeneric equal? (object1 object2)
  (:documentation "Provides a generic interface to EQUAL."))
(defmethod equal? (object1 object2) (equal object1 object2))
(export 'equal?)

(export
 (defmacro nand (&rest expressions)
   "The same as (not (and expressions...))"
   `(not (and ,@expressions))))

(export
 (defmacro nor (&rest expressions)
   "The same as (not (or expressions...))"
   `(not (or ,@expressions))))

(export
 (define (xor b1 b2)
   "Logical xor of booleans b1 and b2."
   (or (and b1 (not b2))
       (and b2 (not b1)))))

(uninstall-syntax!)

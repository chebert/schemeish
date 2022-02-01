(in-package #:schemeish.internals)

(install-syntax!)

(export
 (defun takef (list predicate)
   "Takes initial elements of list that satisfy pred."
   (labels ((rec (list result)
	      (if (or (null list) (not [predicate (first list)]))
		  (values (nreverse result) list)
		  (rec
		   (rest list)
		   (cons (first list) result)))))
     (rec list ()))))

(assert (equal (takef '(2 4 5 8) 'evenp)
	       '(2 4)))
(export
 (defun dropf (list predicate)
   "Drops initial elements of list that don't satisfy pred."
   (labels ((rec (list)
	      (if (or (null list) (not [predicate (first list)]))
		  list
		  (rec (rest list)))))
     (rec list))))

(assert (equal (dropf '(2 4 5 8) 'evenp)
	       '(5 8)))


(export
 (defun splitf (list predicate)
   "Returns the (values (takef list predicate) (dropf list predicate))"
   (takef list predicate)))

(uninstall-syntax!)

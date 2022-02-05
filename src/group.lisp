(in-package #:schemeish.backend)

(install-syntax!)

;;; List:Group

(export
 (define (group key-fn list)
   "Groups elements of list that have the same key-fn into an alist."
   (define (rec list result)
     (cond
       ((null? list)
	(alist-map result
		   (lambda (key list)
		     (cons key (nreverse list)))))
       (t (let ((item (first list)))
	    (let ((key [key-fn item]))
	      (rec (rest list)
		   (alist-update result key (lambda (vals) (cons item vals)) ())))))))
   (rec list ())))

(assert (equal? (group 'car '((0 a b c)
			      (1 a b c)
			      (0 d e f)
			      (2 a b c)
			      (1 d e f)))
		'((1 (1 A B C) (1 D E F)) (2 (2 A B C)) (0 (0 A B C) (0 D E F)))))

(uninstall-syntax!)

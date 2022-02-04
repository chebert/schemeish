(in-package #:schemeish.backend)

(install-syntax!)

;;; Alist

(export
 (define (alist-ref alist key (failure-result))
   "Rerturns the value associated with key in alist, else the failure-result."
   (let ((pair (assoc key alist :test #'equal?)))
     (if (pair? pair)
	 (cdr pair)
	 failure-result))))
(export
 (define (alist-remove alist key)
   "Returns an alist with key removed."
   (remove key alist :test #'equal? :key #'car)))
(export
 (define (alist-set alist key value)
   "Returns an alist with key set to value."
   (acons key value (alist-remove alist key))))


(export
 (define (alist-union alist new-alist)
   (foldl (lambda (pair alist)
	    (alist-set alist (car pair) (cdr pair)))
	  alist new-alist)))

(export
 (define (alist-update alist key updater (failure-result))
   "Applies updater to the value associated with key and updates the result in alist.
Applies updater to failure-result if key is not present."
   (alist-set alist key [updater (alist-ref alist key failure-result)])))

(export
 (define (alist-map alist proc)
   "Alist with proc applied to all values of alist."
   (map (lambda (binding) [proc (car binding) (cdr binding)]) alist)))

(export
 (define (alist-for-each alist proc)
   "Proc applied to all values of alist."
   (for-each (lambda (binding) [proc (car binding) (cdr binding)]) alist)))

(export
 (define (alist-keys alist)
   "A list of all keys in alist."
   (alist-map alist (lambda (key value) (declare (ignore value)) key))))
(export
 (define (alist-values alist)
   "A list of all of the values in alist."
   (alist-map alist (lambda (key value) (declare (ignore key)) value))))

(export
 (define (alist-has-key? alist key)
   "T if the key is present in alist"
   (let ((no-key (gensym)))
     (not (eq? no-key (alist-ref alist key no-key))))))

(export
 (define (alist-set* alist . keys-and-values)
   "Update all of the values in alist with pairs of key value ..."
   (let rec ((keys-and-values keys-and-values)
	     (alist alist))
     (cond
       ((empty? keys-and-values) alist)
       ((empty? (rest keys-and-values)) (error "badly formed arguments."))
       (t (let ((key (first keys-and-values))
		(value (second keys-and-values)))
	    (rec
	     (drop keys-and-values 2)
	     (alist-set alist key value))))))))

(export
 (define (alist . keys-and-values)
   "Constructs an alist from pairs of key value ..."
   (nreverse (apply #'alist-set* () keys-and-values))))

(uninstall-syntax!)
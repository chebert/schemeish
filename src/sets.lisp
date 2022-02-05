(in-package #:schemeish.backend)

(install-syntax!)

(export
 (define (set-member? set value)
   "True if value is a member of set."
   (member value set :test #'equal?)))
(export
 (define (set-add set value)
   "Adds value to set."
   (if (set-member? set value)
       set
       (cons value set))))
(export
 (define (set-remove set value)
   "Removes value from set."
   (remove set value :test #'equal?)))
(export
 (define (set-empty? set)
   "True if set is empty."
   (empty? set)))
(export
 (define (set-count set)
   "Number of elements in set."
   (length set)))
(export
 (define (set->stream set)
   "Returns the elements of set as a stream."
   (list->stream set)))
(export
 (define (set-union . sets)
   "Returns the union of all sets."
   (foldl (lambda (set result)
	    (union set result :test #'equal?))
	  ()
	  sets)))
(export
 (define (set-intersect set . sets)
   "Return a set with all elements in set that are also in all sets."
   (foldl (lambda (set result)
	    (intersection result set :test #'equal?))
	  set
	  sets)))
(export
 (define (set-subtract set . sets)
   "Return a set with all elements in set that are not in any of sets."
   (foldl (lambda (set result)
	    (set-difference result set :test #'equal?))
	  set
	  sets)))
(export
 (define (subset? set1 set2)
   "True if set1 is a subset of set2"
   (set-empty? (set-subtract set1 set2))))
(export
 (define (set=? set1 set2)
   "True if set1 = set2"
   (and (subset? set1 set2)
	(subset? set2 set1))))

(uninstall-syntax!)

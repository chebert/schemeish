(in-package #:schemeish.backend)

(install-syntax!)

;;; Vectors

(export
 (define (vector-ref vector index)
   (aref vector index)))
(export
 (define (vector-set! vector index value)
   (setf (aref vector index) value)))

(export
 (define (safe-vector-ref vector index out-of-bounds-result)
   "Returns the out-of-bounds-result if index is out of bounds."
   (if (>= index (length vector))
       out-of-bounds-result
       (aref vector index))))

(export
 (define (vector->list vector)
   (coerce vector 'list)))
(export
 (define (list->vector list)
   (coerce list 'vector)))

(uninstall-syntax!)

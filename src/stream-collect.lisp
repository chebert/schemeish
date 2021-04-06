(in-package #:schemeish.stream-collect)

(for-macros (install-syntax!))

(defmacro stream-collect (map-form bindings filter-form)
  "Given bindings ((b1 stream1)
                   (b2 stream2) ...)
Generates a stream all combinations of b1,b2...,
Applies a filter to the generated stream using filter-form with b1,b2... bound.
Applies a map to the filtered/generated stream using map-form with b1,b2... bound.
Example:
 (define (prime-sum-pairs n)
   (stream-collect
    (list i j (+ i j))
    ((i (stream-range 1 n))
     (j (stream-range 1 (1- i))))
    (prime? (+ i j))))

 (prime-sum-pairs n) results in all of the (list i j (+ i j)) numbers i,j such that 0 < j < i <= n"
  (stream-collect-form map-form bindings filter-form))

(define (prime? num)
  (let ((root (floor (sqrt num))))
    (not (find-if (lambda (div) (zerop (rem num div))) (range (1+ root) 2)))))

(define (prime-sum-pairs n)
  (stream-collect
   (list i j (+ i j))
   ((i (stream-range 1 n))
    (j (stream-range 1 (1- i))))
   (prime? (+ i j))))

(assert (equal (stream->list (prime-sum-pairs 6))
	       '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))))


(for-macros (uninstall-syntax!))

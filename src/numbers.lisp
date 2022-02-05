(in-package #:schemeish.backend)

(install-syntax!)

(export
 (define even? #'evenp))
(export
 (define odd? #'oddp))
(export
 (define zero? #'zerop))

(export
 (define (quotient n m)
   "Trunacate n/m"
   (truncate n m)))

(export
 (define (number->string number (radix 10))
   "Convert number to string using radix as the base."
   (let ((*print-base* radix))
     (format nil "~S" number))))

(export
 (define (degrees->radians deg)
   "convert degrees to radians"
   (/ (* pi deg) 180)))
(export
 (define (radians->degrees rads)
   "convert radians to degrees."
   (/ (* 180 rads) pi)))

(export
 (define (sqr n)
   "n*n"
   (* n n)))

(export
 (define (sgn x)
   "Return the sign of x: 1,-1, or 0"
   (cond
     ((positive? x) 1)
     ((negative? x) -1)
     ((zero? x) 0))))

(export
 (define number? #'numberp))


(export
 (define negative? #'minusp))
(export
 (define positive? #'plusp))

(uninstall-syntax!)

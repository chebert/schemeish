(cl:defpackage #:sicp-constraint-propagators
  (:use :schemeish.schemeish))

(in-package #:sicp-constraint-propagators)

(for-macros (install-syntax!))

(define (inform-about-value constraint)
  [constraint :i-have-a-value])
(define (inform-about-no-value constraint)
  [constraint :i-lost-my-value])

(define (for-each-except exception f list)
  (for-each f (remove exception list)))

(defvar *connector?* (make-bundle-predicate :connector))
(define (make-connector (value nil) (informant nil) (constraints '()))
  (define (has-value?) informant)
  (define (get-value) value)
  (define (set-value! new-value setter)
    (cond
      ((not (has-value?))
       (setq value new-value)
       (setq informant setter)
       (for-each-except setter
                        'inform-about-value
                        constraints))
      ((not (= value new-value))
       (error "Contradiction: ~S" (list value new-value)))
      (t :ignored)))
  (define (forget-value! retractor)
    (cond
      ((eq? informant retractor)
       (setq informant nil)
       (for-each-except retractor
			'inform-about-no-value
			constraints))
      (t :ignored)))
  (define (connect new-constraint)
    (when (not (member new-constraint constraints))
      (push new-constraint constraints))
    (when (has-value?)
      (inform-about-value new-constraint)))

  (bundle *connector?*
	  has-value?
	  get-value
	  set-value!
	  forget-value!
	  connect))

(define (has-value? connector)
  [[connector :has-value?]])
(define (get-value connector)
  [[connector :get-value]])
(define (set-value! connector new-value informant)
  [[connector :set-value!] new-value informant])
(define (forget-value! connector retractor)
  [[connector :forget-value!] retractor])
(define (connect connector new-constraint)
  [[connector :connect] new-constraint])

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request :I-have-a-value)  
           (process-new-value))
          ((eq? request :I-lost-my-value) 
           (process-forget-value))
          (t 
           (error "Unknown request -- ADDER ~S" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request :I-have-a-value)
           (process-new-value))
          ((eq? request :I-lost-my-value)
           (process-forget-value))
          (t
           (error "Unknown request -- MULTIPLIER ~S" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER ~S" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (sqr (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request :I-have-a-value)
           (process-new-value))
          ((eq? request :I-lost-my-value)
           (process-forget-value))
          (t (error "Unknown request -- SQUARER ~S" request))))
  (connect a me)
  (connect b me)
  me)


(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT ~S" request))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (probe name connector)
  (define (print-probe value)
    (format t "~&Probe: ~A = ~S" name value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request :I-have-a-value)
           (process-new-value))
          ((eq? request :I-lost-my-value)
           (process-forget-value))
          (t
           (error "Unknown request -- PROBE ~S" request))))
  (connect connector me)
  me)

(define (averager a b average)
  (let ((numer (make-connector))
	(denom (make-connector)))
    (adder a b numer)
    (constant 1/2 denom)
    (multiplier numer denom average)
    :ok))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(defparameter *c* (make-connector))
(defparameter *f* (make-connector))
(celsius-fahrenheit-converter *c* *f*)
;; => OK

(probe "Celsius temp" *c*)
(probe "Fahrenheit temp" *f*)

(set-value! *C* 25 'user)
#||
Output:
Probe: Celsius temp = 25
Probe: Fahrenheit temp = 77
||#
;; (set-value! *f* 212 'user)
;; error: Contradiction: (77 212)

(forget-value! *c* 'user)
#||
Output:
Probe: Celsius temp = "?"
Probe: Fahrenheit temp = "?"
||#

(set-value! *f* 212 'user)
#||
Output:
Probe: Fahrenheit temp = 212
Probe: Celsius temp = 100
||#

(defparameter *sqrt* (make-connector))
(defparameter *square* (make-connector))
(probe "sqrt" *sqrt*)
(probe "square" *square*)

(squarer *sqrt* *square*)

(set-value! *square* 16 'user)
#||
Output:
Probe: sqrt = 4.0
Probe: square = 16
||#
;; => NIL
(forget-value! *square* 'user)
#||
Output:
Probe: sqrt = "?"
Probe: square = "?"
||#
;; => NIL
(set-value! *sqrt* 3 'user)
#||
Output:
Probe: square = 9
Probe: sqrt = 3
||#
;; => NIL

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv value)
  (let ((c (make-connector)))
    (constant value c)
    c))

(define (c- x y)
  (c+ x (c* y (cv -1))))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (fahrenheit-celsius-converter f)
  (c* (c- f (cv 32)) (c/ (cv 5) (cv 9))))

(defparameter *f* (make-connector))
(defparameter *c* (fahrenheit-celsius-converter *f*))

(probe "Celsius temp" *c*)
(probe "Fahrenheit temp" *f*)

(set-value! *C* 25 'user)
#||
Output:
Probe: Celsius temp = 25
Probe: Fahrenheit temp = 77
||#
;; => NIL

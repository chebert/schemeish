(cl:defpackage #:sicp-digital-circuits
  (:use :schemeish))

(in-package #:sicp-digital-circuits)

(install-syntax!)

(defvar *the-agenda*)

(define (make-agenda (current-time 0) (segments ()))
  (define (make-time-segment time queue)
    (cons time queue))
  (define (segment-time s) (car s))
  (define (segment-queue s) (cdr s))
  
  (define (empty?) (null? segments))

  (define (first-item)
    (cond ((empty?) (error "Trying to get the first item from an empty agenda."))
	  (t (let ((segment (first segments)))
	       (setq current-time (segment-time segment))
	       (queue-front (segment-queue segment))))))

  (define (remove-first-item!)
    (cond ((empty?) (error "Trying to remove the first item from an empty agenda."))
	  (t (let ((q (segment-queue (first segments))))
	       (queue-delete! q)
	       (when (queue-empty? q)
		 (setq segments (rest segments)))))))

  (define (add! time action)
    (define (belongs-before? segments)
      (or (null? segments)
	  (< time (segment-time (first segments)))))
    (define (make-new-time-segment time action)
      (let ((q (make-queue)))
	(queue-insert! q action)
	(make-time-segment time q)))
    (define (add-to-segments! segments)
      (let ((first (first segments))
	    (rest (rest segments)))
	(cond
	  ((= time (segment-time first))
	   (queue-insert! (segment-queue first) action))
	  ((belongs-before? rest)
	   (set-cdr! segments
		     (cons (make-new-time-segment time action) rest)))
	  (t (add-to-segments! rest)))))
    (cond
      ((belongs-before? segments)
       (setq segments (cons (make-new-time-segment time action) segments)))
      (t (add-to-segments! segments))))

  (define (get-current-time) current-time)

  (bundle nil
	  empty? first-item remove-first-item! add! get-current-time))

(define (empty-agenda? a) [[a :empty?]])
(define (first-agenda-item a) [[a :first-item]])
(define (current-time a) [[a :get-current-time]])
(define (remove-first-agenda-item! a) [[a :remove-first-item!]])
(define (add-to-agenda! time action a) [[a :add!] time action])

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
		  action
		  *the-agenda*))

(define (propagate)
  (cond
    ((empty-agenda? *the-agenda*)
     :done)
    (t
     (let ((first-item (first-agenda-item *the-agenda*)))
       [first-item]
       (remove-first-agenda-item! *the-agenda*)
       (propagate)))))

(define (call-each procedures)
  (for-each (lambda (proc) [proc]) procedures))

(define wire? (make-bundle-predicate :wire))
(define (make-wire (signal-value 0) (action-procedures '()))
  (define (set-signal! new-value)
    (if (not (= signal-value new-value))
        (progn
	  (setq signal-value new-value)
          (call-each action-procedures))
        :done))
  (define (accept-action-procedure! proc)
    (setq action-procedures (cons proc action-procedures))
    [proc])
  (define (get-signal) signal-value)
  (bundle nil set-signal! accept-action-procedure! get-signal))

(define (get-signal wire) [[wire :get-signal]])
(define (set-signal! wire new-value) [[wire :set-signal!] new-value])
(define (add-action! wire action-procedure)
  [[wire :accept-action-procedure!] action-procedure])

(let ((wire (make-wire))
      (result nil))
  (assert (= 0 (get-signal wire)))
  (add-action! wire (lambda () (setq result (get-signal wire))))
  (set-signal! wire 1)
  (assert (= result 1)))

(define (signal->bool signal)
  (cond ((= 0 signal) nil)
	((= 1 signal) t)
	(t (error "bad signal ~s" signal))))
(define (bool->signal bool) (if bool 1 0))

(define (logical procedure)
  (lambda signal-args
    (bool->signal [procedure (map 'signal->bool signal-args)])))

(define (logical-not signal) (bool->signal (not (signal->bool signal))))
(define (logical-and s1 s2) [(logical (lcurry 'for-all 'identity)) s1 s2])
(define (logical-or s1 s2) [(logical (lcurry 'there-exists 'identity)) s1 s2])

(assert (= 1 (logical-not 0)))
(assert (= 0 (logical-not 1)))
(assert (= 0 (logical-and 0 1)))
(assert (= 1 (logical-and 1 1)))
(assert (= 1 (logical-or 0 1)))
(assert (= 0 (logical-or 0 0)))


(defvar *inverter-delay*)
(defvar *and-gate-delay*)
(defvar *or-gate-delay*)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay *inverter-delay*
		   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  :ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
				  (get-signal a2))))
      (after-delay *and-gate-delay*
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  :ok)

(define (or-gate o1 o2 output)
  (define (action-procedure)
    (let ((new-value (logical-or (get-signal o1)
				 (get-signal o2))))
      (after-delay *or-gate-delay*
		   (lambda () (set-signal! output new-value)))))
  (add-action! o1 action-procedure)
  (add-action! o2 action-procedure)
  :ok)

(define (half-adder a b sum carry)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b carry)
    (inverter carry e)
    (and-gate d e sum)
    :ok))

(define (full-adder a b carry-in sum carry-out)
  (let ((partial-sum (make-wire))
	(carry1 (make-wire))
	(carry2 (make-wire)))
    (half-adder b carry-in partial-sum carry1)
    (half-adder a partial-sum sum carry2)
    (or-gate carry1 carry2 carry-out)))

(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (format t "~%~S ~S New-value = ~S"
	     name (current-time *the-agenda*) (get-signal wire)))))

(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)
(defparameter *or-gate-delay* 5)

(defparameter *the-agenda* (make-agenda))

(defparameter *input-1* (make-wire))
(defparameter *input-2* (make-wire))
(defparameter *sum* (make-wire))
(defparameter *carry* (make-wire))

(probe :sum *sum*)
;; :SUM 0 New-value = 0
(probe :carry *carry*)
;; :CARRY 0 New-value = 0

(half-adder *input-1* *input-2* *sum* *carry*)
(set-signal! *input-1* 1)
(propagate)
;; :SUM 8 New-value = 1

(set-signal! *input-2* 1)
(propagate)
;; :CARRY 11 New-value = 1
;; :SUM 16 New-value = 0


(defparameter *the-agenda* (make-agenda))

(defparameter *input-1* (make-wire))
(defparameter *input-2* (make-wire))
(defparameter *carry-in* (make-wire))
(defparameter *sum* (make-wire))
(defparameter *carry-out* (make-wire))

(probe :sum *sum*)
;; :SUM 0 New-value = 0
(probe :carry-out *carry-out*)
;; :CARRY-OUT 0 New-value = 0

(full-adder *input-1* *input-2* *carry-in* *sum* *carry-out*)
;; => :OK

(set-signal! *input-1* 1)
(set-signal! *input-2* 0)
(set-signal! *carry-in* 1)

(propagate)
;; :SUM 8 New-value = 1
;; :CARRY-OUT 16 New-value = 1
;; :SUM 16 New-value = 0

(uninstall-syntax!)

(in-package #:schemeish.queue)

(for-macros (install-syntax!))
(defparameter *queue?* (make-bundle-predicate :queue))
(define (make-queue (front-ptr ()))
  (define rear-ptr (last front-ptr))
  (define (empty?) (null? front-ptr))
  (define (front)
    (cond
      ((empty?)
       (error "Cannot get the front of an empty queue."))
      (t (car front-ptr))))
  (define (insert! item)
    (let ((new-pair (cons item '())))
      (cond
	((empty?)
	 (setq front-ptr new-pair)
	 (setq rear-ptr new-pair))
	(t
	 (set-cdr! rear-ptr new-pair)
	 (setq rear-ptr new-pair)))))
  (define (delete!)
    (cond
      ((empty?)
       (error "Cannot delete from an empty queue."))
      (t
       (setq front-ptr (cdr front-ptr)))))

  (bundle *queue?* (make-queue front-ptr)
	  empty?
	  front
	  insert!
	  delete!))

(define (queue? v) [*queue?* v])
(define (queue-empty? q) [[q :empty?]])
(define (queue-front q) [[q :front]])
(define (queue-insert! q item)
  [[q :insert!] item]
  q)
(define (queue-delete! q)
  [[q :delete!]]
  q)

(assert (queue? (make-queue)))
(assert (queue-empty? (make-queue)))
(let ((q (make-queue)))
  (assert (eq? q (queue-insert! q 1)))
  (assert (= 1 (queue-front q)))
  (queue-insert! q 2)
  (assert (= 1 (queue-front q)))
  (assert (eq? q (queue-delete! q)))
  (assert (= 2 (queue-front q)))
  (assert (queue-empty? (queue-delete! q)))

  (assert (null (ignore-errors (queue-front q))))
  (assert (null (ignore-errors (queue-delete! q)))))
(for-macros (uninstall-syntax!))

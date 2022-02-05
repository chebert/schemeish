(cl:defpackage #:sicp-metacircular-evaluator
  (:use :schemeish)
  (:shadow #:eval #:apply))

(in-package #:sicp-metacircular-evaluator)

(install-syntax!)

(define (eval expr env)
  (cond
    ((self-evaluating? expr) expr)
    ((variable? expr) (lookup-variable-value expr env))
    ((quoted? expr) (text-of-quotation expr))
    ((assignment? expr) (eval-assignment expr env))
    ((definition? expr) (eval-definition expr env))
    ((if? expr) (eval-if expr env))
    ((lambda? expr)
     (make-procedure (lambda-parameters expr)
		     (lambda-body expr)
		     env))
    ((begin? expr)
     (eval-sequence (begin-actions expr) env))
    ((cond? expr)
     (eval (cond->if expr) env))
    ((application? expr)
     (apply (eval (operator expr) env)
	    (list-of-values (operands expr) env)))
    (t
     (error "Unknown expression type -- EVAL ~S" expr))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(t
	 (error "Unknown procedure type -- APPLY ~S" procedure))))

(define (list-of-values exprs env)
  (if (no-operands? exprs)
      '()
      ;; Evaluate left to right
      (let ((first (eval (first-operand exprs) env)))
	(cons first (list-of-values (rest-operands exprs) env)))))

(define (list-of-values-right-to-left exprs env)
  (if (no-operands? exprs)
      '()
      ;; Evaluate right to left
      (let ((rest (list-of-values (rest-operands exprs) env)))
	(cons (eval (first-operand exprs) env) rest))))

(define (eval-if expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))

(define (eval-sequence exprs env)
  (cond ((last-expr? exprs) (eval (first-expr exprs) env))
	(t (eval (first-expr exprs) env)
	   (eval-sequence (rest-exprs exprs) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (eval (assignment-value expr) env)
                       env)
  :ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
      (eval (definition-value expr) env)
    env)
  :ok)

(defparameter *false* nil)
(define (false? datum) (eq? *false* datum))
(define (true? datum) (not (false? datum)))

(define (self-evaluating? expr)
  (cond ((number? expr) t)
	((string? expr) t)
	((boolean? expr) t)
	(t nil)))

(define (boolean? expr)
  (cond ((eq? expr t) t)
	((eq? expr nil) t)
	(t nil)))

(define (variable? expr) (symbol? expr))
(define (quoted? expr) (tagged-list? expr 'quote))
(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (and (pair? expr) (eq? (car expr) tag)))

(define (assignment? expr) (tagged-list? expr 'set!))
(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))

(define (definition? expr) (tagged-list? expr 'define))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))
(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr) ; formal parameters
		   (cddr expr)))) ; body

(define (lambda? expr) (tagged-list? expr 'lambda))
(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? expr) (tagged-list? expr 'if))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr)
  (if (not (null? expr))
      (cadddr expr)
      *false*))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? expr) (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))
(define (last-expr? exprs) (null? (cdr exprs)))
(define (first-expr exprs) (car exprs))
(define (rest-exprs exprs) (cdr exprs))

(define (sequence->expr exprs)
  (cond ((null? exprs) exprs)
	((last-expr? exprs) (first-expr exprs))
	(t (make-begin exprs))))
(define (make-begin exprs) (cons 'begin exprs))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      *false* ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->expr (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF ~S"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->expr (cond-actions first))
                     (expand-clauses rest))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(defparameter *the-empty-environment* '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied ~S ~S" vars vals)
          (error "Too few arguments supplied ~S ~S" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (t (scan (cdr vars) (cdr vals)))))
    (if (eq? env *the-empty-environment*)
        (error "Unbound variable ~S" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (if (eq? env *the-empty-environment*)
        (error "Unbound variable -- SET! ~S" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (define frame (first-frame env))
  (define (scan vars vals)
    (cond ((null? vars)
           (add-binding-to-frame! var val frame))
          ((eq? var (car vars))
           (set-car! vals val))
          (t (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame)))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              *the-empty-environment*)))
    (define-variable! 'true t initial-env)
    (define-variable! 'false *false* initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(defparameter *primitive-procedures*
  (list (list 'car #'car)
	(list 'cdr #'cdr)
	(list 'cons #'cons)
	(list 'null? #'null?)
	(list '+ #'+)
	(list '- #'-)
	(list '* #'*)
	(list '/ #'/)
	(list '= #'=)
	;; More primitives...
	))

(define (primitive-procedure-names)
  (map #'car *primitive-procedures*))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       *primitive-procedures*))

(define (apply-primitive-procedure proc args)
  (cl:apply (primitive-implementation proc) args))


(defparameter *the-global-environment* (setup-environment))


(defparameter *input-prompt* ";;; M-Eval input:")
(defparameter *output-prompt* ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input *input-prompt*)
  (let ((input (read)))
    (let ((output (eval input *the-global-environment*)))
      (announce-output *output-prompt*)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(defparameter *the-global-environment* (setup-environment))
;; => *THE-GLOBAL-ENVIRONMENT*

;; Sample run 1: append
"
(driver-loop)

;;; M-Eval input:
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;;; M-Eval value:
OK

;;; M-Eval input:
(append '(a b c) '(d e f))

;;; M-Eval value:
(A B C D E F)

;;; M-Eval input:
"

;; Sample run 2: map
"
(driver-loop)

;;; M-Eval input:
(define (map proc list)
  (cond
    ((null? list) list)
    (t (cons (proc (car list))
	     (map proc (cdr list))))))

;;; M-Eval value:
OK

;;; M-Eval input:
(map (lambda (x) (cons x '())) '(1 2 3))

;;; M-Eval value:
((1) (2) (3))
"

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (t
         (error "Unknown expression type -- ANALYZE ~S" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) (declare (ignore env)) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) (declare (ignore env)) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var [vproc env] env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var [vproc env] env)
      'ok)))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? [pproc env])
          [cproc env]
          [aproc env]))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) [proc1 env] [proc2 env]))
  (define (rec first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (rec (sequentially first-proc (car rest-procs))
             (cdr rest-procs))))
  (let ((procs (map 'analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (rec (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map 'analyze (operands exp))))
    (lambda (env)
      (execute-application [fproc env]
                           (map (lambda (aproc) [aproc env])
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         [(procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))])
        (t
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION ~S"
          proc))))

(defparameter *the-global-environment* (setup-environment))
(defparameter *fib-test* '(begin
			   (define (fib n)
			     (cond ((= 0 n) 0)
				   ((= 1 n) 1)
				   (t (+ (fib (- n 1)) (fib (- n 2))))))
			   (fib 30)))

(time (eval *fib-test* *the-global-environment*))
;; => 832040
"Evaluation took:
  4.591 seconds of real time
  4.578125 seconds of total run time (4.546875 user, 0.031250 system)
  [ Run times consist of 0.061 seconds GC time, and 4.518 seconds non-GC time. ]
  99.72% CPU
  9,149,610,335 processor cycles
  931,278,112 bytes consed"

(time [(analyze *fib-test*) *the-global-environment*])
;; => 832040
"Evaluation took:
  2.305 seconds of real time
  2.296875 seconds of total run time (2.296875 user, 0.000000 system)
  [ Run times consist of 0.094 seconds GC time, and 2.203 seconds non-GC time. ]
  99.65% CPU
  4,588,646,253 processor cycles
  1,156,917,568 bytes consed"

(let ((proc (analyze *fib-test*)))
  (time [proc *the-global-environment*]))
;; => 832040
"Evaluation took:
  2.388 seconds of real time
  2.390625 seconds of total run time (2.375000 user, 0.015625 system)
  [ Run times consist of 0.077 seconds GC time, and 2.314 seconds non-GC time. ]
  100.13% CPU
  4,752,310,264 processor cycles
  1,156,907,040 bytes consed"

(time (progn
	(define (fib n)
	  (cond ((= 0 n) 0)
		((= 1 n) 1)
		(t (+ (fib (- n 1)) (fib (- n 2))))))
	(fib 30)))
;; => 832040
"Evaluation took:
  0.065 seconds of real time
  0.046875 seconds of total run time (0.046875 user, 0.000000 system)
  72.31% CPU
  137,696,127 processor cycles
  0 bytes consed"

;; Fib test summary
;; EVAL: 4.5-5.5 seconds
;; ANALYZE: 2.3-2.5 seconds
;; NATIVE: 0.01-0.1 seconds

(uninstall-syntax!)

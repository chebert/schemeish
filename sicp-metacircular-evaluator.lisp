(cl:defpackage #:sicp-metacircular-evaluator
  (:use :schemeish)
  (:shadow #:apply #:eval))

(in-package #:sicp-metacircular-evaluator)

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

(define *false* nil)
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
(define *the-empty-environment* '())

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
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

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


(define *primitive-procedures*
  (list (list 'car #'car)
	(list 'cdr #'cdr)
	(list 'cons #'cons)
	(list 'null? #'null?)
	;; More primitives...
	))

(define (primitive-procedure-names)
  (map #'car *primitive-procedures*))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       *primitive-procedures*))

(define (apply-primitive-procedure proc args)
  (cl:apply (primitive-implementation proc) args))


(define *the-global-environment* (setup-environment))


(define *input-prompt* ";;; M-Eval input:")
(define *output-prompt* ";;; M-Eval value:")

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


(define *the-global-environment* (setup-environment))
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

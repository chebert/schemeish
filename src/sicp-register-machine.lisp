(defpackage #:sicp-register-machine
  (:use #:schemeish.schemeish))

(in-package #:sicp-register-machine)

(define (make-new-machine)
  ;; Registers

  ;; Points to the remaining instructions to run
  (define pc (make-register :pc))
  ;; True/false based on the previous test
  (define flag (make-register :flag))

  ;; Stack
  (define stack (make-stack))

  ;; instructions
  (define the-instruction-sequence '())

  ;; Operations alist (op-name thunk)
  (define the-ops
    (list (list :initialize-stack
		(lambda () [stack :initialize]))))

  ;; Alist of (register-name register)
  (define register-table
    (list (list :pc pc) (list :flag flag)))

  ;; Creates a register associated with name in register-table
  ;; if it doesn't already exist
  (define (allocate-register name)
    (if (assoc name register-table)
        (error "Multiply defined register: ~S" name)
        (set! register-table
              (cons (list name (make-register name))
                    register-table)))
    :register-allocated)

  ;; Retrieve the register associated with name in register-table
  (define (lookup-register name)
    (let ((val (assoc name register-table)))
      (if val
          (cadr val)
          (error "Unknown register: ~S" name))))

  ;; Execute all instructions.
  (define (execute)
    (let ((insts (get-contents pc)))
      (if (null? insts)
          :done
          (progn
            [(instruction-execution-proc (car insts))]
            (execute)))))

  ;; Set pc to point to the instruction sequence and execute
  (define (start)
    (set-contents! pc the-instruction-sequence)
    (execute))

  ;; Set the instruction sequence to be seq
  (define (install-instruction-sequence seq)
    (set! the-instruction-sequence seq))

  ;; Append new ops to the known ops
  (define (install-operations ops)
    (set! the-ops (append the-ops ops)))
  
  (define (dispatch message)
    (cond
      ((eq? message :start)
       (start))
      ((eq? message :install-instruction-sequence)
       install-instruction-sequence)
      ((eq? message :allocate-register) allocate-register)
      ((eq? message :get-register) lookup-register)
      ((eq? message :install-operations) install-operations)
      ((eq? message :stack) stack)
      ((eq? message :operations) the-ops)
      (t (error "Unknown request -- MACHINE ~S" message))))
  dispatch)

(define (get-register machine register-name)
  [[machine :get-register] register-name])

(define (start machine)
  [machine :start])
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  :done)

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                [[machine :allocate-register] register-name])
              register-names)
    [[machine :install-operations] ops]    
    [[machine :install-instruction-sequence]
     (assemble controller-text machine)]
    machine))

(define (make-register name)
  (define contents :unassigned)
  (define (get-contents) contents)
  (define (set-contents! value) (set! contents value))

  (bundle nil (make-register name)
	  get-contents set-contents!))

(define (get-contents register)
  [[register :get-contents]])
(define (set-contents! register value)
  [[register :set-contents!] value])

(define (make-stack)
  (define s ())
  (define (pushv x)
    (set! s (cons x s)))
  (define (popv)
    (if (empty? s)
	(error "POPV: Empty stack")
	(let ((top (first s)))
	  (set! s (rest s))
	  top)))
  (define (initialize)
    (set! s '())
    :done)
  (bundle nil ()
	  pushv
	  popv
	  initialize))

(define (pushv stack value)
  [[stack :pushv] value])
(define (popv stack)
  [[stack :popv]])

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

(define (extract-labels text receive)
  (if (null? text)
      [receive '() '()]
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  ;; If next-instr is a symbol, then it is a label
			  ;; associated with the rest of the instructions
			  (if (symbol? next-inst)
			      [receive insts
				       (cons (make-label-entry next-inst
							       insts)
					     labels)]
			      ;; otherwise, next-instr is an instruction
			      [receive (cons (make-instruction next-inst)
					     insts)
				       labels]))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine :pc))
        (flag (get-register machine :flag))
        (stack [machine :stack])
        (ops [machine :operations]))    
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE ~S" label-name))))

;; Dispatch based on the type of instruction 
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (t (error "Unknown instruction type -- ASSEMBLE ~S"
                  inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
		(make-operation-exp
                 value-exp machine labels operations)
		(make-primitive-exp
                 (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target [value-proc])
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
		(make-operation-exp
                 condition machine labels operations)))
          (lambda ()
            (set-contents! flag [condition-proc])
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE ~S" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
		(lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE ~S" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                   (lookup-label labels
                                 (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                   (get-register machine
                                 (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (t (error "Bad GOTO instruction -- ASSEMBLE ~S"
                    inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (pushv stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (popv stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
		(make-operation-exp
                 action machine labels operations)))
          (lambda ()
            [action-proc]
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE ~S" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                 (lookup-label labels
                               (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (t
         (error "Unknown expression type -- ASSEMBLE ~S" exp))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) [p]) aprocs)))))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE ~S" symbol))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (first exp) tag)))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem #'rem) (list '= #'=))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! 'gcd-machine 'a 206)
;; => :DONE
(set-register-contents! 'gcd-machine 'b 40)
;; => :DONE
(start 'gcd-machine)
;; => :DONE
(get-register-contents 'gcd-machine 'a)
;; => 2




(define (vector-ref v index)
  (aref v index))

(define (vector-set! v index value)
  (setf (aref v index) value))

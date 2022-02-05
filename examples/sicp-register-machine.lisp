(defpackage #:sicp-register-machine
  (:use #:schemeish))

(in-package #:sicp-register-machine)

(install-syntax!)

(define (tagged-list? exp tag)
  "True if exp is a list that begins with tag."
  (and (pair? exp)
       (eq? (first exp) tag)))

(define (make-new-machine)
  ;; Registers

  ;; Points to the remaining instructions to run
  (define pc (make-register 'pc))
  ;; True/false based on the previous test
  (define flag (make-register 'flag))

  ;; Stack
  (define stack (make-stack))

  ;; instructions
  (define the-instruction-sequence '())
  (define instruction-count 0)
  (define trace? nil)
  
  ;; Operations alist (op-name . thunk)
  (define the-ops
    (alist 'initialize-stack [stack :initialize]
	   'print-stack-statistics [stack :print-statistics]))

  ;; Alist of (register-name register)
  (define register-table
    (alist 'pc pc 'flag flag))

  (define data-path-unique-insts '())
  (define data-path-entry-points '())
  (define data-path-stored-registers '())
  (define data-path-sources '())
  
  ;; Creates a register associated with name in register-table
  ;; if it doesn't already exist
  (define (allocate-register name)
    (if (assoc name register-table)
        (error "Multiply defined register: ~S" name)
        (set! register-table
              (cons (cons name (make-register name))
                    register-table)))
    :register-allocated)

  ;; Retrieve the register associated with name in register-table
  (define (lookup-register name)
    (let ((val (assoc name register-table)))
      (if val
          (cdr val)
          (error "Unknown register: ~S" name))))

  ;; Execute all instructions.
  (define (execute)
    (let ((insts (get-contents pc)))
      (if (null? insts)
          :done
          (progn
	    (when trace?
	      (display (list 'trace (instruction-text (car insts)))))
            [(instruction-execution-proc (car insts))]
	    (set! instruction-count (1+ instruction-count))
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

  (define (set-data-paths! unique-insts entry-points stored-registers sources)
    (set! data-path-unique-insts unique-insts)
    (set! data-path-entry-points entry-points)
    (set! data-path-stored-registers stored-registers)
    (set! data-path-sources sources))

  (define (reset-instruction-count!)
    (set! instruction-count 0))
  (define (print-instruction-count)
    (display (list 'executed instruction-count 'instructions)))
  
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
      ((eq? message :set-data-paths!) set-data-paths!)
      ((eq? message :unique-insts) data-path-unique-insts)
      ((eq? message :entry-points) data-path-entry-points)
      ((eq? message :stored-registers) data-path-stored-registers)
      ((eq? message :sources) data-path-sources)
      ((eq? message :reset-instruction-count!) (reset-instruction-count!))
      ((eq? message :print-instruction-count) (print-instruction-count))
      ((eq? message :trace-on) (set! trace? t))
      ((eq? message :trace-off) (set! trace? nil))
      (t (error "Unknown request -- MACHINE ~S" message))))
  dispatch)

(define (set-data-paths! machine unique-insts entry-points stored-registers sources)
  [[machine :set-data-paths!] unique-insts entry-points stored-registers sources])

(define (lookup-prim symbol operations)
  "Lookup the primitive op associated with symbol"
  (let ((val (assoc symbol operations)))
    (if val
        (cdr val)
        (error "Unknown operation -- ASSEMBLE ~S" symbol))))

(define (get-register machine register-name)
  [[machine :get-register] register-name])

(define (start machine)
  [machine :start])
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  :done)

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    [[machine :install-operations] ops]    
    [[machine :install-instruction-sequence]
     (assemble controller-text machine)]
    machine))

(define (make-register name (contents :unassigned))
  (declare (ignore name))
  (define (get-contents) contents)
  (define (set-contents! value) (set! contents value))

  (bundle nil
	  get-contents set-contents!))

(define (get-contents register)
  [[register :get-contents]])
(define (set-contents! register value)
  [[register :set-contents!] value])

(define (make-stack)
  (define stacks ())
  (define num-pushes 0)
  (define max-depth 0)
  (define current-depth 0)

  (define (stack-push reg-name x)
    (set! num-pushes (1+ num-pushes))
    (set! current-depth (1+ current-depth))
    (set! max-depth (max max-depth num-pushes))
    (set! stacks (alist-update stacks reg-name (lambda (stack) (cons x stack)))))
  (define (stack-pop reg-name)
    (let ((stack (alist-ref stacks reg-name)))
      (if (empty? stack)
	  (error "STACK-POP: Empty stack")
	  (progn
	    (set! current-depth (1- current-depth))
	    (set! stacks (alist-update stacks reg-name #'rest))
	    (first stack)))))

  (define (initialize)
    (set! stacks '())
    (set! num-pushes 0)
    (set! max-depth 0)
    (set! current-depth 0)
    :done)
  (define (print-statistics)
    (newline)
    (display (list 'total-pushes  '= num-pushes
                   'maximum-depth '= max-depth)))
  (bundle nil
	  stack-push
	  stack-pop
	  initialize
	  print-statistics))

(define (stack-push stack reg-name value)
  [[stack :stack-push] reg-name value])
(define (stack-pop stack reg-name)
  [[stack :stack-pop] reg-name])

(define (symbol< s1 s2)
  (string< (symbol->string s1) (symbol->string s2)))

(define (unique-instructions insts)
  (sort (remove-duplicates insts :test #'equal?) #'symbol< :extract-key #'car))

(define (extract-data-paths texts receive)
  "calls [receive unique entry-points saved/restored-registers register-sources] with the results."
  (cond
    ((null? texts) [receive () () () ()])
    (t (extract-data-paths
	(rest texts)
	(let ((text (first texts)))
	  (lambda (unique entry-points saved/restored-registers register-sources)
	    (let ((unique (adjoin text unique :test #'equal?)))
	      (cond
		((tagged-list? text 'goto)
		 [receive unique
			  (let ((dest (goto-dest text)))
			    (if (register-exp? dest)
				(adjoin (register-exp-reg dest) entry-points)
				entry-points))
			  saved/restored-registers
			  register-sources])
		((tagged-list? text 'branch)
		 [receive unique
			  (let ((dest (branch-dest text)))
			    (if (register-exp? dest)
				(adjoin (register-exp-reg (branch-dest text)) entry-points)
				entry-points))
			  saved/restored-registers
			  register-sources])
		((or (tagged-list? text 'save)
		     (tagged-list? text 'restore))
		 [receive unique
			  entry-points
			  (adjoin (stack-inst-reg-name text) saved/restored-registers)
			  register-sources])
		((tagged-list? text 'assign)
		 (let ((reg-name (assign-reg-name text))
		       (value-exp (assign-value-exp text)))
		   [receive unique
			    entry-points
			    saved/restored-registers
			    (alist-update register-sources
					  reg-name
					  (lambda (sources)
					    (adjoin value-exp sources :test #'equal?)))]))
		(t [receive unique
			    entry-points
			    saved/restored-registers
			    register-sources])))))))))

(define (referenced-registers-in-exp exp)
  (cond ((operation-exp? exp)
	 (let ((operands (operation-exp-operands exp)))
	   (map #'register-exp-reg (filter #'register-exp? operands))))
	((register-exp? exp)
	 (list (register-exp-reg exp)))
	(t ())))

(define (allocate-new-registers old-regs new-regs allocate)
  (let ((new (set-difference new-regs old-regs)))
    (for-each allocate new)
    (append new old-regs)))

(define (extract-register-names texts)
  (foldl
   (lambda (inst regs)
     (cond
       ((tagged-list? inst 'assign)
	(union regs
	       (cons (assign-reg-name inst)
		     (referenced-registers-in-exp (assign-value-exp inst)))))
       ((tagged-list? inst 'test)
	(union regs (referenced-registers-in-exp (test-condition inst))))
       ((tagged-list? inst 'goto)
	(union regs (referenced-registers-in-exp (goto-dest inst))))
       ((or (tagged-list? inst 'restore)
	    (tagged-list? inst 'save))
	(union regs (list (stack-inst-reg-name inst))))
       ((tagged-list? inst 'perform)
	(union regs (perform-action inst)))
       (t regs)))
   ()
   texts))

(define (assemble controller-text machine)
  "Assembles instructions replacing labels with hard-coded jumps."
  (map [machine :allocate-register] (extract-register-names controller-text))
  (extract-labels controller-text
		  (lambda (insts labels)
		    ;; insts and labels are the result of extracting labels from
		    ;; controller-text
		    (extract-data-paths
		     (map #'instruction-text insts)
		     (lambda (unique-instructions entry-points saved/restored-registers register-sources)
		       (set-data-paths! machine
					unique-instructions
					entry-points
					saved/restored-registers
					register-sources)))
		    (update-insts! insts labels machine)
		    insts)))

(define (extract-labels text receive)
  "Calls [receive instrs labels]"
  (cond
    ((null? text) [receive '() '()])
    (t
     (extract-labels (cdr text)
		     (lambda (insts labels)
		       (let ((next-inst (car text)))
			 ;; If next-instr is a symbol, then it is a label
			 ;; associated with the rest of the instructions
			 (if (symbol? next-inst)
			     [receive insts
				      (cons (make-label-entry next-inst insts)
					    labels)]
			     ;; otherwise, next-instr is an instruction
			     [receive (cons (make-instruction next-inst)
					    insts)
				      labels])))))))

(define (update-insts! insts labels machine)
  "Set all the instruction-execution-procedures for insts."
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
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
  (cond ((tagged-list? inst 'assign)
	 ;; (assign reg-name . value-exp)
	 (make-assign inst machine labels ops pc))
        ((tagged-list? inst 'test)
         (make-test inst machine labels ops flag pc))
        ((tagged-list? inst 'branch)
         (make-branch inst machine labels flag pc))
        ((tagged-list? inst 'goto)
         (make-goto inst machine labels pc))
        ((tagged-list? inst 'save)
         (make-save inst machine stack pc))
        ((tagged-list? inst 'restore)
         (make-restore inst machine stack pc))
        ((tagged-list? inst 'perform)
         (make-perform inst machine labels ops pc))
        (t (error "Unknown instruction type -- ASSEMBLE ~S" inst))))

(define (make-assign inst machine labels operations pc)
  "(assign reg-name value-exp)"
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (cond
	      ((operation-exp? value-exp)
	       ;; value-exp := ((op op-name) . operands)
	       (make-operation-exp value-exp machine labels operations))
	      (t (make-primitive-exp (car value-exp) machine labels)))))
      (lambda () ; execution procedure for assign
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
  (declare (ignore machine))
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
  (let ((name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine name)))
      (lambda ()
	(stack-push stack name (get-contents reg))
	(advance-pc pc)))))
(define (make-restore inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine name)))
      (lambda ()
	(set-contents! reg (stack-pop stack name))
	(advance-pc pc)))))
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
		 (cond
		   ((label-exp? e) (error "Cannot apply operation to label"))
		   (t (make-primitive-exp e machine labels))))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) [p]) aprocs)))))

(define (operation-exp? exp)
  (tagged-list? (car exp) 'op))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

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
   (alist 'rem #'rem
	  '= #'=)
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

(define fib-machine
  (make-machine
   (alist '< #'<
	  '- #'-
	  '+ #'+)
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)				 ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n - 1
     (goto (label fib-loop))		 ; perform recursive call
     afterfib-n-1		; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2		; upon return, val contains Fib(n - 2)
     (assign n (reg val))	; n now contains Fib(n - 2)
     (restore val)		; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
      (op +) (reg val) (reg n)) 
     (goto (reg continue))	  ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done)))

;; 0 1 2 3 4 5 6 7
;; 0 1 1 2 3 5 8 13


(set-register-contents! 'fib-machine 'n 7)
;; => :DONE
(set-register-contents! 'fib-machine 'val 0)
;; => :DONE
(start 'fib-machine)
;; => :DONE

(get-register-contents 'fib-machine 'val)

(define fact-machine
  (make-machine
   (alist '= '=
	  '- '-
	  '* '*)
   '((perform (op initialize-stack))
     (assign continue (label fact-done)) ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
     (goto (reg continue))		   ; return to caller
     base-case
     (assign val (const 1))		; base case: 1! = 1
     (goto (reg continue))		; return to caller
     fact-done
     (perform (op print-stack-statistics)))))

(set-register-contents! 'fact-machine 'n 5)
;; => :DONE
(start 'fact-machine)
;; => :DONE
(get-register-contents 'fact-machine 'val)
;; => 120
[[(fact-machine :stack) :print-statistics]]
#||
Output:

(TOTAL-PUSHES = 8 MAXIMUM-DEPTH = 8)
||#
;; => NIL

(define (run-fact! n)
  (set-register-contents! 'fact-machine 'n n)
  (start 'fact-machine))
(run-fact! 5)
#||
Output:

(TOTAL-PUSHES = 8 MAXIMUM-DEPTH = 8)
||#
;; => :DONE

(run-fact! 4)
#||
Output:

(TOTAL-PUSHES = 6 MAXIMUM-DEPTH = 6)
||#
;; => :DONE
(run-fact! 6)
#||
Output:

(TOTAL-PUSHES = 10 MAXIMUM-DEPTH = 10)
||#
;; => :DONE


(define (vector-ref v index)
  (aref v index))

(define (vector-set! v index value)
  (setf (aref v index) value))

(uninstall-syntax!)

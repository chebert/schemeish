(in-package #:schemeish.backend)

(install-syntax!)

(for-macros
  (define (and-let*-form clauses body)
    (cond
      ((empty? clauses) `(progn ,@body))
      (t
       (let ((clause (first clauses)))
	 (cond
	   ((list? clause)
	    (cond
	      ((null? (rest clause))
	       `(and ,(first clause) ,(and-let*-form (rest clauses) body)))
	      (t
	       `(let ((,(first clause) ,(second clause)))
		  (and ,(first clause) ,(and-let*-form (rest clauses) body))))))
	   (t (error "invalid clause in and-let*: ~S" clause))))))))

(export
 (defmacro and-let* ((&rest clauses) &body body)
   "Evaluate each clause from first to last until one is false. If all are true, evaluate body.
Each clause is one of: identifier, (expression), or (identifier expression).
If the clause is (identifier expression) it creates a binding for the rest of the clauses and the body.
Example (and-let* ((list (compute-list))
                   ((pair? list))
                   (item (car list))
                   ((integer? item)))
          (sqrt item))"
   (and-let*-form clauses body)))

(uninstall-syntax!)

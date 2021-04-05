(in-package #:schemeish.expand-lexically)

(for-macros (install-syntax!))

(define (lexical-name->parameter-name symbol)
  (intern (string-append "*" (symbol->string symbol) "*")))


(define (special? symbol)
  "True if symbol is marked as special."
  #+sbcl
  (eq? :special (sb-cltl2:variable-information symbol))
  #-sbcl
  (let ((f (gensym)))
    (null?
     (ignore-errors
      (eval `(let ((,symbol 1))
	       (let ((,f (lambda () ,symbol)))
		 (let ((,symbol 2))
		   (not (eql 2 (funcall ,f)))))))))))

(define (special-form? symbol)
  (member symbol '(cl:block      cl:let*                  cl:return-from
		   cl:catch      cl:load-time-value       cl:setq
		   cl:eval-when  cl:locally               cl:symbol-macrolet
		   cl:flet       cl:macrolet              cl:tagbody
		   cl:function   cl:multiple-value-call   cl:the
		   cl:go         cl:multiple-value-prog1  cl:throw
		   cl:if         cl:progn                 cl:unwind-protect
		   cl:labels     cl:progv
		   cl:let        cl:quote)))

(define (parameter-name? symbol)
  "True if the symbol has a *NAME* naming convention"
  (and-let*
      ((str (symbol->string symbol))
       (length (length str))
       ((>= length 3))
       ((char= #\* (aref str 0)))
       ((char= #\* (aref str (1- length))))
       ((find-if (lambda (c) (not (char= #\* c))) (subseq str 1 (1- length)))))
    t))

(assert (parameter-name? '*get-bundle-type-predicate*))
(assert (not (parameter-name? '***)))

(define (parameter-name->lexical-name symbol (wrap-str "/"))
  "Given a symbol with a special naming convention (like *NAME*), 
return a symbol which follows the naming convetion /NAME/ (wrapped with whatever wrap-str is),
The returned symbol will be in the current package.
This operation fails if the resulting lexical-name is declared to be special."
  (let ((str (symbol->string symbol)))
    (let ((result (intern (string-append wrap-str (subseq str 1 (1- (length str))) wrap-str))))
      (assert (not (special? result)))
      result)))

(assert (eq? (parameter-name->lexical-name '*get-bundle-type-predicate*)
	     '/get-bundle-type-predicate/))

(assert (eq? (parameter-name->lexical-name '*terminal-io*)
	     '/terminal-io/))

;; TODO: combine docs/set arities for compose et al.
;; TODO: more packages (for macros)

(define (lexical-bindings parameter-wrap-string special-fn-append-string)
  (let (package-symbols fn-bindings special-bindings)
    (do-symbols (sym)
      (cond
	;; Ignore duplicate symbols.
	((member sym package-symbols) t)
	((and (fboundp sym)
	      (not (special-form? sym))
	      (not (macro-function sym)))
	 (if (special? sym)
	     (let ((lexical-sym (intern (string-append (symbol-name sym) special-fn-append-string))))
	       (assert (not (special? lexical-sym)))
	       (push `(,lexical-sym #',sym) fn-bindings))
	     (push `(,sym #',sym) fn-bindings))
	 (push sym package-symbols))
	((and (special? sym) (parameter-name? sym))
	 (push (list (parameter-name->lexical-name sym parameter-wrap-string) sym) special-bindings)
	 (push sym package-symbols))))
    (nconc fn-bindings special-bindings)))


(for-macros (uninstall-syntax!))

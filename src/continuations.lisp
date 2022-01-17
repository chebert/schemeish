(cl:in-package :schemeish.continuations)

;; Idea: (WITH-CONTINUATIONS K body...)
;; Expand body into a form that takes/recieves implicit continuations
;; adds (call/cc fn) special form

;; would need to handle at least some of the special forms:
;; block      let*                  return-from      
;; catch      load-time-value       setq             
;; eval-when  locally               symbol-macrolet  
;; flet       macrolet              tagbody          
;; function   multiple-value-call   the              
;; go         multiple-value-prog1  throw            
;; if         progn                 unwind-protect   
;; labels     progv                                  
;; let        quote                          

(install-syntax!)


(uninstall-syntax!)

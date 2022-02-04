(in-package #:schemeish.backend)

(install-syntax!)

;;; Tree

(export
 (define (flatten tree)
   (cond
     ((null? tree) ())
     ((pair? tree) (append (flatten (car tree)) (flatten (cdr tree))))
     (t (list tree)))))

(uninstall-syntax!)

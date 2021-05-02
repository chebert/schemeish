(in-package #:schemeish.package-utils)

(install-syntax!)

(defvar *resolve-package-designator* #'identity
  "Resolves a package designator.")

(define (package? datum) (packagep datum))

(define (package-find unresolved-package-designator)
  "Resolves the designator using *RESOLVE-PACKAGE-DESIGNATOR* before finding the package."
  (cond
    ((package? unresolved-package-designator) unresolved-package-designator)
    (t (find-package [*resolve-package-designator* unresolved-package-designator]))))

(define (package-symbols package)
  "Returns a list of all symbols internal and external in package."
  (let ((symbols ()))
    (do-symbols (symbol (package-find package))
      (push symbol symbols))
    (nreverse symbols)))
(define (package-external-symbols package)
  "Returns a list of all external/exported symbols in package."
  (let ((symbols ()))
    (do-external-symbols (symbol (package-find package))
      (push symbol symbols))
    (nreverse symbols)))
(define (package-exported-symbols package)
  "Alias for package-external-symbols."
  (package-external-symbols package))

(define (symbol-in-package? package symbol)
  "True if symbol belongs to package."
  (eq? (symbol-package symbol) (package-find package)))

(define (package-imported-symbols package)
  "Returns all symbols in package-designator that have been imported into package from another package."
  (filter-not (lcurry 'symbol-in-package? package) (package-symbols package)))

(define (group-by-package symbols)
  "Return symbols as an alist of (package-name . package-symbols)."
  (alist-map (group (compose 'package-name 'symbol-package) symbols)
	     (lambda (name symbols)
	       (cons name (map 'symbol-name symbols)))))

(define (package-non-shadowing-symbols package)
  "List of all of the non-shadowing symbols in package."
  (set-difference (package-symbols package) (package-shadowing-symbols (package-find package))))

(define (package-used-symbols package)
  "List of all of the symbols in package that are from a USE-PACKAGE, and not explicitly imported."
  (intersection (package-symbols package)
		(append-map (lambda (used-package) (package-exported-symbols used-package))
			    (package-use-list (package-find package)))))
(define (package-unused-symbols package)
  "List of all of the explicitly imported symbols, that are not part of a USEd package."
  (set-difference (package-symbols package) (package-used-symbols package)))

(define (symbols-interned-in-package symbols package)
  "Return symbols interned into package."
  (let ((package (package-find package)))
    (mapcar (lambda (s) (intern (symbol-name s) package)) symbols)))

(define (symbols-in-package symbols package)
  "Return the symbols that already exist in package."
  (intersection symbols (package-symbols package)))

(define (package-external-symbols-from package from-package)
  "Return the symbols in package that are exported from from-package."
  (symbols-in-package (package-exported-symbols from-package) package))

(define ((package-use . packages) package)
  "Each of packages is used in package.
Returns a closure that takes and returns the package to modify."
  (use-package (map 'package-find packages) package)
  package)

(define ((package-use-shadowing . shadowing-packages) package)
  "Each of shadowing-package's shadowing-symbols is shadowing-imported-from before being used by package.
Returns a closure that takes/returns the package to modify."
  (map (lambda (shadowing-package)
	 (let ((shadowing-symbols (package-shadowing-symbols shadowing-package)))
	   (shadowing-import shadowing-symbols package)
	   (use-package shadowing-package package)))
       (map 'package-find shadowing-packages))
  package)

(define ((package-re-export-shadowing . re-exported-packages) package)
  "Each of the re-exported-package's exported symbols are exported from package if they are already interned in package.
If the symbol is a shadowing symbol, it is first shadowed in package.
Returns a closure that takes/returns the package to modify."
  (map (lambda (re-exported-package)
	 (let ((external-symbols-in-package (package-external-symbols-from package re-exported-package)))
	   (when (empty? external-symbols-in-package)
	     (warn "PACKAGE-RE-EXPORT-SHADOWING: package ~S does not have any symbols internal to ~S.
This package may need to be USEd before it can be re-exported." (package-name re-exported-package) (package-name package)))
	   ;; Shadow only the symbols shadowed in re-exported-package
	   (shadow (intersection (package-shadowing-symbols re-exported-package)
				 external-symbols-in-package)
		   package)
	   ;; Export all external symbols
	   (export external-symbols-in-package package)))
       (map 'package-find re-exported-packages))
  package)

(define ((document documentation) package)
  "Sets the package documentation for package. Returns a closure that takes and returns the package."
  (setf (documentation package t) documentation)
  package)

(define ((package-export . symbols) package)
  "Interns and exports each symbol-name in symbols.
Returns a closure that takes and returns the package."
  (export (symbols-interned-in-package symbols package) (package-find package))
  package)

(define ((package-shadow . symbols) package)
  "Shadows symbols in package.
Returns a closure that takes and returns the package."
  (shadow symbols (package-find package))
  package)

(define (package-shadowing-export . symbols)
  "Shadows and exports symbols in package.
Returns a closure that takes and returns the package."
  (compose
   (apply #'package-export symbols)
   (apply #'package-shadow symbols)))

(define ((package-import-from from-package . symbols) package)
  "Imports symbols from from-package into package.
Returns a closure that takes and returns the package."
  (import (symbols-interned-in-package symbols (package-find from-package)) package)
  package)

(define ((package-shadowing-import-from from-package . symbols) package)
  "Shadowing-imports symbols from from-package into package.
Returns a closure that takes and returns the package."
  (shadowing-import (symbols-interned-in-package symbols from-package) (package-find package))
  package)

(define ((nickname-package . nicknames) package)
  "Gives new nicknames to package, in addition to existing nicknames.
Returns a closure that takes and returns the package."
  (rename-package package (package-name package)
		  (remove-duplicates (append nicknames (package-nicknames package))))
  package)

(define (unique-package-name)
  "Returns a package-name that is most likely unique."
  (symbol-name (gensym "SCHEMEISH-TEMPORARY-PACKAGE")))

(define (extend-package* package package-fns)
  "Applies package-fns to construct a package from base-package package.
Package-fns are applied from left to right."
  [(apply 'compose (reverse package-fns)) (package-find package)])
(define (extend-package package . package-fns)
  "See EXTEND-PACKAGE*"
  (extend-package* (package-find package) package-fns))

(define (ensure-string symbol-or-string)
  "Returns the string or the name of the symbol."
  (if (symbol? symbol-or-string) (symbol-name symbol-or-string) symbol-or-string))

(define (uninterned symbol-or-string)
  "Return an uninterned symbol with the same name as symbol."
  (make-symbol (ensure-string symbol-or-string)))

(define (package-import-froms package-designator)
  "List of the (:import-from package-name . symbols) forms for all of the
imported, non-shadowing symbols in package. Intended for a defpackage form."
  (let ((package (package-find package-designator)))
    (alist-map (sort (group-by-package (intersection
					(intersection (package-non-shadowing-symbols package)
						      (package-imported-symbols package))
					(package-unused-symbols package)))
		     #'string<
		     :extract-key 'car)
	       (lambda (package-name symbol-names)
		 (list* :import-from
			(make-symbol package-name)
			(map 'make-symbol (sort symbol-names #'string<)))))))

(define (package-shadowing-import-froms package-designator)
  "List of the (:shadqowing-import-from package-name . symbols) forms for all of the
shadowing-imported symbols in package. Intended for a defpackage form."
  (let ((package (package-find package-designator)))
    (alist-map (sort (group-by-package (intersection (package-imported-symbols package)
						     (package-shadowing-symbols package)))
		     #'string<
		     :extract-key 'car)
	       (lambda (package-name symbol-names)
		 (list* :shadowing-import-from
			(make-symbol package-name)
			(map 'make-symbol (sort symbol-names #'string<)))))))

(define (symbol< s1 s2)
  (string< (symbol-name s1) (symbol-name s2)))

(define (defpackage-form package (symbol-name (make-symbol (package-name package))))
  "Construct a defpackage form that constructs an equivalent package."
  (let ((package (find-package package)))
    (let ((documentation (documentation package t))
	  (use-list (map (compose 'make-symbol 'package-name) (package-use-list package)))
	  (exports (map 'uninterned (package-external-symbols package)))
	  (shadow (map 'uninterned (set-difference (package-shadowing-symbols package)
						   (package-imported-symbols package))))
	  (nicknames (map 'make-symbol (package-nicknames package))))
      `(cl:defpackage ,(uninterned symbol-name)
	 ,@(WHEN documentation `((:documentation ,documentation)))
	 ,@(package-import-froms package)
	 ,@(package-shadowing-import-froms package)
	 ,@(when use-list `((:use ,@(sort use-list #'symbol<))))
	 ,@(when exports `((:export ,@(sort exports #'symbol<))))
	 ,@(when shadow `((:shadow ,@(sort shadow #'symbol<))))
	 ,@(when nicknames `((:nicknames ,@(sort nicknames #'symbol<))))))))

(defmacro with-temporary-package (package-name &body body)
  "Construct a temporary package name and bind it to PACKAGE-NAME and wrap BODY in an unwind-protect.
The package is deleted in the cleanup form."
  `(let ((,package-name (make-package (unique-package-name))))
     (unwind-protect (progn ,@body)
       (delete-package ,package-name))))

(define (define-package-form name . package-fns)
  "Constructs a temporary package using package functions, and returns the DEFPACKAGE form
for that package with the given name."
  (with-temporary-package package
    (defpackage-form (extend-package* package package-fns) [*resolve-package-designator* name])))

(define (define-package name . package-fns)
  "Constructs a temporary package using package functions, and evaluates the DEFPACKAGE form
for that package with the given name. Returns the DEFPACKAGE form that is evaluted"
  (let ((form (apply #'define-package-form name package-fns)))
    (eval form)
    form))

(define ((continue-with-warning datum . arguments) condition)
  "Provides a warning before invoking the continue restart."
  (let ((restart (find-restart 'continue condition)))
    (apply #'warn datum arguments)
    (invoke-restart restart)))

(defmacro handling-error (error-binding &body body)
  "Establish a handler-bind with ((error error-binding))."
  `(handler-bind ((error ,error-binding))
     ,@body))

(define (package-delete package-designator (forcefully? t))
  "Deletes package. If forcefully? is true, invokes the continue restart if an error occurs.
Returns the package-name."
  (let ((package (package-find package-designator)))
    (cond
      (package
       (let ((name (package-name package)))
	 (cond
	   (forcefully?
	    (handling-error (continue-with-warning "Forcefully deleting package ~S ~S" name package)
	      (delete-package package)))
	   (t (delete-package package)))
	 name))
      (t package-designator))))

(define (package-dependencies package)
  "Return a list of packages which package has a dependency on."
  (map (compose 'find-package 'car) (group-by-package (package-symbols package))))

(define (independent-package? package packages)
  "True if package does not have any dependencies on packages."
  (let ((dependencies (package-dependencies package)))
    (for-all (lambda (other-package) (not (member other-package dependencies)))
	     (remove package packages))))

(define (independent-packages packages)
  "Return a list of the packages in packages which are not dependent on other packages in packages."
  (let rec ((packages-to-check packages)
	    (independent-packages ()))
    (cond
      ((empty? packages-to-check) independent-packages)
      (t
       (rec (rest packages-to-check)
	    (if (independent-package? (first packages-to-check) packages)
		(cons (first packages-to-check) independent-packages)
		independent-packages))))))

(define (package-hierarchy packages)
  "Return a package hierarchy as a nested list of ((most-independent-packages...) ... (most-dependent-packages...)).
Cyclic dependencies throw an error."
  (let rec ((packages packages)
	    (result ()))
    (cond
      ((empty? packages) (nreverse result))
      (t (let ((independent-packages (independent-packages packages)))
	   (if (empty? independent-packages)
	       (error "Cyclic dependency in package hierarchy.")
	       (rec (set-difference packages independent-packages)
		    (cons independent-packages result))))))))

(define (hierarchical-defpackage-forms packages)
  "Return a list of defpackage forms which have been hierarchically arranged. Cyclic dependencies throw an error."
  (map 'defpackage-form (append* (package-hierarchy packages))))

(define (filter-packages predicate)
  "Return all packages that match predicate"
  (filter predicate (list-all-packages)))

(define (all-packages-with-string-prefix string-prefix)
  "Return all packages that match the given string-prefix"
  (filter-packages (compose (rcurry 'string-starts-with? string-prefix) 'package-name)))

(define (package-file-contents packages)
  "Returns the package.lisp contents organized hierarchically."
  (with-output-to-string (s)
    (format s ";;;; package.lisp~%~%")
    (for-each (lambda (form)
		(format s "~&~S~%~%" form))
	      (hierarchical-defpackage-forms packages))))



(uninstall-syntax!)

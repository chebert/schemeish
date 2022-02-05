(in-package #:schemeish.backend)

(install-syntax!)

;;; Strings

(export
 (define (string-append . strings)
   (apply 'concatenate 'string strings)))

(export
 (define (string-append* strings)
   "Applies string-append to strings."
   (apply #'string-append strings)))

(export
 (define string? #'stringp))

(export
 (define (string-starts-with? string sub-string)
   (and (>= (length string) (length sub-string))
	(string= (subseq string 0 (length sub-string))
		 sub-string))))

(export
 (define (chars-string char count)
   "Return a string with char repeated count times."
   (make-string count :initial-element char)))

(export
 (define (join-strings strings separator)
   "Joins strings with the character separator in between each pair of strings."
   (string-append* (intersperse (chars-string separator 1) strings))))

(export
 (define (string-empty? string)
   "True if string is empty."
   (zero? (length string))))

(export
 (define (split-string-if string split-char?)
   "Return a list of strings that have been split whenever split-char? is true.
Chars that satisfy split-char? will be removed, and empty strings will not be returned."
   (define (not-split-char? char) (not [split-char? char]))
   (define (first-char string) (aref string 0))

   (define (split-string-iter string result)
     (cond
       ((string-empty? string) result)
       ([split-char? (first-char string)]
	;; Remove initial split-char?
	(let ((start (position-if not-split-char? string)))
	  (cond
	    ((null? start) result)
	    (t (split-string-iter (subseq string start) result)))))
       (t
	(let ((end (position-if split-char? string)))
	  (cond
	    ((null? end) (cons string result))
	    (t (split-string-iter (subseq string (1+ end)) (cons (subseq string 0 end) result))))))))

   (nreverse (split-string-iter string ()))))

(export
 (define (split-string string split-char)
   "Splits the string on each occurrence of split-char in string."
   (split-string-if string (lcurry #'char= split-char))))

(assert (equal? (split-string-if "     the three     wise    men   joined hands in holy matrimony.    " (lcurry #'char= #\space))
		'("the" "three" "wise" "men" "joined" "hands" "in" "holy" "matrimony.")))


(export
 (define (string-for-each proc string)
   "Apply proc to each character in string."
   (define end (length string))
   (define (iter index)
     (when (< index end)
       [proc (aref string index)]
       (iter (1+ index))))
   (iter 0)))
(export
 (define (string-map proc string)
   "Applies proc to each character in string, returning a new string 
of the results appended together. Proc is expected to return a character or string"
   (with-output-to-string (s)
     (string-for-each (lambda (char)
			(format s "~A" [proc char]))
		      string))))

(export
 (define (string->list string)
   (coerce string 'list)))
(export
 (define (list->string list)
   (coerce list 'string)))

(uninstall-syntax!)

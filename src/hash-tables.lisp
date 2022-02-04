(in-package #:schemeish.backend)

(install-syntax!)

;;; Hash-tables

(export
 (define (hash-ref table key (failure-result))
   "Returns the value associated with key in the hash-table table or failure-result."
   (multiple-value-bind (value present?) (gethash key table)
     (if present?
	 value
	 failure-result))))
(export
 (define (hash-set! table key value)
   "Sets the value associated with key in hash-table table to value. Returns the value."
   (setf (gethash key table) value)))
(export
 (define (hash-find-keyf table predicate (failure-result))
   "Returns the first key that satisfies [predicate key] in table."
   (loop for key being the hash-keys in table
	 do (when [predicate key]
	      (return-from hash-find-keyf key)))
   failure-result))

(export
 (define (hash-ref-default table key delayed-value)
   "Return the value associated with key in table.
If there is no value, computes [delayed-value] and stores it in the table
before returning it."
   (let* ((no-value (gensym))
	  (value (hash-ref table key no-value)))
     (if (eq? no-value value)
	 (hash-set! table key [delayed-value])
	 value))))

(export
 (define (hash-update! table key updater (failure-result))
   "Updates the value in table associated with key using [updater value].
If no value is associated with key, failure-result is used instead."
   (hash-set! table key [updater (hash-ref table key failure-result)])))

(export
 (define (hash-map table proc)
   "Maps [proc key value] over the keys and values of table, producing a list as a result."
   (loop for key being the hash-keys of table using (hash-value value)
	 collecting [proc key value])))

(export
 (define (hash-keys table)
   "Returns a list of all of the keys in table."
   (loop for key being the hash-keys of table collecting key)))

(export
 (define (hash-values table)
   "Returns a list of all of the values in table."
   (loop for value being the hash-values of table collecting value)))

(export
 (define (hash->alist table)
   "Return an alist representation of the key/value pairs in table."
   (hash-map table #'cons)))

(export
 (define (hash-for-each table proc)
   "Apply [proc key value] to each key/value pair in table."
   (maphash proc table)))

(export
 (define (hash-remove! table key)
   "Removes key and associated value from table"
   (remhash key table)))
(export
 (define (hash-clear! table)
   "Remvoes all keys and values from table."
   (clrhash table)))

(uninstall-syntax!)

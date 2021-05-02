;;;; package.lisp

(DEFPACKAGE #:SCHEMEISH.PACKAGE-DEFINITIONS
  (:DOCUMENTATION "Source of all of the package definitions in SCHEMEISH.
Provides write-package-file! which writes the current schemeish-packages to a file.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.SCHEMEISH
                          #:*
                          #:**
                          #:***
                          #:+
                          #:++
                          #:+++
                          #:-
                          #:/
                          #://
                          #:///)
  (:USE #:SCHEMEISH.PACKAGE-UTILS #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:SCHEMEISH-PACKAGES #:WRITE-PACKAGE-FILE!))

(DEFPACKAGE #:SCHEMEISH.HTML
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.SCHEMEISH
                          #:*
                          #:**
                          #:***
                          #:+
                          #:++
                          #:+++
                          #:-
                          #:/
                          #://
                          #:///)
  (:USE #:SCHEMEISH.SCHEMEISH))

(DEFPACKAGE #:SCHEMEISH.SCHEMEISH
  (:DOCUMENTATION "Provides everything in the schemeish-library. Re-exports CL so that packates can (:use #:schemeish) instead of (:use #:cl)")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BASE
        #:SCHEMEISH.BUNDLE
        #:SCHEMEISH.CUT
        #:SCHEMEISH.DEFINE-STRUCT
        #:SCHEMEISH.LEXICALLY
        #:SCHEMEISH.PACKAGE-UTILS
        #:SCHEMEISH.QUEUE
        #:SCHEMEISH.STREAM-COLLECT
        #:SCHEMEISH.STRUCT)
  (:EXPORT #:&ALLOW-OTHER-KEYS
           #:&AUX
           #:&BODY
           #:&ENVIRONMENT
           #:&KEY
           #:&OPTIONAL
           #:&REST
           #:&WHOLE
           #:*
           #:*BREAK-ON-SIGNALS*
           #:*COMPILE-FILE-PATHNAME*
           #:*COMPILE-FILE-TRUENAME*
           #:*COMPILE-PRINT*
           #:*COMPILE-VERBOSE*
           #:*DEBUG-IO*
           #:*DEBUGGER-HOOK*
           #:*DEFAULT-PATHNAME-DEFAULTS*
           #:*ERROR-OUTPUT*
           #:*FEATURES*
           #:*GENSYM-COUNTER*
           #:*LOAD-PATHNAME*
           #:*LOAD-PRINT*
           #:*LOAD-TRUENAME*
           #:*LOAD-VERBOSE*
           #:*MACROEXPAND-HOOK*
           #:*MODULES*
           #:*PACKAGE*
           #:*PRINT-ARRAY*
           #:*PRINT-BASE*
           #:*PRINT-CASE*
           #:*PRINT-CIRCLE*
           #:*PRINT-ESCAPE*
           #:*PRINT-GENSYM*
           #:*PRINT-LENGTH*
           #:*PRINT-LEVEL*
           #:*PRINT-LINES*
           #:*PRINT-MISER-WIDTH*
           #:*PRINT-PPRINT-DISPATCH*
           #:*PRINT-PRETTY*
           #:*PRINT-RADIX*
           #:*PRINT-READABLY*
           #:*PRINT-RIGHT-MARGIN*
           #:*QUERY-IO*
           #:*RANDOM-STATE*
           #:*READ-BASE*
           #:*READ-DEFAULT-FLOAT-FORMAT*
           #:*READ-EVAL*
           #:*READ-SUPPRESS*
           #:*READTABLE*
           #:*RESOLVE-PACKAGE-DESIGNATOR*
           #:*STANDARD-INPUT*
           #:*STANDARD-OUTPUT*
           #:*TERMINAL-IO*
           #:*THE-EMPTY-STREAM*
           #:*TRACE-OUTPUT*
           #:+
           #:-
           #:/
           #:/=
           #:1+
           #:1-
           #:<
           #:<=
           #:=
           #:>
           #:>=
           #:ABORT
           #:ABS
           #:ACONS
           #:ACOS
           #:ACOSH
           #:ADD-METHOD
           #:ADJOIN
           #:ADJUST-ARRAY
           #:ADJUSTABLE-ARRAY-P
           #:ALIST
           #:ALIST-HAS-KEY?
           #:ALIST-KEYS
           #:ALIST-MAP
           #:ALIST-REF
           #:ALIST-REMOVE
           #:ALIST-SET
           #:ALIST-SET*
           #:ALIST-UPDATE
           #:ALIST-VALUES
           #:ALL-PACKAGES-WITH-STRING-PREFIX
           #:ALLOCATE-INSTANCE
           #:ALPHA-CHAR-P
           #:ALPHANUMERICP
           #:AND
           #:AND-LET*
           #:ANDMAP
           #:APPEND
           #:APPEND*
           #:APPEND-MAP
           #:APPLY
           #:APROPOS
           #:APROPOS-LIST
           #:AREF
           #:ARITHMETIC-ERROR
           #:ARITHMETIC-ERROR-OPERANDS
           #:ARITHMETIC-ERROR-OPERATION
           #:ARRAY
           #:ARRAY-DIMENSION
           #:ARRAY-DIMENSION-LIMIT
           #:ARRAY-DIMENSIONS
           #:ARRAY-DISPLACEMENT
           #:ARRAY-ELEMENT-TYPE
           #:ARRAY-HAS-FILL-POINTER-P
           #:ARRAY-IN-BOUNDS-P
           #:ARRAY-RANK
           #:ARRAY-RANK-LIMIT
           #:ARRAY-ROW-MAJOR-INDEX
           #:ARRAY-TOTAL-SIZE
           #:ARRAY-TOTAL-SIZE-LIMIT
           #:ARRAYP
           #:ASH
           #:ASIN
           #:ASINH
           #:ASSERT
           #:ASSOC
           #:ASSOC-IF
           #:ASSOC-IF-NOT
           #:ATAN
           #:ATANH
           #:ATOM
           #:BASE-CHAR
           #:BASE-STRING
           #:BIGNUM
           #:BIT
           #:BIT-AND
           #:BIT-ANDC1
           #:BIT-ANDC2
           #:BIT-EQV
           #:BIT-IOR
           #:BIT-NAND
           #:BIT-NOR
           #:BIT-NOT
           #:BIT-ORC1
           #:BIT-ORC2
           #:BIT-VECTOR
           #:BIT-VECTOR-P
           #:BIT-XOR
           #:BLOCK
           #:BOOLE
           #:BOOLE-1
           #:BOOLE-2
           #:BOOLE-AND
           #:BOOLE-ANDC1
           #:BOOLE-ANDC2
           #:BOOLE-C1
           #:BOOLE-C2
           #:BOOLE-CLR
           #:BOOLE-EQV
           #:BOOLE-IOR
           #:BOOLE-NAND
           #:BOOLE-NOR
           #:BOOLE-ORC1
           #:BOOLE-ORC2
           #:BOOLE-SET
           #:BOOLE-XOR
           #:BOOLEAN
           #:BOTH-CASE-P
           #:BOUNDP
           #:BREAK
           #:BROADCAST-STREAM
           #:BROADCAST-STREAM-STREAMS
           #:BUILT-IN-CLASS
           #:BUNDLE
           #:BUNDLE-DOCUMENTATION
           #:BUNDLE-PERMISSIONS
           #:BUNDLE?
           #:BUTLAST
           #:BYTE
           #:BYTE-POSITION
           #:BYTE-SIZE
           #:CAAAAR
           #:CAAADR
           #:CAAAR
           #:CAADAR
           #:CAADDR
           #:CAADR
           #:CAAR
           #:CADAAR
           #:CADADR
           #:CADAR
           #:CADDAR
           #:CADDDR
           #:CADDR
           #:CADR
           #:CALL-ARGUMENTS-LIMIT
           #:CALL-METHOD
           #:CALL-NEXT-METHOD
           #:CAR
           #:CASE
           #:CATCH
           #:CCASE
           #:CDAAAR
           #:CDAADR
           #:CDAAR
           #:CDADAR
           #:CDADDR
           #:CDADR
           #:CDAR
           #:CDDAAR
           #:CDDADR
           #:CDDAR
           #:CDDDAR
           #:CDDDDR
           #:CDDDR
           #:CDDR
           #:CDR
           #:CEILING
           #:CELL-ERROR
           #:CELL-ERROR-NAME
           #:CERROR
           #:CHANGE-CLASS
           #:CHAR
           #:CHAR-CODE
           #:CHAR-CODE-LIMIT
           #:CHAR-DOWNCASE
           #:CHAR-EQUAL
           #:CHAR-GREATERP
           #:CHAR-INT
           #:CHAR-LESSP
           #:CHAR-NAME
           #:CHAR-NOT-EQUAL
           #:CHAR-NOT-GREATERP
           #:CHAR-NOT-LESSP
           #:CHAR-UPCASE
           #:CHAR/=
           #:CHAR<
           #:CHAR<=
           #:CHAR=
           #:CHAR>
           #:CHAR>=
           #:CHARACTER
           #:CHARACTERP
           #:CHECK-TYPE
           #:CIS
           #:CLASS
           #:CLASS-NAME
           #:CLASS-OF
           #:CLEAR-INPUT
           #:CLEAR-OUTPUT
           #:CLOSE
           #:CLRHASH
           #:CODE-CHAR
           #:COERCE
           #:COMPILATION-SPEED
           #:COMPILE
           #:COMPILE-FILE
           #:COMPILE-FILE-PATHNAME
           #:COMPILED-FUNCTION
           #:COMPILED-FUNCTION-P
           #:COMPILER-MACRO
           #:COMPILER-MACRO-FUNCTION
           #:COMPLEMENT
           #:COMPLEX
           #:COMPLEXP
           #:COMPOSE
           #:COMPOSE*
           #:COMPUTE-APPLICABLE-METHODS
           #:COMPUTE-RESTARTS
           #:CONCATENATE
           #:CONCATENATED-STREAM
           #:CONCATENATED-STREAM-STREAMS
           #:COND
           #:CONDITION
           #:CONJOIN
           #:CONJOIN*
           #:CONJUGATE
           #:CONS
           #:CONSP
           #:CONST
           #:CONSTANTLY
           #:CONSTANTP
           #:CONTINUE
           #:CONTROL-ERROR
           #:COPY-ALIST
           #:COPY-LIST
           #:COPY-PPRINT-DISPATCH
           #:COPY-READTABLE
           #:COPY-SEQ
           #:COPY-STRUCTURE
           #:COPY-SYMBOL
           #:COPY-TREE
           #:COS
           #:COSH
           #:COUNT
           #:COUNT-IF
           #:COUNT-IF-NOT
           #:CTYPECASE
           #:CUT
           #:DEBUG
           #:DECF
           #:DECLAIM
           #:DECLARATION
           #:DECLARE
           #:DECODE-FLOAT
           #:DECODE-UNIVERSAL-TIME
           #:DEFCLASS
           #:DEFCONSTANT
           #:DEFGENERIC
           #:DEFINE
           #:DEFINE-COMPILER-MACRO
           #:DEFINE-CONDITION
           #:DEFINE-METHOD-COMBINATION
           #:DEFINE-MODIFY-MACRO
           #:DEFINE-PACKAGE
           #:DEFINE-PACKAGE-FORM
           #:DEFINE-SETF-EXPANDER
           #:DEFINE-STRUCT
           #:DEFINE-SYMBOL-MACRO
           #:DEFMACRO
           #:DEFMETHOD
           #:DEFPACKAGE
           #:DEFPACKAGE-FORM
           #:DEFPARAMETER
           #:DEFSETF
           #:DEFSTRUCT
           #:DEFTYPE
           #:DEFUN
           #:DEFVAR
           #:DEGREES->RADIANS
           #:DELAY
           #:DELETE
           #:DELETE-DUPLICATES
           #:DELETE-FILE
           #:DELETE-IF
           #:DELETE-IF-NOT
           #:DELETE-PACKAGE
           #:DENOMINATOR
           #:DEPOSIT-FIELD
           #:DESCRIBE
           #:DESCRIBE-OBJECT
           #:DESTRUCTURING-BIND
           #:DIGIT-CHAR
           #:DIGIT-CHAR-P
           #:DIRECTORY
           #:DIRECTORY-NAMESTRING
           #:DISASSEMBLE
           #:DISJOIN
           #:DISJOIN*
           #:DISPLAY
           #:DISPLAYLN
           #:DIVISION-BY-ZERO
           #:DO
           #:DO*
           #:DO-ALL-SYMBOLS
           #:DO-EXTERNAL-SYMBOLS
           #:DO-SYMBOLS
           #:DOCUMENT
           #:DOCUMENT!
           #:DOCUMENTATION
           #:DOLIST
           #:DOTIMES
           #:DOUBLE-FLOAT
           #:DOUBLE-FLOAT-EPSILON
           #:DOUBLE-FLOAT-NEGATIVE-EPSILON
           #:DPB
           #:DRIBBLE
           #:DROP
           #:DROPF
           #:DYNAMIC-EXTENT
           #:ECASE
           #:ECHO-STREAM
           #:ECHO-STREAM-INPUT-STREAM
           #:ECHO-STREAM-OUTPUT-STREAM
           #:ED
           #:EIGHTH
           #:ELT
           #:EMPTY?
           #:ENCODE-UNIVERSAL-TIME
           #:END-OF-FILE
           #:ENDP
           #:ENOUGH-NAMESTRING
           #:ENSURE-DIRECTORIES-EXIST
           #:ENSURE-GENERIC-FUNCTION
           #:ENSURE-STRING
           #:EQ
           #:EQ?
           #:EQL
           #:EQUAL
           #:EQUAL?
           #:EQUALP
           #:ERROR
           #:ETYPECASE
           #:EVAL
           #:EVAL-WHEN
           #:EVEN?
           #:EVENP
           #:EVERY
           #:EXP
           #:EXPAND-FUNCTION-BODY
           #:EXPAND-TOP-LEVEL-DEFINE
           #:EXPORT
           #:EXPOSE
           #:EXPT
           #:EXTEND-PACKAGE
           #:EXTEND-PACKAGE*
           #:EXTENDED-CHAR
           #:FBOUNDP
           #:FCEILING
           #:FDEFINITION
           #:FFLOOR
           #:FIFTH
           #:FILE-AUTHOR
           #:FILE-ERROR
           #:FILE-ERROR-PATHNAME
           #:FILE-LENGTH
           #:FILE-NAMESTRING
           #:FILE-POSITION
           #:FILE-STREAM
           #:FILE-STRING-LENGTH
           #:FILE-WRITE-DATE
           #:FILL
           #:FILL-POINTER
           #:FILTER
           #:FILTER-MAP
           #:FILTER-NOT
           #:FILTER-PACKAGES
           #:FIND
           #:FIND-ALL-SYMBOLS
           #:FIND-CLASS
           #:FIND-IF
           #:FIND-IF-NOT
           #:FIND-METHOD
           #:FIND-PACKAGE
           #:FIND-RESTART
           #:FIND-SYMBOL
           #:FINDF
           #:FINISH-OUTPUT
           #:FIRST
           #:FIXNUM
           #:FLATTEN
           #:FLET
           #:FLOAT
           #:FLOAT-DIGITS
           #:FLOAT-PRECISION
           #:FLOAT-RADIX
           #:FLOAT-SIGN
           #:FLOATING-POINT-INEXACT
           #:FLOATING-POINT-INVALID-OPERATION
           #:FLOATING-POINT-OVERFLOW
           #:FLOATING-POINT-UNDERFLOW
           #:FLOATP
           #:FLOOR
           #:FMAKUNBOUND
           #:FOLDL
           #:FOLDR
           #:FOR-ALL
           #:FOR-ALL*
           #:FOR-EACH
           #:FOR-MACROS
           #:FORCE
           #:FORCE-OUTPUT
           #:FORMAT
           #:FORMATTER
           #:FOURTH
           #:FRESH-LINE
           #:FROUND
           #:FTRUNCATE
           #:FTYPE
           #:FUNCALL
           #:FUNCTION
           #:FUNCTION-KEYWORDS
           #:FUNCTION-LAMBDA-EXPRESSION
           #:FUNCTIONP
           #:GCD
           #:GENERIC-FUNCTION
           #:GENSYM
           #:GENTEMP
           #:GET
           #:GET-DECODED-TIME
           #:GET-DISPATCH-MACRO-CHARACTER
           #:GET-INTERNAL-REAL-TIME
           #:GET-INTERNAL-RUN-TIME
           #:GET-MACRO-CHARACTER
           #:GET-OUTPUT-STREAM-STRING
           #:GET-PROPERTIES
           #:GET-SETF-EXPANSION
           #:GET-UNIVERSAL-TIME
           #:GETF
           #:GETHASH
           #:GO
           #:GRAPHIC-CHAR-P
           #:GROUP
           #:GROUP-BY-PACKAGE
           #:HANDLER-BIND
           #:HANDLER-CASE
           #:HAS-SPECIFIC-ARITY?
           #:HASH-FIND-KEYF
           #:HASH-REF
           #:HASH-SET!
           #:HASH-TABLE
           #:HASH-TABLE-COUNT
           #:HASH-TABLE-P
           #:HASH-TABLE-REHASH-SIZE
           #:HASH-TABLE-REHASH-THRESHOLD
           #:HASH-TABLE-SIZE
           #:HASH-TABLE-TEST
           #:HIERARCHICAL-DEFPACKAGE-FORMS
           #:HOST-NAMESTRING
           #:IDENTITY
           #:IF
           #:IGNORABLE
           #:IGNORE
           #:IGNORE-ARGS
           #:IGNORE-ERRORS
           #:IMAGPART
           #:IMPORT
           #:IN-PACKAGE
           #:INCF
           #:INDEPENDENT-PACKAGE?
           #:INDEPENDENT-PACKAGES
           #:INITIALIZE-INSTANCE
           #:INLINE
           #:INPUT-STREAM-P
           #:INSPECT
           #:INSTALL-SYNTAX!
           #:INTEGER
           #:INTEGER-DECODE-FLOAT
           #:INTEGER-LENGTH
           #:INTEGERP
           #:INTERACTIVE-STREAM-P
           #:INTERN
           #:INTERNAL-TIME-UNITS-PER-SECOND
           #:INTERSECTION
           #:INVALID-METHOD-ERROR
           #:INVOKE-DEBUGGER
           #:INVOKE-RESTART
           #:INVOKE-RESTART-INTERACTIVELY
           #:ISQRT
           #:KEYWORD
           #:KEYWORDP
           #:LABELS
           #:LAMBDA
           #:LAMBDA-LIST-KEYWORDS
           #:LAMBDA-PARAMETERS-LIMIT
           #:LAST
           #:LCM
           #:LCURRY
           #:LDB
           #:LDB-TEST
           #:LDIFF
           #:LEAST-NEGATIVE-DOUBLE-FLOAT
           #:LEAST-NEGATIVE-LONG-FLOAT
           #:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
           #:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
           #:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
           #:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
           #:LEAST-NEGATIVE-SHORT-FLOAT
           #:LEAST-NEGATIVE-SINGLE-FLOAT
           #:LEAST-POSITIVE-DOUBLE-FLOAT
           #:LEAST-POSITIVE-LONG-FLOAT
           #:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
           #:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
           #:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
           #:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
           #:LEAST-POSITIVE-SHORT-FLOAT
           #:LEAST-POSITIVE-SINGLE-FLOAT
           #:LENGTH
           #:LET
           #:LET*
           #:LEXICALLY
           #:LISP-IMPLEMENTATION-TYPE
           #:LISP-IMPLEMENTATION-VERSION
           #:LIST
           #:LIST*
           #:LIST->STREAM
           #:LIST-ALL-PACKAGES
           #:LIST-LENGTH
           #:LIST-REF
           #:LIST-SET
           #:LIST-TAIL
           #:LIST-UPDATE
           #:LIST?
           #:LISTEN
           #:LISTP
           #:LOAD
           #:LOAD-LOGICAL-PATHNAME-TRANSLATIONS
           #:LOAD-TIME-VALUE
           #:LOCALLY
           #:LOG
           #:LOGAND
           #:LOGANDC1
           #:LOGANDC2
           #:LOGBITP
           #:LOGCOUNT
           #:LOGEQV
           #:LOGICAL-PATHNAME
           #:LOGICAL-PATHNAME-TRANSLATIONS
           #:LOGIOR
           #:LOGNAND
           #:LOGNOR
           #:LOGNOT
           #:LOGORC1
           #:LOGORC2
           #:LOGTEST
           #:LOGXOR
           #:LONG-FLOAT
           #:LONG-FLOAT-EPSILON
           #:LONG-FLOAT-NEGATIVE-EPSILON
           #:LONG-SITE-NAME
           #:LOOP
           #:LOOP-FINISH
           #:LOWER-CASE-P
           #:MACHINE-INSTANCE
           #:MACHINE-TYPE
           #:MACHINE-VERSION
           #:MACRO-FUNCTION
           #:MACROEXPAND
           #:MACROEXPAND-1
           #:MACROLET
           #:MAKE-ARRAY
           #:MAKE-BROADCAST-STREAM
           #:MAKE-BUNDLE-PREDICATE
           #:MAKE-CONCATENATED-STREAM
           #:MAKE-CONDITION
           #:MAKE-DISPATCH-MACRO-CHARACTER
           #:MAKE-ECHO-STREAM
           #:MAKE-HASH-TABLE
           #:MAKE-INSTANCE
           #:MAKE-INSTANCES-OBSOLETE
           #:MAKE-KEYWORD
           #:MAKE-LIST
           #:MAKE-LOAD-FORM
           #:MAKE-LOAD-FORM-SAVING-SLOTS
           #:MAKE-METHOD
           #:MAKE-PACKAGE
           #:MAKE-PATHNAME
           #:MAKE-QUEUE
           #:MAKE-RANDOM-STATE
           #:MAKE-SEQUENCE
           #:MAKE-STRING
           #:MAKE-STRING-INPUT-STREAM
           #:MAKE-STRING-OUTPUT-STREAM
           #:MAKE-SYMBOL
           #:MAKE-SYNONYM-STREAM
           #:MAKE-TWO-WAY-STREAM
           #:MAKUNBOUND
           #:MAP
           #:MAP-INTO
           #:MAP-SUCCESSIVE
           #:MAPC
           #:MAPCAN
           #:MAPCAR
           #:MAPCON
           #:MAPHASH
           #:MAPL
           #:MAPLIST
           #:MASK-FIELD
           #:MAX
           #:MEMBER
           #:MEMBER-IF
           #:MEMBER-IF-NOT
           #:MEMF
           #:MEMO-PROC
           #:MERGE
           #:MERGE-PATHNAMES
           #:METHOD
           #:METHOD-COMBINATION
           #:METHOD-COMBINATION-ERROR
           #:METHOD-QUALIFIERS
           #:MIN
           #:MINUSP
           #:MISMATCH
           #:MOD
           #:MOST-NEGATIVE-DOUBLE-FLOAT
           #:MOST-NEGATIVE-FIXNUM
           #:MOST-NEGATIVE-LONG-FLOAT
           #:MOST-NEGATIVE-SHORT-FLOAT
           #:MOST-NEGATIVE-SINGLE-FLOAT
           #:MOST-POSITIVE-DOUBLE-FLOAT
           #:MOST-POSITIVE-FIXNUM
           #:MOST-POSITIVE-LONG-FLOAT
           #:MOST-POSITIVE-SHORT-FLOAT
           #:MOST-POSITIVE-SINGLE-FLOAT
           #:MUFFLE-WARNING
           #:MULTIPLE-VALUE-BIND
           #:MULTIPLE-VALUE-CALL
           #:MULTIPLE-VALUE-LIST
           #:MULTIPLE-VALUE-PROG1
           #:MULTIPLE-VALUE-SETQ
           #:MULTIPLE-VALUES-LIMIT
           #:NAME-CHAR
           #:NAMESTRING
           #:NAND
           #:NBUTLAST
           #:NCONC
           #:NEGATIVE?
           #:NEWLINE
           #:NEXT-METHOD-P
           #:NICKNAME-PACKAGE
           #:NIL
           #:NINTERSECTION
           #:NINTH
           #:NO-APPLICABLE-METHOD
           #:NO-NEXT-METHOD
           #:NOR
           #:NOT
           #:NOTANY
           #:NOTEVERY
           #:NOTINLINE
           #:NRECONC
           #:NREVERSE
           #:NSET-DIFFERENCE
           #:NSET-EXCLUSIVE-OR
           #:NSTRING-CAPITALIZE
           #:NSTRING-DOWNCASE
           #:NSTRING-UPCASE
           #:NSUBLIS
           #:NSUBST
           #:NSUBST-IF
           #:NSUBST-IF-NOT
           #:NSUBSTITUTE
           #:NSUBSTITUTE-IF
           #:NSUBSTITUTE-IF-NOT
           #:NTH
           #:NTH-VALUE
           #:NTHCDR
           #:NULL
           #:NULL?
           #:NUMBER
           #:NUMBER->STRING
           #:NUMBER?
           #:NUMBERP
           #:NUMERATOR
           #:NUNION
           #:ODD?
           #:ODDP
           #:OPEN
           #:OPEN-STREAM-P
           #:OPTIMIZE
           #:OR
           #:ORMAP
           #:OTHERWISE
           #:OUTPUT-STREAM-P
           #:PACKAGE
           #:PACKAGE-DELETE
           #:PACKAGE-DEPENDENCIES
           #:PACKAGE-ERROR
           #:PACKAGE-ERROR-PACKAGE
           #:PACKAGE-EXPORT
           #:PACKAGE-EXPORTED-SYMBOLS
           #:PACKAGE-EXTERNAL-SYMBOLS
           #:PACKAGE-EXTERNAL-SYMBOLS-FROM
           #:PACKAGE-FILE-CONTENTS
           #:PACKAGE-FIND
           #:PACKAGE-HIERARCHY
           #:PACKAGE-IMPORT-FROM
           #:PACKAGE-IMPORT-FROMS
           #:PACKAGE-IMPORTED-SYMBOLS
           #:PACKAGE-NAME
           #:PACKAGE-NICKNAMES
           #:PACKAGE-NON-SHADOWING-SYMBOLS
           #:PACKAGE-RE-EXPORT-SHADOWING
           #:PACKAGE-SHADOW
           #:PACKAGE-SHADOWING-EXPORT
           #:PACKAGE-SHADOWING-IMPORT-FROM
           #:PACKAGE-SHADOWING-IMPORT-FROMS
           #:PACKAGE-SHADOWING-SYMBOLS
           #:PACKAGE-SYMBOLS
           #:PACKAGE-UNUSED-SYMBOLS
           #:PACKAGE-USE
           #:PACKAGE-USE-LIST
           #:PACKAGE-USE-SHADOWING
           #:PACKAGE-USED-BY-LIST
           #:PACKAGE-USED-SYMBOLS
           #:PACKAGE?
           #:PACKAGEP
           #:PAIR?
           #:PAIRLIS
           #:PARSE-ERROR
           #:PARSE-INTEGER
           #:PARSE-NAMESTRING
           #:PARTITION
           #:PATHNAME
           #:PATHNAME-DEVICE
           #:PATHNAME-DIRECTORY
           #:PATHNAME-HOST
           #:PATHNAME-MATCH-P
           #:PATHNAME-NAME
           #:PATHNAME-TYPE
           #:PATHNAME-VERSION
           #:PATHNAMEP
           #:PEEK-CHAR
           #:PHASE
           #:PI
           #:PLUSP
           #:POP
           #:POSITION
           #:POSITION-IF
           #:POSITION-IF-NOT
           #:POSITIVE?
           #:PPRINT
           #:PPRINT-DISPATCH
           #:PPRINT-EXIT-IF-LIST-EXHAUSTED
           #:PPRINT-FILL
           #:PPRINT-INDENT
           #:PPRINT-LINEAR
           #:PPRINT-LOGICAL-BLOCK
           #:PPRINT-NEWLINE
           #:PPRINT-POP
           #:PPRINT-TAB
           #:PPRINT-TABULAR
           #:PRIN1
           #:PRIN1-TO-STRING
           #:PRINC
           #:PRINC-TO-STRING
           #:PRINT
           #:PRINT-NOT-READABLE
           #:PRINT-NOT-READABLE-OBJECT
           #:PRINT-OBJECT
           #:PRINT-UNREADABLE-OBJECT
           #:PROBE-FILE
           #:PROCEDURE-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-ALLOW-OTHER-KEYS?
           #:PROCEDURE-ARGUMENTS-KEY-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-OPTIONAL-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-REQUIRED-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-REST-ARGUMENT
           #:PROCEDURE-ARITY
           #:PROCEDURE?
           #:PROCLAIM
           #:PROG
           #:PROG*
           #:PROG1
           #:PROG2
           #:PROGN
           #:PROGRAM-ERROR
           #:PROGV
           #:PROVIDE
           #:PSETF
           #:PSETQ
           #:PUSH
           #:PUSHNEW
           #:QUEUE-DELETE!
           #:QUEUE-EMPTY?
           #:QUEUE-FRONT
           #:QUEUE-INSERT!
           #:QUEUE?
           #:QUOTE
           #:QUOTIENT
           #:RADIANS->DEGREES
           #:RANDOM
           #:RANDOM-STATE
           #:RANDOM-STATE-P
           #:RANDOM-STREAM
           #:RANGE
           #:RASSOC
           #:RASSOC-IF
           #:RASSOC-IF-NOT
           #:RATIO
           #:RATIONAL
           #:RATIONALIZE
           #:RATIONALP
           #:RCURRY
           #:READ
           #:READ-BYTE
           #:READ-CHAR
           #:READ-CHAR-NO-HANG
           #:READ-DELIMITED-LIST
           #:READ-FROM-STRING
           #:READ-LINE
           #:READ-PRESERVING-WHITESPACE
           #:READ-SEQUENCE
           #:READER-ERROR
           #:READTABLE
           #:READTABLE-CASE
           #:READTABLEP
           #:REAL
           #:REALP
           #:REALPART
           #:REDUCE
           #:REINITIALIZE-INSTANCE
           #:REM
           #:REMF
           #:REMHASH
           #:REMOVE
           #:REMOVE*
           #:REMOVE-DUPLICATES
           #:REMOVE-IF
           #:REMOVE-IF-NOT
           #:REMOVE-METHOD
           #:REMPROP
           #:REMQ
           #:REMQ*
           #:RENAME-FILE
           #:RENAME-PACKAGE
           #:REPLACE
           #:REQUIRE
           #:REST
           #:RESTART
           #:RESTART-BIND
           #:RESTART-CASE
           #:RESTART-NAME
           #:RETURN
           #:RETURN-FROM
           #:REVAPPEND
           #:REVERSE
           #:ROOM
           #:ROTATEF
           #:ROUND
           #:ROW-MAJOR-AREF
           #:RPLACA
           #:RPLACD
           #:SAFE-VECTOR-REF
           #:SAFETY
           #:SATISFIES
           #:SBIT
           #:SCALE-FLOAT
           #:SCHAR
           #:SEARCH
           #:SECOND
           #:SEQUENCE
           #:SERIOUS-CONDITION
           #:SET
           #:SET!
           #:SET->STREAM
           #:SET-ADD
           #:SET-CAR!
           #:SET-CDR!
           #:SET-COUNT
           #:SET-DIFFERENCE
           #:SET-DISPATCH-MACRO-CHARACTER
           #:SET-EMPTY?
           #:SET-EXCLUSIVE-OR
           #:SET-INTERSECT
           #:SET-MACRO-CHARACTER
           #:SET-MEMBER?
           #:SET-PPRINT-DISPATCH
           #:SET-REMOVE
           #:SET-SUBTRACT
           #:SET-SYNTAX-FROM-CHAR
           #:SET-UNION
           #:SET=?
           #:SETF
           #:SETQ
           #:SEVENTH
           #:SGN
           #:SHADOW
           #:SHADOWING-IMPORT
           #:SHARED-INITIALIZE
           #:SHIFTF
           #:SHORT-FLOAT
           #:SHORT-FLOAT-EPSILON
           #:SHORT-FLOAT-NEGATIVE-EPSILON
           #:SHORT-SITE-NAME
           #:SIGNAL
           #:SIGNED-BYTE
           #:SIGNUM
           #:SIMPLE-ARRAY
           #:SIMPLE-BASE-STRING
           #:SIMPLE-BIT-VECTOR
           #:SIMPLE-BIT-VECTOR-P
           #:SIMPLE-CONDITION
           #:SIMPLE-CONDITION-FORMAT-ARGUMENTS
           #:SIMPLE-CONDITION-FORMAT-CONTROL
           #:SIMPLE-ERROR
           #:SIMPLE-STRING
           #:SIMPLE-STRING-P
           #:SIMPLE-TYPE-ERROR
           #:SIMPLE-VECTOR
           #:SIMPLE-VECTOR-P
           #:SIMPLE-WARNING
           #:SIN
           #:SINGLE-FLOAT
           #:SINGLE-FLOAT-EPSILON
           #:SINGLE-FLOAT-NEGATIVE-EPSILON
           #:SINH
           #:SIXTH
           #:SLEEP
           #:SLOT-BOUNDP
           #:SLOT-EXISTS-P
           #:SLOT-MAKUNBOUND
           #:SLOT-MISSING
           #:SLOT-UNBOUND
           #:SLOT-VALUE
           #:SOFTWARE-TYPE
           #:SOFTWARE-VERSION
           #:SOME
           #:SORT
           #:SPACE
           #:SPECIAL
           #:SPECIAL-OPERATOR-P
           #:SPEED
           #:SPLIT-AT
           #:SPLITF-AT
           #:SQR
           #:SQRT
           #:STABLE-SORT
           #:STANDARD
           #:STANDARD-CHAR
           #:STANDARD-CHAR-P
           #:STANDARD-CLASS
           #:STANDARD-GENERIC-FUNCTION
           #:STANDARD-METHOD
           #:STANDARD-OBJECT
           #:STEP
           #:STORAGE-CONDITION
           #:STORE-VALUE
           #:STREAM
           #:STREAM->LIST
           #:STREAM-APPEND
           #:STREAM-CAR
           #:STREAM-CDR
           #:STREAM-COLLECT
           #:STREAM-CONS
           #:STREAM-DROP
           #:STREAM-ELEMENT-TYPE
           #:STREAM-EMPTY?
           #:STREAM-ERROR
           #:STREAM-ERROR-STREAM
           #:STREAM-EXTERNAL-FORMAT
           #:STREAM-FILTER
           #:STREAM-FIRST
           #:STREAM-FLATMAP
           #:STREAM-FLATTEN
           #:STREAM-FOLD
           #:STREAM-FOR-EACH
           #:STREAM-LENGTH
           #:STREAM-MAP
           #:STREAM-MAP-SUCCESSIVE
           #:STREAM-RANGE
           #:STREAM-REF
           #:STREAM-REST
           #:STREAM-TAKE
           #:STREAM?
           #:STREAMP
           #:STRING
           #:STRING-APPEND
           #:STRING-CAPITALIZE
           #:STRING-DOWNCASE
           #:STRING-EQUAL
           #:STRING-GREATERP
           #:STRING-LEFT-TRIM
           #:STRING-LESSP
           #:STRING-NOT-EQUAL
           #:STRING-NOT-GREATERP
           #:STRING-NOT-LESSP
           #:STRING-RIGHT-TRIM
           #:STRING-STARTS-WITH?
           #:STRING-STREAM
           #:STRING-TRIM
           #:STRING-UPCASE
           #:STRING/=
           #:STRING<
           #:STRING<=
           #:STRING=
           #:STRING>
           #:STRING>=
           #:STRING?
           #:STRINGP
           #:STRUCT
           #:STRUCT->LIST
           #:STRUCT-ACCESSORS
           #:STRUCT-COPY
           #:STRUCT-FORM
           #:STRUCT?
           #:STRUCTURE
           #:STRUCTURE-CLASS
           #:STRUCTURE-OBJECT
           #:STYLE-WARNING
           #:SUBLIS
           #:SUBSEQ
           #:SUBSET?
           #:SUBSETP
           #:SUBST
           #:SUBST-IF
           #:SUBST-IF-NOT
           #:SUBSTITUTE
           #:SUBSTITUTE-IF
           #:SUBSTITUTE-IF-NOT
           #:SUBTYPEP
           #:SVREF
           #:SWAP-ARGS
           #:SXHASH
           #:SYMBOL
           #:SYMBOL->STRING
           #:SYMBOL-FUNCTION
           #:SYMBOL-IN-PACKAGE?
           #:SYMBOL-MACROLET
           #:SYMBOL-NAME
           #:SYMBOL-PACKAGE
           #:SYMBOL-PLIST
           #:SYMBOL-VALUE
           #:SYMBOL?
           #:SYMBOLP
           #:SYMBOLS-IN-PACKAGE
           #:SYMBOLS-INTERNED-IN-PACKAGE
           #:SYNONYM-STREAM
           #:SYNONYM-STREAM-SYMBOL
           #:T
           #:TAGBODY
           #:TAILP
           #:TAKE
           #:TAKEF
           #:TAN
           #:TANH
           #:TENTH
           #:TERPRI
           #:THE
           #:THERE-EXISTS
           #:THERE-EXISTS*
           #:THIRD
           #:THROW
           #:TIME
           #:TRACE
           #:TRANSLATE-LOGICAL-PATHNAME
           #:TRANSLATE-PATHNAME
           #:TREE-EQUAL
           #:TRUENAME
           #:TRUNCATE
           #:TWO-WAY-STREAM
           #:TWO-WAY-STREAM-INPUT-STREAM
           #:TWO-WAY-STREAM-OUTPUT-STREAM
           #:TYPE
           #:TYPE-ERROR
           #:TYPE-ERROR-DATUM
           #:TYPE-ERROR-EXPECTED-TYPE
           #:TYPE-OF
           #:TYPECASE
           #:TYPEP
           #:UNBOUND-SLOT
           #:UNBOUND-SLOT-INSTANCE
           #:UNBOUND-VARIABLE
           #:UNDEFINED-FUNCTION
           #:UNEXPORT
           #:UNINSTALL-SYNTAX!
           #:UNINTERN
           #:UNINTERNED
           #:UNION
           #:UNIQUE-PACKAGE-NAME
           #:UNIQUE-SYMBOL
           #:UNLESS
           #:UNREAD-CHAR
           #:UNSIGNED-BYTE
           #:UNTRACE
           #:UNUSE-PACKAGE
           #:UNWIND-PROTECT
           #:UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
           #:UPDATE-INSTANCE-FOR-REDEFINED-CLASS
           #:UPGRADED-ARRAY-ELEMENT-TYPE
           #:UPGRADED-COMPLEX-PART-TYPE
           #:UPPER-CASE-P
           #:USE-PACKAGE
           #:USE-VALUE
           #:USER-HOMEDIR-PATHNAME
           #:VALUES
           #:VALUES-LIST
           #:VARIABLE
           #:VECTOR
           #:VECTOR-POP
           #:VECTOR-PUSH
           #:VECTOR-PUSH-EXTEND
           #:VECTOR-REF
           #:VECTOR-SET!
           #:VECTORP
           #:WARN
           #:WARNING
           #:WHEN
           #:WILD-PATHNAME-P
           #:WITH-ACCESSORS
           #:WITH-COMPILATION-UNIT
           #:WITH-CONDITION-RESTARTS
           #:WITH-HASH-TABLE-ITERATOR
           #:WITH-INPUT-FROM-STRING
           #:WITH-OPEN-FILE
           #:WITH-OPEN-STREAM
           #:WITH-OUTPUT-TO-STRING
           #:WITH-PACKAGE-ITERATOR
           #:WITH-READABLE-SYMBOLS
           #:WITH-SIMPLE-RESTART
           #:WITH-SLOTS
           #:WITH-STANDARD-IO-SYNTAX
           #:WITH-TEMPORARY-PACKAGE
           #:WRITE
           #:WRITE-BYTE
           #:WRITE-CHAR
           #:WRITE-LINE
           #:WRITE-SEQUENCE
           #:WRITE-STRING
           #:WRITE-TO-STRING
           #:XOR
           #:Y-OR-N-P
           #:YES-OR-NO-P
           #:ZERO?
           #:ZEROP)
  (:SHADOW #:* #:** #:*** #:+ #:++ #:+++ #:- #:/ #:// #:///))

(DEFPACKAGE #:SCHEMEISH.PACKAGE-UTILS
  (:DOCUMENTATION "Provides tools for dealing with CL packages.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BASE
        #:SCHEMEISH.BUNDLE
        #:SCHEMEISH.DEFINE-STRUCT
        #:SCHEMEISH.QUEUE
        #:SCHEMEISH.STRUCT)
  (:EXPORT #:*RESOLVE-PACKAGE-DESIGNATOR*
           #:ALL-PACKAGES-WITH-STRING-PREFIX
           #:DEFINE-PACKAGE
           #:DEFINE-PACKAGE-FORM
           #:DEFPACKAGE-FORM
           #:DOCUMENT
           #:ENSURE-STRING
           #:EXTEND-PACKAGE
           #:EXTEND-PACKAGE*
           #:FILTER-PACKAGES
           #:GROUP-BY-PACKAGE
           #:HIERARCHICAL-DEFPACKAGE-FORMS
           #:INDEPENDENT-PACKAGE?
           #:INDEPENDENT-PACKAGES
           #:NICKNAME-PACKAGE
           #:PACKAGE-DELETE
           #:PACKAGE-DEPENDENCIES
           #:PACKAGE-EXPORT
           #:PACKAGE-EXPORTED-SYMBOLS
           #:PACKAGE-EXTERNAL-SYMBOLS
           #:PACKAGE-EXTERNAL-SYMBOLS-FROM
           #:PACKAGE-FILE-CONTENTS
           #:PACKAGE-FIND
           #:PACKAGE-HIERARCHY
           #:PACKAGE-IMPORT-FROM
           #:PACKAGE-IMPORT-FROMS
           #:PACKAGE-IMPORTED-SYMBOLS
           #:PACKAGE-NON-SHADOWING-SYMBOLS
           #:PACKAGE-RE-EXPORT-SHADOWING
           #:PACKAGE-SHADOW
           #:PACKAGE-SHADOWING-EXPORT
           #:PACKAGE-SHADOWING-IMPORT-FROM
           #:PACKAGE-SHADOWING-IMPORT-FROMS
           #:PACKAGE-SYMBOLS
           #:PACKAGE-UNUSED-SYMBOLS
           #:PACKAGE-USE
           #:PACKAGE-USE-SHADOWING
           #:PACKAGE-USED-SYMBOLS
           #:PACKAGE?
           #:SYMBOL-IN-PACKAGE?
           #:SYMBOLS-IN-PACKAGE
           #:SYMBOLS-INTERNED-IN-PACKAGE
           #:UNINTERNED
           #:UNIQUE-PACKAGE-NAME
           #:WITH-TEMPORARY-PACKAGE))

(DEFPACKAGE #:SCHEMEISH.QUEUE
  (:DOCUMENTATION "Provides a bundle-based implementation of a queue.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.AND-LET #:SCHEMEISH.BASE #:SCHEMEISH.BUNDLE)
  (:EXPORT #:MAKE-QUEUE
           #:QUEUE-DELETE!
           #:QUEUE-EMPTY?
           #:QUEUE-FRONT
           #:QUEUE-INSERT!
           #:QUEUE?))

(DEFPACKAGE #:SCHEMEISH.LEXICALLY
  (:DOCUMENTATION "Provides the lexically and expose macros.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BASE
        #:SCHEMEISH.EXPAND-LEXICALLY)
  (:EXPORT #:EXPOSE #:LEXICALLY))

(DEFPACKAGE #:SCHEMEISH.DEFINE-STRUCT
  (:DOCUMENTATION "Provides define-struct.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.AND-LET #:SCHEMEISH.BASE #:SCHEMEISH.STRUCT)
  (:EXPORT #:DEFINE-STRUCT))

(DEFPACKAGE #:SCHEMEISH.STREAM-COLLECT
  (:DOCUMENTATION "Provides the stream-collect macro.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.EXPAND-STREAM-COLLECT)
  (:EXPORT #:STREAM-COLLECT))

(DEFPACKAGE #:SCHEMEISH.STRUCT
  (:DOCUMENTATION "Provides the basis and expansions for define-struct.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.AND-LET #:SCHEMEISH.BASE)
  (:EXPORT #:STRUCT
           #:STRUCT->LIST
           #:STRUCT-ACCESSORS
           #:STRUCT-COPY
           #:STRUCT-FORM
           #:STRUCT?))

(DEFPACKAGE #:SCHEMEISH.BUNDLE
  (:DOCUMENTATION "Provides bundle and make-bundle-predicate for creating dispatch-style closures.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.AND-LET #:SCHEMEISH.BASE)
  (:EXPORT #:BUNDLE
           #:BUNDLE-DOCUMENTATION
           #:BUNDLE-PERMISSIONS
           #:BUNDLE?
           #:MAKE-BUNDLE-PREDICATE))

(DEFPACKAGE #:SCHEMEISH.EXPAND-LEXICALLY
  (:DOCUMENTATION "Provides tools to expand lexically/expose macros.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.AND-LET #:SCHEMEISH.BASE)
  (:EXPORT #:LEXICAL-BINDINGS
           #:LEXICAL-NAME->PARAMETER-NAME
           #:PARAMETER-NAME->LEXICAL-NAME
           #:PARAMETER-NAME?
           #:SPECIAL-FORM?
           #:SPECIAL?))

(DEFPACKAGE #:SCHEMEISH.CUT
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE)
  (:EXPORT #:CUT))

(DEFPACKAGE #:SCHEMEISH.EXPAND-STREAM-COLLECT
  (:DOCUMENTATION "Provides tools to expand a stream-collect macro form.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE)
  (:EXPORT #:STREAM-COLLECT-FORM))

(DEFPACKAGE #:SCHEMEISH.AND-LET
  (:DOCUMENTATION "Provides the and-let* macro.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE)
  (:EXPORT #:AND-LET*))

(DEFPACKAGE #:SCHEMEISH.BASE
  (:DOCUMENTATION "Provides many core functions and simple macros in addition to basic-syntax, including
  - symbols
  - lists
  - procedures
  - alists
  - sets
  - strings
  - output
  - mutation")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASIC-SYNTAX #:SCHEMEISH.DEFINE)
  (:EXPORT #:*THE-EMPTY-STREAM*
           #:ALIST
           #:ALIST-HAS-KEY?
           #:ALIST-KEYS
           #:ALIST-MAP
           #:ALIST-REF
           #:ALIST-REMOVE
           #:ALIST-SET
           #:ALIST-SET*
           #:ALIST-UPDATE
           #:ALIST-VALUES
           #:ANDMAP
           #:APPEND*
           #:APPEND-MAP
           #:COMPOSE
           #:COMPOSE*
           #:CONJOIN
           #:CONJOIN*
           #:CONST
           #:DEFINE
           #:DEGREES->RADIANS
           #:DELAY
           #:DISJOIN
           #:DISJOIN*
           #:DISPLAY
           #:DISPLAYLN
           #:DOCUMENT!
           #:DROP
           #:DROPF
           #:EMPTY?
           #:EQ?
           #:EQUAL?
           #:EVEN?
           #:EXPAND-FUNCTION-BODY
           #:EXPAND-TOP-LEVEL-DEFINE
           #:FILTER
           #:FILTER-MAP
           #:FILTER-NOT
           #:FINDF
           #:FLATTEN
           #:FOLDL
           #:FOLDR
           #:FOR-ALL
           #:FOR-ALL*
           #:FOR-EACH
           #:FOR-MACROS
           #:FORCE
           #:GROUP
           #:HAS-SPECIFIC-ARITY?
           #:HASH-FIND-KEYF
           #:HASH-REF
           #:HASH-SET!
           #:IGNORE-ARGS
           #:INSTALL-SYNTAX!
           #:LAMBDA
           #:LCURRY
           #:LET
           #:LIST->STREAM
           #:LIST-REF
           #:LIST-SET
           #:LIST-TAIL
           #:LIST-UPDATE
           #:LIST?
           #:MAKE-KEYWORD
           #:MAP
           #:MAP-SUCCESSIVE
           #:MEMF
           #:MEMO-PROC
           #:NAND
           #:NEGATIVE?
           #:NEWLINE
           #:NOR
           #:NULL?
           #:NUMBER->STRING
           #:NUMBER?
           #:ODD?
           #:ORMAP
           #:PAIR?
           #:PARTITION
           #:POSITIVE?
           #:PROCEDURE-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-ALLOW-OTHER-KEYS?
           #:PROCEDURE-ARGUMENTS-KEY-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-OPTIONAL-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-REQUIRED-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-REST-ARGUMENT
           #:PROCEDURE-ARITY
           #:PROCEDURE?
           #:QUOTIENT
           #:RADIANS->DEGREES
           #:RANDOM-STREAM
           #:RANGE
           #:RCURRY
           #:REMOVE*
           #:REMQ
           #:REMQ*
           #:SAFE-VECTOR-REF
           #:SET!
           #:SET->STREAM
           #:SET-ADD
           #:SET-CAR!
           #:SET-CDR!
           #:SET-COUNT
           #:SET-EMPTY?
           #:SET-INTERSECT
           #:SET-MEMBER?
           #:SET-REMOVE
           #:SET-SUBTRACT
           #:SET-UNION
           #:SET=?
           #:SGN
           #:SORT
           #:SPLIT-AT
           #:SPLITF-AT
           #:SQR
           #:STREAM
           #:STREAM->LIST
           #:STREAM-APPEND
           #:STREAM-CAR
           #:STREAM-CDR
           #:STREAM-CONS
           #:STREAM-DROP
           #:STREAM-EMPTY?
           #:STREAM-FILTER
           #:STREAM-FIRST
           #:STREAM-FLATMAP
           #:STREAM-FLATTEN
           #:STREAM-FOLD
           #:STREAM-FOR-EACH
           #:STREAM-LENGTH
           #:STREAM-MAP
           #:STREAM-MAP-SUCCESSIVE
           #:STREAM-RANGE
           #:STREAM-REF
           #:STREAM-REST
           #:STREAM-TAKE
           #:STREAM?
           #:STRING-APPEND
           #:STRING-STARTS-WITH?
           #:STRING?
           #:SUBSET?
           #:SWAP-ARGS
           #:SYMBOL->STRING
           #:SYMBOL?
           #:TAKE
           #:TAKEF
           #:THERE-EXISTS
           #:THERE-EXISTS*
           #:UNINSTALL-SYNTAX!
           #:UNIQUE-SYMBOL
           #:VECTOR-REF
           #:VECTOR-SET!
           #:WITH-READABLE-SYMBOLS
           #:XOR
           #:ZERO?)
  (:SHADOW #:MAP #:SORT #:STREAM))

(DEFPACKAGE #:SCHEMEISH.DEFINE
  (:DOCUMENTATION "Provides DEFINE. See DEFINE's docs for more details.")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.ARGUMENTS #:SCHEMEISH.BASIC-SYNTAX)
  (:EXPORT #:DEFINE
           #:DROPF
           #:EXPAND-FUNCTION-BODY
           #:EXPAND-TOP-LEVEL-DEFINE
           #:LAMBDA
           #:SPLITF-AT
           #:TAKEF
           #:UNIQUE-SYMBOL
           #:WITH-READABLE-SYMBOLS)
  (:SHADOW #:LAMBDA))

(DEFPACKAGE #:SCHEMEISH.BASIC-SYNTAX
  (:DOCUMENTATION "Provides some basic syntax of scheme: FOR-MACROS NAMED-LET, [] reader syntax")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.FOR-MACROS
        #:SCHEMEISH.NAMED-LET
        #:SCHEMEISH.SYNTAX)
  (:EXPORT #:FOR-MACROS
           #:INSTALL-SYNTAX!
           #:LET
           #:UNINSTALL-SYNTAX!
           #:UNIQUE-SYMBOL
           #:WITH-READABLE-SYMBOLS))

(DEFPACKAGE #:SCHEMEISH.SYNTAX
  (:DOCUMENTATION "Provides install/uninstall-syntax! for expanding [fn-value args...] => (funcall fn-value args...)")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:INSTALL-SYNTAX! #:UNINSTALL-SYNTAX!))

(DEFPACKAGE #:SCHEMEISH.ARGUMENTS
  (:DOCUMENTATION "Tools to translate scheme style argument lists to CL style argument lists.")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:ARG-LIST->LAMBDA-LIST))

(DEFPACKAGE #:SCHEMEISH.NAMED-LET
  (:DOCUMENTATION "Provides an optionally named LET which can be used to write a locally recursive form.")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:LET)
  (:SHADOW #:LET))

(DEFPACKAGE #:SCHEMEISH.FOR-MACROS
  (:DOCUMENTATION "Provides FOR-MACROS which expands to (EVAL-WHEN ...)")
  (:USE #:COMMON-LISP)
  (:EXPORT #:FOR-MACROS #:UNIQUE-SYMBOL #:WITH-READABLE-SYMBOLS))


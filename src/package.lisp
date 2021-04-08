;;;; package.lisp

(DEFPACKAGE #:SCHEMEISH.FOR-MACROS
  (:DOCUMENTATION "Provides FOR-MACROS which expands to (EVAL-WHEN ...)")
  (:USE #:COMMON-LISP)
  (:EXPORT #:FOR-MACROS))
(DEFPACKAGE #:SCHEMEISH.NAMED-LET
  (:DOCUMENTATION "Provides an optionally named LET which can be used to write a locally recursive form.")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:LET)
  (:SHADOW #:LET))
(DEFPACKAGE #:SCHEMEISH.ARGUMENTS
  (:DOCUMENTATION "Tools to translate scheme style argument lists to CL style argument lists.")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:ARG-LIST->LAMBDA-LIST))
(DEFPACKAGE #:SCHEMEISH.SYNTAX
  (:DOCUMENTATION "Provides install/uninstall-syntax! for expanding [fn-value args...] => (funcall fn-value args...)")
  (:USE #:COMMON-LISP #:SCHEMEISH.FOR-MACROS)
  (:EXPORT #:INSTALL-SYNTAX! #:UNINSTALL-SYNTAX!))
(DEFPACKAGE #:SCHEMEISH.BASIC-SYNTAX
  (:DOCUMENTATION "Provides some basic syntax of scheme: FOR-MACROS NAMED-LET, [] reader syntax")
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.FOR-MACROS
        #:SCHEMEISH.NAMED-LET
        #:SCHEMEISH.SYNTAX)
  (:EXPORT #:LET #:FOR-MACROS #:INSTALL-SYNTAX! #:UNINSTALL-SYNTAX!))
(DEFPACKAGE #:SCHEMEISH.EXPAND-DEFINE
  (:DOCUMENTATION "Tools to expand define and define-like forms.")
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:SCHEMEISH.ARGUMENTS #:SCHEMEISH.BASIC-SYNTAX #:COMMON-LISP)
  (:EXPORT #:EXPAND-FUNCTION-BODY #:EXPAND-TOP-LEVEL-DEFINE))
(DEFPACKAGE #:SCHEMEISH.DEFINE
  (:DOCUMENTATION "Provides DEFINE. See DEFINE's docs for more details.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASIC-SYNTAX #:SCHEMEISH.ARGUMENTS)
  (:EXPORT #:EXPAND-TOP-LEVEL-DEFINE #:LAMBDA #:DEFINE #:EXPAND-FUNCTION-BODY)
  (:SHADOW #:LAMBDA))
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
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASIC-SYNTAX #:SCHEMEISH.DEFINE)
  (:EXPORT #:CONJOIN*
           #:ALIST-REMOVE
           #:LAMBDA
           #:ZERO?
           #:SET!
           #:LCURRY
           #:MAP-SUCCESSIVE
           #:REMQ*
           #:REMOVE*
           #:EQ?
           #:DISPLAYLN
           #:SQR
           #:STREAM-CAR
           #:EMPTY?
           #:ALIST-HAS-KEY?
           #:STREAM-FIRST
           #:POSITIVE?
           #:SET-INTERSECT
           #:STREAM->LIST
           #:XOR
           #:CONST
           #:APPEND-MAP
           #:EQUAL?
           #:STREAM-FOLD
           #:TAKE
           #:FILTER
           #:STREAM-EMPTY?
           #:ALIST-SET*
           #:FOLDR
           #:CONJOIN
           #:MAP
           #:FILTER-MAP
           #:RADIANS->DEGREES
           #:STREAM-DROP
           #:SORT
           #:STRING-STARTS-WITH?
           #:LIST-REF
           #:NUMBER?
           #:STREAM-REST
           #:FOR-ALL
           #:SET->STREAM
           #:REMQ
           #:RANGE
           #:SET-UNION
           #:EXPAND-FUNCTION-BODY
           #:NUMBER->STRING
           #:PROCEDURE-ARGUMENTS-KEY-ARGUMENTS
           #:DISJOIN*
           #:STREAM
           #:FLATTEN
           #:ALIST-VALUES
           #:ODD?
           #:SYMBOL->STRING
           #:SET-REMOVE
           #:STREAM-CDR
           #:ALIST-SET
           #:LIST?
           #:FOR-MACROS
           #:PROCEDURE-ARGUMENTS-OPTIONAL-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-REQUIRED-ARGUMENTS
           #:FOR-EACH
           #:SET-SUBTRACT
           #:DISJOIN
           #:PARTITION
           #:DROP
           #:ORMAP
           #:ALIST-REF
           #:PAIR?
           #:FINDF
           #:PROCEDURE?
           #:SET-CDR!
           #:DISPLAY
           #:THERE-EXISTS*
           #:MEMF
           #:UNINSTALL-SYNTAX!
           #:NULL?
           #:SYMBOL?
           #:RANDOM-STREAM
           #:FOR-ALL*
           #:NEWLINE
           #:MAKE-KEYWORD
           #:LIST-TAIL
           #:ALIST-UPDATE
           #:STRING-APPEND
           #:SPLIT-AT
           #:SUBSET?
           #:ALIST-MAP
           #:EVEN?
           #:STREAM-MAP
           #:FORCE
           #:STREAM-CONS
           #:STREAM-REF
           #:STREAM-APPEND
           #:LIST-SET
           #:SET=?
           #:EXPAND-TOP-LEVEL-DEFINE
           #:GROUP
           #:SET-ADD
           #:STREAM-RANGE
           #:APPEND*
           #:ALIST-KEYS
           #:THERE-EXISTS
           #:STREAM-LENGTH
           #:PROCEDURE-ARGUMENTS
           #:STREAM-MAP-SUCCESSIVE
           #:FOLDL
           #:LIST->STREAM
           #:NOR
           #:LET
           #:NAND
           #:ALIST
           #:FILTER-NOT
           #:STREAM-FOR-EACH
           #:SWAP-ARGS
           #:PROCEDURE-ARGUMENTS-REST-ARGUMENT
           #:ANDMAP
           #:DOCUMENT!
           #:*THE-EMPTY-STREAM*
           #:SET-EMPTY?
           #:PROCEDURE-ARGUMENTS-ALLOW-OTHER-KEYS?
           #:DEGREES->RADIANS
           #:STREAM?
           #:STREAM-FILTER
           #:HAS-SPECIFIC-ARITY?
           #:NEGATIVE?
           #:STREAM-FLATMAP
           #:PROCEDURE-ARITY
           #:STREAM-TAKE
           #:QUOTIENT
           #:LIST-UPDATE
           #:RCURRY
           #:INSTALL-SYNTAX!
           #:DEFINE
           #:SGN
           #:DELAY
           #:SET-COUNT
           #:STRING?
           #:COMPOSE
           #:MEMO-PROC
           #:STREAM-FLATTEN
           #:SET-CAR!
           #:SET-MEMBER?)
  (:SHADOW #:MAP #:SORT #:STREAM))
(DEFPACKAGE #:SCHEMEISH.AND-LET
  (:DOCUMENTATION "Provides the and-let* macro.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:STREAM #:MAP)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE)
  (:EXPORT #:AND-LET*))
(DEFPACKAGE #:SCHEMEISH.EXPAND-STREAM-COLLECT
  (:DOCUMENTATION "Provides tools to expand a stream-collect macro form.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:MAP #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE)
  (:EXPORT #:STREAM-COLLECT-FORM))
(DEFPACKAGE #:SCHEMEISH.EXPAND-LEXICALLY
  (:DOCUMENTATION "Provides tools to expand lexically/expose macros.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:MAP #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.AND-LET)
  (:EXPORT #:SPECIAL?
           #:PARAMETER-NAME?
           #:LEXICAL-NAME->PARAMETER-NAME
           #:PARAMETER-NAME->LEXICAL-NAME
           #:LEXICAL-BINDINGS
           #:SPECIAL-FORM?))
(DEFPACKAGE #:SCHEMEISH.BUNDLE
  (:DOCUMENTATION "Provides bundle and make-bundle-predicate for creating dispatch-style closures.")
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-TOP-LEVEL-DEFINE
                #:DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:MAP #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.AND-LET)
  (:EXPORT #:BUNDLE-DOCUMENTATION
           #:BUNDLE?
           #:BUNDLE-LIST
           #:MAKE-BUNDLE-PREDICATE
           #:BUNDLE-PERMISSIONS
           #:BUNDLE))
(DEFPACKAGE #:SCHEMEISH.STRUCT
  (:DOCUMENTATION "Provides the basis and expansions for define-struct.")
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-TOP-LEVEL-DEFINE
                #:DEFINE)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:STREAM #:MAP #:SORT)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.AND-LET)
  (:EXPORT #:STRUCT-COPY
           #:STRUCT->LIST
           #:STRUCT-ACCESSORS
           #:STRUCT?
           #:STRUCT-FORM
           #:STRUCT))
(DEFPACKAGE #:SCHEMEISH.STREAM-COLLECT
  (:DOCUMENTATION "Provides the stream-collect macro.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:STREAM #:MAP)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.EXPAND-STREAM-COLLECT)
  (:EXPORT #:STREAM-COLLECT))
(DEFPACKAGE #:SCHEMEISH.DEFINE-STRUCT
  (:DOCUMENTATION "Provides define-struct.")
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:STREAM #:MAP #:SORT)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.AND-LET #:SCHEMEISH.STRUCT)
  (:EXPORT #:DEFINE-STRUCT))
(DEFPACKAGE #:SCHEMEISH.LEXICALLY
  (:DOCUMENTATION "Provides the lexically and expose macros.")
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:STREAM #:SORT #:MAP)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.BASE
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.EXPAND-LEXICALLY)
  (:EXPORT #:LEXICALLY #:EXPOSE))
(DEFPACKAGE #:SCHEMEISH.QUEUE
  (:DOCUMENTATION "Provides a bundle-based implementation of a queue.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-TOP-LEVEL-DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:USE #:COMMON-LISP #:SCHEMEISH.BASE #:SCHEMEISH.AND-LET #:SCHEMEISH.BUNDLE)
  (:EXPORT #:QUEUE-EMPTY?
           #:MAKE-QUEUE
           #:QUEUE?
           #:QUEUE-FRONT
           #:QUEUE-DELETE!
           #:QUEUE-INSERT!))
(DEFPACKAGE #:SCHEMEISH.SERIALIZE
  (:DOCUMENTATION "Provides the serialize function which can recursively serialize lisp data, bundles, and structs
into a form ready for EVAL.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:STREAM #:MAP)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:COMMON-LISP
        #:SCHEMEISH.BASE
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BUNDLE
        #:SCHEMEISH.STRUCT
        #:SCHEMEISH.DEFINE-STRUCT
        #:SCHEMEISH.QUEUE)
  (:EXPORT #:SERIALIZE))
(DEFPACKAGE #:SCHEMEISH.PACKAGE-UTILS
  (:DOCUMENTATION "Provides tools for dealing with CL packages.")
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-FUNCTION-BODY
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE)
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:SORT #:STREAM #:MAP)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:USE #:SCHEMEISH.BASE
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BUNDLE
        #:SCHEMEISH.STRUCT
        #:SCHEMEISH.DEFINE-STRUCT
        #:SCHEMEISH.QUEUE
        #:COMMON-LISP)
  (:EXPORT #:PACKAGE-FILE-CONTENTS
           #:*RESOLVE-PACKAGE-DESIGNATOR*
           #:PACKAGE-DEPENDENCIES
           #:DEFPACKAGE-FORM
           #:DOCUMENT
           #:ALL-PACKAGES-WITH-STRING-PREFIX
           #:ENSURE-STRING
           #:INDEPENDENT-PACKAGE?
           #:PACKAGE-FIND
           #:PACKAGE-DELETE
           #:SYMBOL-IN-PACKAGE?
           #:DEFINE-PACKAGE-FORM
           #:PACKAGE-SHADOWING-EXPORT
           #:WITH-TEMPORARY-PACKAGE
           #:PACKAGE-SHADOWING-IMPORT-FROMS
           #:UNIQUE-PACKAGE-NAME
           #:PACKAGE-HIERARCHY
           #:NICKNAME-PACKAGE
           #:PACKAGE-SHADOW
           #:PACKAGE-EXPORTED-SYMBOLS
           #:PACKAGE-SHADOWING-IMPORT-FROM
           #:PACKAGE-SYMBOLS
           #:INDEPENDENT-PACKAGES
           #:HIERARCHICAL-DEFPACKAGE-FORMS
           #:PACKAGE-IMPORT-FROMS
           #:PACKAGE-USED-SYMBOLS
           #:EXTEND-PACKAGE*
           #:PACKAGE-IMPORTED-SYMBOLS
           #:PACKAGE?
           #:PACKAGE-EXTERNAL-SYMBOLS-FROM
           #:UNINTERNED
           #:PACKAGE-RE-EXPORT-SHADOWING
           #:PACKAGE-UNUSED-SYMBOLS
           #:SYMBOLS-IN-PACKAGE
           #:PACKAGE-NON-SHADOWING-SYMBOLS
           #:DEFINE-PACKAGE
           #:FILTER-PACKAGES
           #:PACKAGE-USE-SHADOWING
           #:GROUP-BY-PACKAGE
           #:PACKAGE-EXPORT
           #:EXTEND-PACKAGE
           #:PACKAGE-IMPORT-FROM
           #:SYMBOLS-INTERNED-IN-PACKAGE
           #:PACKAGE-USE
           #:PACKAGE-EXTERNAL-SYMBOLS))
(DEFPACKAGE #:SCHEMEISH.SCHEMEISH
  (:DOCUMENTATION "Provides everything in the schemeish-library. Re-exports CL so that packates can (:use #:schemeish) instead of (:use #:cl)")
  (:IMPORT-FROM #:SCHEMEISH.SYNTAX
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!
                #:UNINSTALL-SYNTAX!
                #:INSTALL-SYNTAX!)
  (:IMPORT-FROM #:SCHEMEISH.DEFINE
                #:DEFINE
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:EXPAND-TOP-LEVEL-DEFINE
                #:EXPAND-FUNCTION-BODY
                #:DEFINE)
  (:IMPORT-FROM #:SCHEMEISH.FOR-MACROS #:FOR-MACROS #:FOR-MACROS)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:USE #:SCHEMEISH.SERIALIZE
        #:SCHEMEISH.LEXICALLY
        #:SCHEMEISH.DEFINE-STRUCT
        #:SCHEMEISH.STRUCT
        #:SCHEMEISH.QUEUE
        #:SCHEMEISH.BUNDLE
        #:SCHEMEISH.STREAM-COLLECT
        #:SCHEMEISH.AND-LET
        #:SCHEMEISH.BASE
        #:COMMON-LISP)
  (:EXPORT #:MACRO-FUNCTION
           #:READ-LINE
           #:LOOP
           #:DELETE-DUPLICATES
           #:CHAR<=
           #:VECTOR-POP
           #:GENSYM
           #:VALUES-LIST
           #:SYNONYM-STREAM
           #:STREAM->LIST
           #:BASE-CHAR
           #:FMAKUNBOUND
           #:CONJUGATE
           #:LEAST-NEGATIVE-SINGLE-FLOAT
           #:DESCRIBE-OBJECT
           #:EVAL-WHEN
           #:EVENP
           #:*TERMINAL-IO*
           #:COS
           #:STRING-RIGHT-TRIM
           #:SOME
           #:LOGNAND
           #:BOTH-CASE-P
           #:SIMPLE-BASE-STRING
           #:CDDAAR
           #:OTHERWISE
           #:BOOLE-NOR
           #:INTEGER-LENGTH
           #:FORCE
           #:PARSE-INTEGER
           #:LOGBITP
           #:BLOCK
           #:DOLIST
           #:CONCATENATED-STREAM
           #:EQ?
           #:ADD-METHOD
           #:BROADCAST-STREAM-STREAMS
           #:NUMBER?
           #:SIMPLE-ARRAY
           #:COPY-ALIST
           #:SIMPLE-VECTOR-P
           #:CADR
           #:WRITE-BYTE
           #:DISPLAYLN
           #:STREAM-LENGTH
           #:INTEGER
           #:SYMBOL?
           #:STYLE-WARNING
           #:LET
           #:/
           #:COUNT
           #:DISASSEMBLE
           #:NSET-DIFFERENCE
           #:STRING-NOT-GREATERP
           #:RESTART
           #:CDDAR
           #:SET-SUBTRACT
           #:BOUNDP
           #:CLASS-OF
           #:HASH-TABLE-P
           #:WITH-OPEN-FILE
           #:GET-OUTPUT-STREAM-STRING
           #:*PACKAGE*
           #:REMHASH
           #:FBOUNDP
           #:SET-CAR!
           #:BIT-AND
           #:TYPE-ERROR-DATUM
           #:BIT-ORC1
           #:STRING-STREAM
           #:DECODE-FLOAT
           #:STANDARD-CHAR-P
           #:*ERROR-OUTPUT*
           #:BASE-STRING
           #:FUNCTION
           #:READ-BYTE
           #:ALIST-VALUES
           #:MAKE-LIST
           #:MACROEXPAND
           #:MEMBER-IF-NOT
           #:NEXT-METHOD-P
           #:SATISFIES
           #:SYMBOL-PACKAGE
           #:ARRAY
           #:ALIST-KEYS
           #:WITH-OUTPUT-TO-STRING
           #:PROG1
           #:EQ
           #:BREAK
           #:MOST-POSITIVE-DOUBLE-FLOAT
           #:CHAR-UPCASE
           #:REDUCE
           #:CDR
           #:DISPLAY
           #:LEAST-POSITIVE-DOUBLE-FLOAT
           #:SUBSET?
           #:DELAY
           #:FINISH-OUTPUT
           #:UNTRACE
           #:MAKE-SYNONYM-STREAM
           #:WITH-COMPILATION-UNIT
           #:MOST-NEGATIVE-LONG-FLOAT
           #:REMOVE-METHOD
           #:ALIST-SET*
           #:MAPCAR
           #:CASE
           #:STREAM-ERROR
           #:*PRINT-RADIX*
           #:+
           #:SCHAR
           #:LOWER-CASE-P
           #:READ-CHAR
           #:LIST-UPDATE
           #:NO-APPLICABLE-METHOD
           #:NUNION
           #:SET-EXCLUSIVE-OR
           #:ISQRT
           #:NTHCDR
           #:DEFINE-STRUCT
           #:SIMPLE-STRING-P
           #:*MACROEXPAND-HOOK*
           #:MOST-POSITIVE-FIXNUM
           #:&WHOLE
           #:LIST*
           #:COMPILED-FUNCTION
           #:LAMBDA
           #:ODDP
           #:CALL-METHOD
           #:REQUIRE
           #:*
           #:NSUBSTITUTE-IF-NOT
           #:NSUBLIS
           #:METHOD-COMBINATION
           #:WITH-SIMPLE-RESTART
           #:PPRINT-EXIT-IF-LIST-EXHAUSTED
           #:PLUSP
           #:IGNORE
           #:ERROR
           #:PPRINT-POP
           #:SUBTYPEP
           #:ASH
           #:FILE-ERROR
           #:FLOOR
           #:FROUND
           #:BUILT-IN-CLASS
           #:CHAR-DOWNCASE
           #:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
           #:SUBSTITUTE-IF
           #:TRUNCATE
           #:INCF
           #:MAKE-SYMBOL
           #:MULTIPLE-VALUE-LIST
           #:ABORT
           #:BOOLE-1
           #:*PRINT-READABLY*
           #:DEBUG
           #:GET-DISPATCH-MACRO-CHARACTER
           #:DEFTYPE
           #:WRITE
           #:ZEROP
           #:STRUCT
           #:USER-HOMEDIR-PATHNAME
           #:LIST->STREAM
           #:LOGEQV
           #:DELETE-IF
           #:BOOLE-ORC2
           #:INVALID-METHOD-ERROR
           #:T
           #:FLOATING-POINT-OVERFLOW
           #:STRING<
           #:STREAM-RANGE
           #:CHAR-EQUAL
           #:RATIONALIZE
           #:GET
           #:STRUCT-FORM
           #:SLOT-BOUNDP
           #:LISTP
           #:MAPLIST
           #:DO*
           #:*PRINT-LEVEL*
           #:*COMPILE-FILE-TRUENAME*
           #:NOR
           #:FILE-LENGTH
           #:FORCE-OUTPUT
           #:LONG-FLOAT
           #:INPUT-STREAM-P
           #:CDAAR
           #:BOOLE-IOR
           #:ASIN
           #:NOTINLINE
           #:RPLACD
           #:PSETF
           #:APPEND
           #:WITH-INPUT-FROM-STRING
           #:CALL-ARGUMENTS-LIMIT
           #:CHAR-INT
           #:COMPILE
           #:EVEN?
           #:NIL
           #:COMPILATION-SPEED
           #:DO-EXTERNAL-SYMBOLS
           #:INSPECT
           #:&BODY
           #:NTH
           #:LISTEN
           #:THERE-EXISTS
           #:WRITE-STRING
           #:PRINT-NOT-READABLE-OBJECT
           #:RPLACA
           #:FCEILING
           #:LOAD-TIME-VALUE
           #:CADAR
           #:PROG
           #:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
           #:GET-INTERNAL-REAL-TIME
           #:MAKE-BUNDLE-PREDICATE
           #:PACKAGE
           #:CLASS-NAME
           #:STREAM-APPEND
           #:DRIBBLE
           #:BOOLE-XOR
           #:PACKAGE-USED-BY-LIST
           #:REAL
           #:STRING-LEFT-TRIM
           #:SOFTWARE-VERSION
           #:COPY-PPRINT-DISPATCH
           #:SLOT-MISSING
           #:NSUBST-IF-NOT
           #:MOST-NEGATIVE-SINGLE-FLOAT
           #:STRING<=
           #:LOGCOUNT
           #:PPRINT-TABULAR
           #:SPECIAL
           #:ARITHMETIC-ERROR-OPERATION
           #:ENSURE-DIRECTORIES-EXIST
           #:UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
           #:CONST
           #:SPECIAL-OPERATOR-P
           #:MAKE-CONDITION
           #:PROVIDE
           #:DEFINE-SETF-EXPANDER
           #:*COMPILE-PRINT*
           #:PSETQ
           #:ANDMAP
           #:NSTRING-DOWNCASE
           #:STREAM-TAKE
           #:LOGICAL-PATHNAME
           #:BYTE
           #:FILTER
           #:WARNING
           #:NTH-VALUE
           #:AND
           #:NOT
           #:INTERNAL-TIME-UNITS-PER-SECOND
           #:ATANH
           #:RCURRY
           #:OPEN-STREAM-P
           #:CLEAR-INPUT
           #:DOUBLE-FLOAT-EPSILON
           #:IMAGPART
           #:ARRAY-DISPLACEMENT
           #:DEGREES->RADIANS
           #:PATHNAME-TYPE
           #:DECLARE
           #:DOCUMENT!
           #:CHAR-NAME
           #:PROCEDURE-ARGUMENTS-KEY-ARGUMENTS
           #:SINGLE-FLOAT-EPSILON
           #:READTABLEP
           #:STANDARD
           #:MEMBER-IF
           #:KEYWORDP
           #:TYPE
           #:PATHNAME-HOST
           #:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
           #:NEGATIVE?
           #:MAKE-TWO-WAY-STREAM
           #:NINTERSECTION
           #:VECTOR-PUSH
           #:COSH
           #:SINH
           #:STORE-VALUE
           #:GET-SETF-EXPANSION
           #:PROCEDURE-ARITY
           #:REMOVE
           #:FILE-STRING-LENGTH
           #:PATHNAME-VERSION
           #:-
           #:POSITIVE?
           #:SIMPLE-CONDITION
           #:CHECK-TYPE
           #:STRING/=
           #:VECTOR
           #:ALIST-MAP
           #:READ-SEQUENCE
           #:MOST-POSITIVE-SHORT-FLOAT
           #:SIMPLE-VECTOR
           #:FIND-METHOD
           #:MERGE-PATHNAMES
           #:TAN
           #:ZERO?
           #:ARRAY-RANK-LIMIT
           #:ARITHMETIC-ERROR-OPERANDS
           #:CONCATENATE
           #:PRINC-TO-STRING
           #:SIMPLE-WARNING
           #:*MODULES*
           #:SET-MEMBER?
           #:FFLOOR
           #:SHADOWING-IMPORT
           #:Y-OR-N-P
           #:LEXICALLY
           #:SUBST-IF
           #:STREAM
           #:MACROLET
           #:*PRINT-CIRCLE*
           #:DOUBLE-FLOAT-NEGATIVE-EPSILON
           #:IMPORT
           #:WITH-SLOTS
           #:COMPILER-MACRO
           #:STRING-UPCASE
           #:CAAR
           #:PARTITION
           #:TENTH
           #:APPEND-MAP
           #:*PRINT-RIGHT-MARGIN*
           #:PACKAGEP
           #:REMOVE*
           #:PROCEDURE-ARGUMENTS
           #:STABLE-SORT
           #:OPTIMIZE
           #:MAKE-HASH-TABLE
           #:UPPER-CASE-P
           #:REMOVE-IF
           #:CHAR-NOT-GREATERP
           #:UPDATE-INSTANCE-FOR-REDEFINED-CLASS
           #:WITH-STANDARD-IO-SYNTAX
           #:CONSP
           #:INSTALL-SYNTAX!
           #:SEVENTH
           #:LIST-ALL-PACKAGES
           #:*PRINT-BASE*
           #:SORT
           #:PACKAGE-ERROR
           #:MASK-FIELD
           #:UNSIGNED-BYTE
           #:FOLDR
           #:SHARED-INITIALIZE
           #:COND
           #:FILE-POSITION
           #:NULL
           #:LEAST-NEGATIVE-DOUBLE-FLOAT
           #:STREAM-REST
           #:REMQ*
           #:ALIST
           #:1+
           #:SIMPLE-BIT-VECTOR
           #:STREAM-REF
           #:METHOD-QUALIFIERS
           #:SECOND
           #:PROCEDURE-ARGUMENTS-REST-ARGUMENT
           #:SET=?
           #:DOCUMENTATION
           #:CONSTANTLY
           #:BIT-IOR
           #:SINGLE-FLOAT-NEGATIVE-EPSILON
           #:STREAM?
           #:SPEED
           #:FOR-EACH
           #:FIND-IF-NOT
           #:UNDEFINED-FUNCTION
           #:DIRECTORY-NAMESTRING
           #:NUMERATOR
           #:*LOAD-TRUENAME*
           #:FIND-ALL-SYMBOLS
           #:METHOD-COMBINATION-ERROR
           #:EIGHTH
           #:BOOLE-EQV
           #:RESTART-CASE
           #:MERGE
           #:SAFETY
           #:BYTE-POSITION
           #:PATHNAME-DEVICE
           #:COUNT-IF-NOT
           #:*BREAK-ON-SIGNALS*
           #:MAPL
           #:EQUALP
           #:FIND
           #:CHAR-CODE
           #:MULTIPLE-VALUE-SETQ
           #:QUEUE-INSERT!
           #:MOD
           #:COPY-SYMBOL
           #:SLEEP
           #:CODE-CHAR
           #:EXPT
           #:LOGANDC2
           #:SYMBOL->STRING
           #:COPY-LIST
           #:RESTART-NAME
           #:PPRINT-LOGICAL-BLOCK
           #:*GENSYM-COUNTER*
           #:RENAME-PACKAGE
           #:UNWIND-PROTECT
           #:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
           #:POP
           #:ENOUGH-NAMESTRING
           #:STREAM-FILTER
           #:NCONC
           #:INVOKE-DEBUGGER
           #:PROG*
           #:BOOLE
           #:WRITE-LINE
           #:USE-PACKAGE
           #:DPB
           #:CELL-ERROR
           #:ARRAY-IN-BOUNDS-P
           #:LIST-REF
           #:ARRAYP
           #:WITH-OPEN-STREAM
           #:RATIONALP
           #:CADDAR
           #:DECLAIM
           #:ALIST-HAS-KEY?
           #:FILE-STREAM
           #:BOOLE-SET
           #:MOST-POSITIVE-LONG-FLOAT
           #:&AUX
           #:FDEFINITION
           #:*PRINT-PPRINT-DISPATCH*
           #:SET-ADD
           #:SIMPLE-TYPE-ERROR
           #:TWO-WAY-STREAM-INPUT-STREAM
           #:STREAM-FLATMAP
           #:FIXNUM
           #:SINGLE-FLOAT
           #:HASH-TABLE-SIZE
           #:MAKE-STRING
           #:MAKE-KEYWORD
           #:PRINT
           #:MAKE-LOAD-FORM
           #:TRUENAME
           #:ROUND
           #:*COMPILE-VERBOSE*
           #:TYPEP
           #:BOOLE-AND
           #:TANH
           #:CTYPECASE
           #:STORAGE-CONDITION
           #:IGNORE-ERRORS
           #:REPLACE
           #:*QUERY-IO*
           #:STREAM-CAR
           #:READTABLE-CASE
           #:HASH-TABLE-COUNT
           #:FUNCTION-KEYWORDS
           #:SIMPLE-ERROR
           #:DENOMINATOR
           #:STRUCT->LIST
           #:PI
           #:ARRAY-DIMENSION-LIMIT
           #:SET-COUNT
           #:BOOLE-C2
           #:ASSERT
           #:LOGNOR
           #:MACHINE-VERSION
           #:SCALE-FLOAT
           #:LDB
           #:DECODE-UNIVERSAL-TIME
           #:PRINT-NOT-READABLE
           #:*PRINT-ARRAY*
           #:LIST-LENGTH
           #:CDAADR
           #:COMPUTE-RESTARTS
           #:SPLIT-AT
           #:GET-DECODED-TIME
           #:ASINH
           #:PACKAGE-NAME
           #:TREE-EQUAL
           #:MAKE-SEQUENCE
           #:1-
           #:DO-ALL-SYMBOLS
           #:CHAR-NOT-EQUAL
           #:LISP-IMPLEMENTATION-TYPE
           #:LEAST-NEGATIVE-SHORT-FLOAT
           #:CONS
           #:QUEUE?
           #:LOGXOR
           #:MAKE-LOAD-FORM-SAVING-SLOTS
           #:ROOM
           #:SEQUENCE
           #:PPRINT-FILL
           #:PACKAGE-NICKNAMES
           #:ED
           #:MAKE-CONCATENATED-STREAM
           #:MOST-NEGATIVE-SHORT-FLOAT
           #:*READTABLE*
           #:STREAM-ERROR-STREAM
           #:SERIOUS-CONDITION
           #:SHORT-SITE-NAME
           #:CDDR
           #:MACROEXPAND-1
           #:SHORT-FLOAT
           #:TRACE
           #:FILTER-NOT
           #:EXPOSE
           #:LOGORC2
           #:MULTIPLE-VALUE-CALL
           #:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
           #:DEFINE
           #:REMOVE-IF-NOT
           #:BROADCAST-STREAM
           #:CONCATENATED-STREAM-STREAMS
           #:COPY-TREE
           #:UPGRADED-ARRAY-ELEMENT-TYPE
           #:&ENVIRONMENT
           #:MAKE-STRING-INPUT-STREAM
           #:ARITHMETIC-ERROR
           #:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
           #:ACOSH
           #:DEFCONSTANT
           #:FIRST
           #:UNUSE-PACKAGE
           #:STRUCTURE-CLASS
           #:CELL-ERROR-NAME
           #:ORMAP
           #:NO-NEXT-METHOD
           #:GENTEMP
           #:STEP
           #:&OPTIONAL
           #:IN-PACKAGE
           #:HANDLER-BIND
           #:*PRINT-PRETTY*
           #:MINUSP
           #:STRING
           #:*THE-EMPTY-STREAM*
           #:STANDARD-GENERIC-FUNCTION
           #:FILE-WRITE-DATE
           #:NEWLINE
           #:CAAAAR
           #:SIN
           #:AND-LET*
           #:MOST-NEGATIVE-DOUBLE-FLOAT
           #:CADDDR
           #:ELT
           #:FLOAT-PRECISION
           #:ALIST-SET
           #:THROW
           #:ALLOCATE-INSTANCE
           #:SYNONYM-STREAM-SYMBOL
           #:LAST
           #:NSUBSTITUTE
           #:HASH-TABLE-REHASH-SIZE
           #:MAKE-ARRAY
           #:BIT-XOR
           #:TAILP
           #:MOST-POSITIVE-SINGLE-FLOAT
           #:INLINE
           #:NOTANY
           #:NAMESTRING
           #:INTERN
           #:SIGNAL
           #:DOTIMES
           #:STRINGP
           #:ALPHA-CHAR-P
           #:EVERY
           #:TYPECASE
           #:DYNAMIC-EXTENT
           #:FIFTH
           #:EVAL
           #:FUNCTIONP
           #:NUMBER
           #:PHASE
           #:DIGIT-CHAR-P
           #:EMPTY?
           #:SUBST
           #:ASSOC-IF
           #:PPRINT-INDENT
           #:INTEGERP
           #:FORMAT
           #:MAX
           #:TRANSLATE-LOGICAL-PATHNAME
           #:SET-UNION
           #:COPY-SEQ
           #:BOOLE-C1
           #:NUMBER->STRING
           #:CDADAR
           #:BUTLAST
           #:ACONS
           #:STRING>
           #:MAKE-QUEUE
           #:/=
           #:PPRINT-NEWLINE
           #:COMPUTE-APPLICABLE-METHODS
           #:CATCH
           #:RETURN
           #:MISMATCH
           #:STREAM-CONS
           #:DIGIT-CHAR
           #:OR
           #:GET-PROPERTIES
           #:DEFINE-METHOD-COMBINATION
           #:CADAAR
           #:SLOT-MAKUNBOUND
           #:ADJUST-ARRAY
           #:LOAD
           #:UNLESS
           #:CDAR
           #:BUNDLE-PERMISSIONS
           #:FIND-RESTART
           #:FILL
           #:BOOLE-ORC1
           #:RASSOC-IF
           #:VECTORP
           #:SET-EMPTY?
           #:SWAP-ARGS
           #:CLASS
           #:PATHNAME-NAME
           #:WRITE-SEQUENCE
           #:LAMBDA-PARAMETERS-LIMIT
           #:MAKE-INSTANCES-OBSOLETE
           #:STRING-LESSP
           #:WITH-ACCESSORS
           #:BOOLEAN
           #:LENGTH
           #:ASSOC-IF-NOT
           #:BUNDLE
           #:POSITION-IF-NOT
           #:MAPC
           #:PARSE-NAMESTRING
           #:WARN
           #:CAAAR
           #:LEAST-POSITIVE-LONG-FLOAT
           #:MAPCAN
           #:FILE-ERROR-PATHNAME
           #:CHARACTER
           #:>=
           #:QUEUE-EMPTY?
           #:WRITE-CHAR
           #:FILTER-MAP
           #:PROCLAIM
           #:MAKE-DISPATCH-MACRO-CHARACTER
           #:COMPOSE
           #:PACKAGE-SHADOWING-SYMBOLS
           #:THE
           #:SET-REMOVE
           #:STREAM-ELEMENT-TYPE
           #:EXPAND-FUNCTION-BODY
           #:DROP
           #:FILE-AUTHOR
           #:QUEUE-DELETE!
           #:DEFGENERIC
           #:DELETE-PACKAGE
           #:LOOP-FINISH
           #:RANDOM
           #:TAKE
           #:RATIO
           #:BIT-NAND
           #:OPEN
           #:DEFCLASS
           #:MUFFLE-WARNING
           #:LDIFF
           #:DO
           #:CDDDR
           #:*DEFAULT-PATHNAME-DEFAULTS*
           #:SIGNUM
           #:POSITION
           #:GET-INTERNAL-RUN-TIME
           #:NRECONC
           #:REALPART
           #:STREAMP
           #:NSET-EXCLUSIVE-OR
           #:DEPOSIT-FIELD
           #:GROUP
           #:STRING-DOWNCASE
           #:FLOATING-POINT-INEXACT
           #:ALIST-REMOVE
           #:TYPE-ERROR
           #:LEAST-NEGATIVE-LONG-FLOAT
           #:UNBOUND-SLOT-INSTANCE
           #:NREVERSE
           #:VARIABLE
           #:REINITIALIZE-INSTANCE
           #:UNBOUND-SLOT
           #:COMPILE-FILE-PATHNAME
           #:*LOAD-PRINT*
           #:CONJOIN*
           #:WHEN
           #:DELETE-IF-NOT
           #:SHORT-FLOAT-NEGATIVE-EPSILON
           #:ADJUSTABLE-ARRAY-P
           #:MAKE-STRING-OUTPUT-STREAM
           #:MAKE-PACKAGE
           #:INITIALIZE-INSTANCE
           #:EXP
           #:COMPLEX
           #:BIT
           #:QUEUE-FRONT
           #:MAP
           #:RANDOM-STREAM
           #:CONTROL-ERROR
           #:SBIT
           #:COPY-READTABLE
           #:BOOLE-2
           #:LOAD-LOGICAL-PATHNAME-TRANSLATIONS
           #:INTERSECTION
           #:INTERACTIVE-STREAM-P
           #:ECHO-STREAM-INPUT-STREAM
           #:ASSOC
           #:MAKUNBOUND
           #:DEFINE-COMPILER-MACRO
           #:CHANGE-CLASS
           #:STREAM-EXTERNAL-FORMAT
           #:NINTH
           #:ARRAY-TOTAL-SIZE
           #:ALIST-REF
           #:ENDP
           #:DO-SYMBOLS
           #:STREAM-CDR
           #:*LOAD-PATHNAME*
           #:SUBST-IF-NOT
           #:STRING-TRIM
           #:SET-DIFFERENCE
           #:REST
           #:MACHINE-TYPE
           #:SYMBOL
           #:ACOS
           #:SHADOW
           #:STREAM-FOLD
           #:SPACE
           #:MEMO-PROC
           #:PUSHNEW
           #:*COMPILE-FILE-PATHNAME*
           #:ARRAY-DIMENSION
           #:*PRINT-MISER-WIDTH*
           #:REMPROP
           #:MAKE-INSTANCE
           #:LABELS
           #:NSUBSTITUTE-IF
           #:PRINT-OBJECT
           #:QUOTE
           #:REALP
           #:POSITION-IF
           #:UPGRADED-COMPLEX-PART-TYPE
           #:SUBSEQ
           #:STREAM-EMPTY?
           #:FOURTH
           #:CONTINUE
           #:RASSOC
           #:CDDDDR
           #:MIN
           #:SYMBOL-VALUE
           #:CERROR
           #:INVOKE-RESTART-INTERACTIVELY
           #:CADADR
           #:MAP-INTO
           #:BOOLE-NAND
           #:CIS
           #:IF
           #:APPEND*
           #:WITH-HASH-TABLE-ITERATOR
           #:DEFMETHOD
           #:RASSOC-IF-NOT
           #:PROGV
           #:GETF
           #:GET-UNIVERSAL-TIME
           #:BIGNUM
           #:BUNDLE-LIST
           #:TWO-WAY-STREAM
           #:SHIFTF
           #:AREF
           #:FUNCALL
           #:UNREAD-CHAR
           #:OUTPUT-STREAM-P
           #:ECASE
           #:CDDDAR
           #:CAAADR
           #:END-OF-FILE
           #:FOLDL
           #:PRIN1-TO-STRING
           #:MACHINE-INSTANCE
           #:BIT-ANDC1
           #:SVREF
           #:PROBE-FILE
           #:REMOVE-DUPLICATES
           #:NSUBST
           #:=
           #:BIT-EQV
           #:ARRAY-ELEMENT-TYPE
           #:DESTRUCTURING-BIND
           #:PRINC
           #:MULTIPLE-VALUE-BIND
           #:CALL-NEXT-METHOD
           #:RADIANS->DEGREES
           #:CHAR
           #:SUBSETP
           #:THERE-EXISTS*
           #:SET-CDR!
           #:MAKE-RANDOM-STATE
           #:*PRINT-LINES*
           #:SIMPLE-STRING
           #:LEAST-POSITIVE-SINGLE-FLOAT
           #:FTYPE
           #:CONJOIN
           #:PRINT-UNREADABLE-OBJECT
           #:ALIST-UPDATE
           #:FILE-NAMESTRING
           #:GCD
           #:FRESH-LINE
           #:CLOSE
           #:STREAM-DROP
           #:PATHNAME-DIRECTORY
           #:NSUBST-IF
           #:LEAST-POSITIVE-SHORT-FLOAT
           #:PROCEDURE-ARGUMENTS-ALLOW-OTHER-KEYS?
           #:<
           #:STRING-GREATERP
           #:STANDARD-OBJECT
           #:ARRAY-DIMENSIONS
           #:GET-MACRO-CHARACTER
           #:TYPE-OF
           #:MEMF
           #:DECF
           #:COMPILER-MACRO-FUNCTION
           #:SIMPLE-CONDITION-FORMAT-ARGUMENTS
           #:PROCEDURE-ARGUMENTS-OPTIONAL-ARGUMENTS
           #:DIRECTORY
           #:COUNT-IF
           #:MAKE-PATHNAME
           #:CAADDR
           #:SET
           #:READER-ERROR
           #:FLOAT-DIGITS
           #:SHORT-FLOAT-EPSILON
           #:MAKE-BROADCAST-STREAM
           #:READ
           #:SQR
           #:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
           #:LOGANDC1
           #:CHAR>
           #:EQL
           #:*TRACE-OUTPUT*
           #:DISJOIN
           #:&ALLOW-OTHER-KEYS
           #:FIND-IF
           #:YES-OR-NO-P
           #:FORMATTER
           #:LIST?
           #:IGNORABLE
           #:SYMBOL-NAME
           #:SIXTH
           #:SIMPLE-CONDITION-FORMAT-CONTROL
           #:UNINTERN
           #:SLOT-EXISTS-P
           #:FIND-SYMBOL
           #:DEFUN
           #:DEFINE-SYMBOL-MACRO
           #:LAMBDA-LIST-KEYWORDS
           #:STREAM-FOR-EACH
           #:PPRINT-DISPATCH
           #:*READ-EVAL*
           #:STREAM-MAP
           #:CCASE
           #:TAGBODY
           #:*READ-SUPPRESS*
           #:RENAME-FILE
           #:METHOD
           #:WITH-PACKAGE-ITERATOR
           #:APPLY
           #:CHAR-CODE-LIMIT
           #:RATIONAL
           #:STREAM-FLATTEN
           #:STRUCT-COPY
           #:CHAR-LESSP
           #:*PRINT-LENGTH*
           #:FTRUNCATE
           #:COMPILED-FUNCTION-P
           #:SYMBOLP
           #:HAS-SPECIFIC-ARITY?
           #:VECTOR-PUSH-EXTEND
           #:STRING?
           #:REM
           #:PATHNAMEP
           #:WITH-CONDITION-RESTARTS
           #:SETQ
           #:PPRINT-TAB
           #:LOGNOT
           #:SUBSTITUTE
           #:IDENTITY
           #:INTEGER-DECODE-FLOAT
           #:SUBLIS
           #:TERPRI
           #:SETF
           #:UNINSTALL-SYNTAX!
           #:*LOAD-VERBOSE*
           #:PACKAGE-ERROR-PACKAGE
           #:NSTRING-CAPITALIZE
           #:*PRINT-GENSYM*
           #:SET-DISPATCH-MACRO-CHARACTER
           #:DEFMACRO
           #:SET-INTERSECT
           #:HASH-TABLE-REHASH-THRESHOLD
           #:STANDARD-CLASS
           #:PROCEDURE?
           #:SET!
           #:QUOTIENT
           #:ARRAY-HAS-FILL-POINTER-P
           #:GRAPHIC-CHAR-P
           #:PROGN
           #:LOGORC1
           #:FIND-CLASS
           #:LCM
           #:SET-SYNTAX-FROM-CHAR
           #:SET-MACRO-CHARACTER
           #:CADDR
           #:COMPLEMENT
           #:PATHNAME-MATCH-P
           #:STRING=
           #:BUNDLE?
           #:ETYPECASE
           #:LONG-SITE-NAME
           #:<=
           #:DEFSETF
           #:CHAR/=
           #:SIGNED-BYTE
           #:CHARACTERP
           #:PEEK-CHAR
           #:STRING-NOT-LESSP
           #:LISP-IMPLEMENTATION-VERSION
           #:ARRAY-ROW-MAJOR-INDEX
           #:STRUCT?
           #:DESCRIBE
           #:NOTEVERY
           #:EXPAND-TOP-LEVEL-DEFINE
           #:&KEY
           #:*READ-BASE*
           #:XOR
           #:SXHASH
           #:BIT-NOT
           #:STRING-NOT-EQUAL
           #:UNEXPORT
           #:ROTATEF
           #:FIND-PACKAGE
           #:STREAM-FIRST
           #:ARRAY-TOTAL-SIZE-LIMIT
           #:USE-VALUE
           #:STRING-CAPITALIZE
           #:*PRINT-CASE*
           #:SLOT-UNBOUND
           #:MAPHASH
           #:ALPHANUMERICP
           #:ENCODE-UNIVERSAL-TIME
           #:SLOT-VALUE
           #:*RANDOM-STATE*
           #:LCURRY
           #:LOG
           #:STANDARD-CHAR
           #:PPRINT-LINEAR
           #:*PRINT-ESCAPE*
           #:>
           #:BIT-VECTOR
           #:CHAR-NOT-LESSP
           #:SET-PPRINT-DISPATCH
           #:PROGRAM-ERROR
           #:CHAR-GREATERP
           #:LIST-SET
           #:PAIRLIS
           #:UNION
           #:MOST-NEGATIVE-FIXNUM
           #:CDAAAR
           #:BIT-ANDC2
           #:&REST
           #:FINDF
           #:GETHASH
           #:ATOM
           #:TIME
           #:BIT-NOR
           #:ECHO-STREAM
           #:EQUAL
           #:READ-FROM-STRING
           #:FUNCTION-LAMBDA-EXPRESSION
           #:ODD?
           #:FOR-MACROS
           #:READ-CHAR-NO-HANG
           #:DIVISION-BY-ZERO
           #:LOGIOR
           #:FLOAT-SIGN
           #:*READ-DEFAULT-FLOAT-FORMAT*
           #:CLEAR-OUTPUT
           #:WILD-PATHNAME-P
           #:STANDARD-METHOD
           #:STRING>=
           #:TWO-WAY-STREAM-OUTPUT-STREAM
           #:ECHO-STREAM-OUTPUT-STREAM
           #:UNBOUND-VARIABLE
           #:*FEATURES*
           #:TYPE-ERROR-EXPECTED-TYPE
           #:MEMBER
           #:PACKAGE-USE-LIST
           #:SEARCH
           #:APROPOS
           #:COPY-STRUCTURE
           #:EXTENDED-CHAR
           #:DEFINE-MODIFY-MACRO
           #:RANDOM-STATE
           #:*DEBUG-IO*
           #:CAADR
           #:CLRHASH
           #:BOOLE-ANDC1
           #:DEFSTRUCT
           #:SOFTWARE-TYPE
           #:DEFVAR
           #:LIST-TAIL
           #:MULTIPLE-VALUES-LIMIT
           #:*STANDARD-INPUT*
           #:DELETE
           #:MAPCON
           #:CEILING
           #:DEFPACKAGE
           #:REMQ
           #:LONG-FLOAT-NEGATIVE-EPSILON
           #:CHAR=
           #:LOGTEST
           #:DEFINE-CONDITION
           #:BOOLE-ANDC2
           #:KEYWORD
           #:FLATTEN
           #:DELETE-FILE
           #:COMPLEXP
           #:REVERSE
           #:PUSH
           #:ARRAY-RANK
           #:STRING-EQUAL
           #:MAKE-METHOD
           #:LONG-FLOAT-EPSILON
           #:PAIR?
           #:DEFPARAMETER
           #:GENERIC-FUNCTION
           #:PATHNAME
           #:STRUCTURE
           #:HANDLER-CASE
           #:CAADAR
           #:CONDITION
           #:RESTART-BIND
           #:REMF
           #:STRING-STARTS-WITH?
           #:PROG2
           #:BIT-VECTOR-P
           #:NULL?
           #:LOGICAL-PATHNAME-TRANSLATIONS
           #:*STANDARD-OUTPUT*
           #:PPRINT
           #:FLOATING-POINT-INVALID-OPERATION
           #:MAKE-ECHO-STREAM
           #:NBUTLAST
           #:BYTE-SIZE
           #:ENSURE-GENERIC-FUNCTION
           #:FLOATING-POINT-UNDERFLOW
           #:APROPOS-LIST
           #:SGN
           #:INVOKE-RESTART
           #:READ-PRESERVING-WHITESPACE
           #:HASH-TABLE-TEST
           #:NAND
           #:RANGE
           #:COMPILE-FILE
           #:FLET
           #:MAP-SUCCESSIVE
           #:HOST-NAMESTRING
           #:NAME-CHAR
           #:SYMBOL-PLIST
           #:RETURN-FROM
           #:NUMBERP
           #:ROW-MAJOR-AREF
           #:LDB-TEST
           #:ABS
           #:LOGAND
           #:MULTIPLE-VALUE-PROG1
           #:STREAM-MAP-SUCCESSIVE
           #:CHAR<
           #:DECLARATION
           #:COERCE
           #:RANDOM-STATE-P
           #:ATAN
           #:PRIN1
           #:EQUAL?
           #:STREAM-COLLECT
           #:NSTRING-UPCASE
           #:HASH-TABLE
           #:FLOATP
           #:SYMBOL-FUNCTION
           #:FOR-ALL*
           #:GO
           #:READ-DELIMITED-LIST
           #:FILL-POINTER
           #:CDADR
           #:CHAR>=
           #:DOUBLE-FLOAT
           #:EXPORT
           #:WRITE-TO-STRING
           #:ADJOIN
           #:BIT-ORC2
           #:LIST
           #:STRUCT-ACCESSORS
           #:READTABLE
           #:PROCEDURE-ARGUMENTS-REQUIRED-ARGUMENTS
           #:CDADDR
           #:DISJOIN*
           #:LET*
           #:THIRD
           #:STRUCTURE-OBJECT
           #:REVAPPEND
           #:LOCALLY
           #:STRING-APPEND
           #:SIMPLE-BIT-VECTOR-P
           #:CDDADR
           #:CONSTANTP
           #:BUNDLE-DOCUMENTATION
           #:SQRT
           #:SERIALIZE
           #:SYMBOL-MACROLET
           #:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
           #:*DEBUGGER-HOOK*
           #:FOR-ALL
           #:TRANSLATE-PATHNAME
           #:PARSE-ERROR
           #:FLOAT-RADIX
           #:VALUES
           #:BOOLE-CLR
           #:FLOAT
           #:CAR
           #:SUBSTITUTE-IF-NOT
           #:SET->STREAM)
  (:SHADOW #:++ #:+++ #:// #:/// #:** #:*** #:+ #:/ #:* #:-))

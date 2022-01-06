(in-package :schemeish.markup)

(install-syntax!)

(define (last-space-pos-before text width)
  "Returns the position of the last space before width in text or nil."
  (position #\space text :from-end t :end width))
(define (first-space-pos-after text width)
  "Returns the position of the first space after width in text or nil."
  (position #\space text :start width))

(define (word-wrap text width)
  "Attempt to apply word-wrap to text, returning a list of lines. Width is the number of characters per line.
Splits on spaces. If a word is longer than the width, it is not split."
  (define (wrap-line text space-pos result)
    (word-wrap-iter (subseq text (1+ space-pos)) (cons (subseq text 0 space-pos) result)))

  (define (wrap-last-line text result)
    (cons text result))
  
  (define (long-word-wrap text result)
    ;; Look for the first space >= width
    (let ((space-pos (first-space-pos-after text width)))
      (cond
	((null? space-pos) (wrap-last-line text result))
	(t (wrap-line text space-pos result)))))

  (define (word-wrap-iter text result)
    (cond
      ((zero? (length text)) result)
      ((<= (length text) width) (wrap-last-line text result))
      (t (let ((space-pos (last-space-pos-before text width)))
	   (cond
	     ((null? space-pos) (long-word-wrap text result))
	     (t (wrap-line text space-pos result)))))))

  (nreverse (word-wrap-iter text ())))

(assert (equal? (word-wrap "" 20)
		()))
(assert (equal? (word-wrap "the quick brown fox jumped over the lazy dog. Three blind mice. see how they run." 20)
		'("the quick brown fox"
		  "jumped over the"
		  "lazy dog. Three"
		  "blind mice. see how"
		  "they run.")))
(assert (equal? (word-wrap "aalksdjflkjsdlfkjsldkfjllksjlkdjfslkdjf" 20)
		'("aalksdjflkjsdlfkjsldkfjllksjlkdjfslkdjf")))
(assert (equal? (word-wrap "aasdflkjasdlfkjalsdkfjasldkfj asdlfkjasldkfjlsdkjdf" 20)
		'("aasdflkjasdlfkjalsdkfjasldkfj" "asdlfkjasldkfjlsdkjdf")))

(define (escape-string string chars-to-escape (escape-char #\\))
  "Return string with any chars-to-escape prepended with escape-char."
  (list->string (append-map (lambda (char)
			      (cond
				((member char chars-to-escape) (list escape-char char))
				(t (list char))))
			    (string->list string))))

(define (string-empty? string)
  "True if string is empty."
  (zero? (length string)))

(define (split-string-if string split-char?)
  "Return a list of strings that have been split whenever split-char? is true.
Chars that satisfy split-char? will be removed, and empty strings will not be returned."
  (define (not-split-char? char) (not [split-char? char]))
  
  (define (split-string-iter string result)
    (cond
      ((string-empty? string) result)
      ([split-char? (aref string 0)]
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

  (nreverse (split-string-iter string ())))

(define (split-string string split-char)
  (split-string-if string (lcurry #'char= split-char)))

(assert (equal? (split-string-if "     the three     wise    men   joined hands in holy matrimony.    " (lcurry #'char= #\space))
		'("the" "three" "wise" "men" "joined" "hands" "in" "holy" "matrimony.")))


(define renderer? (make-bundle-predicate 'renderer?))
(define (%markup-string-renderer stream width (prefixes ()) (escape-chars ()) (freshline? t))
  (define (push-render-prefix prefix)
    (set! freshline? nil)
    (push prefix prefixes))
  (define (pop-render-prefix)
    (set! freshline? nil)
    (pop prefixes))

  (define (push-escape-char char)
    (push char escape-chars))
  (define (pop-escape-char)
    (pop escape-chars))

  (define (render-inline strings)
    (set! freshline? nil)
    (for-each (lambda (inline-text)
		(format stream "~A" inline-text))
	      strings))

  (define (render-inline-escaped strings)
    (render-inline (map (lambda (string)
			  (escape-string string escape-chars))
			strings)))

  (define (render-prefixes)
    (set! freshline? t)
    (loop for prefix in (reverse prefixes) do
      (format stream "~A" prefix)))
  (define (render-freshline)
    (unless freshline?
      (render-newline 1)))
  (define (render-newline count)
    (loop for i below count do
      (format stream "~%")
      (render-prefixes)))

  (define (column)
    (loop for prefix in prefixes summing (length prefix)))

  (define (render-block-text text)
    (let ((lines (word-wrap text (- width (column)))))
      (loop for line in lines
	    for i from 0 do
	      (render-inline (list line))
	      (when (< i (1- (length lines)))
		(render-freshline)))))

  (define (render-preformatted-text text)
    (let ((lines (split-string text #\newline)))
      (for-each (lambda (line)
		  (render-freshline)
		  (render-inline (list line)))
		lines)))

  (define (copy-with-new-stream new-stream)
    (%markup-string-renderer new-stream width prefixes escape-chars freshline?))

  (bundle renderer?
	  render-inline
	  render-inline-escaped
	  render-freshline
	  render-newline
	  render-block-text
	  render-preformatted-text
	  push-escape-char
	  pop-escape-char
	  push-render-prefix
	  pop-render-prefix
	  copy-with-new-stream))

(define (markup-string-renderer stream width)
  "Create a markup renderer that can render to a string."
  (%markup-string-renderer stream width))

(define (render-inline renderer . strings)
  "Render without escaping any chars. Don't render a newline."
  (assert (renderer? renderer))
  [[renderer :render-inline] strings])
(define (render-inline-escaped renderer . strings)
  "Render escaping chars. Don't render a newline."
  (assert (renderer? renderer))
  [[renderer :render-inline-escaped] strings])
(define (render-freshline renderer)
  "If at the start of a line (ignoring prefixes) do nothing. Otherwise render a newline with prefixes."
  (assert (renderer? renderer))
  [[renderer :render-freshline]])
(define (render-preformatted-text renderer text)
  "Render text without word-wrap. Render prefixes at each new line."
  (assert (renderer? renderer))
  [[renderer :render-preformatted-text] text])
(define (render-newline renderer (count 1))
  "Render a newline, and any prefixes."
  (assert (renderer? renderer))
  [[renderer :render-newline] count])
(define (push-render-prefix renderer prefix)
  "Start prefixing lines with the given prefix (in addition to other prefixes)."
  (assert (renderer? renderer))
  [[renderer :push-render-prefix] prefix])
(define (pop-render-prefix renderer)
  "Stop prefixing lines with the last pushed prefix."
  (assert (renderer? renderer))
  [[renderer :pop-render-prefix]])
(define (push-escape-char renderer char)
  "Start escaping the given char."
  (assert (renderer? renderer))
  [[renderer :push-escape-char] char])
(define (pop-escape-char renderer)
  "Stop escaping the last pushed escape char."
  (assert (renderer? renderer))
  [[renderer :pop-escape-char]])
(define (render-block-text renderer block-text)
  "Render a block of text. Apply word-wrap."
  (assert (renderer? renderer))
  [[renderer :render-block-text] block-text])
(define (copy-renderer-with-new-stream renderer stream)
  "Return a copy of renderer, set to render to stream."
  (assert (renderer? renderer))
  [[renderer :copy-with-new-stream] stream])

(define (render-length renderer render)
  "Return the length of the resulting string, if render were to be called with the given renderer.
Makes a temporary copy of the renderer so that renderer is not affected."
  (assert (renderer? renderer))
  (length (with-output-to-string (stream)
	    (let ((temp (copy-renderer-with-new-stream renderer stream)))
	      [render temp]))))

(defparameter *markup-render-width* 20)
(define (render-markup-to-string markup)
  "Render the markup object to a string."
  (with-output-to-string (s)
    (render-markup markup (markup-string-renderer s *markup-render-width*))))

(define (render-markup markup renderer)
  "Render the markup object to renderer."
  (assert (renderer? renderer))
  [[markup :render] renderer])
(define (markup-constructor markup)
  "Return the constructor form for markup as a list."
  [[markup :constructor]])

(define (inline-markup? markup)
  "Return true if the markup is considered inline."
  [[markup :inline?]])

(define (inline-text? object)
  "A string is inline-text if it doesn't start with space,
and it doesn't contain a newline or tabs."
  (and (string? object)
       (not (char= #\space (aref object 0)))
       (not (char= #\space (aref object (1- (length object)))))
       (not (find #\newline object))
       (not (find #\tab object))))

(define (inline-text text)
  "Return an inline markup object that can render text.
Text must be inline. See inline-text?"
  (define (constructor)
    `(inline-text ,text))
  (define (render renderer)
    (render-inline-escaped renderer text))
  (define (inline?) t)

  (assert (inline-text? text))
  (bundle nil constructor render inline?))

(define (chars-string char count)
  "Return a string with count chars."
  (make-string count :initial-element char))

;; # Heading level 1
;; ## Heading .. 2
;; ###### Heading level 6
(define (heading level inline-markup)
  "Given a level from 1-6 and an inline-markup, create a heading."
  (define (constructor)
    `(heading ,level ,(markup-constructor inline-markup)))
  (define (render renderer)
    (render-inline renderer
		   (chars-string #\# level)
		   " ")
    (render-markup inline-markup renderer))
  (define (inline?) nil)
  
  (assert (<= 1 level 6))
  (assert (inline-markup? inline-markup))
  (bundle nil
	  constructor
	  render
	  inline?))

(render-markup-to-string (heading 3 (inline-text "Heading")))
(render-markup-to-string (heading 3 (code "Code")))
(render-markup-to-string (heading 3 (bold (inline-text "bold"))))

(define (replace-newlines-with-spaces string)
  "Return string with all newlines replaced with spaces."
  (cl:map 'string (lambda (char)
		    (if (char= char #\newline)
			#\space
			char))
	  string))

(define (remove-carriage-returns string)
  "Return string without any carriage returns."
  (remove #\return string))

(define (intersperse element list)
  "Return list with element placed between every other element."
  (define (intersperse-loop list result)
    (cond
      ((empty? list) (nreverse result))
      (t (intersperse-loop (rest list)
			   (list* (first list) element result)))))
  (cond
    ((empty? list) ())
    (t
     (cons (first list) (intersperse-loop (rest list) ())))))

(assert (equal? (intersperse :a ())
		()))
(assert (equal? (intersperse :a '(:b))
		'(:b)))
(assert (equal? (intersperse :b '(:a :c))
		'(:a :b :c)))
(assert (equal? (intersperse :i '(:e :e :o))
		'(:e :i :e :i :o)))

(define (join-strings strings separator)
  "Joins strings with separator."
  (apply #'string-append (intersperse (chars-string separator 1) strings)))

(define (block-text strings)
  "Joins strings with a space character. Removes carriage returns. Replaces newlines with spaces."
  (replace-newlines-with-spaces (remove-carriage-returns (join-strings strings #\space))))

;; Paragraphs are separated by blank lines
;;
;; Paragraph text...
;; More paragraph text...
;;
(define (paragraph . texts)
  "Given a list of texts, join them into a block text and return a paragraph.
Carriage returns will be removed and newlines will be replaced with spaces.
Word-wrap will automatically be applied to paragraphs. Texts are joined with space.
See block-text."
  (define (constructor)
    `(paragraph ,@texts))
  (define (render renderer)
    (render-block-text renderer (block-text texts)))
  (define (inline?) nil)
  
  (bundle nil render constructor inline?))

;; Line break. Line with 2+ spaces followed by a return.
(define (br)
  "Return a line-break (newline)."
  (define (constructor)
    `(br))
  (define (render renderer)
    (render-newline renderer))
  (define (inline?) nil)
  (bundle nil render constructor inline?))

(define (bold markup)
  "Return a markup that has been emboldened. If the given markup is inline,
the returned markup will also be inlined. Rendered as **bold**."
  (define (constructor)
    `(bold ,(markup-constructor markup)))
  (define (render renderer)
    (push-escape-char renderer #\*)
    (render-inline renderer "**")
    (render-markup markup renderer)
    (render-inline renderer "**")
    (pop-escape-char renderer))

  (define (inline?) (inline-markup? markup))
  (bundle nil constructor render inline?))

(define (italic markup)
  "Return a markup that has been italicized. If the given markup is inline,
the returned markup will also be inlined. Rendered as *italics*."
  (define (constructor)
    `(italic ,(markup-constructor markup)))
  (define (render renderer)
    (push-escape-char renderer #\*)
    (render-inline renderer "*")
    (render-markup markup renderer)
    (render-inline renderer "*")
    (pop-escape-char renderer))
  (define (inline?) (inline-markup? markup))
  (bundle nil render constructor inline?))

(define (block-quote markup)
  "Return a markup object with the given markup as quoted text.

;; Block quote
;; > quoted text
;; >
;; > with multiple paragraphs
;; >> and a nested quote"
  (define (constructor)
    `(block-quote ,(markup-constructor markup)))
  (define (render renderer)
    (push-render-prefix renderer "> ")
    (render-inline renderer "> ")
    (render-markup markup renderer)
    (pop-render-prefix renderer))
  (define (inline?) nil)
  (bundle nil constructor render inline?))

(define (ordered-list . markups)
  "Given a list of markup objects, return an ordered list.

;; Numbered lists
;; 1. first
;; 2. second
;;    1. first nested
;;    2. second nested
;; 3. third"
  (define (constructor)
    `(ordered-list ,@(map #'markup-constructor markups)))
  
  (define (render renderer)
    (define (render-list-item markup num)
      "Render a single list item for the given 0-based index."
      (let ((number-text (format nil "~A. " num)))
	;; Render the number prefix.
	(render-inline renderer number-text)
	;; Push indentation
	(push-render-prefix renderer (chars-string #\space (length number-text))))
      ;; Render the list item.
      (render-markup markup renderer)
      ;; Pop indentaiton
      (pop-render-prefix renderer))

    (unless (empty? markups)
      ;; Render the first item on the current line.
      (render-list-item (first markups) 1)
      ;; Render each of the rest of the items with a freshline.
      (let recurse ((markups (rest markups))
		    (num 2))
	(unless (empty? markups)
	  (render-freshline renderer)
	  (render-list-item (first markups) num)
	  (recurse (rest markups) (1+ num))))))

  (define (inline?) nil)
  (bundle nil constructor render inline?))


(define (unordered-list . markups)
  "Given a list of markups, return a bullet-list markup object.

;; Unordered Lists
;; - item
;;
;;     this item has a paragrah. it is indented 4 spaces.
;;
;; - item
;;   - indented item
;; - item
;;         this is a code block. it is indented 8 spaces."
  (define (constructor)
    `(ordered-list ,@(map #'markup-constructor markups)))
  (define (render renderer)
    (define (render-list-item markup)
      "Render a single list item. Prefix with a bullet point, and render markup with indentation."
      (let ((prefix-text "- "))
	;; Render a bullet point
	(render-inline renderer prefix-text)
	;; Push indentation
	(push-render-prefix renderer (chars-string #\space (length prefix-text))))
      ;; render the markup object
      (render-markup markup renderer)
      ;; Pop indentation
      (pop-render-prefix renderer))

    (unless (empty? markups)
      ;; Render the first item on the current line.
      (render-list-item (first markups))
      ;; Render the rest of the items with a freshline
      (for-each (lambda (markup)
		  (render-freshline renderer)
		  (render-list-item markup))
		(rest markups))))
  (define (inline?) nil)
  (bundle nil constructor render inline?))

(render-markup-to-string
 (ordered-list
  (inline-text "element 1")
  (block-quote (paragraph "the quick brown fox jumped over the lazy dog."))
  (inline-text "element 3.")
  (ordered-list (inline-text "sub-element 1")
		(block-quote (paragraph "another quote. another very long quote. Another quote that is too long")))
  (inline-text "element 5.")
  (unordered-list (inline-text "unordered element 1")
		  (paragraph "a long unquoted paragraph that spans at least a few lines.")
		  (ordered-list (inline-text "A")
				(inline-text "B")
				(unordered-list (inline-text "B.A")
						(inline-text "B.B"))))))

;; `inline-code`
(define (code inline-text)
  "Given the inline-text, return an inline code block"
  (define (inline?) t)
  (define (render renderer)
    (render-inline renderer "``" inline-text "``"))
  (define (constructor)
    `(code ,inline-text))
  (assert (inline-text? inline-text))
  (bundle nil inline? render constructor))

;; Code block
;; ```
;; code...
;; ```
(define (code-block preformatted-text)
  "Given the preformatted text string, return a fenced code block, using ``` as separators."
  (define text (remove-carriage-returns preformatted-text))
  (define (inline?) nil)
  (define (render renderer)
    ;; begin the code fence
    (render-inline renderer "```")
    ;; Render the text as preformatted-text
    (render-preformatted-text renderer text)
    ;; End the code fence
    (render-freshline renderer)
    (render-inline renderer "```"))
  (define (constructor)
    `(code-block ,text))
  (bundle nil inline? render constructor))

(render-markup-to-string
 (unordered-list (code-block (format nil "~S" '(define (horizontal-bar)
						(define (constructor)
						  `(horizontal-bar))
						(define (render renderer)
						  (render-freshline renderer)
						  (render-newline renderer)
						  (render-inline renderer "___")
						  (render-newline renderer 2))

						(define (inline?) nil)
						(bundle nil constructor render inline?))))
		 (inline-text "text")))

;; Horizontal bar
;;
;; ---
;;
(define (horizontal-bar)
  "Construct a markup object which represents a horizontal separator bar."
  (define (constructor)
    `(horizontal-bar))
  (define (render renderer)
    (render-freshline renderer)
    (render-newline renderer)
    (render-inline renderer "____________")
    (render-newline renderer 2))

  (define (inline?) nil)
  (bundle nil constructor render inline?))

(render-markup-to-string (horizontal-bar))

(render-markup-to-string (unordered-list (block-quote (unordered-list (block-quote (paragraph "the quick brown fox jumped over the lazy dog"))
								      (inline-text "e2")))
					 (inline-text "e2")))


;; Link
;; [link text](url)
;; <url>
(define (link url (inline-text))
  "Construct an inline-markup object which represents a link to a url."
  (define (constructor)
    `(link ,url ,inline-text))
  (define (render renderer)
    (if inline-text
	(render-inline renderer "[" inline-text "](" url ")")
	(render-inline renderer "<" url ">")))

  (define (inline?) t)
  (assert (or (null? inline-text)
	      (inline-text? inline-text)))
  (assert (inline-text? url))
  (bundle nil constructor render inline?))

(render-markup-to-string (link "url" "text"))


(define (rows->columns rows)
  "Convert a list of rows to a list of columns.
Assumes that each row has the same number of elements."
  (define (columns-iter rows columns)
    (cond
      ((empty? (first rows)) columns)
      (t (columns-iter (map #'rest rows)
		       (cons (map #'first rows) columns)))))

  (cond
    ((empty? rows) ())
    (t (let ((num-columns (length (first rows))))
	 (unless (for-all (lambda (row) (= (length row) num-columns)) (rest rows))
	   (error "All rows must be the same length.")))
       (nreverse (columns-iter rows ())))))

(define (hash-ref-default table key delayed-value)
  (or (hash-ref table key)
      (hash-set! table key [delayed-value])))

;; TABLES
;; | COLUMN 1 | column 2 |
;; | ---      | ---      |
;; | Row 1 C1 | Row 1 C2 |
;; | Row 2 C1 | Row 2 C2 |
(define (table column-headers rows (column-alignments (map (const :left) column-headers)))
  "Create a table given a list of column-headers and a list of rows. Each row must be a list
with the same number of elements as column-headers. Column-alignments must also
be the same length as column-headers, and each alignment should be one of '(:LEFT :RIGHT :CENTER)."
  (define (constructor)
    `(table (list ,@(map (lambda (header)
			   (markup-constructor header))
			 column-headers))
	    (list ,@(map (lambda (row)
			   `(list ,@(map #'markup-constructor row)))
			 rows))
	    ',column-alignments))

  (define (render-element-with-padding renderer markup pad-left-string pad-right-string)
    "Renders a single element in the table."
    (push-escape-char renderer #\|)
    (render-inline renderer " " pad-left-string)
    (render-markup markup renderer)
    (render-inline renderer pad-right-string " ")
    (pop-escape-char renderer))
  
  (define (render-element-without-padding renderer markup)
    "Renders a single element in the table, but without any padding."
    (render-element-with-padding renderer markup "" ""))

  (define element-width-table (make-hash-table))
  (define (element-width renderer markup)
    "Return the computed or memoized string-width associated with rendering markup."
    (define (compute-element-width)
      (render-length renderer (lambda (r) (render-element-without-padding r markup))))
    (hash-ref-default element-width-table markup
		      compute-element-width))

  (define (pad-length column-width markup-width)
    "Return the number of characters of padding needed."
    (- column-width markup-width))
  (define (left-pad alignment pad-length)
    "Return the number of characters of padding needed on the left."
    (case alignment
      (:left 0)
      (:right pad-length)
      (:center (truncate pad-length 2))))
  (define (right-pad pad-length left-pad)
    "Return the number of characters of padding needed on the right."
    (- pad-length left-pad))

  
  ;; Convert rows to columns to determine column widths.
  (define columns (rows->columns (cons column-headers rows)))
  (define (render renderer)    
    (define (render-element markup column-width alignment pad-char)
      "Render an element of markup with the proper amount of padding."
      (let* ((pad-length (pad-length column-width (element-width renderer markup)))
	     (left-pad (left-pad alignment pad-length)))
	(render-element-with-padding renderer markup
				     (chars-string pad-char left-pad)
				     (chars-string pad-char (right-pad pad-length left-pad)))))

    (define (column-width column)
      "Get the column-width based on the maximum element-width within column."
      (foldl #'max 0 (map (cut (element-width renderer ?)) column)))

    (define column-widths (map (lambda (column) (column-width column)) columns))
    (define (render-row row pad-char)
      "Render a row of elements using pad-char as the character for padding."
      (render-inline renderer "|")
      (for-each (lambda (markup column-width alignment)
		  (render-element markup column-width alignment pad-char)
		  (render-inline renderer "|"))
		row column-widths column-alignments))

    ;; Render the headers, using space for padding.
    (render-row column-headers #\space)
    (render-newline renderer)
    ;; Next render the separator, using - for padding.
    (render-row (map (const (inline-text "---")) column-headers) #\-)
    ;; Finally, render the rows, using space for padding.
    (for-each (lambda (row)
		(render-newline renderer)
		(render-row row #\space))
	      rows))
  
  (define (inline?) nil)

  (unless (for-all (lambda (column) (for-all #'inline-markup? column)) columns)
    (error "All elements of table must be inline-markup."))
  (unless (= (length column-headers) (length column-alignments))
    (error "Must have the same number of column-headers and column-alignments."))
  (unless (for-all (cut (member ? '(:left :right :center))) column-alignments)
    (error "Column alignments can only be one of '(:LEFT :RIGHT :CENTER)"))
  (bundle nil inline? constructor render))

(render-markup-to-string (table (list (inline-text "Header1") (bold (inline-text "Header2")) (italic (bold (inline-text "header3"))))
				(list (list (inline-text "1.1") (inline-text "1.2") (inline-text "1.3"))
				      (list (inline-text "2.1") (inline-text "2.2") (inline-text "2.3")))
				'(:center :right :left)))



;; Defstruct to replace bundles
;; TODO: Should I always escape pipes and *s?
;; TODO: C-S-\ should output (lambda (|)) (where pipe is the cursor)

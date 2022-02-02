(in-package :schemeish.backend)

(install-syntax!)

(define (word-wrap-line text width no-wrap-continue wrap-continue)
  "Attempt to apply word wrapping to text. 
Word wrapping is only applied to the first line. Calls either [no-wrap-continue text] or [wrap-continue first-line rest-text].
Width is the number of characters per line.
Splits on spaces. If a word is longer than width it is not split."

  (define (last-space-pos-before)
    "Returns the position of the last space before width in text or nil."
    (position #\space text :from-end t :end width))
  (define (first-space-pos-after)
    "Returns the position of the first space after width in text or nil."
    (position #\space text :start width))

  (define (wrap-line text space-pos)
    [wrap-continue (subseq text 0 space-pos) (subseq text (1+ space-pos))])

  (cond
    ((<= (length text) width) [no-wrap-continue text])
    (t (let ((space-pos (last-space-pos-before)))
	 (cond
	   ((null? space-pos)
	    (let ((space-pos (first-space-pos-after)))
	      (cond
		((null? space-pos) [no-wrap-continue text])
		(t (wrap-line text space-pos)))))
	   (t (wrap-line text space-pos)))))))

(export
 (define-struct html-tag
     (name inner-tags-and-texts attribute-alist)
     :documentation "Represents an html tag which can be rendered to text."))

(export
 (define (html-tag name inner-tags-and-texts attribute-alist)
   (unless (string? name)
     (error "Tag name must be a string."))
   (unless (for-all (lambda (tag-or-text)
		      (or (html-tag? tag-or-text)
			  (string? tag-or-text)))
		    inner-tags-and-texts)
     (error "Inner-tags-and-texts should all be either an html-tag or an inner text."))
   (unless (for-all (lambda (pair)
		      (and (string? (car pair))
			   (string? (cdr pair))))
		    attribute-alist)
     (error "Each attribute should be a string name and a string value."))
   (make-html-tag name inner-tags-and-texts attribute-alist)))

(define (escape-string string chars-to-escape (escape-char #\\))
  "Return string with any chars-to-escape prepended with escape-char."
  (define (escaped char)
    (cond ((member char chars-to-escape)
	   (list->string (list escape-char char)))
	  (t char)))
  (string-map escaped string))

(define (escape-html string)
  "Returns string with special html chars escaped."
  (define escape-table
    (alist #\" "&quot;"
	   #\& "&amp;"
	   #\< "&lt;"
	   #\> "&gt;"))

  (define (escaped-char char)
    (alist-ref escape-table char char))
  
  (string-map escaped-char string))

(export
 (define (render-html-tag tag stream)
   "Renders the html-tag to the provided stream."
   (define (render-attribute name value)
     (format stream " ~A=~S" name (escape-html value)))
   (define (render-tag-or-text tag-or-text)
     (cond
       ((html-tag? tag-or-text)
	(render-html-tag tag-or-text stream))
       ((string? tag-or-text)
	(let ((text tag-or-text))
	  (format stream "~A" (escape-html text))))
       (t (error "Unexpected value in inner-html: ~S. Expected html-tag or string." tag-or-text))))

   (define name (html-tag-name tag))
   (define tags-and-texts (html-tag-inner-tags-and-texts tag))
   
   (format stream "<~A" name)
   (alist-for-each (html-tag-attribute-alist tag) render-attribute)
   (cond
     ((empty? tags-and-texts) (format stream "/>"))
     (t (format stream ">")
	(for-each render-tag-or-text tags-and-texts)
	(format stream "</~A>" name)))))

(export
 (define (render-html-tag-to-string tag)
   "Renders the html-tag to a string."
   (with-output-to-string (stream)
     (render-html-tag tag stream))))

(for-macros
  (export
   (define-struct markup
       (constructor-form render-text html-tag)
       :documentation "Represents a markup object. Constructor-form is a form which can re-construct the markup object.
Render-text is a function which takes a text-renderer renders itself to the text-renderer. Html-tag is an
html representation of the markup object.")))
(export
 (define-struct inline-markup
     ()
     :super 'markup
     :documentation "A markup object which is meant to be used inline."))

(define-struct text-renderer
    (render-freshline
     render-newline
     render-prefix

     render-inline
     render-preformatted-text

     render-without-word-wrap

     push-prefix
     pop-prefix
     
     copy-with-new-stream)
    :documentation "Describes a text-renderer's interface.
Render-freshline renders a newline and any prefixes unless at the beginning of a line.
Render-newline renders a newline and any prefixes.
Render-prefix renders a prefix, marking the freshline-state as true.

Render-inline renders a string in the current line, applying word-wrap.
Render-preformatted text renders a string, ignoring word-wrap and respecting the original spacing.

Render-without-word-wrap temporarily disables word-wrapping and performs the provided render function.

push-prefix adds a new prefix to be rendered on newlines.
pop-prefix removes the last pushed prefix.

copy-with-new-stream returns a copy of renderer which will render to the provided stream instead.")
(export 'text-renderer?)

(define (markup constructor-form render inline? html-tag)
  (unless (list? constructor-form)
    (error "expected a list for constructor-form but got ~S" constructor-form))
  (unless (and (procedure? render) (has-specific-arity? (procedure-arity render) 1))
    (error "expected render to be a function of one argument"))
  (unless (html-tag? html-tag)
    (error "Expected an html-tag but got ~S" html-tag))
  [(if inline? #'make-inline-markup #'make-markup) constructor-form render html-tag])

(export
 (define (render-markup markup renderer)
   "Render the markup object to renderer."
   [(markup-render-text markup) renderer]))
(export
 (define (markup-constructor markup)
   "Return the constructor form for markup as a list."
   (markup-constructor-form markup)))
(export
 (define (render-markup-html-tag-to-string markup)
   "Renders markup's html-tag to a string."
   (render-html-tag-to-string (markup-html-tag markup))))

(export
 (lexically
   (define (%markup-string-renderer stream width prefixes freshline? column word-wrap?)
     (define (push-prefix prefix)
       (push prefix prefixes))
     (define (pop-prefix)
       (pop prefixes))

     (define (remaining-width) (- width column))

     (define (render-unformatted string)
       "Renders string directly without applying any wrapping. Sets the freshline? status to nil."
       (format stream "~A" string)
       (set! column (+ column (length string)))
       (set! freshline? nil))

     (define (render-inline-wrapped string width)
       "Render string with word-wrapping applied to the current line with the given width.
If there is more text to render after wrapping, passes it to render-inline."
       (word-wrap-line
	string width
	(lambda (text)
	  ;; The text either had no spaces, or was short enough to fit.
	  (unless (< (length text) width)
	    ;; If the line is long, make sure that we are at the start of a freshline.
	    (render-freshline))
	  (render-unformatted text))
	(lambda (line rest-text)
	  ;; Line-wrapping ocurred.
	  ;; Render the first line, followed by a newline, before processing the rest of the text.
	  (render-unformatted line)
	  (render-freshline)
	  (render-inline rest-text))))
     
     (define (render-inline string)
       (let* ((remaining-width (remaining-width))
	      (no-room? (<= remaining-width 0)))
	 (cond
	   ((not word-wrap?)
	    ;; We are not applying word-wrap.
	    (render-unformatted string))
	   ((and no-room? freshline?)
	    ;; No room remaining on any lines.
	    ;; Perform word wrapping as if we have at least one character.
	    (render-inline-wrapped string 1))
	   (no-room?
	    ;; No room remaining on this line.
	    ;; Try again at the start of the next line.
	    (render-newline 1)
	    (render-inline string))
	   (t
	    ;; We have have room to put a word.
	    ;; Apply word wrapping.
	    (render-inline-wrapped string remaining-width)))))

     (define (render-prefix prefix)
       (render-unformatted prefix)
       (set! freshline? t))

     (define (render-newline count)
       (when (positive? count)
	 (repeat (lambda () (format stream "~%")) count)
	 (set! column 0)
	 (set! freshline? t)
	 (for-each render-prefix (reverse prefixes))))

     (define (render-freshline)
       (unless freshline?
	 (render-newline 1)))

     (define (render-preformatted-text text)
       (for-each (lambda (line)
		   (render-freshline)
		   (render-unformatted line))
		 (split-string text #\newline)))

     (define (copy-with-new-stream new-stream)
       (%markup-string-renderer new-stream width prefixes freshline? column word-wrap?))

     (define (render-without-word-wrap render)
       (set! word-wrap? nil)
       [render self]
       (set! word-wrap? t))

     (define self
       (make-text-renderer render-freshline render-newline
			   render-prefix render-inline
			   render-preformatted-text
			   render-without-word-wrap
			   push-prefix pop-prefix
			   copy-with-new-stream))
     self)

   (define (markup-string-renderer stream width)
     "Create a markup renderer that can render to a string."
     (assert (streamp stream))
     (assert (not (negative? width)))
     (%markup-string-renderer stream width () t 0 t))

   (expose-functions markup-string-renderer)))

(export
 (define (render-inline renderer . strings)
   "Render without escaping any chars at the end of the current line."
   [(text-renderer-render-inline renderer) (string-append* strings)]))

(export
 (defparameter *escape-chars*
   (string->list "*_<>\\|")))
(export
 (define (render-inline-escaped renderer . strings)
   "Apply escape-string to strings before rendering inline."
   (render-inline renderer (escape-string (string-append* strings) *escape-chars*))))

(export
 (define (render-freshline renderer)
   "If at the start of a line (ignoring prefixes) do nothing. Otherwise render a newline with prefixes."
   [(text-renderer-render-freshline renderer)]))
(export
 (define (render-preformatted-text renderer text)
   "Render text without word-wrap. Render prefixes at each new line."
   [(text-renderer-render-preformatted-text renderer) text]))
(export
 (define (render-newline renderer (count 1))
   "Render a newline, and any prefixes."
   [(text-renderer-render-newline renderer) count]))
(export
 (define (push-render-prefix renderer prefix)
   "Start prefixing lines with the given prefix (in addition to other prefixes)."
   [(text-renderer-push-prefix renderer) prefix]))
(export
 (define (pop-render-prefix renderer)
   "Stop prefixing lines with the last pushed prefix."
   [(text-renderer-pop-prefix renderer)]))
(export
 (define (copy-renderer-with-new-stream renderer stream)
   "Return a copy of renderer, set to render to stream."
   [(text-renderer-copy-with-new-stream renderer) stream]))
(export
 (define (render-without-word-wrap renderer render)
   "Return a copy of renderer, set to render with word-wrap disabled"
   [(text-renderer-render-without-word-wrap renderer) render]))
(export
 (define (render-prefix renderer prefix-text)
   "Render prefix-text setting the freshline status to true."
   [(text-renderer-render-prefix renderer) prefix-text]))

(export
 (define (render-length renderer render)
   "Return the length of the resulting string, if render were to be called with the given renderer.
Makes a temporary copy of the renderer so that renderer is not affected."
   (assert (and (procedure? render) (has-specific-arity? (procedure-arity render) 1)))
   (length (with-output-to-string (stream)
	     (let ((temp (copy-renderer-with-new-stream renderer stream)))
	       [render temp])))))

(export
 (defparameter *markup-render-width* 20))
(export
 (define (render-markup-to-string markup)
   "Render the markup object to a string."
   (with-output-to-string (s)
     (render-markup markup (markup-string-renderer s *markup-render-width*)))))


(export
 (define (inline-text? object)
   "A string is inline-text if it doesn't contain a newline or tabs."
   (and (string? object)
	(not (char= #\space (aref object 0)))
	(not (char= #\space (aref object (1- (length object)))))
	(not (find #\newline object))
	(not (find #\tab object)))))

(define (replace-newlines-with-spaces string)
  "Return string with all newlines replaced with spaces."
  (define (replace-newline char)
    (if (char= char #\newline)
	#\space
	char))
  (string-map replace-newline string))

(define (remove-carriage-returns string)
  "Return string without any carriage returns."
  (remove #\return string))

(define (block-text string)
  "Removes carriage returns. Replaces newlines with spaces."
  (replace-newlines-with-spaces (remove-carriage-returns string)))

(export
 (define (inline-text text)
   "Return an inline markup object that can render text.
Text must be inline. See inline-text?"
   (define constructor
     `(inline-text ,text))
   (define (render renderer)
     (render-inline-escaped renderer text))
   (define tag
     (html-tag "span"
	       (list text)
	       ()))

   (assert (inline-text? text))
   (markup constructor render t tag)))

(export
 (define (heading level inline-markup)
   "Given a level from 1-6 and an inline-markup, create a heading.
# Heading level 1
#### *Bold Heading* level 4"
   (define constructor
     `(heading ,level ,(markup-constructor inline-markup)))
   (define (render renderer)
     (render-freshline renderer)
     (render-inline renderer
		    (chars-string #\# level)
		    " ")
     (render-markup inline-markup renderer)
     (render-freshline renderer)
     (render-newline renderer))
   (define tag
     (html-tag (format nil "h~A" level)
	       (list (markup-html-tag inline-markup))
	       ()))
   
   (assert (<= 1 level 6))
   (assert (inline-markup? inline-markup))
   (markup constructor render nil tag)))

;; Paragraphs are separated by blank lines
;;
;; Paragraph text...
;; More paragraph text...
;;
(export
 (define (paragraph . strings-and-markups)
   "Create a paragraph given a strings-and-markups.
Carriage returns will be removed and newlines will be replaced with spaces.
Word-wrap will automatically be applied to paragraphs. Texts are joined with space.
See block-text."
   (define constructor
     `(paragraph ,@strings-and-markups))
   (define (render renderer)
     (render-freshline renderer)
     (for-each (lambda (string-or-markup)
		 (if (string? string-or-markup)
		     (render-inline renderer (block-text string-or-markup))
		     (render-markup string-or-markup renderer)))
	       strings-and-markups)
     (render-freshline renderer)
     (render-newline renderer))
   (define tag
     (html-tag "p" (map (lambda (v)
			  (if (markup? v)
			      (markup-html-tag v)
			      v))
			strings-and-markups) ()))

   (assert (for-all (lambda (v) (or (string? v) (markup? v))) strings-and-markups))
   (markup constructor render nil tag)))

;; Line break. Line with 2+ spaces followed by a return.
(export
 (define (br)
   "Return a line-break."
   (define constructor
     `(br))
   (define (render renderer)
     (render-newline renderer))
   (define tag
     (html-tag "br" () ()))
   (markup constructor render nil tag)))

(export
 (define (bold markup)
   "Return a markup that has been emboldened. If the given markup is inline,
the returned markup will also be inlined. Rendered as *bold*."
   (define constructor
     `(bold ,(markup-constructor markup)))
   (define (render renderer)
     (cond
       ((inline-markup? markup)
	(render-inline renderer "*" (render-markup-to-string markup) "*"))
       (t
	(render-inline renderer "*")
	(render-markup markup renderer)
	(render-inline renderer "*"))))
   (define tag
     (html-tag "b" (list (markup-html-tag markup)) ()))

   (markup constructor render (inline-markup? markup) tag)))

(export
 (define (italic markup)
   "Return a markup that has been italicized. If the given markup is inline,
the returned markup will also be inlined. Rendered as _italics_."
   (define constructor
     `(italic ,(markup-constructor markup)))
   (define (render renderer)
     (cond
       ((inline-markup? markup)
	(render-inline renderer "_" (render-markup-to-string markup) "_"))
       (t
	(render-inline renderer "_")
	(render-markup markup renderer)
	(render-inline renderer "_"))))
   (define tag
     (html-tag "i" (list (markup-html-tag markup)) ()))

   (markup constructor render (inline-markup? markup) tag)))

(export
 (define (block-quote markup)
   "Return a markup object with the given markup as quoted text.

Block quote
> quoted text
>
> with multiple paragraphs
>> and a nested quote"
   (define constructor
     `(block-quote ,(markup-constructor markup)))
   (define (render renderer)
     (push-render-prefix renderer "> ")
     (render-prefix renderer "> ")
     (render-markup markup renderer)
     (pop-render-prefix renderer))
   (define tag
     (html-tag "blockquote"
	       (list (markup-html-tag markup))
	       ()))

   (markup constructor render nil tag)))

(export
 (define (list-item-tag markup)
   (html-tag "li" (list (markup-html-tag markup)) ())))

(export
 (define (ordered-list . markups)
   "Given a list of markup objects, return an ordered list.

Numbered lists
1. first
2. second
   1. first nested
   2. second nested
3. third"
   (define constructor
     `(ordered-list ,@(map #'markup-constructor markups)))
   
   (define (render renderer)
     (define (render-list-item markup num)
       "Render a single list item for the given 0-based index."
       (let ((number-text (format nil "~A. " num)))
	 ;; Push indentation
	 (push-render-prefix renderer (chars-string #\space (length number-text)))
	 ;; Render the number prefix.
	 (render-prefix renderer number-text))
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

   (define tag
     (html-tag "ol"
	       (map #'list-item-tag markups)
	       ()))
   
   (markup constructor render nil tag)))


(export
 (define (unordered-list . markups)
   "Given a list of markups, return a bullet-list markup object.

Unordered Lists
- item

    this item has a paragrah. it is indented 4 spaces.

- item
  - indented item
  - item
    ```
    this is a code block.
    ```."
   (define constructor
     `(ordered-list ,@(map #'markup-constructor markups)))
   (define (render renderer)
     (define (render-list-item markup)
       "Render a single list item. Prefix with a bullet point, and render markup with indentation."
       (let ((prefix-text "- "))
	 ;; Push indentation
	 (push-render-prefix renderer (chars-string #\space (length prefix-text)))
	 ;; Render a bullet point
	 (render-prefix renderer prefix-text))
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
   (define tag
     (html-tag "ul"
	       (map #'list-item-tag markups)
	       ()))
   (markup constructor render nil tag)))

(export
 (define (code inline-text)
   "Given the inline-text, return an inline code block. `inline code`"
   (define (render renderer)
     (render-inline renderer "``" inline-text "``"))
   (define constructor
     `(code ,inline-text))
   (define tag
     (html-tag "code" (list inline-text) ()))
   (assert (inline-text? inline-text))
   (markup constructor render t tag)))

(export
 (define (code-block preformatted-text)
   "Given the preformatted text string, return a fenced code block, using ``` as separators.

```
code...
```"
   (define text (remove-carriage-returns preformatted-text))
   (define (render renderer)
     ;; begin the code fence
     (render-inline renderer "```")
     ;; Render the text as preformatted-text
     (render-preformatted-text renderer text)
     ;; End the code fence
     (render-freshline renderer)
     (render-inline renderer "```"))
   (define constructor
     `(code-block ,text))
   (define tag
     (html-tag "pre" (list (html-tag "code" (list preformatted-text) ())) ()))
   (assert (string? preformatted-text))
   (markup constructor render nil tag)))

(export
 (define (horizontal-bar)
   "Construct a markup object which represents a horizontal separator bar.

__________
"
   (define constructor
     `(horizontal-bar))
   (define (render renderer)
     (render-freshline renderer)
     (render-newline renderer)
     (render-inline renderer "____________")
     (render-newline renderer 2))

   (define tag
     (html-tag "hr" () ()))
   (markup constructor render nil tag)))

(export
 (define (link url (inline-text))
   "Construct an inline-markup object which represents a link to a url. [link text](url) or <url>"
   (define constructor
     `(link ,url ,inline-text))
   (define (render renderer)
     (if inline-text
	 (render-inline renderer "[" inline-text "](" url ")")
	 (render-inline renderer "<" url ">")))
   (define tag
     (html-tag "a"
	       (when inline-text
		 (list inline-text))
	       (alist "href" url)))

   (assert (or (null? inline-text)
	       (inline-text? inline-text)))
   (assert (inline-text? url))
   (markup constructor render t tag)))

(define (rows->columns rows)
  "Convert a list of rows to a list of columns.
Assumes that each row has the same number of elements."
  (define (columns-iter rows columns)
    (cond
      ((empty? (first rows)) columns)
      (t (columns-iter (map #'rest rows)
		       (cons (map #'first rows) columns)))))

  (assert (list? rows))
  (cond
    ((empty? rows) ())
    (t (let ((num-columns (length (first rows))))
	 (unless (for-all (lambda (row) (= (length row) num-columns)) (rest rows))
	   (error "All rows must be the same length.")))
       (nreverse (columns-iter rows ())))))

(export
 (define (table column-headers rows (column-alignments (map (const :left) column-headers)))
   "Create a table given a list of column-headers and a list of rows. Each row must be a list
with the same number of elements as column-headers. Column-alignments must also
be the same length as column-headers, and each alignment should be one of '(:LEFT :RIGHT :CENTER).

| Header 1 | Header 2 |
| -------- | -------- |
| Row 1 C1 | Row 1 C2 |
| Row 2 C1 | Row 2 C2 |"
   (define constructor
     `(table (list ,@(map (lambda (header)
			    (markup-constructor header))
			  column-headers))
	     (list ,@(map (lambda (row)
			    `(list ,@(map #'markup-constructor row)))
			  rows))
	     ',column-alignments))

   (define (render-element-with-padding renderer markup pad-left-string pad-right-string)
     "Renders a single element in the table."
     (render-inline renderer " " pad-left-string)
     (render-markup markup renderer)
     (render-inline renderer pad-right-string " "))
   
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
   (define (render-formatted renderer)    
     (define (render-element markup column-width alignment pad-char)
       "Render an element of markup with the proper amount of padding."
       (let* ((pad-length (pad-length column-width (element-width renderer markup)))
	      (left-pad (left-pad alignment pad-length)))
	 (render-element-with-padding renderer markup
				      (chars-string pad-char left-pad)
				      (chars-string pad-char (right-pad pad-length left-pad)))))

     (define (column-width column)
       "Get the column-width based on the maximum element-width within column."
       (foldl #'max 0 (map (cut (element-width renderer _)) column)))

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
	       rows)
     (render-freshline renderer))

   (define (render renderer)
     (render-without-word-wrap renderer render-formatted))

   (define (row-tag element-tag-name row)
     (define (tag-element markup)
       (html-tag element-tag-name (list (markup-html-tag markup)) ()))
     (html-tag "tr" (map tag-element row) ()))
   (define tag
     (html-tag "table"
	       (list*
		(row-tag "th" column-headers)
		(map (lcurry row-tag "td") rows))
	       ()))
   
   (unless (for-all (lambda (column) (for-all #'inline-markup? column)) columns)
     (error "All elements of table must be inline-markup."))
   (unless (= (length column-headers) (length column-alignments))
     (error "Must have the same number of column-headers and column-alignments."))
   (unless (for-all (cut (member _ '(:left :right :center))) column-alignments)
     (error "Column alignments can only be one of '(:LEFT :RIGHT :CENTER)"))
   (markup constructor render nil tag)))

(export
 (define (seq . markups)
   "Return a sequence of markups. Inline markups will be rendered together directly or separated by a newline.
Non-inline markups will be rendered with a fresh line before/after."
   (define constructor
     `(seq ,@(map #'markup-constructor markups)))
   (define (render renderer)
     (for-each (lambda (markup)
		 (cond
		   ((inline-markup? markup)
		    (render-markup markup renderer)
		    (render-without-word-wrap renderer (cut (render-inline _ " "))))
		   (t (render-markup markup renderer))))
	       markups))
   (define (inner-markup-tag markup)
     (cond ((inline-markup? markup)
	    (html-tag "span" (list (markup-html-tag markup) " ") ()))
	   (t (markup-html-tag markup))))
   (define tag
     (html-tag "span" (map inner-markup-tag markups) ()))

   (markup constructor render nil tag)))


;; Examples

(defparameter *lorem-ipsum* "Dolorem voluptatem rerum omnis consequatur. Sit doloremque et tenetur adipisci vel qui eum. Sunt voluptas et nostrum est aperiam ipsa ut odio. Sit expedita dolorem officia et vel iure excepturi debitis. Et ea quos rerum ut repellat ab ut libero.")

(define (show-markup markup)
  (list (render-markup-to-string markup)
	(render-markup-html-tag-to-string markup)))


(show-markup (table (list (inline-text "Header1") (bold (inline-text "Header2")) (italic (bold (inline-text "header3"))))
		    (list (list (inline-text "1.1") (inline-text "1.2") (inline-text "1.3"))
			  (list (inline-text "2.1") (inline-text "2.2") (inline-text "2.3")))
		    '(:center :right :left)))

(show-markup (seq (heading 1 (inline-text "Book 1"))
		  (table (list (inline-text "Header1") (bold (inline-text "Header2")) (italic (bold (inline-text "header3"))))
			 (list (list (inline-text "1.1") (inline-text "1.2") (inline-text "1.3"))
			       (list (inline-text "2.1") (inline-text "2.2") (inline-text "2.3")))
			 '(:center :right :left))
		  (heading 2 (italic (inline-text "Book2")))
		  (inline-text "The quick brown fox") (bold (inline-text "jumped")) (inline-text "over the lazy * dog.")))

(show-markup (ordered-list
	      (unordered-list (inline-text "unordered element 1")
			      (paragraph "a long unquoted paragraph that spans at least a few lines.")
			      (ordered-list (inline-text "A")
					    (inline-text "B")
					    (unordered-list (inline-text "B.A")
							    (inline-text "B.B"))))))

(show-markup
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

(show-markup
 (link "https://www.freeformatter.com/html-escape.html" "\"The link formatter\""))

(show-markup
 (unordered-list (block-quote (unordered-list (block-quote (paragraph *lorem-ipsum*))
					      (inline-text "e2")))
		 (horizontal-bar)
		 (inline-text "e2")))

(show-markup (heading 3 (inline-text "Heading")))
(show-markup (heading 3 (code "Code")))
(show-markup (heading 3 (bold (inline-text "bold"))))

(show-markup (ordered-list (inline-text "element 1")
			   (inline-text "element 2")
			   (inline-text "element 3")))

(uninstall-syntax!)

;; Ideas for guards
;; a guard is a function which takes the same arguments as another function
;; a guard may perform checks on the function's arguments
;; a guarded function is a function which calls a guard before calling the underlying function
;; maybe there is a table of guarded<->unguarded functions
;; maybe guarded functions can return their unguarded functions
;; be able to switch between calling guarded/unguarded functions
;; be able to unguard individual functions
;; be able to use guards for generating documentation on types.


;; #guard(guard-clauses...)
;; #doc documentation-object
;; (documentation-string documentation-object) => string

;; (define name-and-args
;;   docstring or #doc documentation-object
;;   declarations
;;   #guard(guard-clauses...)
;;   defines
;;   body)


;; TODO: define clauses
;; TODO: examples

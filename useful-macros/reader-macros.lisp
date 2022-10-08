;; useful_macros.lisp -- A collection of really handy macros
;;
;; DM/HMSC  11/97
;; -----------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------

#+:lispworks
(progn
  (editor:setup-indent "nlet" 2)
  (editor:setup-indent "nlet-tail" 2)
  (editor:setup-indent "if-let" 2 2 4)
  (editor:setup-indent "when-let" 1)
  (editor:setup-indent "with-gensyms" 1 2 4)
  (editor:setup-indent "with-tail-pure-code" 2 2 4)
  (editor:setup-indent "aif" 2 2 4)
  (editor:setup-indent "aif*" 2 2 4)
  (editor:setup-indent "awhen" 2 2 4)
  (editor:setup-indent "alet" 2)
  (editor:setup-indent "alet-fsm" 0)
  (editor:setup-indent "arun-fsm" 2 2 4)
  (editor:setup-indent "with-slot-accessors" 2 2 4)
  (editor:setup-indent "define-bind*-handler" 2 2)
  (editor:setup-indent "defmacro!" 2)
  (editor:setup-indent "defpan" 2 2 4)
  ;; (editor:setup-indent "dlambda" 2 2 4)
  (editor:setup-indent "dlambda" 0 2 0 t)
  
  (editor:setup-indent "plambda" 2 2 4)
  (editor:setup-indent "ichain-before" 2 2 4)
  (editor:setup-indent "ichain-after" 2 2 4)
  (editor:setup-indent "ichain-intercept" 2 2 4)
  (editor:setup-indent "alet-hotpatch" 2 2 4)
  (editor:setup-indent "let-hotpatch" 2 2 4)
  (editor:setup-indent "sublet" 2 2 4)
  (editor:setup-indent "sublet*" 2 2 4)
  (editor:setup-indent "pandoriclet" 2 2 4)
  (editor:setup-indent "with-pandoric" 2 2 4)
  (editor:setup-indent "defpan" 2)
  (editor:setup-indent "dis" 2 2 4)
  (editor:setup-indent "with-fast-stack" 2 2 4)
  (editor:setup-indent "with-conses-counted" 2 2 4)
  (editor:setup-indent "with-cons-pool" 2 2 4)
  (editor:setup-indent "with-dynamic-cons-pool" 2 2 4)
  (editor:setup-indent "nif" 2 2 4)
  (editor:setup-indent "fast-progn" 2 2 4)
  (editor:setup-indent "safe-progn" 2 2 4)

  (editor:setup-indent "curried-lambda" 1)
  )

;; ------------------------------------------------------

;; --------------------------------------------------------------
;; Allow extended number syntax:
;;   - embedded underscore separators 123_445.789_443
;;   - allow 1+2j or 1-2j or just 2j, where j in [jJiI]
;;   - allow dates in yyyy/mm/dd format
;;   - allow sexigisimal time in |hh:mm:ss.ss| format (bars needed because of #\:)
;;   - allow hyphenated numbers as in telephone numbers, SSN's, and UUID's

(defun remove-separators (s)
  (delete #\, (remove #\_ s)))
  
(defun delete-separators (s)
  (delete #\, (delete #\_ s)))
  
(defun match-number (s)
  (multiple-value-bind (start end)
      (#~m/^[+-]?[0-9][0-9_,]*(\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?/ s)
    (when start
      (values (read-from-string (delete-separators (subseq s start end)))
              (subseq s end))
      )))

(defun match-complex-ij (s)
  (#~m/^[iIjJ]$/ s))

(defun convert-real-or-complex (s)
  (multiple-value-bind (val srest)
      (match-number s)
    (when val
      (cond ((= 0 (length srest)) val)
            ((match-complex-ij srest) (complex 0 val))
            ((multiple-value-bind (ival sresti)
                 (match-number srest)
               (and ival
                    (match-complex-ij sresti)
                    (complex val ival))))
            (t nil)))
    ))

(defun convert-sexigisimal (s)
  ;; hh:mm:ss.ss, or hh:mm
  (multiple-value-bind (start end gstart gend)
      (#~m/^([+-])?([0-9]+):([0-9]{1,2})(:[0-9]{1,2}(\.[0-9_,]*)?)?$/ s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((sign   (aref gstart 0))
           (hstart (aref gstart 1))
           (hend   (aref gend   1))
           (mstart (aref gstart 2))
           (mend   (aref gend   2))
           (sstart (aref gstart 3))
           (send   (aref gend   3))
           (sfrac  (aref gstart 4)))
        (ignore-errors
          (let* ((hh   (read-from-string (subseq s hstart hend)))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (valm (+ mm (* 60 hh)))
                 (ss   (if sstart
                           (read-from-string (delete-separators
                                              (subseq s (1+ sstart) send)))
                         0))
                 (val  (if sstart
                           (+ (* 60 valm)
                              (if sfrac
                                  (float ss 1d0)
                                ss))
                         valm)))
            (if (and sign
                     (char= (char s sign) #\-))
                (- val)
              val)
            ))))))

(defun convert-utc-date (s)
  ;; yyyy/mm/dd [hh:mm:ss] [UTC[+/-nn]]]
  ;; if UTC is elided then convert by default TZ and DST
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})([ ]+([0-9]{1,2}):([0-9]{1,2})(:([0-9]{1,2}))?)?([ ]+UTC([-+][0-9]{1,2})?)?$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart  (aref gstart 0))
           (yend    (aref gend   0))
           (mstart  (aref gstart 1))
           (mend    (aref gend   1))
           (dstart  (aref gstart 2))
           (dend    (aref gend   2))
           (hstart  (aref gstart 4))
           (hend    (aref gend   4))
           (mmstart (aref gstart 5))
           (mmend   (aref gend   5))
           (sstart  (aref gstart 7))
           (send    (aref gend   7))
           (utstart (aref gstart 8))
           (tzstart (aref gstart 9))
           (tzend   (aref gend   9)))
        (ignore-errors
          (let* ((yyyy (read-from-string (subseq s ystart  yend)))
                 (mm   (read-from-string (subseq s mstart  mend)))
                 (dd   (read-from-string (subseq s dstart  dend)))
                 (hrs  (if hstart
                           (read-from-string (subseq s hstart  hend))
                         0))
                 (mins (if mmstart
                           (read-from-string (subseq s mmstart mmend))
                         0))
                 (secs (if sstart
                           (read-from-string (subseq s sstart  send))
                         0))
                 (tz   (when utstart
                         (list
                          (if tzstart
                              (- (read-from-string (subseq s tzstart tzend)))
                            0)))))
            (apply #'encode-universal-time secs mins hrs dd mm yyyy tz))
          ))) ))

#|
;; now extended syntax in above def
(defun convert-utc-date (s)
  ;; yyyy/mm/dd
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2}) UTC$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 0))
           (yend   (aref gend   0))
           (mstart (aref gstart 1))
           (mend   (aref gend   1))
           (dstart (aref gstart 2))
           (dend   (aref gend   2)))
        (ignore-errors
          (let* ((yyyy (read-from-string (subseq s ystart yend)))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy 0)) ;; makes UTC date
          )))))
|#

#|
;; subsumed by convert-utc-date
(defun convert-date (s)
  ;; yyyy/mm/dd
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 0))
           (yend   (aref gend   0))
           (mstart (aref gstart 1))
           (mend   (aref gend   1))
           (dstart (aref gstart 2))
           (dend   (aref gend   2)))
        (ignore-errors
          (let* ((yyyy (read-from-string (subseq s ystart yend)))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy))
          )))))
|#

(defun convert-american-short-date (s)
  ;; mm/dd/yy 
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{1,2})/([0-9]{1,2})/([0-9]{1,2})$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 2))
           (yend   (aref gend   2))
           (mstart (aref gstart 0))
           (mend   (aref gend   0))
           (dstart (aref gstart 1))
           (dend   (aref gend   1)))
        (ignore-errors
          (let* ((yyyy (+ 2000 (read-from-string (subseq s ystart yend))))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy)
            ))))))

(defun convert-hyphenated-number (s)
  ;; xxxx-xx-xxxx  as in telephone numbers, SSN's, and UUID's
  (if (#~m/^[0-9]+(\-[0-9]+)*$/ s)
      (read-from-string (delete #\- s))))
    
(defun convert-other-base-number (s)
  ;; 0xNNNN_NNNN_NNN
  (when (or (#~m/^0[xXoObB]/ s)
            (#~m/^[0-9]+[rR]/ s))
    (ignore-errors
      (read-from-string (format nil "#~A" (remove-separators s))))
    ))
    
(defun read-extended-number-syntax (s)
  (cond ((convert-real-or-complex s))
        ((convert-sexigisimal s))
        ((convert-utc-date s))
        ;; ((convert-date s))
        ((convert-american-short-date s))
        ((convert-hyphenated-number s))
        ((convert-other-base-number s))
        ))

;; Reader macro for #N
;; Parses a variety of numbers

(defun |reader-for-#N| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((v   (read stream t nil t))
         (ans (or (when (or (stringp v)
                            (symbolp v))
                    (read-extended-number-syntax (string v)))
                  v)))
    (unless *read-suppress*
      ans)))

(set-dispatch-macro-character
 #\# #\n '|reader-for-#N|)

#| ;; example
#n1_000
#n|12:45|
#n2009/08/15
#n1+2j
|#
;; --------------------------------------

(defmacro! alet (letargs &rest body)
  `(let ((,a!this) ,@letargs)
     (setq ,a!this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply ,a!this params))))
  
(defmacro! alet* (letargs &rest body)
  `(let* ((,a!this) ,@letargs)
     (setq ,a!this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply ,a!this params))))
  
(defmacro! alet-fsm (&rest states)
  `(macrolet ((,a!state (s)
                `(setq ,',a!this #',s)))
     (labels (,@states) #',(caar states))))
  
(defmacro! arun-fsm (bindings feeder &rest clauses)
  `(block ,g!block
     (let ((,g!machine
            (alet ,bindings
                (macrolet ((,a!finish (val)
                             `(return-from ,',g!block ,val)))
                  (alet-fsm ,@clauses)))))
       (tagbody
        ,g!again
        (funcall ,g!machine ,feeder)
        (go ,g!again))
       )))

;; ----------------------------------------------------------------------
;; Swift-like string interpolation

#+:LISPWORKS
(defmacro string-interp (str)
  (let* ((pat   #.(lw:precompile-regexp "«.*»"))
         (nel   (length str))
         (parts nil)
         (fmt-str (with-output-to-string (s)
                    (um:nlet iter ((start  0))
                      (if (< start nel)
                          (multiple-value-bind (pos len)
                              (lw:find-regexp-in-string pat str :start start)
                            (cond (pos
                                   (let ((end (+ pos len)))
                                     (princ (subseq str start pos) s)
                                     (princ "~A" s)
                                     (push (read-from-string (subseq str (1+ pos) (1- end))) parts)
                                     (go-iter end)))
                                  (t
                                   (princ (subseq str start) s))
                                  ))
                        (princ (subseq str start) s)
                        )))))
    `(format nil ,fmt-str ,@(nreverse parts))
    ))

;; ----------------------------------------------------------------------
;; Nestable suggestion from Daniel Herring
;; rewritten (DM/RAL) using our state-machine macro
;; Use backslash for escaping literal chars.
;; E.g., #"this is a "test" of...."#
;; DM/RAL 12/21 - now incorporates Swift-style string interpolation.

(defun |reader-for-#"| (stream sub-char numarg)
   (declare (ignore sub-char numarg))
   (arun-fsm
       ;; initial bindings
       ((chars (make-rubber-vector
                :element-type 'character))
        (depth 1))
       ;; feeder clause
       (read-char stream)
     ;; state machine - initial state first
     (normal (ch)
             (case ch
               ((#\#) 
                (keep ch)
                (state read-sharp))
                   
               ((#\") 
                (state read-quote))
                   
               ((#\\)
                (state read-escape))
                   
               (t
                (keep ch))
               ))
     
     (read-escape (ch)
                  (keep ch)
                  (state normal))
     
     (read-sharp (ch)
                 (case ch
                   ((#\")
                    (keep ch)
                    (incf depth)
                    (state normal))
                   
                   ((#\\)
                    (state read-escape))
                       
                    (t
                     (keep ch)
                     (state normal))
                    ))
     
     (read-quote (ch)
                 (case ch
                   ((#\#)
                    (decf depth)
                    (when (zerop depth)
                      (we-are-done))
                    (keep #\")
                    (keep #\#)
                    (state normal))
                       
                   ((#\")
                    (keep ch))

                   ((#\\)
                    (keep #\")
                    (state read-escape))
                       
                   (t
                    (keep #\")
                    (keep ch)
                    (state normal))
                   ))
     ;; not a state, but becomes a labels clause that can be used
     (keep (ch)
           (vector-push-extend ch chars))
     (we-are-done ()
                  (finish (unless *read-suppress*
                            `(string-interp ,(split-and-trim chars)))
                          ))
     ))

(unless (fboundp 'whitespace-char-p)
  (defun whitespace-char-p (ch)
    (member ch '(#\space #\tab #\newline #\vt #\page #\return))))

(defun split-and-trim (vec &optional (ignore-first-line t))
  ;; vec is vector of character (aka string)
  ;;
  ;; Split vector at #\newlines, count minimum leading whitespace,
  ;; then remove that much whitespace from each line, then concatenate
  ;; the lines back to one vector.
  (labels ((leading-ws-count (line)
             (or (position-if (complement #'whitespace-char-p) line)
                 (length line))))
    (let* ((lines  (split-string vec :delims '(#\newline)))
           (lines-to-test (if ignore-first-line (cdr lines) lines))
           (nws     (reduce #'min (or (mapcar #'leading-ws-count lines-to-test) (list 0))))
           (trimmer (um:rcurry #'subseq nws))
           (trimmed (mapcar trimmer lines-to-test)))
      (when ignore-first-line
        (push (car lines) trimmed))
      (apply #'um:paste-strings #\newline trimmed)
      )))
   
(set-dispatch-macro-character
 #\# #\" '|reader-for-#"|)
  
;; --------------------------------------------
;; Reader macro for #>
;; like the Bourne shell > to-lists for surrounding strings

(defun |reader-for-#>| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (arun-fsm
      ;; bindings
      (pattern
       (patlen 0)
       output
       (outlen 0))
      ;; feeder
      (read-char stream)
    ;; machine states - initial first
    (start (ch)
           ;; get stop-pattern
           (cond ((char= ch #\newline)
                  (phase2))

                 ((whitespace-char-p ch)
                  (state skip-to-eol))

                 (t 
                  (push ch pattern)
                  (incf patlen))
                 ))
    (skip-to-eol (ch)
                 (when (char= ch #\newline)
                   (phase2)))
    (phase2 ()
            (if (zerop patlen)
                (we-are-done)
              (state absorb)))
    (absorb (ch)
            ;; get string till stop-pattern
            (push ch output)
            (incf outlen)
            (when (and (>= outlen patlen)
                       (every #'char= pattern output))
              (we-are-done)))
    (we-are-done ()
                 (finish (unless *read-suppress*
                           (split-and-trim
                            (coerce
                             (nreverse
                              (nthcdr patlen output))
                             'string)
                            nil)
                           )))
    ))

#|
(defun first-arg (a &rest ignored)
  (declare (ignore ignored))
  a)
|#

(set-dispatch-macro-character
 #\# #\> '|reader-for-#>|)

#| ;; example
#>.end
This is a test
of the #> reader macro
.end
|#
;; --------------------------------------------

;; --------------------------------------------
;; Reader macro for #$
;; Takes a list and applies the car as a function to the cdr as parameters
;;
;; This version is from Martin Dirichs

(defvar $-reader-macros (make-hash-table :test 'equalp))

(defun |reader-for-#$| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((inp (read stream t nil t)))
    (if (and (consp inp)
             (symbolp (car inp)))
        (let ((fn (get-$-dispatch-reader (car inp))))
          (if fn
              (apply fn (cdr inp))
            (unless *read-suppress*
              (error "No $-Reader Macro for ~A" (car inp)))))
      (unless *read-suppress*
        (error "badly formed #$ input: ~A" inp)))))

(defun set-$-dispatch-reader (key fn)
  (unless (symbolp key)
    (error "$-dispatch names must be symbols"))
  (setf (gethash (string key) $-reader-macros) fn))

(defun get-$-dispatch-reader (key)
  (gethash (string key) $-reader-macros key))

#|
(set-dispatch-macro-character
 #\# #\$ '|reader-for-#$|)

(set-$-dispatch-reader :test (lambda (&rest data)
                               (match data
                                 ((x) :when (numberp x) (/ x))
                                 (_   (list 'quote data)))))

#$(:test 15)
#$(:test :this)
|#
;; ----------------------------------------------------------
;; Reader for #/
;; Takes a function name and applies to stream following the second '/'

(defvar /-reader-macros (make-hash-table :test 'equalp))

(defun |reader-for-#/| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key    (first (segment-reader stream #\/ 1)))
         (reader (get-/-dispatch-reader key)))
    (if reader
        (funcall reader stream)
      (unless *read-suppress*
        (error "No /-Reader Macro for ~A" key)))))

    
(defun set-/-dispatch-reader (key fn)
  (setf (gethash (string key) /-reader-macros) fn))

(defun get-/-dispatch-reader (key)
  (gethash (string key) /-reader-macros))

(set-dispatch-macro-character
 #\# #\/ '|reader-for-#/|)

#|
(set-/-dispatch-reader "test"
                       (lambda (stream)
                         (let ((data (read stream t nil t)))
                           (match data
                             ((x) :when (numberp x) (/ x))
                             (_   (list 'quote data))))))

#/test/1.2
#/test/this
|#
;; ---------------------------------------------------
;; Symbol Aliases #?name
;; #?name looks up name in per-package alist and returns cdr symbol

(defvar *symbol-aliases-table* (make-hash-table)) ;; one alist per package

(defun aliases (&optional (package *package*))
  (gethash (find-package package) *symbol-aliases-table*))

(defsetf aliases (&optional (package *package*)) (alist)
  `(setf (gethash (find-package ,package) *symbol-aliases-table*) ,alist))

(defun lookup-alias (keysym &optional (package *package*))
  (let* ((keysym-name (symbol-name keysym))
         (package     (find-package package)))
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern keysym-name package)))
    (or (cdr (assoc keysym (aliases package)))
        (error "No alias named ~A" keysym))))
  
(defun alias (keysym sym &optional (package *package*))
  (unless keysym
    (error "Can't alias NIL"))
  (let* ((package (find-package package))
         (alist   (aliases package)))
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern (symbol-name keysym) package)))
    (if sym
        (let ((pair (assoc keysym alist)))
          (if pair
              (setf (cdr pair) sym)
            (setf (aliases package) (acons keysym sym alist))) )
      ;; else -- (alias keysym) without an assoc simply unaliases the keysym
      (setf (aliases package) (delete keysym alist :key 'first))) ))

(defun unalias (keysym &optional (package *package*))
  (alias keysym nil package))

(defun |reader-for-#?| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key (read stream t nil t)))
    (unless *read-suppress*
      (lookup-alias key)) )) ;; note: can't alias NIL

(set-dispatch-macro-character
 #\# #\? '|reader-for-#?|)

#| ;; example
(alias 'this 'that)
(quote #?this)
(aliases)
(unalias 'this)
|#

;; ---------------------------------------------------
#|
;; Package Aliases #!name
;; #!name looks up name in per-package alist and returns cdr symbol

(defvar *symbol-aliases-table* (make-hash-table)) ;; one alist per package

(defun aliases (&optional (package *package*))
  (gethash (find-package package) *symbol-aliases-table*))

(defsetf aliases (&optional (package *package*)) (alist)
  `(setf (gethash (find-package ,package) *symbol-aliases-table*) ,alist))

(defun lookup-alias (keysym &optional (package *package*))
  (let* ((keysym-name (symbol-name keysym))
         (package     (find-package package)))
    #|
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern keysym-name package)))
    |#
    (or (cdr (assoc keysym (aliases package)))
        (error "No alias named ~A" keysym))))
  
(defun alias (keysym sym &optional (package *package*))
  (unless keysym
    (error "Can't alias NIL"))
  (let* ((package (find-package package))
         (alist   (aliases package)))
    #|
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern (symbol-name keysym) package)))
    |#
    (if sym
        (let ((pair (assoc keysym alist)))
          (if pair
              (setf (cdr pair) sym)
            (setf (aliases package) (acons keysym sym alist))) )
      (setf (aliases package) (delete keysym alist :key 'first))) ))

(defun unalias (keysym &optional (package *package*))
  (alias keysym nil package))

(defun |reader-for-#?| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key (read stream t nil t)))
    (lookup-alias key)) ) ;; note: can't alias NIL

(set-dispatch-macro-character
 #\# #\? '|reader-for-#?|)

#| ;; example
(alias 'this 'that)
(quote #?this)
(aliases)
(unalias 'this)
|#
|#
;; ------------------------------------------------------------

(defun read-chars-till-delim (stream delims &rest first-char)
  (let ((chars (copy-list first-char)))
    (do ((ch (read-char stream)
             (read-char stream)))
        ((find ch delims))
      (push ch chars))
    (coerce (nreverse chars) 'string)))
             
;; -------------------------------------------------------

(defmacro! defaliasfn (new-name old-name)
  `(defun ,new-name (&rest ,g!args)
     (apply ',old-name ,g!args)))

(defmacro! defcapture (new-name old-name)
  `(unless (fboundp ',new-name)
     (when (fboundp ',old-name)
       (setf (symbol-function ',new-name) (symbol-function ',old-name)))))
     
;; -------------------------------------------------------

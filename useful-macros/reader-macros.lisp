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

(defpackage :com.ral.useful-macros.reader-macros
  (:use :common-lisp :com.ral.useful-macros))

(in-package :com.ral.useful-macros.reader-macros)

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
      (#~m/^[+-]?[0-9][0-9]*(\.[0-9]*([eEdDsSfF][+-]?[0-9]+)?)?/ s)
    (when start
      (values (read-from-string (subseq s start end))
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
  ;; return sec
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
                 (valm (* 60 (+ mm (* 60 hh))))
                 (ss   (if sstart
                           (read-from-string (subseq s (1+ sstart) send))
                         0))
                 (val  (if sstart
                           (+ valm
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
      ;; Our upgraded Lisp Reader already handles these.
      ;; See: 
      (read-from-string (concatenate 'string "#" s)))
    ))
    
(defun read-extended-number-syntax (s)
  (let ((s  (remove-separators s))) ;; sep "," or "_"
    (cond ((convert-real-or-complex s))
          ((convert-sexigisimal s))
          ((convert-utc-date s))
          ;; ((convert-date s))
          ((convert-american-short-date s))
          ((convert-hyphenated-number s))
          ((convert-other-base-number s))
          )))

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
#N|123-45-6789|
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

(defmacro string-interp (str)
  ;; Substitute value of form following $ inside string.
  ;; Escape literal $ using "...\\$..."
  ;; Prefer established $ conventions. No need for ${}, since we are Lisp.
  (let* ((len   (length str))
         (parts nil)
         (fmt   (with-output-to-string (s)
                  (nlet iter ((start  0))
                    (flet
                        ((final ()
                           (princ (subseq str start) s))
                         (stuff-it (pos)
                           (cond ((or (zerop pos)
                                      (and (plusp pos)
                                           (char/= (char str (1- pos)) #\\)
                                           (princ (subseq str start pos) s)
                                           ))
                                  (princ "~A" s)
                                  (multiple-value-bind (val new-pos)
                                      (read-from-string str t nil
                                                        :start (1+ pos)
                                                        :preserve-whitespace t)
                                    (push val parts)
                                    (go-iter new-pos)))
                                 
                                 (t
                                  ;; elide the #\\
                                  (princ (subseq str start (1- pos)) s)
                                  (princ #\$ s)
                                  (go-iter (1+ pos)))
                                 )))
                      (when (< start len)
                        (let ((pos (position #\$ str :start start)))
                          (if pos
                              (stuff-it pos)
                            (final))))
                      ))
                  )))
    `(format nil ,fmt ,@(nreverse parts))
    ))

;; -------------------------------------------------------------------------------

#+:SBCL
(defun whitespace-char-p (ch)
  (sb-unicode:whitespace-p ch))

(defun trim-common-leading-ws-from-lines (str)
  ;; split str into lines, discarding common leading whitespace, then
  ;; rejoin into one string
  (labels ((get-trimmed-lines (&optional (start 0) min-nws lines)
             (let* ((p    (position #\newline str :start start))
                    (new-start (and p (1+ p)))
                    (line (subseq str start new-start))
                    ;; lines with only whitespace do not contribute to the leading whitespace calc.
                    (pnws (position-if (complement #'whitespace-char-p) line))
                    (new-min-nws (or
                                  (and pnws
                                       min-nws
                                       (min min-nws pnws))
                                  pnws
                                  min-nws))
                    (new-lines (acons (and pnws
                                           (plusp pnws))
                                      (or
                                       (and pnws
                                            line)
                                       (and p
                                            #.(make-string 1 :initial-element #\newline))
                                       "")
                                      lines)))
               (if new-start
                   ;; counting on tail call optimization here...
                   (get-trimmed-lines new-start new-min-nws new-lines)
                 (nreverse
                  (mapcar (lambda (pair)
                            (if (car pair)
                                (subseq (cdr pair) new-min-nws)
                              (cdr pair)))
                          new-lines)))
               )))
    (apply #'concatenate 'string (get-trimmed-lines))
    ))

(defun interpolated-string (numarg str)
  (unless *read-suppress*
    (let ((ans (trim-common-leading-ws-from-lines str)))
      (if numarg
          `(string-interp ,ans)
        ans))
    ))

;; ----------------------------------------------------------------------
;; Nestable suggestion from Daniel Herring rewritten (DM/RAL) using
;; our state-machine macro Use backslash for escaping literal chars.
;; E.g., #"this is a "test" of...."#, i.e., read string up through
;; trailing ("#).
;;
;; DM/RAL 12/21 - now incorporates Swift-style string interpolation.
;; DM/RAL 2022/11/04 06:52:54 - use prefix numarg to cause string
;; interpolation, as in #1> or #1".
;;
;; DM/RAL 2024/10/27 11:25:43 UTC - cleaned up substantially by going
;; back to Doug Comer's original code.

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  ;; numarg here means interpolation
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (let ((chars (coerce (nreverse chars) 'string)))
      ;; this bit is added to Doug's code to enable string interpolation
      (interpolated-string numarg chars))
    ))


;;; #|
;;; ;; Doug Comer's original definition
;;; (defun |#"-reader| (stream sub-char numarg)
;;;   (declare (ignore sub-char numarg))
;;;   (let (chars)
;;;     (do ((prev (read-char stream) curr)
;;;          (curr (read-char stream) (read-char stream)))
;;;         ((and (char= prev #\") (char= curr #\#)))
;;;       (push prev chars))
;;;     (unless *read-suppress*
;;;       (coerce (nreverse chars) 'string))))
;;; |#

(set-dispatch-macro-character
  #\# #\" #'|#"-reader|)

;;; #|
;;; (defun |reader-for-#"| (stream sub-char numarg)
;;;    (declare (ignore sub-char))
;;;    (arun-fsm
;;;        ;; initial bindings
;;;        ((chars (make-rubber-vector
;;;                 :element-type 'character))
;;;         (depth 1))
;;;        ;; feeder clause
;;;        (read-char stream)
;;;      ;; state machine - initial state first
;;;      (normal (ch)
;;;              (case ch
;;;                ((#\#) 
;;;                 (keep ch)
;;;                 (state read-sharp))
;;;                    
;;;                ((#\") 
;;;                 (state read-quote))
;;;                    
;;;                ((#\\)
;;;                 (state read-escape))
;;;                    
;;;                (t
;;;                 (keep ch))
;;;                ))
;;;      
;;;      (read-escape (ch)
;;;                   (keep ch)
;;;                   (state normal))
;;;      
;;;      (read-sharp (ch)
;;;                  (case ch
;;;                    ((#\")
;;;                     (keep ch)
;;;                     (incf depth)
;;;                     (state normal))
;;;                    
;;;                    ((#\\)
;;;                     (state read-escape))
;;;                        
;;;                     (t
;;;                      (keep ch)
;;;                      (state normal))
;;;                     ))
;;;      
;;;      (read-quote (ch)
;;;                  (case ch
;;;                    ((#\#)
;;;                     (decf depth)
;;;                     (when (zerop depth)
;;;                       (we-are-done))
;;;                     (keep #\")
;;;                     (keep #\#)
;;;                     (state normal))
;;;                        
;;;                    ((#\")
;;;                     (keep ch))

;;;                    ((#\\)
;;;                     (keep #\")
;;;                     (state read-escape))
;;;                        
;;;                    (t
;;;                     (keep #\")
;;;                     (keep ch)
;;;                     (state normal))
;;;                    ))
;;;      ;; not a state, but becomes a labels clause that can be used
;;;      (keep (ch)
;;;            (vector-push-extend ch chars))
;;;      (we-are-done ()
;;;                   (finish (unless *read-suppress*
;;;                             (let ((ans (if numarg
;;;                                            chars
;;;                                          (trim-common-leading-ws-from-lines chars))))
;;;                               (if numarg
;;;                                   ans
;;;                                 `(string-interp ,ans)))
;;;                             )))
;;;      ))

;;; (set-dispatch-macro-character
;;;  #\# #\" '|reader-for-#"|)

;;; |#

;; --------------------------------------------
;; Reader macro for #>
;; like the Bourne shell > to-lists for surrounding strings
;; String is interpolatable. Use prefix numarg to enable interpolation.
;;
;; DM/RAL 2024/10/27 11:25:43 UTC - cleaned up substantially by going
;; back to Doug Comer's original code.

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  ;; numarg enables string interpolation
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                (cdr pointer)
                pattern))
        (if (null pointer)
          (return)))
      (let ((str (coerce
                  (nreverse
                   (nthcdr (length pattern) output))
                  'string)
                 ))
        ;; this bit is added to Doug's code to enable string interpolation
        (interpolated-string numarg str))
      )))

(set-dispatch-macro-character
  #\# #\> #'|#>-reader|)

;;; #|
;;; ;; Doug Comer's original code
;;; (defun |#>-reader| (stream sub-char numarg)
;;;   (declare (ignore sub-char numarg))
;;;   (let (chars)
;;;     (do ((curr (read-char stream)
;;;                (read-char stream)))
;;;         ((char= #\newline curr))
;;;       (push curr chars))
;;;     (let* ((pattern (nreverse chars))
;;;            (pointer pattern)
;;;            (output))
;;;       (do ((curr (read-char stream)
;;;                  (read-char stream)))
;;;           ((null pointer))
;;;         (push curr output)
;;;         (setf pointer
;;;               (if (char= (car pointer) curr)
;;;                 (cdr pointer)
;;;                 pattern))
;;;         (if (null pointer)
;;;           (return)))
;;;       (coerce
;;;         (nreverse
;;;           (nthcdr (length pattern) output))
;;;         'string))))

;;; (set-dispatch-macro-character
;;;   #\# #\> #'|#>-reader|)
;;; |#

;;; #|
;;; (defun |reader-for-#>| (stream sub-char numarg)
;;;   (declare (ignore sub-char))
;;;   ;; numarg means string interpolation
;;;   (arun-fsm
;;;       ;; bindings
;;;       (tstpos 
;;;        (pattern (make-array 16
;;;                             :element-type (stream-element-type stream)
;;;                             :adjustable t
;;;                             :fill-pointer 0))
;;;        (s  (make-array 16
;;;                        :element-type (stream-element-type stream)
;;;                        :adjustable t
;;;                        :fill-pointer 0)))
;;;       ;; feeder
;;;       (read-char stream nil stream t)
;;;     ;; machine states - initial first
;;;     (start (ch)
;;;            ;; get stop-pattern
;;;            (cond ((eq ch stream) ;; eof
;;;                   (we-are-done ""))
;;;                  
;;;                  ((char= ch #\newline)
;;;                   (phase2))
;;;                  
;;;                  ((whitespace-char-p ch)
;;;                   (state skip-to-eol))
;;;                  
;;;                  (t
;;;                   (vector-push-extend ch pattern))
;;;                  ))
;;;     (skip-to-eol (ch)
;;;                  ;; ignore everything after pattern to EOL
;;;                  (cond ((eq ch stream) ;; eof
;;;                         (we-are-done ""))
;;;                        
;;;                        ((char= ch #\newline)
;;;                         (phase2))
;;;                        ))
;;;     (phase2 ()
;;;             (setf tstpos  (- (length pattern)))
;;;             (if (zerop tstpos)
;;;                 (we-are-done "")
;;;               (state absorb)))
;;;     (absorb (ch)
;;;             (cond ((eq ch stream)
;;;                    (we-are-done s))
;;;                   
;;;                   (t
;;;                    (vector-push-extend ch s)
;;;                    (incf tstpos)
;;;                    (when (and (>= tstpos 0)
;;;                               (string= pattern s :start2 tstpos))
;;;                      (setf (fill-pointer s) tstpos)
;;;                      (we-are-done s)
;;;                      ))
;;;                   ))
;;;     (we-are-done (str)
;;;                  (finish (unless *read-suppress*
;;;                            (let ((ans (trim-common-leading-ws-from-lines str)))
;;;                              (if numarg
;;;                                  `(string-interp ,ans)
;;;                                ans))
;;;                            )))
;;;     ))

;;; (set-dispatch-macro-character
;;;  #\# #\> '|reader-for-#>|)
;;; |#


#| ;; example
(progn 
#>.end
This is a test
of the #> reader macro
.end)
|#

#|
(let ((x #>.end
        this
        is

          a

        test
        .end))
  (terpri)
  (princ x))
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
    (unless *read-suppress*
      (if (and (consp inp)
               (symbolp (car inp)))
          (let ((fn (get-$-dispatch-reader (car inp))))
            (if fn
                (apply fn (cdr inp))
              (error "No $-Reader Macro for ~A" (car inp))))
        (error "badly formed #$ input: ~A" inp)))))

(defun set-$-dispatch-reader (key fn)
  (unless (symbolp key)
    (error "$-dispatch names must be symbols"))
  (setf (gethash (string key) $-reader-macros) fn))

(defun get-$-dispatch-reader (key)
  (gethash (string key) $-reader-macros key))

(set-dispatch-macro-character
 #\# #\$ '|reader-for-#$|)

#|
(set-$-dispatch-reader :test (ac:alambda
                              ((x) when (numberp x)
                               (/ x))
                              (data
                               (list 'quote data))))

#|
#$(:test 15)
#$(:test :this)
|#
|#
;; ----------------------------------------------------------
;; Reader for #/
;; Takes a function name and applies to stream following the second '/'

(defvar /-reader-macros (make-hash-table :test 'equalp))

(defun |reader-for-#/| (stream sub-char numarg)
  (let* ((key    (first (segment-reader stream #\/ 1)))
         (reader (get-/-dispatch-reader key)))
    (unless *read-suppress*
      (if reader
          (funcall reader stream sub-char numarg)
        (error "No /-Reader Macro for ~A" key)))))

    
(defun set-/-dispatch-reader (key fn)
  (setf (gethash (string key) /-reader-macros) fn))

(defun get-/-dispatch-reader (key)
  (gethash (string key) /-reader-macros))

(set-dispatch-macro-character
 #\# #\/ '|reader-for-#/|)

#|
(set-/-dispatch-reader "test"
                       (lambda (stream &rest _)
                         (declare (ignore _))
                         (let ((data (read stream t nil t)))
                           (ac:match data
                             (x when (numberp x) (/ x))
                             (_   (list 'quote data))))))

#|
#/test/1.2
#/test/this
|#
|#
;; ----------------------------------------------------------
;; Literal String Reader - incl embedded quotes

(set-/-dispatch-reader "lit"
                       (lambda (stream &rest _)
                         (declare (ignore _))
                         (let ((delim (read-char stream t nil t)))
                           (case delim
                             (#\(  (setf delim #\)))
                             (#\[  (setf delim #\]))
                             (#\{  (setf delim #\}))
                             (#\«  (setf delim #\»)) )
                           (read-chars-till-delim stream (list delim))
                           )))

(set-macro-character #\} (get-macro-character #\) nil))
(set-macro-character #\] (get-macro-character #\) nil))
(set-macro-character #\» (get-macro-character #\) nil))
;; ----------------------------------------------------------

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

;; ------------------------------------------------------------

(defun read-chars-till-delim (stream delims &rest first-char)
  (let ((chars (copy-list first-char)))
    (do ((ch (read-char stream nil stream)
             (read-char stream nil stream)))
        ((or (eq ch stream)
             (find ch delims)))
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

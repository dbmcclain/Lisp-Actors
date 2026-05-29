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
;; DM/RAL  2026/05/28 10:10:55 UTC -- upgraded reader allows for colons embedded in numbers.
;;   1:23:32.35
;;   1:23
;;   1°23′32″.35  (not quote and double quote, use ^z-' and ^z-")
;;   1°23′        etc...
;; --------------------------------------------------------------
;; Allow extended number syntax:
;;   - embedded underscore separators 123_445.789_443
;;   - allow 1+2j or 1-2j or just 2j, where j in [jJiI]
;;   - allow dates in yyyy/mm/dd format
;;   - allow sexigisimal time in hh:mm:ss.ss format
;;   - allow hyphenated numbers as in telephone numbers, SSN's, and UUID's

(defun remove-separators (s)
  (delete #\, (remove #\_ s)))

#| ;; Unused...
(defun delete-separators (s)
  (delete #\, (delete #\_ s)))
|#
#|
(cl-ppcre:parse-string "^[+-]?[0-9]+(\\.[0-9]*([eEdDsSfF][+-]?[0-9]+)?)?")
(cl-ppcre:parse-string "^[iIjJ]$")
(cl-ppcre:parse-string "^([+-])?([0-9]+):([0-5][0-9])(:[0-5][0-9](\\.[0-9_,]*)?)?$")
(cl-ppcre:parse-string "^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})([ ]+([0-9]{1,2}):([0-9]{1,2})(:([0-9]{1,2}))?)?([ ]+UTC([-+][0-9]{1,2})?)?$")
(cl-ppcre:parse-string "^[0-9]+(-[0-9]+)*$")
(cl-ppcre:parse-string "^[0-9]+(\\.[0-9]+)*$")
(match-number "100.")
(ppcre:parse-string "^([+-])?([0-9]+)[o°]((([0-9]{1,2})['])([0-9]{1,2}[\"]([.][0-9_,]*)?)?)?$")
|#

(defun match-number (s)
  (multiple-value-bind (start end)
      (#~m/^[+-]?[0-9]+([.][0-9]*([eEdDsSfF][+-]?[0-9]+)?)?/ s)
    (when start
      (multiple-value-bind (val tailpos)
          (read-from-string s t nil :start start :end end)
        (and (eql tailpos end)
             (values val (subseq s end)))
        ))))

(defun match-complex-ij (s)
  (#~m/^[iIjJ]$/ s))

(defun convert-real-or-complex (s)
  (multiple-value-bind (val srest)
      (match-number s)
    (when val
      (cond ((zerop (length srest)) val)
            ((match-complex-ij srest) (complex 0 val))
            ((multiple-value-bind (ival sresti)
                 (match-number srest)
               (and ival
                    (match-complex-ij sresti)
                    (complex val ival))))
            ))
    ))

;; --------------------------------------------
#|
(defun tst (s)
  (#~m/^([+-])?([0-9]+):([0-9]{1,2}([.][0-9]+||:([0-9]{1,2}([.][0-9]+)?)))$/ s))

(tst "1:00")
(tst "1:00.234")
(tst "1:00:00")
(tst "1:00:00.234")

(convert-sexigisimal-1 "1:00")
(convert-sexigisimal-1 "1:00.234")
(convert-sexigisimal-1 "1:00:00")
(convert-sexigisimal-1 "1:00:00.234")
|#

(defun convert-sexigisimal-1 (s)
  ;; With colon separators, there is no way to ascertain degrees or hours. Just assume degrees.
  ;; ddd:mm.mmmm or ddd:mm:ss.ssss
  ;; return arcsec
  (multiple-value-bind (start end gstart gend)
      (#~m/^([+-])?([0-9]+):([0-9]{1,2}([.][0-9]+||:([0-9]{1,2}([.][0-9]+)?)))$/ s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((sign   (aref gstart 0))
           (hstart (aref gstart 1))
           (hend   (aref gend   1))
           (mstart (aref gstart 2.))
           (sstart (aref gstart 4.)))
        (let* ((ss   (if sstart
                         (read-from-string s t nil
                                           :start sstart)
                       0))
               (mm   (* 60. (read-from-string s t nil
                                              :start mstart
                                              :end (and sstart
                                                        (1- sstart)))))
               (hh   (* 3600. (read-from-string s t nil
                                                :start hstart
                                                :end hend)))
               (sgn  (if (and sign
                              (char= #\- (char s sign)))
                         -1
                       1)))
          (* 1/1296000 sgn (+ hh mm ss)) ;; convert to turns
          )))))

;; --------------------------------------------
#|
(defun tst (s)
  (#~m/^([+-])?([0-9]+)[dh]([.][0-9]+||([0-9]{1,2})m([.][0-9]+||([0-9]{1,2})s([.][0-9]+)?)?)?$/ s))

(with-standard-io-syntax
  (pprint
   (ppcre:parse-string
    "^([+-])?([0-9]+)[dh°]([.][0-9]+|([0-9]{1,2})[m’'′]([.][0-9]+|([0-9]{1,2})[s”\"＂″]([.][0-9]+)?)?)?$"))
  (values))
  
(defun tst (s)
  (cl-ppcre:scan
       (load-time-value
        (ppcre:create-scanner
         "^([+-])?([0-9]+)[dh°]([.][0-9]+|([0-9]{1,2})[m’'′]([.][0-9]+|([0-9]{1,2})[s”\"＂″]([.][0-9]+)?)?)?$")
        t)
       s))

(tst "1d")
(tst "+1d")
(tst "+1d.234")
(tst "+1d00m")
(tst "+1d00m.234")
(tst "+1d00m00s")
(tst "+1d00m00s.234")

(convert-sexigisimal-2 "1d")
(convert-sexigisimal-2 "+1h")
(convert-sexigisimal-2 "+1d.234")
(convert-sexigisimal-2 "+1d00m")
(convert-sexigisimal-2 "+1d00m.234")
(convert-sexigisimal-2 "+1°00’.234")
(convert-sexigisimal-2 "+1d00m00s")
(convert-sexigisimal-2 "+1d00m00s.234")
(convert-sexigisimal-2 "+1°00’00”.234")
(convert-sexigisimal-2 "+1°13’01”.234")
(convert-sexigisimal-2 "+1°13'01”.234")
(convert-sexigisimal-2 "+1h00m00s.234")
|#
#|
(cl-ppcre:parse-string "\\.[0-9]+")
(setf (cl-ppcre:parse-tree-synonym 'frac) (cl-ppcre:parse-string "[.][0-9]+"))
(lw:find-regexp-in-string "\\.\\([0-9]+\\)" "123.456" :brackets-limits t)
(inspect (lw:precompile-regexp "\\.\\([0-9]+\\)"))
(lw:find-regexp-in-string
 "^\\([+-]\\)?\\([0-9]+\\)[dh°]\\(\\.[0-9]+\\|\\([0-9][0-9]?\\)[m’'′]\\(\\.[0-9]+\\|\\([0-9][0-9]?\\)[s”\"＂″]\\(\\.[0-9]+\\)?\\)?\\)?$"
 "+1h00m00s.234"
 :brackets-limits t)
|#

#|
(convert-sexigisimal-2b "+1h00m00s.234")
(convert-sexigisimal-2b "+1h00m00s")
(convert-sexigisimal-2b "+1h00m.234")
(convert-sexigisimal-2b "+1h.234")
(convert-sexigisimal-2b "+1m.234")
(convert-sexigisimal-2b "+1m00s.234")
(convert-sexigisimal-2b "+1s.234")
|#

;; --------------------------------------------

(defun cvt-hms (sign whole tail frac follow-fn scale)
  ;; common code for both hours & minutes & seconds
  (let ((ans (if frac
                 (* scale
                    (read-from-string (concatenate 'string whole frac)))
               ;; else
               (let ((sub  (if tail
                               (funcall follow-fn tail)
                             0)))
                 (and sub
                      (+ sub (* scale (read-from-string whole))))
                 ))))
    (when ans
      (if (and sign
               (string= sign "-"))
          (- ans)
        ans))
    ))

(defun convert-seconds (s)
  (cl-ppcre:register-groups-bind (sign secs skind sfrac)
      ((load-time-value
        (ppcre:create-scanner
         "^([+-])?([0-9]+)([s\”\"\＂\″])(\\.[0-9]+)?$")
        t)
       s :sharedp t)
    ;; (format t "~%Secs: ~S ~S ~S" sign secs sfrac)
    (values (cvt-hms sign secs nil sfrac nil 1)
            skind)
    ))

(defun convert-minutes (s)
  (or
   (cl-ppcre:register-groups-bind (sign mins mkind mtail mfrac)
       ((load-time-value
         (ppcre:create-scanner
          "^([+-])?([0-9]+)([m\’\'\′])((\\.[0-9]+)|[0-9].*)?$")
         t)
        s :sharedp t)
     ;; (format t "~%Mins: ~S ~S ~S ~S" sign mins mtail mfrac)
     (return-from convert-minutes
       (values (cvt-hms sign mins mtail mfrac #'convert-seconds 60.)
               mkind)))
   ;; or
   (convert-seconds s)))

(defun convert-hours (s)
  #|
  ;; nnn°nn′nn″.nnn  or NNNhNNmNNs.NNN - can use 'd' or '°', ' (quote) or ′ U+2032, 
  ;; nnn′nn″.nnn  or  NNNmNNs.NNN      - double quote " or ″ U+2033
  ;; nnn″.nnn  or NNNs.NNN
  |#
  (or
   (cl-ppcre:register-groups-bind (sign degs dkind dtail dfrac)
       ((load-time-value
         (ppcre:create-scanner
          "^([+-])?([0-9]+)([dh°])((\\.[0-9]+)|[0-9].*)?$")
         t)
        s :sharedp t)
     ;; (format t "~%Degs: ~S ~S ~S ~S ~S" sign degs dkind dtail dfrac)
     (return-from convert-hours
       (values (cvt-hms sign degs dtail dfrac #'convert-minutes 3600.)
               dkind)))
   ;; or
   (convert-minutes s)))

(defun convert-sexigisimal-2b (s)
  (multiple-value-bind (val kind)
      (convert-hours s)
    (when val
      (* 1/1296000   ;; convert to turns
         (if (find (char kind 0) "hms")
             (* 15. val)
           val)))
    ))
                
;; --------------------------------------------
                   
#+nil     
(defun convert-sexigisimal-2a (s)
  #|
  ;; nnn°nn′nn″.nnn  or NNNhNNmNNs.NNN - can use 'd' or '°', ' (quote) or ′ U+2032, 
  ;; nnn′nn″.nnn  or  NNNmNNs.NNN      - double quote " or ″ U+2033
  ;; nnn″.nnn  or NNNs.NNN
  |#
  (or
   (cl-ppcre:register-groups-bind (sign degs dfrac mins mfrac secs sfrac)
       ;; nn°nn′nn″.nnn
       ((load-time-value
         (ppcre:create-scanner
          `        "^([+-])?([0-9]+)[dh°](\\.[0-9]+|([0-9]{1,2})[m\’\'\′](\\.[0-9]+|([0-9]{1,2})[s\”\"\＂\″](\\.[0-9]+)?)?)?$")
         t)
        s :sharedp t)
     ;; (terpri)
     ;; (print (list sign degs dfrac mins mfrac secs sfrac))
     (let ((val  (cond (secs
                        (+ (read-from-string (if sfrac
                                                 (concatenate 'string secs sfrac)
                                               secs))
                           (* 60. (+ (read-from-string mins)
                                     (* 60. (read-from-string degs))))))
                       (mins
                        (* 60. (+ (read-from-string (if mfrac
                                                        (concatenate 'string mins mfrac)
                                                      mins))
                                  (* 60. (read-from-string degs)))))
                       (t
                        (* 3600. (read-from-string (if dfrac
                                                       (concatenate 'string degs dfrac)
                                                     degs))))
                       )))
       (if (and sign
                (string= sign "-"))
           (- val)
         val)))
   (cl-ppcre:register-groups-bind (sign mins mfrac secs sfrac)
       ;; nn′nn″.nnn
       ((load-time-value
         (ppcre:create-scanner
          `        "^([+-])?([0-9]+)[m\’\'\′](\\.[0-9]+|([0-9]{1,2})[s\”\"\＂\″](\\.[0-9]+)?)?$")
         t)
        s :sharedp t)
     ;; (terpri)
     ;; (print (list sign mins mfrac secs sfrac))
     (let ((val  (cond (secs
                        (+ (read-from-string (if sfrac
                                                 (concatenate 'string secs sfrac)
                                               secs))
                           (* 60. (read-from-string mins))))
                       (t
                        (* 60. (read-from-string (if mfrac
                                                     (concatenate 'string mins mfrac)
                                                   mins))))
                       )))
       (if (and sign
                (string= sign "-"))
           (- val)
         val)))
   (cl-ppcre:register-groups-bind (sign secs sfrac)
       ;; nn″.nnn
       ((load-time-value
         (ppcre:create-scanner
          `        "^([+-])?([0-9]+)[s\”\"\＂\″](\\.[0-9]+)?$")
         t)
        s :sharedp t)
     ;; (terpri)
     ;; (print (list sign secs sfrac))
     (let ((val  (read-from-string (if sfrac
                                       (concatenate 'string secs sfrac)
                                     secs))))
       (if (and sign
                (string= sign "-"))
           (- val)
         val)))
   ))

;; --------------------------------------------

#+nil
(defun convert-sexigisimal-2 (s)
  ;; NNNdNNmNNs.NNN and NNhNNmNNs.NNN, ddd°mm’ss”.sss
  ;;            (use Option-shift-8, ^z-', and ^z-" = U+00B0, U+2019, and U+201D)
  ;;            Non-interfering chars: U+FF02 (＂) and U+FF07 (＇)
  ;;                                   U+2032 (′) and U+2033 (″)
  ;; Here we can distinguish degrees versus hours, using the suffix d, h, ° after the major value.
  ;; Return arcsec. Hours get multiplied by 15.
  (multiple-value-bind (start end gstart gend)
      ;; (#~m/^([+-])?([0-9]+)[dh°]([.][0-9]+||([0-9]{1,2})[m’'′]([.][0-9]+||([0-9]{1,2})[s”″]([.][0-9]+)?)?)?$/ s)
      (cl-ppcre:scan
       (load-time-value
        (ppcre:create-scanner
         "^([+-])?([0-9]+)[dh°]([.][0-9]+|([0-9]{1,2})[m’'′]([.][0-9]+|([0-9]{1,2})[s”\"＂″]([.][0-9]+)?)?)?$")
        t)
       s)
         
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((sign   (aref gstart 0))
           (hstart (aref gstart 1))
           (hend   (aref gend    1))
           (mstart (aref gstart 3.))
           (mend   (aref gend   3.))
           (sstart (aref gstart 5.))
           (send   (aref gend   5.)))
        (let* ((ss   (if sstart
                         (read-from-string (remove (char s send) (subseq s sstart)))
                       0))
               (mm   (if mstart
                         (* 60. (read-from-string (remove (char s mend) (subseq s mstart sstart))))
                       0))
               (hflag (char s hend))
               (hmul  (if (char= hflag #\h) 15. 1))
               (hh    (* 3600. (read-from-string (remove hflag (subseq s hstart mstart)))))
               (sgn   (if (and sign
                               (char= #\- (char s sign)))
                          -1
                        1)))
          (* sgn hmul (+ hh mm ss))))
      )))

;; --------------------------------------------

(defun convert-sexigisimal (s)
  (or (convert-sexigisimal-1 s)
      (convert-sexigisimal-2b s)))

;; --------------------------------------------

(defun convert-utc-date (s)
  ;; yyyy/mm/dd [hh:mm:ss] [UTC[+/-nn]]]
  ;; if UTC is elided then convert by default TZ and DST
  (cl-ppcre:register-groups-bind (year mon day timetail hour minutes secstail seconds zonetail timezone)
      ((load-time-value
        (ppcre:create-scanner
         "^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})([ ]+([0-9]{1,2}):([0-9]{1,2})(:([0-9]{1,2}))?)?([ ]+UTC([-+][0-9]{1,2})?)?$")
        t)
       s :sharedp t)
    (declare (ignore timetail secstail))
    (let* ((yyyy  (read-from-string year))
           (mm    (read-from-string mon))
           (dd    (read-from-string day))
           (hrs   (if hour
                      (read-from-string hour)
                    0))
           (mins  (if minutes
                      (read-from-string minutes)
                    0))
           (secs  (if seconds
                      (read-from-string seconds)
                    0))
           (tz   (when zonetail
                       (list
                        (if timezone
                            (- (read-from-string timezone))
                          0)))))
      (apply #'encode-universal-time secs mins hrs dd mm yyyy tz)
      )))
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
        (let* ((yyyy (read-from-string (subseq s ystart yend)))
               (mm   (read-from-string (subseq s mstart mend)))
               (dd   (read-from-string (subseq s dstart dend))))
          (encode-universal-time 0 0 0 dd mm yyyy 0)) ;; makes UTC date
        ))))
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
        (let* ((yyyy (read-from-string (subseq s ystart yend)))
               (mm   (read-from-string (subseq s mstart mend)))
               (dd   (read-from-string (subseq s dstart dend))))
          (encode-universal-time 0 0 0 dd mm yyyy))
        ))))
|#

(defun convert-american-short-date (s)
  ;; mm/dd/yy
  (cl-ppcre:register-groups-bind (year mon day)
      ((load-time-value
        (ppcre:create-scanner
         "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{1,2})$")
        t)
       s :sharedp t)
    (let* ((yyyy  (+ 2000. (read-from-string year)))
           (mm    (read-from-string mon))
           (dd    (read-from-string day)))
      (encode-universal-time 0 0 0 dd mm yyyy)
      )))

;; --------------------------------------------

(defun must-read-full-string (str)
  (multiple-value-bind (val endpos)
      (read-from-string str)
    (and (= endpos (length str))
         val)
    ))
  
(defun convert-hyphenated-number (s)
  ;; xxxx-xx-xxxx  as in telephone numbers, SSN's, and UUID's
  (when (#~m/^[0-9]+(-[0-9]+)*$/ s)
    (must-read-full-string (delete #\- s))))
    
(defun convert-other-base-number (s)
  ;; 0xNNNN_NNNN_NNN
  (cond ((#~m/^0[xXoObB]/ s)
         (must-read-full-string (concatenate 'string "#" (subseq s 1))))
        ((#~m/^[0-9]+[rR]/ s)
         (must-read-full-string (concatenate 'string "#" s)))
        ((#~m/^0[tT]/ s)  ;; special 0t prefix for decimal
         (must-read-full-string (subseq s 2)))
        ))

;; --------------------------------------------

(defun read-extended-number-syntax (s)
  (ignore-errors
    (with-vanilla-readtable
      (let ((*read-base* 10.)
            (*read-default-float-format* 'double-float)
            (s  (remove-separators s))) ;; sep "," or "_"
        (or (convert-sexigisimal s)
            (convert-real-or-complex s)
            (convert-utc-date s)
            ;; (convert-date s)
            (convert-american-short-date s)
            (convert-other-base-number s)
            (convert-hyphenated-number s)
            )))))

(defun read-accumulated-string-or-fallback (str stream)
  (or (read-extended-number-syntax str)
      ;; Fallback: put the buffer back in front of STREAM and re-READ
      ;; with standard syntax (no angle reader macro).
      (let* ((pushback (make-string-input-stream str))
             (combined (make-concatenated-stream pushback stream)))
        (with-vanilla-readtable
          (read combined t nil t)))))

;; --------------------------------------------

(defun token-terminating-char-p (ch)
  (or (null ch)
      (whitespace-char-p ch)
      (multiple-value-bind (fn non-terminating-p)
          (get-macro-character ch)
        (and fn
             (not non-terminating-p)))
      ))

(defun read-chars-to-end-of-token (stream first-char)
  (let ((buffer (make-char-buffer)))
    (vector-push-extend first-char buffer)
    (with-vanilla-readtable
      (prog ()
        again
        (let ((ch (peek-char nil stream nil nil t)))
          (unless (token-terminating-char-p ch)
            (vector-push-extend (read-char stream t nil t) buffer)
            (go again))
          ))
      (coerce buffer 'string))
    ))

;; --------------------------------------------

(defun get-delim (ch)
  (and (find ch "~`'\":@#$%^&*_=|\\?/.,([{<«")
       (case ch
         (#\(  #\))
         (#\[  #\])
         (#\{  #\})
         (#\<  #\>)
         (#\«  #\»)
         (t    ch))
       ))

;; --------------------------------------------
;; Reader macro for #N 
;; Parses a variety of numbers

(defun |reader-for-#N| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((ch     (read-char stream t nil t))
         (delim  (get-delim ch))
         (str    (if delim
                     (read-chars-till-delim stream delim)
                   (read-chars-to-end-of-token stream ch))))
    (unless *read-suppress*
      (read-accumulated-string-or-fallback str stream))
    ))

(set-dispatch-macro-character
 #\# #\n '|reader-for-#N|)

#| ;; example
#n1_000
#n|12:45|
#n2009/08/15
#N|123-45-6789|
#n1+2j
|#
;; --------------------------------------------
;; From Pascal Bourguignon

(defun read-extended-numeric-syntax (stream char)
  "Reader macro function. CHAR is the first digit (or sign) that triggered us.
   Buffer characters until a terminator; if it parses as an angle, return the
   angle; otherwise, push the buffer back via a concatenated-stream and call
   READ with the angle reader macro disabled."
  (let ((str (read-chars-to-end-of-token stream char)))
    (unless *read-suppress*
      (read-accumulated-string-or-fallback str stream))
    ))
  
(defun install-angle-reader (&optional (readtable *readtable*))
  "Install the angle reader macro on digits and signs."
  (dolist (c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)) ; <- the magic is here ;-)
    (set-macro-character c #'read-extended-numeric-syntax t readtable))
  readtable)

;; --------------------------------------
;; For constructing state machines...

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
  ;; Escape literal $ using "...\$..."
  ;; Prefer established $ conventions, but braces ${} needed only for
  ;; identifier separation. A sexpr can stand without braces.
  ;;
  ;; Note: We want this to be a macro so that we can separately call
  ;; on STRING-INTERP with an arbitrary string argument constructed
  ;; however. It needs to be a macro so that it can properly capture
  ;; local lexical binding values.
  (let ((len (length str)))
    (nlet iter ((start 0)
                (pos   0)
                (esc   nil)
                (parts nil))
      
      (labels ((peek (off)
                 (let ((ix (+ pos off)))
                   (and (< ix len)
                        (char str ix)))))
        
        (if (>= pos len)
            `(concatenate 'string
                          ,@(nreverse
                             (cons (subseq str start)
                                   parts)))
          ;; else
          (let ((ch      (char str pos))
                (new-pos (1+ pos))
                (next-ch (peek 1)))
            
            (cond (esc
                   (labels ((digit-char-p* (ch radix)
                              (and ch
                                   (digit-char-p ch radix)))
                            (xdig (n)
                              (digit-char-p* (peek n) 16.))
                            (all-xdig (nlst)
                              (every #'xdig nlst))
                            (xconv (start end)
                              (incf new-pos (1- end))
                              (let ((*read-base* 16.))
                                (code-char
                                 (read-from-string
                                  (subseq str (+ pos start) (+ pos end)))
                                 )))
                            (xerr  (msg end)
                              (error msg (subseq str (1- pos)
                                                 (min len (+ pos end))))
                              ))
                     (let ((new-ch  (case ch
                                      (#\n  #\Newline)   ;; #x0A
                                      (#\r  #\Return)    ;; #x0D
                                      (#\t  #\Tab)       ;; #x09
                                      (#\b  #\Backspace) ;; #x08
                                      (#\v  #\VT)        ;; #x0B
                                      (#\f  #\Page)      ;; #x0C
                                      (#\a  #\Bell)      ;; #x07  - #\a for Alert
                                      (#\e  #\Escape)    ;; #x1B
                                      (#\x  (if (and (<= (+ pos 3.) len)  ;; \xnn
                                                     (all-xdig '(1 2.)))
                                                (xconv 1 3.)
                                              (xerr "Invalid Hex ~A" 3.)))
                                      (#\U  (if (and (<= (+ pos 6.) len)  ;; \U+nnnn
                                                     (char= #\+ (char str new-pos))
                                                     (all-xdig '(2. 3. 4. 5.)))
                                                (xconv 2. 6.)
                                              (xerr "Invalid Unicode ~A" 6.)))
                                      (t    ch)) ))
                       (go-iter new-pos new-pos nil (cons
                                                     (make-string 1 :initial-element new-ch)
                                                     parts))
                       )))
                  
                  ((char= #\\ ch)
                   (go-iter new-pos new-pos t (cons
                                               (subseq str start pos)
                                               parts)))
                  
                  ((and (char= #\$ ch)
                        (eql #\{ next-ch))
                   (let* ((endpos   (position #\} str :start new-pos))
                          (s        (if endpos
                                        (concatenate 'string
                                                     "(progn "
                                                     (subseq str (1+ new-pos) endpos)
                                                     ")" )
                                      (error "No closing #\\} for string interpolation at offset ~A)" pos)))
                          (val      (read-from-string s))
                          (epos     (1+ endpos)))
                     (go-iter epos epos nil
                              (list* `(princ-to-string ,val)
                                     (subseq str start pos)
                                     parts))
                     ))
                  
                  ((and (char= #\$ ch)
                        (eql #\( next-ch))
                   (multiple-value-bind (val epos)
                       (read-from-string str t nil
                                         :start new-pos
                                         :preserve-whitespace t)
                     (go-iter epos epos nil (list* `(princ-to-string ,val)
                                                   (subseq str start pos)
                                                   parts))
                     ))
                  
                  (t
                   (go-iter start new-pos nil parts))
                  )))
        ))
    ))

#|
(let ((x 15.))
  #1"x = ${x}'s\n"#)

(let ((x 15.))
  #1"x = ${x's\n"#)  ;; <- produce missing #\} error

(let ((x 15.))
  #1"x = $x\n"#)     ;; <- produce "x = $x"

(let ((x 15.))
  (string-interp "x = ${x}\\n"))

(string-interp "\\U+03BB")         ;; <- should be "λ"
(string-interp "abc \\U+X3bbdef")  ;; <- should be invalid
 |#
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
  (let ((ans str)) ;; (trim-common-leading-ws-from-lines str)))
    (if numarg
        `(string-interp ,ans)
      ans)))

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
;; back to Doug Hoyte's original code.

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  ;; numarg here means interpolation
  (let ((buffer  (make-char-buffer))
        (ch      (read-char stream)))
    (nlet iter ((prev ch)
                (curr (read-char stream)))
      (unless (and (char= prev #\") (char= curr #\#))
        (vector-push-extend prev buffer)
        (go-iter curr (read-char stream))))
    (unless *read-suppress*
      (interpolated-string numarg (coerce buffer 'string)))
    ))


;;; #|
;;; ;; Doug Hoyte's original definition
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
;; Offers string interpolation when prefix numarg. E.g., #1>
;;
;; DM/RAL 2024/10/27 11:25:43 UTC - cleaned up substantially by going
;; back to Doug Hoyte's original code.

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  ;; numarg enables string interpolation
  (let* ((end-pattern  (read-chars-till-delim stream #\newline))
         (npat     (length end-pattern))
         (buffer   (make-char-buffer)))
    (nlet iter ((index  0))
      (when (< index npat)
        (let ((ch (read-char stream)))
          (vector-push-extend ch buffer)
          (go-iter (if (char= ch (char end-pattern index))
                       (1+ index)
                     0))
          )))
    (unless *read-suppress*
      (decf (fill-pointer buffer) npat)
      (interpolated-string numarg (coerce buffer 'string)))
    ))

(set-dispatch-macro-character
  #\# #\> #'|#>-reader|)

;;; #|
;;; ;; Doug Hoyte's original code
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
                         (let ((delim (get-delim (read-char stream t nil t))))
                           (if delim
                               (read-chars-till-delim stream delim)
                             (error "Delimiter char needed"))
                           )))

#|
(set-macro-character #\} (get-macro-character #\) nil))
(set-macro-character #\] (get-macro-character #\) nil))
(set-macro-character #\» (get-macro-character #\) nil))
|#
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

;; -------------------------------------------------------

(defmacro! defaliasfn (new-name old-name)
  `(defun ,new-name (&rest ,g!args)
     (apply ',old-name ,g!args)))

(defmacro! defcapture (new-name old-name)
  `(unless (fboundp ',new-name)
     (when (fboundp ',old-name)
       (setf (symbol-function ',new-name) (symbol-function ',old-name)))))
     
;; -------------------------------------------------------

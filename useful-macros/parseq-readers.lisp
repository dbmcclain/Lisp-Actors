;; parseq-readers.lisp - extended number syntax parsing
;;
;; DM/RAL  2026/06/11T21:29:50U
;; ----------------------------------

(defpackage #:parseq-readers
  (:use #:common-lisp #:parseq)
  (:export
   #:read-extended-number
   ))

(in-package #:parseq-readers)

;; --------------------------------------------

(defvar *dbg* t)

(defun dbg (str)
  (when *dbg*
    (terpri)
    (princ str)))

(defun trim-seps (txt)
  (remove-if (um:rcurry #'find "_,") txt))

(defun rd (str &rest strs)
  (let ((s  (if strs
                (apply #'concatenate 'string str strs)
              str)))
    (multiple-value-bind (val endpos)
        (read-from-string s)
      (and (= endpos (length s))
           val)
      )))

(defun signify (sgn val)
  (if (eql sgn #\-)
      (- val)
    val))

(defun int-of-base (digs base)
  (if digs
      (let ((*read-base* base))
        (rd digs))
    0))

;; ----------------------------------

(defrule sign       () (char "+-"))
(defrule sep        () (char "_,"))
(defrule bdigit     () (char "01"))
(defrule odigit     () (char "01234567"))
(defrule xdigit     () (or digit (char "abcdefABCDEF")))
(defrule exp-marker () (char "eEdDfFsSlL"))
(defrule imag-mark  () (char "jJiI"))
(defrule x-marker   () (char "xX"))
(defrule o-marker   () (char "oO"))
(defrule b-marker   () (char "bB"))
(defrule t-marker   () (char "tT"))
(defrule r-marker   () (char "rR"))
(defrule c-marker   () (char "cC"))
(defrule dd-marker  () (char "°dh"))
(defrule mm-marker  () (char "′'m"))
(defrule ss-marker  () (char "″\"s"))

(defrule digits     () (and (+ digit) (* (and sep (+ digit))))
  (:string)
  (:function #'trim-seps))

(defrule xdigits    () (and (+ xdigit) (* (and sep (+ xdigit))))
  (:string)
  (:function #'trim-seps))

(defrule odigits    () (and (+ odigit) (* (and sep (+ odigit))))
  (:string)
  (:function #'trim-seps))

(defrule bdigits    () (and (+ bdigit) (* (and sep (+ bdigit))))
  (:string)
  (:function #'trim-seps))


(defrule gdigit  () alphanumeric
  (:test (ch) (digit-char-p ch *read-base*)))

(defrule gdigits () (and (+ gdigit) (* (and sep (+ gdigit))))
  (:string)
  (:function #'trim-seps))
        

(defrule fracpart () (and #\. digits)
  (:string))

(defrule exponent () (and exp-marker (? sign) digits))

;; --------------------------------------------

;; Floats
(defrule unsigned-fpnum () (or (and digits #\. digits (? exponent))
                               (and digits exponent))
  (:string)
  (:lambda (txt)
    ;; (dbg "unsigned-fpnum")
    (let ((*read-base* 10.))
      (rd txt))))

;; Rationals - in any base
(defrule unsigned-rational () (and unsigned-integer #\/ unsigned-integer)
  (:lambda (n _ d)
    (declare (ignore _))
    (/ n d)))

;; --------------------------------------------

(defrule hex-int  () (and #\0 x-marker (? xdigits))
  (:lambda (z x digs)
    (declare (ignore z x))
    ;; (dbg "hex-int")
    (int-of-base digs 16.)))

(defrule octal-int  () (and #\0 o-marker (? odigits))
  (:lambda (z o digs)
    (declare (ignore z o))
    ;; (dbg "octal-int")
    (int-of-base digs 8.)))

(defrule binary-int  () (and #\0 b-marker (? bdigits))
  (:lambda (z b digs)
    (declare (ignore z b))
    ;; (dbg "binary-int")
    (int-of-base digs 2.)))

(defrule decimal-t-int () (and #\0 t-marker (? digits))
  (:lambda (z tt digs)
    (declare (ignore z tt))
    (int-of-base digs 10.)))

(defrule decimal-dot-int () (and digits #\.)
  (:lambda (digs d)
    (declare (ignore d))
    (int-of-base digs 10.)))

(defrule default-radix-int () gdigits
  (:function #'rd))

;; specify character with hex code, 0c48 -> #\H
(defrule char-hex-int () (and #\0 c-marker (? xdigits))
  (:lambda (z c digs)
    (declare (ignore z c))
    (code-char (int-of-base digs 16.))))

;; --------------------------------------------
;; nnRnnnn - We need to know the radix before parsing the tail
;; digits...  But that isn't possible here...

(defrule alphanums () (and (+ alphanumeric) (* (and sep (+ alphanumeric))))
  (:string)
  (:function #'trim-seps))

(defrule r-int () (and digits r-marker alphanums)
  (:lambda (base-str _ digit-str)
    (declare (ignore _))
    ;; (dbg "radix-int")
    (let ((*read-base* (let ((*read-base* 10.))
                         (rd base-str))))
      (and (<= 2. *read-base* 36.)
           (every (um:rcurry #'digit-char-p *read-base*) digit-str)
           (rd digit-str))
      )))

;; --------------------------------------------

(defrule unsigned-integer  () (or hex-int
                                  char-hex-int
                                  octal-int
                                  binary-int
                                  r-int
                                  decimal-t-int
                                  decimal-dot-int
                                  default-radix-int))

;; --------------------------------------------
;; HH:MM, HH:MM.mmm, HH:MM:SS, HH:MM:SS.sss
;; Assume time and convert to turns at 86400s/turn

(defrule unsigned-sexi  () (and digits #\: digits
                                (? (or fracpart
                                       (and #\: digits
                                            (? fracpart)))))
  (:lambda (hrs _ mins tail)
    (declare (ignore _))
    ;; (dbg "sexi")
    ;; (dbg lst)
    (let* ((*read-base* 10.)
           (hh  (* 3600. (rd hrs)))
           (ss  (if (consp tail)
                    (destructuring-bind (_ secs frac) tail
                      (declare (ignore _))
                      (if frac
                          (rd secs frac)
                        (rd secs)))
                  0))
           (mm  (* 60. (if (stringp tail)
                           (rd mins tail)
                         (rd mins)))))
      (* 1/86400 (+ hh mm ss))
      )))

;; --------------------------------------------
;; DD°MM′SS″.sss

(defun hms-handler (str marker tail scale)
  (let ((*read-base* 10.)
        (sf (* (if (find marker "hms")
                   15.
                 1)
               scale)))
    (cond ((null tail)
           (* sf (rd str)))
          ((stringp tail)
           (* sf (rd str tail)))
          (t
           (+ (* sf (rd str))
              tail))
          )))
    
(defrule unsigned-ss  () (and digits ss-marker (? fracpart))
  (:function (um:rcurry #'hms-handler #.(* 1/60 1/60 1/360))
   ))

(defrule unsigned-mm  () (and digits mm-marker (? (or fracpart unsigned-ss)))
  (:function (um:rcurry #'hms-handler #.(* 1/60 1/360))
   ))

(defrule unsigned-dd  () (and digits dd-marker (? (or fracpart unsigned-mm)))
  (:function (um:rcurry #'hms-handler 1/360)
   ))

;; --------------------------------------------
;; UTC Timestamp  yyyy/mm/ddThh:mm:ss.sssU-7

(defrule ts-date  () (and digits #\/ digits #\/ digits)
  (:lambda (yr _1 mo _2 da)
    (declare (ignore _1 _2))
    ;; (dbg "date")
    (let ((*read-base* 10.))
      (let ((yy (rd yr)))
        (list (if (< yy 100)
                  (+ 2000. yy)
                yy)
              (rd mo)
              (rd da)))
      )))

(defrule ts-tz  () (and sign digits)
  (:lambda (sgn digs)
    ;; (dbg "ts-tz")
    ;; (dbg lst)
    (let ((*read-base* 10.))
      (- (signify sgn (rd digs))))
    ))

(defun turns-to-hhmmss (turns)
  (let ((secs (round (* turns 3600.))))
    (multiple-value-bind (mins ss)
        (truncate secs 60.)
      (multiple-value-bind (hrs mm)
          (truncate mins 60.)
        (list (mod hrs 24.) mm ss))
      )))

(defrule ut-timestamp () (and ts-date
                              (? (and (or #\T #\t) unsigned-sexi))
                              (? (and (or #\U #\u) (? ts-tz))))
  (:lambda ((yyyy mo dd) turns tz)
    ;; (dbg "timestamp")
    ;; (dbg lst)
    ;;
    ;; TZ mising - use default
    ;; TZ "U"    - use GMT
    ;; TZ "U±hh" - use GMT±hh
    (let ((tz-hrs  (and tz
                        (list (or (second tz)
                                  0)))))
      (flet ((enc (hh mm ss)
               (apply #'encode-universal-time
                      ss mm hh
                      dd mo yyyy
                      tz-hrs)))
        (cond (turns
               (apply #'enc (turns-to-hhmmss (second turns))))
              (t
               (enc 0 0 0))
              )))
    ))

;; --------------------------------------------

(defrule unsigned-real   () (or unsigned-fpnum
                                unsigned-sexi
                                unsigned-dd
                                unsigned-mm
                                unsigned-ss
                                ut-timestamp
                                unsigned-rational
                                unsigned-integer))

;; --------------------------------------------

(defrule signed-real     () (and (? sign) unsigned-real)
  (:function #'signify))

(defrule pr-imaginary () (and signed-real imag-mark)
  (:lambda (val _)
    (declare (ignore _))
    (complex 0 val)
    ))

(defrule pr-complex () (and signed-real sign unsigned-real imag-mark)
  (:lambda (re sgn im _)
    (declare (ignore _))
    (complex re (signify sgn im))
    ))

;; --------------------------------------------
;; Dashed numbers conflict with simple complex numbers with integer
;; real and imaginary parts. So we have to treat them separately after
;; we give a chance for complex number resolution.

(defrule unsigned-dashed-number () (and gdigits (* (and #\- gdigits)))
  (:string)
  (:lambda (str)
    (remove #\- str)))

(defrule dashed-number () (and (? sign) unsigned-dashed-number)
  (:lambda (sgn str)
    ;; (dbg "dashed-number")
    ;; (dbg (list sgn str tl))
    (signify sgn (rd str))
    ))

;; --------------------------------------------

(defrule eos () (* char) ;; test for End of String (EOS)
  (:test (&rest es)
   (null es)))

(defun select-first (&rest args)
  (car args))

(defrule sole-imaginary () (and pr-imaginary eos)
  (:function #'select-first))

(defrule sole-complex () (and pr-complex eos)
  (:function #'select-first))

(defrule sole-real () (and signed-real eos)
  (:function #'select-first))

(defrule sole-dashed () (and dashed-number eos)
  (:function #'select-first))

(defrule pr-number  () (or sole-imaginary
                           sole-complex
                           sole-dashed
                           sole-real))

;; --------------------------------------------

(defun read-extended-number (s)
  (um:with-vanilla-readtable  ;; for testing in-situ
    (progn ;; ignore-errors
      (parseq 'pr-number s :junk-allowed t))))

;; --------------------------------------------
#|                               
(parseq 'pr-number "+1°22′34″.33")
(parseq 'pr-number "1.23e40")
(parseq 'digits "1_234_56")
(parseq 'pr-number "1s")
(parseq 'pr-number "1s.0")
(parseq 'pr-number "1d.0")
(parseq 'pr-number "1s0")

(parseq 'gdigits "123_456")
(parseq 'dashed-number "123_4-56")
(parseq 'dashed-number "-1-2")

(parseq 'pr-number "0x1_00")
(parseq 'pr-number "1+2j")
(parseq 'pr-number "-1.2-3/5j")

(parseq 'pr-number "12:22:13.3")
(parseq 'pr-number "-1:00:00")

(parseq 'unsigned-ss "12″.3")

(parseq 'pr-number "-12°13'26″.3")

(read-extended-number "2026/10/12T12:23:32.3U-7")
(parseq 'dashed-number "520-235-5207")

(read-extended-number "+1°22′34″.33")
(read-extended-number "-1°00′00″")
(read-extended-number "+0°01′00″")
(read-extended-number "+0°00′01″")
(read-extended-number "26/06/11U")
(read-extended-number "-520-235-5207")
(read-extended-number "1+2j")
(read-extended-number "-1-2")
(CL-USER::.X (read-extended-number "36rdbm"))
(read-extended-number "junk")

;; About 15.8s for 10^5 iters, so 158μs/iter
;; Allocation   =  9,464,407,104 bytes, or 94,644 bytes/iter
;; 301 Page faults
(time
 (dotimes (ix 100_000)
   (read-extended-number "2026/10/12T12:23:32.3U-7")))

;; --------------------------------------------
|#

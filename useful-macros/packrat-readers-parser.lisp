;; packrat-readers-parser.lisp
;;
;; DM/RAL  2026/06/10T23:55:32U
;; ----------------------------------

(in-package #:packrat-readers)

;; ----------------------------------

(defrule sign              (character-ranges #\+ #\-))
(defrule sep               (character-ranges #\_ #\,))
(defrule digit             (character-ranges (#\0 #\9)))
(defrule bdigit            (character-ranges #\0 #\1))
(defrule odigit            (character-ranges (#\0 #\7)))
(defrule xdigit            (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))
(defrule exp-marker        (character-ranges #\e #\E #\d #\D #\f #\F #\s #\S #\l #\L))
(defrule imag-mark         (character-ranges #\j #\J))
(defrule x-marker          (character-ranges #\x #\X))
(defrule o-marker          (character-ranges #\o #\O))
(defrule b-marker          (character-ranges #\b #\B))
(defrule t-marker          (character-ranges #\t #\T))
(defrule r-marker          (character-ranges #\r #\R))
(defrule dd-marker         (character-ranges #\° #\d #\h))
(defrule mm-marker         (character-ranges #\′ #\' #\m))
(defrule ss-marker         (character-ranges #\″ #\" #\s))

(defrule digits            (or (and digits sep (+ digit))
                               (+ digit))
  (:text t))

(defrule xdigits           (or (and xdigits sep (+ xdigit))
                               (+ xdigit))
  (:text t))

(defrule odigits           (or (and odigits sep (+ odigit))
                               (+ odigit))
  (:text t))

(defrule bdigits           (or (and bdigits sep (+ bdigit))
                               (+ bdigit))
  (:text t))

(defun gdigit (str start end)
  (if (< start end)
      (let ((ch  (char str start)))
        (if (digit-char-p ch *read-base*)
            (values ch (1+ start) t)
          (values nil start)))
    (values nil start)))

(defrule gdigit        (function gdigit))
(defrule gdigits       (or  (and gdigits sep (+ gdigit))
                            (+ gdigit))
  (:text t))
        

(defrule fracpart          (and #\. digits)
  (:text t))

(defrule exponent          (and exp-marker (? sign) digits))

;; --------------------------------------------

(defvar *dbg* t)

(defun dbg (str)
  (when *dbg*
    (terpri)
    (princ str)))

(defun trim-seps (txt)
  (remove-if (um:rcurry #'find "_,") txt))

(defun rd (&rest strs)
  (let ((str (trim-seps
              (if (cdr strs)
                  (apply #'concatenate 'string strs)
                (car strs)))))
    (multiple-value-bind (val endpos)
        (read-from-string str)
      (and (= endpos (length str))
           val)
      )))

(defun signify (lst)
  (if (and (characterp (car lst))
           (char= #\- (car lst)))
      (- (second lst))
    (second lst)))

(defun int-of-base (digs base)
  (if digs
      (let ((*read-base* base))
        (rd digs))
    0))

;; --------------------------------------------

;; Floats
(defrule unsigned-fpnum    (or (and digits #\. digits (? exponent))
                               (and digits exponent))
  (:text t)
  (:lambda (txt)
    ;; (dbg "unsigned-fpnum")
    (let ((*read-base* 10.))
      (rd txt))))

;; Rationals - in any base
(defrule unsigned-rational (and gdigits #\/ gdigits)
  (:text t)
  (:lambda (txt)
    ;; (dbg "unsigned-rational")
    (rd txt)))

(defrule hex-int  (and #\0 x-marker (? xdigits))
  (:lambda (lst)
    ;; (dbg "hex-int")
    (int-of-base (third lst) 16.)))

(defrule octal-int  (and #\0 o-marker (? odigits))
  (:lambda (lst)
    ;; (dbg "octal-int")
    (int-of-base (third lst) 8.)))

(defrule binary-int  (and #\0 b-marker (? bdigits))
  (:lambda (lst)
    ;; (dbg "binary-int")
    (int-of-base (third lst) 2.)))

;; --------------------------------------------
;; nnRnnnn - We need to know the radix before parsing the tail
;; digits...  But that isn't possible here...

(defrule alpha     (character-ranges (#\a #\z) (#\A #\Z)))
(defrule alphanum  (or digit alpha))
(defrule alphanums (or (and alphanums sep (+ alphanum))
                       (+ alphanum))
  (:text t))

(defrule r-int (and digits r-marker alphanums)
  (:lambda (lst)
    ;; (dbg "radix-int")
    (destructuring-bind (base-str _ digit-str) lst
      (declare (ignore _))
      (let ((*read-base* (let ((*read-base* 10.))
                           (rd base-str)))
            trimmed)
        (or (and (<= 2. *read-base* 36.)
                 (every (um:rcurry #'digit-char-p *read-base*) (setf trimmed (trim-seps digit-str)))
                 (rd trimmed))
            (invalid-expression-error (format nil "~dR~A" *read-base* digit-str)))
        ))))

;; --------------------------------------------

(defrule decimal-int (or (and #\0 t-marker (? digits))
                         (and digits #\.))
  (:lambda (lst)
    ;; (dbg "decimal int")
    (cond ((characterp (car lst))
           (int-of-base (third lst) 10.))
          (t
           (let ((*read-base* 10.))
             (rd (first lst))))
          )))

(defrule default-radix-int gdigits
  (:lambda (txt)
    ;; (dbg "default-radix-int")
    (rd txt)))

;; --------------------------------------------

(defrule unsigned-integer  (or hex-int
                               octal-int
                               binary-int
                               r-int
                               decimal-int
                               default-radix-int))

;; --------------------------------------------
;; HH:MM, HH:MM.mmm, HH:MM:SS, HH:MM:SS.sss
;; Assume time and convert to turns at 86400s/turn

(defrule unsigned-sexi     (and digits #\: digits
                                (? (or fracpart
                                       (and #\: digits
                                            (? fracpart)))))
  (:lambda (lst)
    ;; (dbg "sexi")
    ;; (dbg lst)
    (destructuring-bind (hrs _ mins tail) lst
      (declare (ignore _))
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
        ))))

;; --------------------------------------------
;; DD°MM′SS″.sss

(defrule unsigned-ss  (and digits ss-marker (? fracpart))
  (:lambda (lst)
    ;; (dbg "ss")
    ;; (dbg lst)
    (destructuring-bind (secs marker tail) lst
      (let ((*read-base* 10.)
            (sf (* (if (char= marker #\s)
                       15.
                     1)
                   1/60 1/60 1/360)))
        (* sf (if tail
                  (rd secs tail)
                (rd secs)))
        ))))

(defrule unsigned-mm  (and digits mm-marker (? (or fracpart unsigned-ss)))
  (:lambda (lst)
    ;; (dbg "mm")
    ;; (dbg lst)
    (destructuring-bind (mins marker tail) lst
      (let ((*read-base* 10.)
            (sf  (* (if (char= marker #\m)
                        15.
                      1)
                    1/60 1/360)))
        (cond ((null tail)
               (* sf (rd mins)))
              ((stringp tail)
               (* sf (rd mins tail)))
              (t
               (+ (* sf (rd mins))
                  tail))
              )))
    ))

(defrule unsigned-dd  (and digits dd-marker (? (or fracpart unsigned-mm)))
  (:lambda (lst)
    ;; (dbg "dd")
    ;; (dbg lst)
    (destructuring-bind (hrs marker tail) lst
      (let ((*read-base* 10.)
            (sf  (* (if (char= marker #\h)
                        15.
                      1)
                    1/360)))
        (cond ((null tail)
               (* sf (rd hrs)))
              ((stringp tail)
               (* sf (rd hrs tail)))
              (t
               (+ (* sf (rd hrs))
                  tail))
              )))
    ))

;; --------------------------------------------
;; UTC Timestamp  yyyy/mm/ddThh:mm:ss.sssU-7

(defrule ts-date  (and digits #\/ digits #\/ digits)
  (:lambda (lst)
    ;; (dbg "date")
    (let ((*read-base* 10.))
      (destructuring-bind (yr _1 mo _2 da) lst
        (declare (ignore _1 _2))
        (let ((yy (rd yr)))
          (list (if (< yy 100)
                    (+ 2000. yy)
                  yy)
                (rd mo)
                (rd da)))
        ))))

(defrule ts-tz  (and sign digits)
  (:lambda (lst)
    ;; (dbg "ts-tz")
    ;; (dbg lst)
    (let ((*read-base* 10.))
      (destructuring-bind (sgn digs) lst
        (- (signify (list sgn (rd digs)))))
      )))

(defun turns-to-hhmmss (turns)
  (let ((secs (round (* turns 3600.))))
    (multiple-value-bind (mins ss)
        (truncate secs 60.)
      (multiple-value-bind (hrs mm)
          (truncate mins 60.)
        (list (mod hrs 24.) mm ss))
      )))

(defrule ut-timestamp (and ts-date
                           (? (and (or #\T #\t) unsigned-sexi))
                           (? (and (or #\U #\u) (? ts-tz))))
  (:lambda (lst)
    ;; (dbg "timestamp")
    ;; (dbg lst)
    (destructuring-bind ((yyyy mo dd) turns tz) lst
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
      )))

;; --------------------------------------------

(defrule unsigned-real     (or unsigned-fpnum
                               unsigned-sexi
                               unsigned-dd
                               unsigned-mm
                               unsigned-ss
                               ut-timestamp
                               unsigned-rational
                               unsigned-integer))

;; --------------------------------------------

(defrule pr-integer  (and (? sign) unsigned-integer)
  (:function signify))

(defrule pr-real     (and (? sign) unsigned-real)
  (:function signify))

(defrule pr-complex  (or (and pr-real imag-mark)
                         (and pr-real sign unsigned-real imag-mark))
  (:lambda (lst)
    (cond ((= 2 (length lst))
           ;; (dbg "imaginary")
           (complex 0 (car lst)))
          (t
           ;; (dbg "complex")
           (complex (car lst) (signify (list
                                        (second lst)
                                        (third lst)))))
          )))

;; --------------------------------------------

(defrule pr-number  (or pr-complex pr-real))

;; --------------------------------------------
;; Dashed numbers conflict with simple complex numbers with integer
;; real and imaginary parts. So we have to treat them separately after
;; we give a chance for complex number resolution.

(defrule unsigned-dashed-number (or (and unsigned-dashed-number #\- gdigits)
                                    gdigits)
  (:text t))

(defrule dashed-number (and (? sign) unsigned-dashed-number)
  (:lambda (lst)
    ;; (dbg "dashed-number")
    ;; (dbg lst)
    (destructuring-bind (sgn str) lst
      (signify (list sgn (rd (remove #\- str)) ))
      )))

;; --------------------------------------------

(defun read-extended-number (s)
  (flet ((try-nums (kind)
           (ignore-errors
             (multiple-value-bind (ans endp ok)
                 (parse kind s :junk-allowed t)
               (and ok
                    (null endp)
                    ans)
               ))))
    (or (try-nums 'pr-number)
        (try-nums 'dashed-number))
    ))

;; --------------------------------------------
#|                               
(parse 'pr-number "+1°22′34″.33")
(parse 'pr-number "1.23e40")
(parse 'digits "1_234_56")
(parse 'pr-number "1s")
(parse 'pr-number "1s.0")
(parse 'pr-number "1d.0")
(parse 'pr-number "1s0")

(parse 'pr-number "0x1_00")
(parse 'pr-number "1+2j")
(parse 'pr-number "-1.2-3/5j")

(parse 'pr-number "12:22:13.3")
(parse 'pr-number "-1:00:00")

(parse 'unsigned-ss "12″.3")

(parse 'pr-number "-12°13'26″.3")

(parse 'pr-number "2026/10/12T12:23:32.3U-7")
(parse 'dashed-number "520-235-5207")

(read-extended-number "26/06/11U")
(read-extended-number "520-235-5207")
(read-extended-number "-1-2j")
(read-extended-number "-1-2")
(CL-USER::.X (read-extended-number "36rdbm"))

(describe-grammar 'pr-number)

;; About 68s for 10^5 iters, so 680μs/iter
;; Allocation   = 34,755,853,968 bytes, or 347,558 bytes/iter
;; 5181 Page faults
(time
 (dotimes (ix 100_000)
   (read-extended-number "2026/10/12T12:23:32.3U-7")))
|#

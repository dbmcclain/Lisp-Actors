;; reader-regexps.lisp - RE's for extended number syntax parsing
;;
;; Superseded by PARSEQ parser in parseq-readers.lisp.  ;; DM/RAL  2026/06/12T09:06:14U
;;
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

;; --------------------------------------------

(in-package :com.ral.useful-macros.reader-macros)

;; --------------------------------------------

(defun remove-separators (s)
  (remove-if (um:rcurry #'find ",_") s))

;; --------------------------------------------

(defmacro ltv-scanner (str)
  `(load-time-value
    (cl-ppcre:create-scanner ,str)
    t))

(eval-always
  (defmacro def-lexical-synonym (name arg)
    `(cl-ppcre:define-parse-tree-synonym ,name ,(if (stringp arg)
                                                    (cl-ppcre:parse-string arg)
                                                  arg))))

(def-lexical-synonym sign         "[-+]")
(def-lexical-synonym digit        "[0-9]")

(def-lexical-synonym digits       "(?&digit)+")
(def-lexical-synonym nn4          "(?&digit){4}")
(def-lexical-synonym nn1or2       "(?&digit){1,2}")
(def-lexical-synonym frac         "\\.(?&digits)")
(def-lexical-synonym signed-whole "((?&sign))?((?&digits))")
(def-lexical-synonym exponent     "[eEdDsSfFlL](?&sign)?(?&digits)")

;; --------------------------------------------

(defun match-number (s)
  (multiple-value-bind (start end)
      (cl-ppcre:scan
       (ltv-scanner "^(?&signed-whole)(\\.((?&digits)(?&exponent)?)?\|/(?&digits))?")
       s)
    (when start
      (multiple-value-bind (val tailpos)
          (read-from-string s t nil :start start :end end)
        (and (eql tailpos end)
             (values val (subseq s end)))
        ))))

(defun match-complex-ij (s)
  (cl-ppcre:scan
   (ltv-scanner "^[iIjJ]$")
   s))

(defun convert-real-or-complex (s)
  (multiple-value-bind (val srest)
      (match-number s)
    (when val
      (cond ((zerop (length srest))
             val)
            ((match-complex-ij srest)
             (complex 0 val))
            ((multiple-value-bind (ival sresti)
                 (match-number srest)
               (and ival
                    (match-complex-ij sresti)
                    (complex val ival)) ))
            ))
    ))

;; --------------------------------------------

(def-lexical-synonym x.xx  "(?&nn1or2)(?&frac)?")
(def-lexical-synonym hms   "^(?&signed-whole):((?&x.xx))(?::((?&x.xx)))?$")
;; NOTE: (?: ... ) is Perl syntax for grouping without also registering span of ...

(defun convert-sexigisimal-1 (s)
  ;; Just assume that the most common use of ambiguous sexigismal
  ;; notation will probably be hh:mm:ss.
  (cl-ppcre:register-groups-bind (sign hrs mins secs)
      ((ltv-scanner "(?&hms)")
       s :sharedp t)
    (let* ((hh  (* 3600. (read-from-string hrs)))
           (mm  (* 60. (read-from-string mins)))
           (ss  (if secs
                    (read-from-string secs)
                  0))
           (sgn (if (and sign
                         (char= #\- (char sign 0)))
                    -1
                  1)))
      (* 1/86400 sgn (+ hh mm ss)) ;; convert to turns
      )))

#|
(convert-sexigisimal-1 "+10:23.234")
(convert-sexigisimal-1 "-10:23:17.234")
(convert-sexigisimal-1 "1:00")
|#
;; --------------------------------------------

(def-lexical-synonym __s "^(?&signed-whole)([″s\"])((?&frac))?$")
(def-lexical-synonym _ms "^(?&signed-whole)([′m'])(((?&frac))|(?&digit).*)?$")
(def-lexical-synonym dms "^(?&signed-whole)([dh°])(((?&frac))|(?&digit).*)?$")

(defun read-fully-from-strings (&rest strings)
  (let* ((str (apply #'concatenate 'string strings))
         (len (length str)))
    (multiple-value-bind (val endpos)
        (read-from-string str)
      (and (eql endpos len)
           val))))

(defun cvt-hms (sign whole tail frac follow-fn scale)
  ;; common code for both hours & minutes & seconds
  (let ((ans (if frac
                 (* scale
                    (read-fully-from-strings whole frac))
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
      ((ltv-scanner "(?&__s)")
       s :sharedp t)
    (values (cvt-hms sign secs nil sfrac nil 1)
            skind)
    ))

(defun convert-minutes (s)
  (or
   (cl-ppcre:register-groups-bind (sign mins mkind mtail mfrac)
       ((ltv-scanner "(?&_ms)")
        s :sharedp t)
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
       ((ltv-scanner "(?&dms)")
        s :sharedp t)
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

(defun convert-sexigisimal (s)
  (or (convert-sexigisimal-1 s)
      (convert-sexigisimal-2b s)))

;; --------------------------------------------

(def-lexical-synonym ts-date  "((?&nn4))/((?&nn1or2))/((?&nn1or2))")
(def-lexical-synonym ts-time  "[Tt]((?&nn1or2)):((?&nn1or2)):((?&nn1or2)(?&frac)?)")
(def-lexical-synonym ts-tz    "([Uu]((?&sign)(?&nn1or2))?)")

(defun convert-utc-date (s)
  (cl-ppcre:register-groups-bind (yyyy mo dd hh mm ss tztail tz)
      ((ltv-scanner "^(?&ts-date)(?&ts-time)?(?&ts-tz)?$")
       s :sharedp t)
    (let* ((yr  (read-from-string yyyy))
           (mon (read-from-string mo))
           (day (read-from-string dd))
           (hrs (if hh
                    (read-from-string hh)
                  0))
           (mins (if mm
                     (read-from-string mm)
                   0))
           (secs (if ss
                     (read-from-string ss)
                   0))
           (isecs  (floor secs))
           (fsecs  (- secs isecs))
           (tz     (when tztail
                     ;; If "U" specified, we either also have +/- tz hours, or else,
                     ;; if lone "U" not followed by +/- tz, then we default to GMT.
                     ;; If "U" not specified, then we default to local time zone.
                     ;;
                     ;; E.g., if we are 7 hours behind GMT, then U-7 is our local time zone.
                     (list
                      (if tz
                          (- (read-from-string tz))
                        0)))))
      (+ (apply #'encode-universal-time isecs mins hrs day mon yr tz)
         fsecs)
      )))

#|
(decode-universal-time #N|2026/06/08|)
(decode-universal-time #N|2026/06/08T11:32:23|)
(decode-universal-time #N|2026/06/08T11:32:23U|)
(decode-universal-time (floor #N|2026/06/08T11:32:23.301U|))
(decode-universal-time #N|2026/06/08T11:32:23U-7|)
(decode-universal-time #N|2026/06/08U-7|)
(decode-universal-time #N|2026/06/08U|)
|#

(defun convert-american-short-date (s)
  ;; mm/dd/yy
  (cl-ppcre:register-groups-bind (year mon day)
      ((ltv-scanner
         "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{1,2})$")
       s :sharedp t)
    (let* ((yyyy  (+ 2000. (read-from-string year)))
           (mm    (read-from-string mon))
           (dd    (read-from-string day)))
      (encode-universal-time 0 0 0 dd mm yyyy)
      )))

;; --------------------------------------------

(defun convert-hyphenated-number (s)
  ;; xxxx-xx-xxxx  as in telephone numbers, SSN's, and UUID's
  (when (#~m/^[0-9]+(-[0-9]+)*$/ s)
    (read-fully-from-strings (remove #\- s))))
    
(defun convert-other-base-number (s)
  ;; 0xNNNN_NNNN_NNN
  (cond ((#~m/^0[xXoObB]/ s)
         (read-fully-from-strings "#" (subseq s 1)))
        ((#~m/^[0-9]+[rR]/ s)
         (read-fully-from-strings "#" s))
        ((#~m/^0[tT]/ s)  ;; special 0t prefix for decimal
         (read-fully-from-strings (subseq s 2)))
        ))

;; --------------------------------------------

(defun read-extended-numbers (s)
  (ignore-errors
    (let ((s  (remove-separators s))) ;; sep "," or "_"
      (or (convert-sexigisimal s)
          (convert-real-or-complex s)
          (convert-utc-date s)
          ;; (convert-date s)
          (convert-american-short-date s)
          (convert-other-base-number s)
          (convert-hyphenated-number s)
          ))))

#|
;; About 84s for 10^5 iters, so 840μs/iter
;; Allocation   = 38,992,235,512 bytes, or 389,922 bytes/iter
;; 2292 Page faults
(time
 (dotimes (ix 100_000)
   (read-extended-numbers "2026/10/12T12:23:32.3U-7")))
|#

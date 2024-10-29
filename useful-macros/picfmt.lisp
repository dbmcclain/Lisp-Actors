;; picfmt.lisp
;;
;; DM/RAL  2024/10/26 09:51:33 UTC
;; ----------------------------------

(defpackage #:picfmt
  (:use #:common-lisp)
  (:export
   #:fmt
   #:pic-formatter

   #:hms
   #:ndp
   #:2dp
   
   #:insert-spacers

   #:in-base
   #:hex
   #:octal
   #:binary
   #:decimal
   ))

(in-package #:picfmt)

;; ----------------------------------
;; As distasteful as the use of globals appears, it is really the only
;; way, in Lisp, to avoid the overhead of producng a list with closures
;; every time we print.
;;
;; By defining all the verbs as global functions, feeding off global
;; (dynamic) bindings for parameters, we can set up the dispatch table
;; just once and for all.
;;
;; General Keyword Args:
;;   :ndpl           - nbr of decimal places to use, def = 0
;;   :dpchar         - decimal point character, def = #\.
;;   :colon-char     - what char to use for sexigisimal output, def = #\:
;;   :comma-char     - comma separator char for integer portion, def = #\,
;;   :comma-interval - how often :comma-char is inserted, def = 3
;;   :sep-char       - separator char for fraction portion, def = #\_
;;   :sep-interval   - how often :sep-char is inserted, def = 5
;;   :width          - how wide the output string should be, def = NIL
;;   :fill-char      - char to use to pad out to width, def = #\Space

;; And yes... inspired by Forth's pictured formatting:
;; <# # # '.' hold #s sign #>

(defvar *pad*   nil)
(defvar *nabs*  nil)
(defvar *args*  nil)
(defvar *n*     nil)

(defun d ()
  (multiple-value-bind (q r)
      (truncate *nabs* *print-base*)
    (setf *nabs* q)
    (hold (digit-char r *print-base*))
    ))

(defun ds ()
  (d)
  (unless (zerop *nabs*)
    (ds)))

(defun dc ()
  (let ((*print-base* 6))
    (d))
  (hold (getf *args* :colon-char #\:)))

(defun ddc ()
  (d)
  (dc))

(defun dp ()
  (hold (getf *args* :dpchar #\.)))

(defun nd ()
  ;; should have a numeric arg on the *args* pseudo-stack
  (dotimes (ix (pop *args*))
    (d)))

(defun ndpl ()
  (dotimes (ix (getf *args* :ndpl 0))
    (d)))

(defun rstr (s)
  (map nil #'hold s))

(defun sign ()
  (when (minusp *n*)
    (hold #\-)))

(defun sign+ ()
  (hold (if (minusp *n*) #\- #\+)))

(defun hold (ch)
  (vector-push-extend ch *pad*))

;; --------------------------------------------
;; Dispatch table
(um:eval-always
  (defvar *fns*
    '(:d	d        ;; 1 digit, #
      :ds	ds       ;; all the rest, but at least one
      :dc	dc       ;; :#, sexagisimal digit + ':'
      :ddc	ddc      ;; :##, 2 digits mod 60 + ':'
      :dp       dp       ;; prints the decimal point char (keyword arg :dpchar)
      :nd	nd       ;; n digits, by stack arg
      :ndpl     ndpl     ;; n digits, by arg :ndpl, def 0.
      :sign	sign     ;; - if negative
      :sign+	sign+))  ;; - if negative, else +

  (defun pic-worker-fn (fmtlst)
    `(lambda ()
       ,@(mapcan (lambda (sym)
                   (cond ((symbolp sym)
                          (um:when-let (fn (getf *fns* (um:kwsymb sym)))
                            `((,fn))))
                         ((characterp sym)
                          `((hold ,sym)))
                         ((stringp sym)
                          `((rstr ,(reverse sym))))
                         ))
                 (reverse fmtlst))
       )))
;; --------------------------------------------

(defun make-pad ()
  (make-array 80
              :element-type 'character
              :adjustable   t
              :fill-pointer 0))

(defun kw-args (lst)
  ;; skip to keywords portion of lst
  (and lst
       (if (keywordp (car lst))
           lst
         (kw-args (cdr lst)))
       ))

(defun %pic-run (fn *n* &rest args)
  (let* ((*pad*  (make-pad))
         (kwargs (kw-args args))
         (*nabs* (abs (round (* (expt 10. (getf kwargs :ndpl 0)) *n*))))
         (*args* args))
    (funcall fn)
    (let ((ans (apply #'insert-spacers (nreverse *pad*) *args*))
          (wd  (getf kwargs :width)))
      (if wd
          (let ((nel  (length ans)))
            (concatenate 'string
                         (make-string (max 0 (- wd nel))
                                      :initial-element (getf kwargs :fill-char #\space))
                         ans))
        ans))
    ))

;; --------------------------------------------

(defmacro pic-formatter (fmtlst)
  (let ((n    (gensym))
        (args (gensym)))
    `(lambda (,n &rest ,args)
       (apply #'%pic-run ,(pic-worker-fn fmtlst) ,n ,args))
    ))

(defmacro fmt (fmtlst n &rest args)
  `(funcall (pic-formatter ,fmtlst) ,n ,@args))

#|
(fmt (sign+ ds ddc ddc dp nd) (* 86400. (expt 10. 6) 100.75) 6 :ndpl 6)
(let ((fmtlst '(sign ds #\. nd)))
  (inspect (compile nil (eval `(pic-formatter ,fmtlst)))))
|#
;; --------------------------------------------

(defun hms (turns &rest args)
  (let ((*print-base* 10.)
        (x  (* turns 86400.))
        (nd (getf args :ndpl 0)))
    (if (plusp nd)
        (apply (pic-formatter (sign ds ddc ddc dp ndpl)) x args)
      (apply (pic-formatter (sign ds ddc ddc)) x args))
    ))

#|
(hms 0.8 :ndpl 2)
(fmt (sign+ ds ddc ddc dp d " hrs") (* 86400. 0.8) :ndpl 1)
|#

(defun ndp (ndpl x &rest args)
  (if (>= (abs x) 1e12)
      (apply #'insert-spacers
             (format nil "~,vE" ndpl x) args)
    (apply (pic-formatter (sign ds #\. ndpl))
           x :ndpl ndpl args)
    ))

(defun 2dp (x &rest args)
  (apply #'ndp 2 x args))

#|
(2dp (- pi))
(ndp 9 pi)
(ndp 7 1e12)
|#

;; --------------------------------------------

(defun split-at-decimal (str &key (dpchar #\.))
  ;; We punt and work on the assumption that any formatted number with
  ;; a decimal place indicator will either use #\. or #\, .
  ;;
  ;; Furthermore, we assume that if there is a #\: in the string, for
  ;; sexagisimal format, then it is fine from first occurrent to
  ;; either the decmal point or end of string.
  
  (declare (ignore dpchar))
  (multiple-value-bind (start end)
      (#~m"([: ].+[.,])|([: ].+$)|[.,]" str)  ;; PPCRE is much easier to use than LW regexps
    (if start
        (values
         (subseq str 0 start)      ;; return beginning, middle, end
         (subseq str start end)
         (subseq str end))
      ;; else
      str)
    ))

#|
(let ((str "+24000:00:00.0")
      (pat ":.*[.]"))
  (multiple-value-bind (start len)
      (lw:find-regexp-in-string pat str)
    (if start
        (subseq str start (+ start len))
      "no")))

(let ((str "+24000:00:00.00"))
  (split-at-decimal str))
(let ((str "+24000 00 00.00"))
  (split-at-decimal str))

;; 0....5...10...15
(#~m"(:.+[.,])|(:.+$)|[.,]" "+24000:00:00.00")
;; (#~m%((:[0-9]{2}){1,2}(\.|$))|\.% "+24000:00:00.0")

(#~m"(:.+[.,])|[.,]" "+24000:00:00.0")
(lw:find-regexp-in-string ":.+\\([.,]\\|$\\)\\|[.,]" "+24000:00:00.0")

(lw:find-regexp-in-string #":.+\([.,]\|$\)\|[.,]"# "+24000:00:00.0")

(identity #":.+\([.,]\|$\)\|[.,]"#)
|#

(defun common-insert-spacers (str spacer-char spacer-interval)
  (let ((*pad*  (make-pad))
        (limit  (length str)))
    (um:nlet iter ((pos 0)
                   (ct  0))
      (when (< pos limit)
        (let ((ch (char str pos)))
          (if (digit-char-p ch *print-base*)
              (if (>= ct spacer-interval)
                  (progn
                    (hold spacer-char)
                    (hold ch)
                    (go-iter (1+ pos) 1))
                ;; else
                (progn
                  (hold ch)
                  (go-iter (1+ pos) (1+ ct))))
            ;; else
            (setf *pad* (concatenate 'string *pad* (subseq str pos)))
            ))))
    *pad*))
  
(defun insert-fraction-spacers (str &key (spacer-char #\_) (spacer-interval 5))
  (if (characterp spacer-char)
      (common-insert-spacers str spacer-char spacer-interval)
    ;; else
    str))

(defun select-comma-interval ()
  (case *print-base*
    ((10. 8.)  3)
    ((16. 2 )  4)
    (t    5)))

(defun select-comma-char ()
  (case *print-base*
    (10.  #\,)
    (t    #\_)))

(defun insert-comma-spacers (str &key
                                 (comma-char (select-comma-char))
                                 (comma-interval (select-comma-interval)))
  (if (characterp comma-char)
      (nreverse
       (common-insert-spacers (reverse str)
                              comma-char
                              comma-interval))
    ;; else
    str))

;; --------------------------------------------

(defun insert-spacers (str &key
                           (dpchar #\.)
                           (comma-char (select-comma-char))
                           (comma-interval (select-comma-interval))
                           (spacer-char #\_)
                           (spacer-interval 5)
                           &allow-other-keys)
  (multiple-value-bind (pref dp suf)
      (split-at-decimal str :dpchar dpchar)
    (concatenate 'string
                 (insert-comma-spacers pref
                                       :comma-char comma-char
                                       :comma-interval comma-interval)
                 (or dp "")
                 (or (insert-fraction-spacers suf
                                              :spacer-char spacer-char
                                              :spacer-interval spacer-interval)
                     ""))
    ))

;; --------------------------------------------

(defvar *expecting-spacers* nil)
(defvar *spacer-args*       nil)

(defun common-code (obj stream)
  (let* ((*expecting-spacers* nil)
         (str  (apply #'insert-spacers
                      (princ-to-string obj)
                      *spacer-args*)))
    (let ((*print-escape* nil))
      (print-object str stream))
    obj))

#+:LISPWORKS
(lw:defadvice
    (princ insert-number-spacers :around)
    (obj &optional (stream *standard-output*))
  (cond ((and (integerp obj)
              (not *print-readably*)
              *expecting-spacers*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))

#|
(hcl:delete-advice princ insert-number-spacers)

(trace princ (method print-object (integer t)))
(trace print-object)
(untrace)

(princ-to-string edec:*ed-gen*)
(in-base 36. edec:*ed-gen*)

(princ-to-string edec:*ed-r*)
(in-base 36. edec:*ed-r*)
(in-base 36. edec:*ed-r* :comma-char nil)
|#

#|
(defmethod print-object :around ((obj integer) stream)
  (cond ((and (not *print-readably*)
              *expecting-spacers*)
         (common-code obj stream))
        (t
         (call-next-method))
        ))
|#  
#|
#+:LISPWORKS
(lw:defadvice
    ((method print-object (integer t))
     insert-number-spacers :around)
    (obj stream)
  (cond ((and (not *print-readably*)
              *expecting-spacers*)
         (common-code obj stream))
        (t
         (lw:call-next-advice obj stream))
        ))
|#
#|
(hcl:delete-advice (method print-object (real t)) insert-number-spacers)
|#

;; --------------------------------------------
;; In the following, arg N could a number or anything that contains a
;; number.

(defun in-base (base n &rest args)
  (let ((*print-base*        base)
        (*spacer-args*       args)
        (*expecting-spacers* t))
    (princ-to-string n)))

(defun hex (n &rest args)
  (apply #'in-base 16. n args))

(defun octal (n &rest args)
  (apply #'in-base 8. n args))

(defun binary (n &rest args)
  (apply #'in-base 2. n args))

(defun decimal (n &rest args)
  (apply #'in-base 10. n args))

#|
(hex edec:*ed-r*)
(octal edec:*ed-r*)
(binary edec:*ed-r*)
(in-base 36 edec:*ed-r*)
(in-base 36 edec:*ed-r* :comma-char nil)
(in-base 17 edec:*ed-r*)
(in-base 16.
         (let ((*read-base* 36.))
           (read-from-string "dbm")))
|#

;; --------------------------------------------


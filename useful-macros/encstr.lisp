;; -*- Mode: Lisp; Author: dbm/ral; coding: utf-8 -*-

(in-package :useful-macros)

;; ---------------------------------------------------------

(defun utf-8-encoding (char-code)
  (cond ((< char-code #x80)
         (list char-code))
        ((< char-code #x800)
         (list 
          (+ #xC0
             (ldb (byte 5 6) char-code))
          (+ #x80
             (ldb (byte 6 0) char-code))))
        ((< char-code #x10000)
         (list (+ #xE0
                  (ldb (byte 4 12) char-code))
               (+ #x80
                  (ldb (byte 6 6) char-code))
               (+ #x80
                  (ldb (byte 6 0) char-code))))
        (t
         (list (+ #xF0
                  (ldb (byte 3 18) char-code))
               (+ #x80
                  (ldb (byte 6 12) char-code))
               (+ #x80
                  (ldb (byte 6 6) char-code))
               (+ #x80
                  (ldb (byte 6 0) char-code))))
        ))

(defun utf-8-code-chars (char-code)
  (let ((codes (utf-8-encoding char-code)))
    (map-into codes #'code-char codes)))

;; ---------------------------------------------

(defmacro dostring ((ch str) &body body)
  `(loop for ,ch across ,str do ,@body))

(defmacro dovector ((x vec) &body body)
  `(loop for ,x across ,vec do ,@body))

#+:LISPWORKS
(progn
  (editor:setup-indent "dostring" 1)
  (editor:setup-indent "dovector" 1))

;; ---------------------------------------------

(defun encstr (str)
  ;; use embedded !n, !r, !p, !t, !xnnnn, !!
  (with-output-to-string (s nil
                            :element-type 'character)
    (let ((mach
           (alet ((hex-inp   0)
                  (hex-ct    0)
                  hex-enc)
               (alet-fsm
                 ;; --------------------------------------
                 ;; states - first one is start
                 ;; --------------------------------------
                 (start (ch)
                        (case ch
                          ((#\\)
                           (state escape))
                          ((:eos))
                          (t
                           (stash ch))
                          ))
                 (escape (ch)
                         (let ((enc '((#\n . #\newline)
                                      (#\r . #\return)
                                      (#\t . #\tab)
                                      (#\f . #\page)
                                      (#\v . #\vt)
                                      (#\a . #\bell)
                                      (#\b . #\backspace)
                                      (#\0 . #\null)
                                      (#\\ . #\\))))
                           (state start)
                           (if-let (pair (assoc ch enc))
                               (stash (cdr pair))
                             (if (or (eql ch #\u)
                                     (eql ch #\x))
                                 (progn
                                   (setf hex-enc ch)
                                   (state collect-hexcode))
                               (start ch))
                             )))
                 (collect-hexcode (ch)
                                  ;; #\u+3bb = Greek Lambda char
                                  ;; it won't fit in a simple-base-string
                                  (if-let (v (and (characterp ch)
                                                     (digit-char-p ch 16)))
                                      (progn
                                        (ash-dpbf hex-inp 4 v)
                                        (incf hex-ct)
                                        (when (>= hex-ct 4)
                                          (stash-hex)
                                          (setf hex-ct  0
                                                hex-inp 0)
                                          (state start)))
                                    (progn
                                      (when (plusp hex-ct)
                                        (stash-hex)
                                        (setf hex-ct  0
                                              hex-inp 0))
                                      (state start)
                                      (start ch))))
                 ;; --------------------------------------
                 ;; helper functions
                 ;; --------------------------------------
                 (stash (ch)
                        (princ ch s))
                 (stash-hex ()
                            (case hex-enc
                              ((#\x)
                               ;; Direct Unicode
                               (stash (code-char hex-inp)))
                              ((#\u)
                               ;; UTF-8 Encoding
                               (map nil #'stash (utf-8-code-chars hex-inp)))
                              ))
                 ))))
      (dostring (ch str)
        (funcall mach ch))
      (funcall mach :eos)
      )))

;; ---------------------------------------------
;; SBS - convert string to UTF-8 simple-base-string

(defgeneric sbs (str)
  (:method (str)
   str)
  (:method ((str string))
   (with-output-to-string (s nil
                             :element-type 'base-char)
     (dostring (ch str)
       (let ((code (char-code ch)))
         (if (> code #x7F)
             (dolist (x (utf-8-encoding code))
               (princ (code-char x) s))
           (princ ch s))
         ))
     )))


;; ---------------------------------------------
;; UCS - convert UTF-8 string to Unicode

(defgeneric ucs (str)
  (:method ((str string))
   str)
  (:method ((str simple-base-string))
   (with-output-to-string (s nil
                             :element-type 'character)
     (with-input-from-string (inp str)
       (handler-case
           (nlet iter ((ch (read-char inp)))
             (labels ((read-follow ()
                        (let* ((ch2   (read-char inp))
                               (code2 (char-code ch2)))
                          (unless (<= #x80 code2 #xBF)
                            (bad-seq))
                          (ldb (byte 6 0) code2)
                          ))
                      (combine (x &rest xs)
                        (if (endp xs)
                            x
                          (apply #'combine (logior (ash x 6)
                                                   (car xs))
                                 (cdr xs))))
                      (bad-seq ()
                        (error "Invalid UTF-8 coding sequence")))
               
               (let ((code (char-code ch)))
                 (cond ((<= #x00 code #x7F)
                        (princ ch s))
                       
                       ((<= #x80 code #xC1)
                        (bad-seq))
                       
                       ((<= #xC2 code #xDF)
                        (let* ((code2 (read-follow))
                               (dec   (combine (ldb (byte 5 0) code) code2)))
                          (princ (code-char dec) s)))
                       
                       ((<= #xE0 code #xEF)
                        (let* ((code2 (read-follow))
                               (code3 (read-follow))
                               (dec   (combine (ldb (byte 4 0) code) code2 code3)))
                          (unless (or (<= #x0800 dec #xD7FF)
                                      (<= #xE000 dec #xFFFF))
                            (bad-seq))
                          (princ (code-char dec) s)))
                       
                       ((<= #xF0 code #xF4)
                        (let* ((code2 (read-follow))
                               (code3 (read-follow))
                               (code4 (read-follow))
                               (dec   (combine (ldb (byte 3 0) code) code2 code3 code4)))
                          (unless (<= #x10000 dec #x10FFFF)
                            (bad-seq))
                          (princ (code-char dec) s)))
                       
                       (t
                        ;; #xF5-FF
                        (bad-seq))
                       ))
               (go-iter (read-char inp))
               ))
         (end-of-file ()))
       ))))

;; ---------------------------------------------

#|
(with-open-file (f "~/junk.txt"
                   :external-format :utf-8
                   :element-type 'character
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (write-line ";; -*- Mode: text; coding: utf-8 -*-" f)
  #|
  (write-string
   ;; (map 'simple-base-string #'code-char '(#x80 #x90 #xA0 #xB0 #xC0 #xD0 #xE0 #xF0))
   ;; (encstr "\x3bb")
   (coerce
    (loop for ix from 0 below 256 collect (code-char ix))
    'string)
   |#
  (dotimes (ix 4097) (write-char (code-char ix) f))
   f))

(princ #\u+03bb)
(princ #\λ)
(lw:push-end item lst)
(lw:push-end-new item lst)

(plt:fplot 'plt '(-10 10)
           (λ (x)
             (/ (sin x) x))
           :clear t
           :title (encstr "Wavelength \\x3bb"))
 |#


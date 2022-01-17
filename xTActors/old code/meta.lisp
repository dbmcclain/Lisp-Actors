;; meta.lisp -- Baker's parser META
;;
;; DM/RAL 05/21
;; ------------------------------------------------------------

(defpackage #:meta
  (:use :common-lisp)
  (:export
   ))

(in-package #:meta)

(defvar *meta-readtable* (copy-readtable))

(defstruct meta
  (:print-function
   (lambda (m s d)
     (let ((char (meta-char m))
           (form (meta-form m)))
       (ecase char
         ((#\@ #\! #\$) (format s "~A~A" char form))
         (#\[  (format s "[~{~A~^ ~}]" form))
         (#\{  (format s "{~{~A~^ ~}}" form))))))
  char
  form)

(defun meta-reader (s c)
  (make-meta
   :char c
   :form (read s)))

(um:eval-always
  (let ((*readtable* *meta-readtable*))
    (mapc #'(lambda (c)
              (set-macro-character c #'meta-reader))
          '(#\@ #\$ #\!))
    
    (set-macro-character #\[
                         #'(lambda (s c)
                             (make-meta
                              :char c
                              :form (read-delimited-list #\] s t))))
    
    (set-macro-character #\{
                         #'(lambda (s c)
                             (make-meta
                              :char c
                              :form (read-delimited-list #\} s t))))
    
    (mapc #'(lambda (c)
              (set-macro-character c (get-macro-character #\) nil)))
          '(#\] #\}))))
  
  
(um:eval-always
  (defun compileit (x)
    (typecase x
      (meta
       (ecase (meta-char x)
         (#\!  (meta-form x))
         (#\[  `(and ,@(mapcar #'compileit (meta-form x))))
         (#\{  `(or ,@(mapcar #'compileit (meta-form x))))
         (#\$  `(not (do () ((not ,(compileit (meta-form x)))))))
         (#\@  (let ((f (meta-form x)))
                 `(match-type ,(car f) ,(cadr f))))
         ))
      (t    `(match ,x)))))
  
(defmacro matchit (x) (compileit x))

(defun ctoi (d)
  (- (char-code d)
     #.(char-code #\0)))

(um:eval-always
  (let ((*readtable* *meta-readtable*))
    (defun parse-int ()
      (let ((s 1)
            d
            (n 0))
        (and
         (matchit
          [{#\+ [#\- !(setq s -1)] []}
          @(digit d) !(setq n (ctoi d))
          $[@(digit d) !(setq n (+ (* n 10) (ctoi d)))]])
         (* s n))))))
  
(deftype digit () '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(um:eval-always
  (let ((*readtable* *meta-readtable*))
    (defun parse-integer ()
      (let (d)
        (matchit [{#\+ #\- []} @(digit d) $@(digit d) ])))))

  

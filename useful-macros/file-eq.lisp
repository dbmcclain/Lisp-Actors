;; file-eq.lisp
;;
;; DM/RAL  2022/11/09 07:28:26
;; ----------------------------------

(defpackage #:com.ral.useful-macros.file-eq
  (:use #:common-lisp))

(in-package #:com.ral.useful-macros.file-eq)

;; ----------------------------------

(defun um:get-ino (fname)
  ;; Return dev and inode for given file. Two files are the same if
  ;; they have the same dev and inode, regardless of how you reach
  ;; them through filenames, links, softlinks, etc.
  (labels ((finder (s)
             (let ((len  (length s)))
               (lambda (str)
                 (and (> (length str) len)
                      (string-equal s str :end2 len)))
               )))
    (let* ((txt (with-output-to-string (s)
                  (sys:call-system-showing-output
                   `("/usr/bin/stat" "-s"
                     ,(namestring (truename fname)))
                   :output-stream s)))
           (items (um:split-string txt))
           (dev (find-if (finder "st_dev=") items))
           (ino (find-if (finder "st_ino=") items)))
      (values dev ino))))


(defun um:file-eq (fname1 fname2)
  (multiple-value-bind (dev1 ino1)
      (um:get-ino fname1)
    (multiple-value-bind (dev2 ino2)
        (um:get-ino fname2)
      (and (string-equal dev1 dev2)
           (string-equal ino1 ino2))
      )))

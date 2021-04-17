;; ffs.lisp -- Flexible File System
;; Recursive prefix mappings on filenames
;;
;; DM/RAL 04/21
;; --------------------------------------------------------
;; Ye Olde FFS - in a nutshell...

(defpackage #:ffs
  (:use #:common-lisp)
  (:export
   #:mappings
   #:map-filename
   ))

(in-package #:ffs)

(defun find-prefix-translation (str trans)
  (let ((pos 0)
        (pre nil))
    (dolist (pair trans)
      (let* ((pat    (car pair))
             (patlen (length pat)))
        (when (> patlen pos)
          (let ((new-pos (or (mismatch str pat)
                             patlen)))
            (when (>= new-pos patlen)
              (setf pos new-pos
                    pre (cdr pair)))
            )) ))
    (values pos pre)))

(defun prefix-translation (str trans)
  (loop
   (multiple-value-bind (pos pre)
       (find-prefix-translation str trans)
     (if pre
         (setf str (concatenate 'string pre (subseq str pos)))
       (return (pathname str))
       ))))

(defvar mappings
  '(("projects:" . "~/projects/")
    ("lisp:"     . "projects:Lispworks/")
    ("actors:"   . "lisp:actors/")))

(defun map-filename (fname)
  (prefix-translation fname mappings))

(defun ffs-reader (stream)
  (map-filename (string (read stream))))

(um:set-/-dispatch-reader 'ffs #'ffs-reader)

#|
(prefix-translation "actors:examples/diddly.txt" *ffs*)

(list #/ffs/"actors:examples/diddly.txt")
|#

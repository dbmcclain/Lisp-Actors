
(setf (logical-pathname-translations "JUNK")
      '(("**;*.*" "/Users/davidmcclain/projects/Lispworks/**/*.*")))

(translate-logical-pathname "JUNK:diddly;do;x.lisp")
(translate-logical-pathname "JUNK:diddly;x.lisp")
(translate-logical-pathname "JUNK:x.lisp")
(logical-pathname "JUNK:**;*.*")

(setf (logical-pathname-translations "JUNK")
      '(("**" "/Users/davidmcclain/projects/Lispworks/**/")))

(setf (logical-pathname-translations "PROJECTS")
        `(("DYLIB;**;*.*"        "/usr/local/lib64/**/*.*")
          ("LIB;**;*.*"          "/usr/local/lib/**/*.*")
          ("DYLIB64;**;*.*"      "/usr/local/lib64/**/*.*")
          ("LISPLIB;**;*.*"      "/usr/local/Lisp/Source/**/*.*")
          ;; ("LISP;**;*.*"            "PROJECTS:Lispworks;**;")
          ("LISP;**;*.*"         "~/projects/Lispworks/**/*.*")
          ("**;*.*"              ,(concatenate 'string
                                               "~/projects"
                                               "/**/*.*"))))
(translate-logical-pathname "PROJECTS:LISP;diddly;do;x.lisp")
(translate-logical-pathname "PROJECTS:LISP;xyzzy.txt")

(with-open-file (f
                 (translate-logical-pathname "PROJECTS:LISP;xyzzy.txt")
                 :direction :output
                 :if-does-not-exist :create
                 :if-exists :rename)
  (write-string "Hello there..." f)
  (write-string (date-string (get-universal-time) t) f))

(date-string (get-universal-time) t)
(date-string (get-universal-time))
(date-string)
(date-string nil t)

;; --------------------------------------------------------
;; Ye Olde FFS - in a nutshell...

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

(defvar *ffs*
  '(("projects:" . "~/projects/")
    ("lisp:"     . "projects:Lispworks/")
    ("actors:"   . "lisp:actors/")))

(defun ffs-reader (stream)
  (prefix-translation (string (read stream)) *ffs*))

(defun set-ffs-translations (lst)
  (setf *ffs* lst))

(um:set-/-dispatch-reader 'ffs #'ffs-reader)

#|
(prefix-translation "actors:examples/diddly.txt" *ffs*)

(list #/ffs/"actors:examples/diddly.txt")
|#

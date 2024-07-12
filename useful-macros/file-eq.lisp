;; file-eq.lisp
;;
;; DM/RAL  2022/11/09 07:28:26
;; ----------------------------------

(defpackage #:com.ral.useful-macros.file-eq
  (:use #:common-lisp))

(in-package #:com.ral.useful-macros.file-eq)

;; ----------------------------------

#|
#+:LISPWORKS
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
                   `("/usr/bin/stat" "-sL"
		     ,(namestring (truename fname)))
                   :output-stream s)
		  ))
	   (items (um:split-string txt))
	   (dev (find-if (finder "st_dev=") items))
	   (ino (find-if (finder "st_ino=") items)))
      (values dev ino))))
|#

(defun um:get-ino (fname)
  (let ((stat  (sys:get-file-stat fname)))
    (values (sys:file-stat-device stat)
            (sys:file-stat-inode  stat))
    ))

#+:SBCL
(defun file-string (filename)
  (with-open-file (f filename)
    (with-output-to-string (s)
      (loop for line = (read-line f nil f)
              until (eql line f)
              do (progn
                   (princ line s)
                   (terpri s))
                 ))))

#+:SBCL
(defun call-system-showing-output (prog &rest args)
  (with-output-to-string (s)
    (sb-ext:run-program prog args
                        :wait t
                        :output s)))

#+:SBCL
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
    (let* ((txt (call-system-showing-output
                 "/usr/bin/stat" "-sL" (namestring (truename fname))) )
           (items (um:split-string txt))
           (dev (find-if (finder "st_dev=") items))
           (ino (find-if (finder "st_ino=") items)))
      (values dev ino))))

#+:ALLEGRO
(defun um:get-ino (fname)
  ;; Return dev and inode for given file. Two files are the same if
  ;; they have the same dev and inode, regardless of how you reach
  ;; them through filenames, links, softlinks, etc.
  (labels ((finder (s)
             (let ((len  (length s)))
               (lambda (str)
                 (and (> (length str) len)
                      (string-equal s str :end2 len)))
               ))
	   (get-from-shell (stream)
	     (with-output-to-string (s)
	       (do ((ch (read-char-no-hang stream)
			(read-char-no-hang stream)))
		   ((null ch))
		 (write-char ch s)))))
    (let* ((txt (multiple-value-bind (shell-stream _ pid)
		    (excl:run-shell-command 
		     (vector "/usr/bin/stat -sL "
			     (namestring (truename fname))
			     (format nil " ; exit~%"))
		     :wait   nil
		     :output :stream)
		  (declare (ignore _))
		  (unwind-protect
		      (get-from-shell shell-stream)
		    (close shell-stream)
		    (sys:reap-os-subprocess pid)
		    )))
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

(defun um:file-time-to-utc (time)
  ;; Convert a Unix file time to Lisp UTC form
  (+ time #.(encode-universal-time 0 0 0 1 1 1970)))

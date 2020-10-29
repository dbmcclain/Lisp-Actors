#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :um)

;; SAFE-CALL-SYSTEM - fixes a bug in LWM that prevents proper
;; character set translation of extended chars in filenames, making a
;; straightforward CALL-SYSTEM fail when it should otherwise succeed.
;;
;; DM/RAL 02/17 (LWM 7.0)

(defmethod safe-call-system ((v vector) &rest args &key &allow-other-keys)
  (apply #'safe-call-system (coerce v 'list) args))

(defmethod safe-call-system ((lst list) &rest args &key &allow-other-keys)
  (apply #'safe-call-system (format nil "~{~A ~}" lst) args))

(defmethod safe-call-system ((cmd string) &rest args &key showing &allow-other-keys)
  (let ((syscmd (if showing
                    #'sys:call-system-showing-output
                  #'sys:call-system)))
    (remf args :showing)
    (if (some (lambda (c)
                (> (char-code c) 127))
              cmd)
        (let ((scrfname (hcl:create-temp-file :directory (or (probe-file "/Volumes/ramdisk/")
                                                             "/tmp/"))))
          (unwind-protect
              (progn
                (with-open-file (s scrfname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format '(:UTF-8 :eol-style :lf))
                  (write-line cmd s))
                (apply syscmd (list "/bin/sh" (namestring scrfname)) args))
          (delete-file scrfname)))
      ;; else
      (apply syscmd cmd args))
    ))


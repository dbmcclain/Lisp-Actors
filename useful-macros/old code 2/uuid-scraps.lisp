
(defun find-ethernet-address ()
  (let ((devices (directory "/sys/class/net/*")))
    ; ignore loopback interface(s)
    (setf devices (remove-if (lambda (path)
                   (equalp 0
                       (search "lo" (car (last (pathname-directory path))) :test #'equalp)))
                 devices))
    (mapcar (lambda (path)
          (with-open-file (s (make-pathname
                  :directory (pathname-directory path)
                  :name "address"))
                (read-line s nil nil nil)))
        devices)))

(defun microsecond-time ()
  (declare (optimize (speed 3) (debug 0)))
  (rlet ((now :timeval)
         (since :timeval))
    (#_gettimeofday now (%null-ptr))
    (ccl::%sub-timevals since now ccl::*lisp-start-timeval*)
    (+ (* #.(truncate 1e6) (the (unsigned-byte 32) (pref since :timeval.tv_sec)))
       (the fixnum (pref since :timeval.tv_usec)))))

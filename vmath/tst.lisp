
(defparameter *var* 15)

(progn
  (mp:process-run-function "Test Process" ()
                           (lambda ()
                             (declare (special *var*))
                             (setf *var* 32)
                             (sleep 1)
                             (debug-stream:debug-print
                              (format nil "*var* = ~A" *var*))
                             (setf *var* 33)
                             (sleep 1)
                             (debug-stream:debug-print
                              (format nil "*var* = ~A" *var*))))
  (let ((*var* 99))
    (declare (special *var*))
    (sleep 1)
    (debug-stream:debug-print (format nil "~&(main) *var* = ~A" *var*))))



#|
(defun ensure-par-safe-behavior (beh)
  (check-type beh function)
  (locally
    #F
    (declare (function beh))
    ;; Take care to avoid race conditition as a result of BECOME
    (labels ((go-around-beh (&rest msg)
               (send* self msg))
             (restore-behavior (cx)
               (declare (ignore cx))
               (setf (actor-beh self) #'swap-out-beh))
             (swap-out-beh (&rest msg)
               (if (sys:compare-and-swap (actor-beh self)
                                         #'swap-out-beh
                                         #'go-around-beh)
                   (handler-bind ((error #'restore-behavior))
                     (progn
                       (apply beh msg)
                       (setf (actor-beh self) (or (shiftf *new-beh* nil)
                                                  #'swap-out-beh))))
                 ;; else -- something changed beh behind our backs...
                 (send* self msg))))
      #'swap-out-beh)))
|#
#||#

(defun appendq (qhd qtl)
  #F
  (declare (cons qhd qtl))
  (when (car qtl)
    (if (car qhd)
        (setf (cddr qhd) (car qtl))
      (setf (car qhd) (car qtl)))
    (setf (cdr qhd) (cdr qtl))
    ))


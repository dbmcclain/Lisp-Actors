
(defun >lfp (dpl num exp)
  (%>lfp num (- exp dpl)))

(defun %>lfp (num exp)
  (multiple-value-bind (lw lf)
      (round (log num 2d0))
    (multiple-value-bind (w f)
        (round (* exp 198096465) 59632978)
      (multiple-value-bind (w2 f2)
          (round (+ f (* lf 59632978)) 59632978)
        (list (round (* f2 #.(ash 1 16)) 59632978)
              (+ lw w w2))
        ))))

      (+ lnum< (- lnum>)
  (let ((val (+ (integer-length num)
                (log (/ num 
  (let ((bexp (+ (integer-length num)
                 (
  (multiple-value-bind (w f)
      (round (* exp (log 10 2)))
  (let* ((lnum> (integer-length num))
         (w (+ (integer-length num)
  (round
   (* #.(expt 2 16)
      (+ (log num 2)
         (* #.(log 10 2) exp)))))
   
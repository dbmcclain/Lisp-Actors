
(in-package :loenc)

(defun dump (vec)
  (format t "~%____   0  1  2  3   4  5  6  7   8  9  A  B   C  D  E  F")
  (let ((nvec (length vec)))
    (loop for off from 0 below nvec by 16 do
          (let* ((row  (subseq vec off (min nvec (+ off 16))))
                 (nrow (length row))
                 (grps (um:group (coerce row 'list) 4)))
            (format t "~%~4,'0x  ~{~{~2,'0x ~} ~}" off grps)
            (let* ((rem4  (mod nrow 4))
                   (rem16 (ash (- 16 nrow) -2))
                   (nsp   (+ (* rem16 13)
                             (if (plusp rem4)
                                 (* 3 (- 4 rem4))
                               0))))
              (princ (make-string nsp :initial-element #\space)))
            (princ (map 'string (lambda (x)
                                  (let ((ch (code-char x)))
                                    (if (graphic-char-p ch)
                                        ch
                                      #\.)))
                        row))
            )))
  vec)

               
              
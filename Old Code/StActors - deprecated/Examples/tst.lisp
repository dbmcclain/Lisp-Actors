
(in-package :um)

(defun tst ()
  (loop repeat 10000000 do
        (let* ((n (random #.(ash 1 10)))
               (nb> (ceiling-log2 n))
               (nb< (floor-log2 n)))
          (assert (and (<= nb< nb>)
                       (<= (ash 1 nb<) n (ash 1 nb>)))))))

(tst)

(dolist (n '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536))
  (assert (= (ceiling-log2 n) (floor-log2 n))))


(defun tst (&optional (n 1000000))
  (time
   (loop repeat n do
         ;; (ash 1 512)
         (unchecked-pwr2 512)
         )))
(tst)                      

(disassemble #'tst)
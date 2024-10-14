;; edwards-tpm.lisp
;;
;; DM/RAL  2023/02/22 02:35:27
;; ----------------------------------

(in-package #:edec-mm)

;; ----------------------------------

#|
(DEFUN SMULT (PT)
  (COND ((ED-VALID-POINT-P PT)
         (LET ((WC (MAKE-BIPOLAR-WINDOW-CACHE :NBITS 4 :PT PT)) (ANS (ED-PROJECTIVE (ED-NEUTRAL-POINT))))
           (SETF (AREF (BIPOLAR-WINDOW-CACHE-PRECV WC) 8) ANS)
           (FLET ((ACC (IX)
                    (SETF ANS
                          (ED-PROJECTIVE-ADD
                           (GET-PREC WC IX)
                           (ED-PROJECTIVE-DOUBLE
                            (ED-PROJECTIVE-DOUBLE (ED-PROJECTIVE-DOUBLE (ED-PROJECTIVE-DOUBLE ANS))))))))
             (ACC 0)
             (ACC 1)
             (ACC 2)
             (ACC -1)
             (ACC 1)
             (ACC -4)
             (ACC 7)
             (ACC 0)
             (ACC 3)
             (ACC 6)
             (ACC 5)
             (ACC -2)
             (ACC -7)
             (ACC -3)
             (ACC -1)
             (ACC 2)
             (ACC -6)
             (ACC -2)
             (ACC -3)
             (ACC 3)
             (ACC -3)
             (ACC -3)
             (ACC 4)
             (ACC 6)
             (ACC 3)
             (ACC -4)
             (ACC 7)
             (ACC 3)
             (ACC 0)
             (ACC 3)
             (ACC -5)
             (ACC 1)
             (ACC 6)
             (ACC 7)
             (ACC 2)
             (ACC -1)
             (ACC -1)
             (ACC -4)
             (ACC -2)
             (ACC -3)
             (ACC -2)
             (ACC 5)
             (ACC 3)
             (ACC 3)
             (ACC -8)
             (ACC -7)
             (ACC -1)
             (ACC 2)
             (ACC 5)
             (ACC -8)
             (ACC -4)
             (ACC -7)
             (ACC 1)
             (ACC -5)
             (ACC -7)
             (ACC -6)
             (ACC -7)
             (ACC -8)
             (ACC 3)
             (ACC -3)
             (ACC -2)
             (ACC 5)
             (ACC 4)
             (ACC 7)
             ANS)))
        (T (ED-PROJECTIVE (ED-NEUTRAL-POINT)))))
|#

(DEFUN SMULT (PT)
  #F
  (LET ((WC  (make-bipolar-window-cache :nbits 4 :pt pt))
        (zp  (ed-projective (ed-neutral-point)))
        (mac (uuid:uuid-mac (uuid:make-v1-uuid)))
        (ans pt))
    (COND ((ED-VALID-POINT-P PT)
           (labels ((setup ()
                    (setf wc    (make-bipolar-window-cache :nbits 4 :pt ans)
                          ans   zp
                          (aref (bipolar-window-cache-precv wc) 8) ans))
                  (quad ()
                    (ED-PROJECTIVE-DOUBLE
                     (ED-PROJECTIVE-DOUBLE
                      (ED-PROJECTIVE-DOUBLE
                       (ED-PROJECTIVE-DOUBLE ANS)))))
                  (ACC (IX)
                    (SETF ANS
                          (ED-PROJECTIVE-ADD
                           (GET-PREC WC IX)
                           (quad))
                          )))
             (dolist (enc '((1 0 2)
                            (1 0 3)
                            (1 0 7)
                            (1 0 3 -7 1 1)
                            (1 0 6 -2 -6 3)
                            (1 0 4 -7 6 5 -7 -3 -5)
                            (1 0 2 1 -8 -7 -1 7 -2 -1 -7 3 5 -2 -6 -7 -8 -6 7 7 -4 -5 2
                               -6 0 -3 7 6 1 -7 5 -5 2 -3 1 4 1 1 6 3 7 2 -6 -7 -2 5 -2 -3 -2 -3)
                            ))
               (loop repeat (car enc)
                     do
                       (setup)
                       (dolist (jx (cdr enc))
                         (acc jx))))
             (ed-projective-add ans
                                (ed-mul pt (1+ mac)))
             ))
        (T zp)
        )))
        
(defun delivered-smult ()
  (let ((status 0))
    #|
    (format t "~d Args~%" (length sys:*line-arguments-list*))
    (loop for ix from 1
          for arg in sys:*line-arguments-list*
          do
            (format t "Arg ~d: ~A~%" ix arg))
    |#
    (handler-case
        (let* ((hexpt (cadr sys:*line-arguments-list*))
               (pt    (ed-decompress-pt (read-from-string hexpt)))
               (ans   (ed-compress-pt (smult pt))))
          (with-standard-io-syntax
            (format t "#x~X~%" (int ans))) )

      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))

;; ------------------------------------------------------------------
;; 507170953780350711035567992110731051963338283488364274453071588128895853895
;; = 1+ {{ 2, 1},                       (0 2)
;;       { 3, 1},                       (0 3)
;;       { 67078289, 1},                (0 4 0 0 -7 -7 -7 1)
;;       { 34811885351, 1},             (0 1 -8 2 -5 -1 3 3 7 2 7)
;;       { 82249918767968177, 1},       (0 1 2 4 3 6 -2 -2 -8 7 0 -8 3 0 -5 1)
;;       { 592649578008604471, 1},      (0 1 -8 4 -6 -8 4 -6 7 -5 6 -8 -3 -1 -1 3 7)
;;       { 742608891092997079873, 1}}   (0 3 -8 4 2 -4 5 -4 4 -1 0 4 5 -3 6 -8 3 4 1)

#|
(with-standard-io-syntax
  (print (- 507170953780350711035567992110731051963338283488364274453071588128895853895
            (uuid:uuid-mac {19729638-bc31-11ed-a59d-f643f5d48a64}) 1))
  (values))

(loop for enc in '((2 1) (3 1) (7 1) (10513 1) (23971 1) (60180683 1)
                   (796223798301257383750907967191458613626940280471134985437 1))
        collect
        (list* (cadr enc) (windows (car enc) 4)))
|#  

(defun gen-mult (n)
  (let ((ws (edec::windows n 4.)))
    `(defun smult (pt)
       (cond ((ed-valid-point-p pt)
              (let ((wc   (make-bipolar-window-cache
                           :nbits 4.
                           :pt    pt))
                    (ans  (ed-projective (ed-neutral-point))))
                (setf (aref (bipolar-window-cache-precv wc) 8.) ans)
                (flet ((acc (ix)
                         (setf ans (ed-projective-add (get-prec wc ix)
                                                      (ed-projective-double
                                                       (ed-projective-double
                                                        (ed-projective-double
                                                         (ed-projective-double ans))))
                                                      ))
                         ))
                  ,@(loop for w in ws collect `(acc ,w))
                  ans)))
             (t
              (ed-projective (ed-neutral-point)))
             ))
    ))

#|
(let ((*print-level* nil))
  (print (gen-mult com.ral.actors.secure-comm::x))
  (values))
|#


(in-package :edec)

(ql:quickload :cl-algebraic-data-type)

;; ------------------------------------------------------------------------

(defun to-u256 (x)
  (vector (ldb (byte 64 0) x)
          (ldb (byte 64 64) x)
          (ldb (byte 64 128) x)
          (ldb (byte 64 192) x)))

(defun from-u256 (xv)
  (dpb (aref xv 0) (byte 64 0)
       (dpb (aref xv 1) (byte 64 64)
            (dpb (aref xv 2) (byte 64 128)
                 (dpb (aref xv 3) (byte 64 192) 0)))))

(defvar *r*      (to-u256 *ed-r*))
(defvar *rinv*   (with-mod (ash 1 64) (m- (m/ *ed-r*))))
(defvar *rsqr*   (mod (ash 1 512) *ed-r*))

(defvar *rone*   (mod (ash 1 256) *ed-r*))
(defvar *1/rone* (with-mod *ed-r* (m/ (ash 1 256))))

;; ------------------------------------------------------------------

(adt:defdata fr
  (scaled integer)
  (unscaled integer))

(defmethod to-scaled ((x fr))
  (adt:match fr x
    ((scaled _) x)
    ((unscaled u) (scaled (mul-mod u *rsqr* *r* *rinv*)))
    ))

(defmethod to-scaled ((x integer))
  (to-scaled (unscaled x)))

(defmethod to-unscaled ((x fr))
  (adt:match fr x
    ((unscaled _) x)
    ((scaled s) (unscaled (mul-collapse s *r* *rinv*)))
    ))

(defmethod to-unscaled ((x integer))
  (unscaled x))

(defmethod int ((x fr))
  (unscaled%0 (to-unscaled x)))

(defun mpy (a b)
  (let ((a (to-scaled a))
        (b (to-scaled b)))
    (scaled (mul-mod (scaled%0 a) (scaled%0 b) *r* *rinv*))))

;; ------------------------------------------------------------------

(defun split_u64 (x)
  (values (ldb (byte 32 32) x)
          (ldb (byte 32  0) x)))

(defun combine_u64 (hi lo)
  (dpb hi (byte 32 32)
       (dpb lo (byte 32 0) 0)))

#|
(defun mac-digit (av start bv c)
  (declare (fixnum start)
           (integer c))
  (labels ((mac-with-carry (a b c carry)
             (declare (integer a b c)
                      (cons carry))
             (multiple-value-bind (ahi alo)
                 (split_u64 a)
               (declare (integer ahi alo))

               (multiple-value-bind (bhi blo)
                   (split_u64 b)
                 (declare (integer bhi blo))

                 (multiple-value-bind (chi clo)
                     (split_u64 c)
                   (declare (integer chi clo))

                   (multiple-value-bind (cyhi cylo)
                       (split_u64 (car carry))
                     (declare (integer cyhi cylo))
                     
                     (multiple-value-bind (xhi xlo)
                         (split_u64 (+ (* blo clo)
                                       alo cylo))
                       (declare (integer xhi xlo))

                       (multiple-value-bind (yhi ylo)
                           (split_u64 (* blo chi))
                         (declare (integer yhi ylo))

                         (multiple-value-bind (zhi zlo)
                             (split_u64 (* bhi clo))
                           (declare (integer zhi zlo))

                           (multiple-value-bind (rhi rlo)
                               (split_u64 (+ xhi ylo zlo ahi cyhi))
                             (declare (integer rhi rlo))

                             (setf (car carry) (+ (* bhi chi)
                                                  rhi yhi zhi))
                             (combine_u64 rlo xlo)))))))))))
    (unless (zerop c)
      (let* ((carry (list 0)))
        (loop for ib from 0
              for ia from start
              do
              (symbol-macrolet ((ai (aref av ia))
                                (bi (aref bv ib)))
                (cond ((< ib 4)
                       (setf ai (mac-with-carry ai bi c carry)))
                      
                      ((not (zerop (car carry)))
                       (setf ai (mac-with-carry ai 0 c carry)))
                      
                      (t
                       (loop-finish))
                      )))))))
|#
#||#
(defun mac-digit (av start bv c)
  (declare (optimize (safety 0) (float 0)))
  (labels ((mac-with-carry (a b c carry)
             (multiple-value-bind (ahi alo)
                 (split_u64 a)
               (declare (type (signed-byte 64) ahi alo))
               
               (multiple-value-bind (bhi blo)
                   (split_u64 b)
                 (declare (type (signed-byte 64) bhi blo))

                 (multiple-value-bind (chi clo)
                   (split_u64 c)
                   (declare (type (signed-byte 64) chi clo))

                   (multiple-value-bind (cyhi cylo)
                       (split_u64 (car carry))
                     (declare (type (signed-byte 64) cyhi cylo))
                     
                     (multiple-value-bind (xhi xlo)
                         (split_u64 (sys:int64-to-integer
                                     (sys:int64+
                                      (sys:int64* blo clo)
                                      (sys:int64+ alo cylo))))
                       (declare (type (signed-byte 64) xhi xlo))

                       (multiple-value-bind (yhi ylo)
                           (split_u64 (sys:int64-to-integer
                                       (sys:int64* blo chi)))
                         (declare (type (signed-byte 64) yhi ylo))

                         (multiple-value-bind (zhi zlo)
                             (split_u64 (sys:int64-to-integer
                                         (sys:int64* bhi clo)))
                           (declare (type (signed-byte 64) zhi zlo))

                           (multiple-value-bind (rhi rlo)
                               (split_u64
                                (sys:int64-to-integer
                                 (sys:int64+ xhi
                                             (sys:int64+ ylo
                                                         (sys:int64+ zlo
                                                                     (sys:int64+ ahi cyhi))))))
                             (declare (type (signed-byte 64) rhi rlo))

                             (setf (car carry) (sys:int64-to-integer
                                                (sys:int64+
                                                 (sys:int64* bhi chi)
                                                 (sys:int64+ rhi
                                                             (sys:int64+ yhi zhi)))))
                             (combine_u64 rlo xlo)))))))))))
    (unless (zerop c)
      (let* ((carry (list 0)))
        (loop for ib from 0
              for ia from start
              do
              (symbol-macrolet ((ai (aref av ia))
                                (bi (aref bv ib)))
                (cond ((< ib 4)
                       (setf ai (mac-with-carry ai bi c carry)))
                      
                      ((not (zerop (car carry)))
                       (setf ai (mac-with-carry ai 0 c carry)))
                      
                      (t
                       (loop-finish))
                      )))))))
#||#

(defun finalize-mul (res modulus inv)
  (loop for i fixnum from 0 below 4 do
        (let ((k  (with-mod #.(ash 1 64)
                    (m* inv (aref res i)))))
          (declare (integer k))
          (mac-digit res i modulus k)))
  (let ((ans (from-u256 (subseq res 4)))
        (q   (from-u256 modulus)))
    (declare (integer ans q))
    (if (> ans q)
        (- ans q)
      ans)))

(defun mul-reduce (this by modulus inv)
  (let* ((res  (make-array 8 :initial-element 0)))
    (loop for i fixnum from 0 below 4
          for xi across this
          do
          (mac-digit res i by xi))
    (finalize-mul res modulus inv)))

(defun mul-collapse (x modulus inv)
  (let* ((xv  (to-u256 x))
         (res (make-array 8 :initial-element 0)))
    (replace res xv)
    (finalize-mul res modulus inv)))

(defun mul-mod (a b modulus inv)
  (let* ((av  (to-u256 a))
         (bv  (to-u256 b)))
    (mul-reduce av bv modulus inv)))
 
;; ----------------------------------------------------------
#|
(let* ((n 10000)
       (as (loop repeat n collect (random-between 1 *ed-r*)))
       (bs (loop repeat n collect (random-between 1 *ed-r*))))
  (time (map nil 'mpy as bs))
  (with-mod *ed-r*
    (time (map nil 'm* as bs)))
  (map nil (lambda (a b)
             (let ((p1 (int (mpy a b)))
                   (p2 (with-mod *ed-r*
                         (m* a b))))
               (unless (= p1 p2)
                 (error "mpy = ~A, m* = ~A" p1 p2))))
       as bs))

;; -----------------------------------------------------
;; check signed/unsigned arithmetic

(let* ((a  #xffffffff)
       (b  #xffffffff))
  (declare (type (signed-byte 64) a b))
  (format t "a = ~A~%" (hex-str a))
  (let ((x (sys:int64-to-integer (sys:int64* a b))))
    (format t "x = ~A~%" (hex-str x))
    (multiple-value-bind (xhi xlo) (split_u64 x)
      (format t "xhi = ~A~%" (hex-str xhi))
      (format t "xlo = ~A~%" (hex-str xlo)))))
 |#
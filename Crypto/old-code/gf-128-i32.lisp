;; gf-128-i32.lisp -- Galois Polynomial Field of order 128
;; DM/Acudora  02/12
;; ----------------------------------------------------

(in-package :ecc-crypto-b128)

;;------------------------------------------------
;; ECC over the Field F(2^128)
;; based on underlying polynomial field GF(2^128)

(defstruct gf-val
  wds)

;; -----------------------------------------------

(defun raw-wrd (wds ix)
  #F
  (declare (sys:simple-int32-vector wds)
           (fixnum ix))
  (sys:typed-aref '(unsigned-byte 32) wds (* 4 ix)))

(defun put-raw-wrd (v wds ix)
  #F
  (declare ((unsigned-byte 32) v)
           (sys:simple-int32-vector wds)
           (fixnum ix))
  (setf (sys:typed-aref '(unsigned-byte 32) wds (* 4 ix)) v))

(defsetf raw-wrd (wrd ix) (v)
  `(put-raw-wrd ,v ,wrd ,ix))

;; -----------------------------------------------

(fli:define-c-union lchimera
  (l  :uint32)
  (b  (:c-array :uint8 4)))

(defun ntohl (v)
  #F
  (declare ((unsigned-byte 32) v))
  (fli:with-dynamic-foreign-objects ()
    (let ((cv (fli:allocate-dynamic-foreign-object
               :type 'lchimera)))
      (setf (fli:dereference (fli:foreign-slot-pointer cv 'l)) v)
      (dpb (fli:foreign-aref (fli:foreign-slot-pointer cv 'b) 0)
           (byte 8 24)
           (dpb (fli:foreign-aref (fli:foreign-slot-pointer cv 'b) 1)
                (byte 8 16)
                (dpb (fli:foreign-aref (fli:foreign-slot-pointer cv 'b) 2)
                     (byte 8 8)
                     (dpb (fli:foreign-aref (fli:foreign-slot-pointer cv 'b) 3)
                          (byte 8 0)
                          0)))) )))

(defun htonl (v)
  (ntohl v))

(defun get-wrd (wds ix)
  #F
  (declare (sys:simple-int32-vector wds)
           (fixnum ix))
  (let ((v (raw-wrd wds ix)))
    (declare ((unsigned-byte 32) v))
    (ntohl v)))

(defun put-wrd (v wds ix)
  #F
  (declare ((unsigned-byte 32) v)
           (sys:simple-int32-vector wds)
           (fixnum ix))
  (let ((vn (htonl v)))
    (setf (raw-wrd wds ix) vn)
    v))

(defsetf get-wrd (wds ix) (v)
  `(put-wrd ,v ,wds ,ix))
  
;; -----------------------------------------------

(defmethod print-object ((obj gf-val) stream)
  (format stream "#<gf-val ~{#x~8,'0x~^ ~}"
          (let ((wds (gf-val-wds obj)))
            (declare (sys:simple-int32-vector wds))
            (loop for ix from 0 below 4 collect
                  (get-wrd wds ix)) )))

#|
(defmethod print-object ((obj sys:simple-int32-vector) stream)
  (format stream "#<sys:simple-int32-vector>"))
|#

(defun make-gf-wds (&optional (val 0))
  #F
  (let ((wds (sys:make-typed-aref-vector 16)))
    (fill-wds wds val)))

(defun fill-wds (wds val)
  #F
  (declare (sys:simple-int32-vector wds))
  (cond
   ((integerp val)
    (loop for ix fixnum from 3 downto 0 do
          (setf (get-wrd wds ix) (ldb (byte 32 0) val)
                val (ash val -32))))
   
   ((or (consp val)
        (vectorp val))
    (let* ((nval (length val))
           (nz   (- 4 nval)))
      (loop for ix fixnum from 0 below nz do
            (setf (raw-wrd wds ix) 0))
      (loop for ix fixnum from nz below 4
            for jx from 0
            do
            (setf (get-wrd wds ix) (elt val jx))))))
  wds)

(defun clone-gf-val (x)
  #F
  (declare (gf-val x))
  (let ((wdsx  (gf-val-wds x))
        (wdsy  (make-gf-wds)))
    (declare (sys:simple-int32-vector wdsx wdsy))
    (loop for ix from 0 below 4 do
          (setf (raw-wrd wdsy ix) (raw-wrd wdsx ix)))
    (make-gf-val :wds wdsy)))

(defun fill-gf-val (gfv val)
  #F
  (declare (gf-val gfx))
  (fill-wds (gf-val-wds gfv) val)
  gfv)

(defun new-gf-val (&optional (val 0))
  #F
  (make-gf-val :wds (make-gf-wds val)))

(defun gf-one ()
  #F
  (let ((wds (make-gf-wds)))
    (declare (sys:simple-int32-vector wds))
    (setf (get-wrd wds 3) 1)
    (make-gf-val :wds wds)))

(defun gf-zero ()
  (new-gf-val))

(defun gf+= (gfv incr)
  #F
  (declare (gf-val gfv incr))
  (let* ((wdsa (gf-val-wds gfv))
         (wdsb (gf-val-wds incr)))
    (declare (sys:simple-int32-vector wdsa wdsb))
    (loop for ix fixnum from 0 below 4 do
          (setf (raw-wrd wdsa ix)
                (logxor (raw-wrd wdsa ix)
                        (raw-wrd wdsb ix)) ))
    gfv))

(defun gf+ (arg &rest args)
  #F
  (reduce #'gf+= args :initial-value (clone-gf-val arg)))

(defun gf- (&rest args)
  #F
  (apply #'gf+ args))

(defvar *nbits* 128)

(defvar $prim
  ;; B128: t^128 + t^7 + t^2 + t^1 + 1
  (new-gf-val (list #x00000000 #x00000000 #x00000000 #x00000087 )))

(defun shl= (gfv &optional (n 1))
  #F
  (declare (gf-val gfv)
           (fixnum n))
  (let* ((wds (gf-val-wds gfv)))
    (declare (gf-val ans)
             (sys:simple-int32-vector wds))
    (multiple-value-bind (nw nbits) (truncate n 32)
      (declare (fixnum nw nbits))
      (when (plusp nw)
        (loop for ix fixnum from 0 
              for jx fixnum from nw below 4
              do
              (setf (raw-wrd wds ix) (raw-wrd wds jx)
                    (raw-wrd wds jx) 0))
        (loop for ix fixnum from (- 4 nw) below nw do
              (setf (raw-wrd wds ix) 0)) )
      
      (when (plusp nbits)
        (let ((v1   (get-wrd wds 0))
              (nbc  (- 32 nbits)))
          (declare ((unsigned-byte 32) v1)
                   (fixnum nbc))
          (loop for ix fixnum from 0 below (- 3 nw)
                for jx fixnum from 1
                do
                (let* ((v2   (get-wrd wds jx))
                       (msb  (if (zerop v1)
                                 v1
                               (ash (ldb (byte nbc 0) v1) nbits)))
                       (lsb  (if (zerop v2)
                                 v2
                               (ldb (byte nbits nbc) v2))))
                  (declare ((unsigned-byte 32) v2 msb lsb))
                  (setf (get-wrd wds ix) (logior msb lsb)
                        v1 v2)))
          (setf (get-wrd wds (- 3 nw))
                (ash (ldb (byte nbc 0) v1) nbits)) ))
      gfv)))

(defun shl (gfv &optional (n 1))
  #F
  (shl= (clone-gf-val gfv) n))

(defun step-state= (gfv)
  #F
  (declare (gf-val gfv))
  (let* ((wds  (gf-val-wds gfv))
         (v    (get-wrd wds 0)))
    (declare (sys:simple-int32-vector wds)
             ((unsigned-byte 32) v))
    (shl= gfv)
    (when (logbitp 31 v)
      (setf (get-wrd wds 3) 
            (logxor (get-wrd wds 3) #x87))))
  gfv)

(defun step-state (gfv)
  #F
  (step-state= (clone-gf-val gfv)))

(defun gf< (a b)
  #F
  (declare (gf-val a b))
  (let ((wdsa (gf-val-wds a))
        (wdsb (gf-val-wds b)))
    (declare (sys:simple-int32-vector wdsa wdsb))
    (um:nlet-tail iter ((ix 0))
      (declare (fixnum ix))
      (if (< ix 4)
          (let ((a  (get-wrd wdsa ix))
                (b  (get-wrd wdsb ix)))
            (declare ((unsigned-byte 32) a b))
            (if (< a b)
                t
              (iter (1+ ix))))
        nil)) ))

(defun gf= (a b)
  #F
  (declare (gf-val a b))
  (let ((wdsa (gf-val-wds a))
        (wdsb (gf-val-wds b)))
    (declare (sys:simple-int32-vector wdsa wdsb))
    (um:nlet-tail iter ((ix 0))
      (declare (fixnum ix))
      (if (< ix 4)
          (let ((a (raw-wrd wdsa ix))
                (b (raw-wrd wdsb ix)))
            (declare ((unsigned-byte 32) a b))
            (if (= a b)
                (iter (1+ ix))
              nil))
        t))))

(defun gf-zerop (x)
  #F
  (declare (gf-val x))
  (let ((wdsx (gf-val-wds x)))
    (declare (sys:simple-int32-vector wdsx))
    (um:nlet-tail iter ((ix 0))
      (declare (fixnum ix))
      (if (< ix 4)
          (if (zerop (raw-wrd wdsx ix))
              (iter (1+ ix))
            nil)
        t) )))

(defun gf=1p (x)
  #F
  (declare (gf-val x))
  (let ((wdsx (gf-val-wds x)))
    (declare (sys:simple-int32-vector wdsx))
    (um:nlet-tail iter ((ix 0))
      (declare (fixnum ix))
      (if (< ix 3)
          (if (zerop (raw-wrd wdsx ix))
              (iter (1+ ix))
            nil)
        (= 1 (get-wrd wdsx 17))) )))

(defun gf-integer-length (x)
  #F
  (declare (gf-val x))
  (let ((wdsx (gf-val-wds x)))
    (declare (sys:simple-int32-vector wdsx))
    (um:nlet-tail iter ((len 128)
                        (ix  0))
      (declare (fixnum len ix))
      (if (< ix 4)
          (let ((v (get-wrd wdsx ix)))
            (declare ((unsigned-byte 32) v))
            (if (zerop v)
                (iter (- len 32) (1+ ix))
              (+ len (integer-length v))))
        0))))

(defun gf-logbitp (index v)
  #F
  (declare (fixnum index)
           (gf-val v))
  (let ((wds (gf-val-wds v)))
    (declare (sys:simple-int32-vector wds))
    (multiple-value-bind (nwd nbit) (truncate index 32)
      (declare (fixnum nwd nbit))
      (logbitp nbit (get-wrd wds (- 3 nwd))) )))
      
(defun %gf* (a b)
  #F
  (declare (gf-val a b))
  ;; right-to-left
  (when (gf< a b)  ;; get smaller arg in b
    (rotatef a b))
  (cond ((gf-zerop b) b)
        (t  (do ((ans   (clone-gf-val a))
                 (index (- (gf-integer-length b) 2) (1- index)))
                ((minusp index) ans)
              (declare (gf-val ans)
                       (fixnum index))
              (step-state= ans)
              (when (gf-logbitp index b)
                (gf+= ans a)) ))
        ))

(defun gf* (arg &rest args)
  #F
  (reduce #'%gf* args :initial-value (clone-gf-val arg)))

;; -----------------------------------------------------------------------

(defun gf^ (gfv n)
  #F
  (declare (gf-val  gfv)
           (integer n))
  ;; right-to-left
  (labels ((expt (n)
             (declare (integer n))
             (cond ((zerop n) #.(gf-one))
                   ((= 1 n)   gfv)
                   (t  (do ((ans   (clone-gf-val gfv))
                            (index (- (integer-length n) 2) (1- index)))
                           ((minusp index) ans)
                         (declare (gf-val ans)
                                  (fixnum index))
                         (setf ans (gf* ans ans))
                         (when (logbitp index n)
                           (setf ans (gf* ans gfv))) ))
                   )))
    (if (minusp n)
        (gfinv (expt (- n)))
      (expt n))))


(defun gfdeg (x)
  #F
  (declare (gf-val x))
  (1- (gf-integer-length x)))

(defun gfinv (gfv)
  ;; extended Euclidean algorithm
  #F
  (declare (gf-val gfv))
  (let* ((u  (clone-gf-val gfv))
         (v  (clone-gf-val $prim))
         (g1 (gf-one))
         (g2 (gf-zero)))
    (declare (gf-val u v g1 g2))
    (loop until (gf=1p u) do
          (when (gf-zerop u)
            (error "Zero has no inverse"))
          (let ((j (- (gf-integer-length u) (gf-integer-length v))))
            (declare (fixnum j))
            (when (minusp j)
              (rotatef u  v)
              (rotatef g1 g2)
              (setf j (- j)))
            (gf+= u  (shl v  j))
            (gf+= g1 (shl g2 j)) ))
    g1))

(defun gfmod (x)
  ;; produce x mod f(z)
  #F
  (declare (gf-val x))
  (gfinv (gfinv x)))

(defun gf/ (a b)
  #F
  (declare (gf-val a b))
  (gf* a (gfinv b)))

(defun gf^2 (x)
  #F
  (declare (gf-val x))
  (gf* x x))

;; -----------------------------------------------------------------------------
;;

(defun make-gf-lagrange-interpolator (shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (gf* prod (gf+ x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (destructuring-bind (x y) share
                   (let ((xs (mapcar #'first (remove share shares))))
                     (gf+ sum
                          (gf* y (gf/ (lprod x0 xs)
                                      (lprod x xs)) )) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun solve-gf-lagrange (x0 &rest shares)
  (let ((fn (make-gf-lagrange-interpolator shares)))
    (funcall fn x0)))


;; fr-ver2.lisp -- arbitrary prime fields using Montgomery multiplication (scaled/unscaled elements)
;;
;; DM/RAL  03/19
;; ------------------------------------------------------------------------------------------------

(in-package :edec)

;; ------------------------------------------------------------------------
;; operations on vectors of 64-bit chunks
;; mostly mutating operations

(defun to-u64s (x nchunks)
  (let ((ans (make-array nchunks
                         :element-type '(unsigned-byte 64)
                         :initial-element 0)))
    (um:nlet iter ((ix  0))
      (if (< ix nchunks)
          (progn
            (setf (aref ans ix) (ldb (byte 64 (ash ix 6)) x))
            (go-iter (1+ ix)))
        ans))
    ))

(defun from-u64s (v)
  (let ((ans     0)
        (nchunks (length v)))
    (um:nlet iter ((ix 0))
      (if (< ix nchunks)
          (progn
            (setf ans (dpb (aref v ix) (byte 64 (ash ix 6)) ans))
            (go-iter (1+ ix)))
        ans))))

(defun mod-adj (v modv)
  (unless (v< v modv)
    (sub-no-borrow v modv))
  v)

(defun add-no-carry (vres v2 &optional modv)
  (let ((pcy (list 0)))
    (dotimes (ix (length vres))
      (setf (aref vres ix) (adc (aref vres ix) (aref v2 ix) pcy)))
    (when modv
      (mod-adj vres modv))))

(defun split_u64 (x)
  (values (ldb (byte 32 32) x)
          (ldb (byte 32  0) x)))

(defun combine_u64 (hi lo)
  (dpb hi (byte 32 32)
       (dpb lo (byte 32 0) 0)))

(defun adc (a64 b64 pcy)
  (multiple-value-bind (ahi alo) (split_u64 a64)
    (multiple-value-bind (bhi blo) (split_u64 b64)
      (multiple-value-bind (c rlo) (split_u64 (+ alo blo (car pcy)))
        (multiple-value-bind (c rhi) (split_u64 (+ ahi bhi c))
          (setf (car pcy) c)
          (combine_u64 rhi rlo))))))

(defun sub-no-borrow (vres v2 &optional modv)
  (let ((pcy (list 0)))
    (dotimes (ix (length vres))
      (setf (aref vres ix) (sbb (aref vres ix) (aref v2 ix) pcy)))
    (when modv
      (unless (zerop (car pcy))
        (add-no-carry vres modv)))
    ))

(defun sbb (a64 b64 pcy)
  (multiple-value-bind (ahi alo) (split_u64 a64)
    (multiple-value-bind (bhi blo) (split_u64 b64)
      (let ((ten #.(ash 1 32)))
        (multiple-value-bind (c rlo) (split_u64 (- (+ ten alo) blo (car pcy)))
          (multiple-value-bind (c rhi) (split_u64 (- (+ ten ahi) bhi (if (zerop c) 1 0)))
            (setf (car pcy) (if (zerop c) 1 0))
            (combine_u64 rhi rlo)))))))

(defun cmpv (v1 v2)
  (um:nlet iter ((ix (length v1)))
    (if (zerop ix)
        :equal
      (let* ((i (1- ix))
             (a (aref v1 i))
             (b (aref v2 i)))
        (cond ((< a b) :less)
              ((> a b) :greater)
              (t       (go-iter i))
              )))))

(defun v= (v1 v2)
  (eq :equal (cmpv v1 v2)))

(defun v< (v1 v2)
  (eq :less (cmpv v1 v2)))

(defun zero-from-proto (v)
  (make-array (length v) :initial-element 0))

(defun one-from-proto (v)
  (let ((ans (zero-from-proto v)))
    (setf (aref ans 0) 1)
    ans))

(defun div2 (v)
  (um:nlet iter ((ix (length v))
                 (tmp 0))
    (unless (zerop ix)
      (let* ((i       (1- ix))
             (new-tmp (aref v i)))
        (setf (aref v i) (dpb tmp (byte 1 63)
                              (ldb (byte 63 1) new-tmp)))
        (go-iter i (ldb (byte 1 0) new-tmp))
        ))))

(defun inv-mod (xv modulus)
  (let* ((one  (one-from-proto modulus))
         (u    (copy-seq xv))
         (v    (copy-seq modulus))
         (b    (one-from-proto modulus))
         (c    (zero-from-proto modulus)))
    (um:nlet iter ()
      (cond ((v= u one)  b)
            ((v= v one)  c)
            (t
             (um:while (evenp (aref u 0))
               (div2 u)
               (when (oddp (aref b 0))
                 (add-no-carry b modulus))
               (div2 b))
             (um:while (evenp (aref v 0))
               (div2 v)
               (when (oddp (aref c 0))
                 (add-no-carry c modulus))
               (div2 c))
             (cond ((v< u v)
                    (sub-no-borrow v u)
                    (sub-no-borrow c b modulus))

                   (t
                    (sub-no-borrow u v)
                    (sub-no-borrow b c modulus))
                   )
             (go-iter))
            ))))

;; ------------------------------------------------------------------------
;; Montgomery Multiplication

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
      (let* ((nel   (length bv))
             (carry (list 0)))
        (loop for ib from 0
              for ia from start
              do
              (symbol-macrolet ((ai (aref av ia))
                                (bi (aref bv ib)))
                (cond ((< ib nel)
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
      (let* ((nel   (length bv))
             (carry (list 0)))
        (loop for ib from 0
              for ia from start
              do
              (symbol-macrolet ((ai (aref av ia))
                                (bi (aref bv ib)))
                (cond ((< ib nel)
                       (setf ai (mac-with-carry ai bi c carry)))
                      
                      ((not (zerop (car carry)))
                       (setf ai (mac-with-carry ai 0 c carry)))
                      
                      (t
                       (loop-finish))
                      )))))))
#||#

(defun montgy-finalize (res modulus inv)
  (declare (integer inv))
  (let ((nel (length modulus)))
    (loop for i fixnum from 0 below nel do
          (let ((k  (ldb (byte 64 0) (* inv (aref res i)))))
            (declare (integer k))
            (mac-digit res i modulus k)))
    (mod-adj (subseq res nel) modulus)))

(defun montgy-mul (this by modulus inv)
  (let* ((nel  (length modulus))
         (res  (make-array (ash nel 1) :initial-element 0)))
    (loop for i fixnum from 0 below nel
          for xi across this
          do
          (mac-digit res i by xi))
    (montgy-finalize res modulus inv)))

(defun montgy-reduce (x modulus inv)
  (let* ((nel  (length modulus))
         (res  (make-array (ash nel 1) :initial-element 0)))
    (replace res x)
    (montgy-finalize res modulus inv)))

;; ------------------------------------------------------------------------
;; Fields and Field Elements

(defclass field ()
  ((modulus  :accessor field-modulus  :initarg :modulus)
   (nchunks  :accessor field-nchunks)
   (modv     :accessor field-modv)
   (minv     :accessor field-minv)
   (msqrv    :accessor field-msqrv)
   (mcubev   :accessor field-mcubev)
   (monev    :accessor field-monev)
   (sqrtfn   :accessor field-sqrtfn)
   (tonell-shanks-params :accessor field-tonelli-shanks-params :initform nil)
   ))

(defmethod initialize-instance :after ((fld field) &key modulus &allow-other-keys)
  (let ((nc  (ceiling (integer-length modulus) 64)))
    (setf (field-nchunks fld)  nc
          (field-modv fld)     (to-u64s modulus nc)
          (field-minv fld)     (with-mod #.(ash 1 64) (m- (m/ modulus)))
          (field-msqrv fld)    (to-u64s (mod (ash 1 (* 2 nc 64)) modulus) nc)
          (field-mcubev fld)   (to-u64s (mod (ash 1 (* 3 nc 64)) modulus) nc)
          (field-monev fld)    (to-u64s (mod (ash 1 (* 1 nc 64)) modulus) nc)
          (field-sqrtfn fld)   (cond ((= 3 (logand modulus 3))
                                      (let ((exp (ash (1+ modulus) -2)))
                                        (lambda (x)
                                          (let* ((x  (to-scaled x))
                                                 (rt (f^ x exp)))
                                            (if (f= (fsqr rt) x)
                                                rt
                                              (error "Not a square residue"))))
                                        ))
                                     (t 'tonelli-shanks))
          )))

(defmethod make-field ((modulus integer))
  (make-instance 'field :modulus modulus))


(defclass element ()
  ((field  :accessor element-field  :initarg :field)
   (vec    :accessor element-vec    :initarg :vec)
   ))

(defclass scaled-element (element)
  ())

(defmethod scaling ((e scaled-element))
  :scaled)

(defclass unscaled-element (element)
  ())

(defmethod scaling ((e unscaled-element))
  :unscaled)

(defmethod make-element ((field field) (value integer) &optional su)
  (make-instance 'unscaled-element
                 :field field
                 :vec   (to-u64s (mod value (field-modulus field))
                                 (field-nchunks field))
                 ))

(defmethod make-element ((field field) (value vector) &optional (su :unscaled))
  (assert (= (length value) (field-nchunks field)))
  (make-instance (if (eq :unscaled su)
                     'unscaled-element
                   'scaled-element)
                 :field field
                 :vec   value))

(defmethod assert-same-field ((e1 element) (e2 element))
  (assert (eq (element-field e1) (element-field e2)) nil "Not same field"))

(defmethod basic-f+ ((e1 scaled-element) (e2 unscaled-element))
  (basic-f+ (to-unscaled e1) e2))

(defmethod basic-f+ ((e1 unscaled-element) (e2 scaled-element))
  (basic-f+ e1 (to-unscaled e2)))

(defmethod basic-f+ ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (modv (field-modv fld))
         (vres (copy-seq (element-vec e1)))
         (v2   (element-vec e2)))
    (add-no-carry vres v2 modv)
    (make-element fld vres (scaling e1))))

(defmethod basic-f+ ((e1 element) (e2 integer))
  (basic-f+ e1 (make-element (element-field e1) e2)))

(defmethod basic-f+ ((e1 integer) (e2 element))
  (basic-f+ (make-element (element-field e2) e1) e2))

(defun f+ (e1 &rest es)
  (let ((ans e1))
    (dolist (e2 es)
      (setf ans (basic-f+ ans e2)))
    ans))

;; -------------------------------------

(defmethod basic-f- ((e1 scaled-element) (e2 unscaled-element))
  (basic-f- (to-unscaled e1) e2))

(defmethod basic-f- ((e1 unscaled-element) (e2 scaled-element))
  (basic-f- e1 (to-unscaled e2)))

(defmethod basic-f- ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (modv (field-modv fld))
         (vres (copy-seq (element-vec e1)))
         (v2   (element-vec e2)))
    (sub-no-borrow vres v2 modv)
    (make-element fld vres (scaling e1))))

(defmethod basic-f- ((e1 element) (e2 integer))
  (basic-f- e1 (make-element (element-field e1) e2)))

(defmethod basic-f- ((e1 integer) (e2 element))
  (basic-f- (make-element (element-field e2) e1) e2))

(defmethod fneg ((e element))
  (basic-f- (make-element (element-field e) 0) e))

(defun f- (e1 &rest es)
  (if es
      (let ((ans e1))
        (dolist (e2 es)
          (setf ans (basic-f- ans e2)))
        ans)
    ;; else, simple negation
    (fneg e1)))

;; -------------------------------------

(defmethod f= ((e1 unscaled-element) (e2 scaled-element))
  (f= e1 (to-unscaled e2)))

(defmethod f= ((e1 scaled-element) (e2 unscaled-element))
  (f= (to-unscaled e1) e2))

(defmethod f= ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (v= (element-vec e1) (element-vec e2)))

(defmethod f= ((e1 element) (e2 integer))
  (f= e1 (make-element (element-field e1) e2)))

(defmethod f= ((e1 integer) (e2 element))
  (f= (make-element (element-field e2) e1) e2))

;; -------------------------------------

(defmethod basic-f* ((e1 element) (e2 element))
  (assert-same-field e1 e2)
  (let* ((fld  (element-field e1))
         (e1   (to-scaled e1))
         (e2   (to-scaled e2)))
    (make-element fld (montgy-mul (element-vec e1)
                                  (element-vec e2)
                                  (field-modv fld)
                                  (field-minv fld))
                  :scaled)))

(defmethod basic-f* ((e1 element) (e2 integer))
  (basic-f* e1 (make-element (element-field e1) e2)))

(defmethod basic-f* ((e1 integer) (e2 element))
  (basic-f* (make-element (element-field e2) e1) e2))

(defun f* (e1 &rest es)
  (let ((ans e1))
    (dolist (e2 es)
      (setf ans (basic-f* ans (to-scaled e2))))
    ans))

(defmethod fsqr ((e element))
  (let ((e (to-scaled e)))
    (basic-f* e e)))

;; -------------------------------------
;; scaled / unscaled.
;;
;; Multiplication, exponentiation, division, and reciprocal require
;; scaled arguments.
;;
;; Simple addition, subtraction, and equality comparison, works well
;; for arguments both scaled, or both unscaled. When presented with
;; mixed scaling, we should prefer unscaled since it requires only a
;; Montgomery reduction.

(defmethod to-scaled ((e scaled-element))
  e)

(defmethod to-scaled ((e unscaled-element))
  (let ((fld  (element-field e)))
    (make-element fld
                  (montgy-mul (element-vec e)
                              (field-msqrv fld)
                              (field-modv fld)
                              (field-minv fld))
                  :scaled)))

(defmethod to-unscaled ((e unscaled-element))
  e)

(defmethod to-unscaled ((e scaled-element))
  (let ((fld  (element-field e)))
    (make-element fld
                  (montgy-reduce (element-vec e)
                                 (field-modv fld)
                                 (field-minv fld))
                  :unscaled)))
  
(defmethod int ((e element))
  (let ((e (to-unscaled e)))
    (from-u64s (element-vec e))))

;; -------------------------------------

(defmethod finv ((e element))
  (let* ((fld  (element-field e))
         (modv (field-modv fld))
         (e    (to-scaled e))
         (v    (element-vec e)))
    (make-element fld
                  (montgy-mul (inv-mod v modv)
                              (field-mcubev fld)
                              modv
                              (field-minv fld))
                  :scaled)))

(defmethod basic-f/ ((e1 element) (e2 element))
  (basic-f* e1 (finv e2)))

(defmethod basic-f/ ((e1 element) (e2 integer))
  (basic-f* e1 (finv (make-element (element-field e1) e2))))

(defmethod basic-f/ ((e1 integer) (e2 element))
  (basic-f* (make-element (element-field e2) e1) (finv e2)))

(defmethod f/ (e1 &rest es)
  (if es
      (let ((ans (to-scaled e1)))
        (dolist (e2 es)
          (setf ans (basic-f/ ans e2)))
        ans)
    ;; else - simple inverse
    (finv e1)))

;; -----------------------------------------------------
;; Precomputed powers cache

(defclass window-cache ()
  ((precv  :reader   window-cache-precv ;; precomp x^n cache
           :initarg  :precv)
   (x^1    :reader   window-cache-x^1
           :initarg  :x^1)
   (op-sqr :reader   window-cache-op-sqr
           :initarg  :op-sqr)
   (op-mul :reader   window-cache-op-mul
           :initarg  :op-mul)))

(defun make-window-cache (&key nbits x^1 op-mul op-sqr)
  (let ((precv (make-array (ash 1 nbits) :initial-element nil)))
    (setf (aref precv 1) x^1) ;; slot 0 never accessed
    (make-instance 'window-cache
                   :precv  precv
                   :x^1    x^1
                   :op-sqr op-sqr
                   :op-mul op-mul)))

(defmethod get-prec ((wc window-cache) (ix integer))
  ;; compute powers of x^n, n = 1..(2^nbits-1) on demand
  (declare (fixnum ix))
  (with-accessors ((precv  window-cache-precv)
                   (x^1    window-cache-x^1)
                   (op-mul window-cache-op-mul)
                   (op-sqr window-cache-op-sqr)) wc
    (or (aref precv ix)
        (setf (aref precv ix)
              (if (oddp ix)
                  (funcall op-mul x^1 (get-prec wc (1- ix)))
                (funcall op-sqr (get-prec wc (ash ix -1))))
              ))))

;; -----------------------------------------------------
;; Generalized fixed-window exponentiation algorithm
;;
;; Used by both modular exponentiation, and Cipolla algorithm for
;; modular square roots.

(defmethod generalized-windowed-exponentiation (x n &key window-nbits op-mul op-sqr)
  ;; modular x^n using fixed-width window algorithm
  (let* ((ans   nil)
         (wc    (make-window-cache
                 :nbits  window-nbits
                 :x^1    x
                 :op-mul op-mul
                 :op-sqr op-sqr))
         (nbits (integer-length n)))
    (declare (fixnum nbits))
    (loop for pos fixnum from (* window-nbits (floor nbits window-nbits)) downto 0 by window-nbits do
          (when ans
            (loop repeat window-nbits do
                  (setf ans (funcall op-sqr ans))))
          (let ((bits (ldb (byte window-nbits pos) n)))
            (declare (fixnum bits))
            (unless (zerop bits)
              (let ((y (get-prec wc bits)))
                (setf ans (if ans
                              (funcall op-mul ans y)
                            y)))
              )))
    ans))
    
;; -----------------------------------------------------
;; Prime-Field Arithmetic

(defmethod f^ ((base element) (exp integer))
  ;; base^exponent mod modulus, for any modulus
  ;; use a 4-bit fixed window algorithm
  (generalized-windowed-exponentiation (to-scaled base) exp
                                       :window-nbits 4
                                       :op-mul       'basic-f*
                                       :op-sqr       'fsqr))

(defmethod fchi ((e element))
  ;; fchi(x) -> {-1,0,+1}
  ;; = +1 when x is square residue
  ;; =  0 when x = 0
  ;; = -1 when x is non-square
  (let ((exp (ash (1- (field-modulus (element-field e))) -1)))
    (int (f^ e exp))))

(defmethod field-quadratic-residue-p ((e element))
  (= 1 (fchi e)))

;; -----------------------------------------------------------

(defmethod get-tonelli-shanks-params ((f field))
  (or (field-tonelli-shanks-params f)
      (setf (field-tonelli-shanks-params f)
            (multiple-value-bind (q s)
                (um:nlet iter ((q  (1- (field-modulus f)))
                                    (s  0))
                  (declare (integer q)
                           (fixnum s))
                  (if (oddp q)
                      (values q s)
                    (go-iter (ash q -1)
                             (1+ s))))
              (declare (integer q)
                       (fixnum s))
              (let* ((one (to-scaled (make-element f 1)))
                     (z (um:nlet iter ((x (to-scaled (make-element f 2))))
                         ;; on average, about 2 iters
                         (if (field-quadratic-residue-p x)
                             (go-iter (f+ x one))
                           x))))
                (list q s z))))))

(defmethod tonelli-shanks ((e element))
  "Tonelli-Shanks algorithm for Sqrt in prime field"
  (let* ((e    (to-scaled e))
         (fld  (element-field e))
         (zero (to-scaled (make-element fld 0)))
         (one  (to-scaled (make-element fld 1))))
    (destructuring-bind (q s z) (get-tonelli-shanks-params (element-field e))
      (declare (integer q)
               (fixnum  s))
      (um:nlet iter ((m  s)
                     (c  (f^ z q))
                     (tt (f^ e q))
                     (r  (f^ e (ash (1+ q) -1))))
        (declare (fixnum m))
        (cond ((f= tt zero)  zero)
              ((f= tt one)   r)
              (t
               (let* ((i  (um:nlet iteri ((i  1)
                                          (x  (fsqr tt)))
                            (declare (fixnum i))
                            (cond ((= i m)     (error "Not a quadratic residue"))
                                  ((f= x one)  i)
                                  (t           (go-iteri (1+ i) (fsqr x)))
                                  )))
                      (b  (f^ c (ash 1 (- m i 1))))
                      (new-m  i)
                      (new-c  (fsqr b))
                      (new-tt (f* tt new-c))
                      (new-r  (f* r b)))
                 (declare (fixnum i new-m))
                 (go-iter new-m new-c new-tt new-r)))
              ))
      )))

(defmethod fsqrt ((e element))
  (funcall (field-sqrtfn (element-field e)) e))

;; ----------------------------------------------------------
#|
(let* ((n 10000)
       (fld (make-field *ed-r*))
       (asn (loop repeat n collect (random-between 1 *ed-r*)))
       (bsn (loop repeat n collect (random-between 1 *ed-r*)))
       (maker (um:curry 'make-element fld))
       (as  (map 'vector maker asn))
       (bs  (map 'vector maker bsn)))
  (time (map nil 'basic-f* as bs))
  (time (map nil (lambda (a b)
                   (mod (* a b) *ed-r*))
             asn bsn))
  (map nil (lambda (a b)
             (let ((p1 (int (f* (funcall maker a) (funcall maker b))))
                   (p2 (mod (* a b) *ed-r*)))
               (unless (= p1 p2)
                 (error "f* = ~A, m* = ~A" p1 p2))))
       asn bsn))

;; -------------------------------

(let* ((x #x0ffffffff)
       (y (sys:int64-to-integer
           (sys:int64* (sys:integer-to-int64 x)
                       (sys:integer-to-int64 x)))))
  (print y))
|#

;; edwards.lisp -- Edwards Curves
;; DM/RAL 07/15
;; -----------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :edwards-ecc)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------------------
;; Debug Instrumentation
#|
(defvar *watcher*
  (ac:make-actor
   (let ((counts (make-hash-table)))
     (lambda (&rest msg)
       (um:dcase msg
         (:reset ()
          (clrhash counts))
         (:read ()
          (um:accum acc
            (maphash (lambda (k v)
                       (acc (list k v)))
                     counts)))
         (:tally (kwsym)
          (let ((ct (gethash kwsym counts 0)))
            (setf (gethash kwsym counts) (1+ ct))))
         )))))

(defun clear-counters ()
  (ac:send *watcher* :reset))

(defun read-counters ()
  (ac:ask *watcher* :read))

(defun tally (kwsym)
  (ac:send *watcher* :tally kwsym))
|#
;; ----------------------------------------------------------------

(defun ed-neutral-point ()
  (get-cached-symbol-data '*edcurve*
                          :ed-neutral-point *ed-c*
                          (lambda ()
                            (make-ecc-pt
                             :x 0
                             :y *ed-c*))))

(defun ed-neutral-point-p (pt)
  (ed-pt= pt (ed-neutral-point)))

;; -------------------------------------------------------------------

#-:WINDOWS
(defun have-fast-impl ()
  (fast-ed-curve-p *edcurve*))

#+:WINDOWS
(defmacro have-fast-impl ()
  nil)

(defstub _Curve1174-affine-mul)
(defstub _Curve1174-projective-mul)
(defstub _Curve1174-projective-add)
(defstub _Curve1174-to-affine)

(defstub _Ed3363-affine-mul)
(defstub _Ed3363-projective-mul)
(defstub _Ed3363-projective-add)
(defstub _Ed3363-to-affine)

(defstub %ecc-fast-mul)
(defstub %ecc-fast-add)
(defstub %ecc-fast-to-affine)

;; -------------------------------------------------------------------

(defmacro modq (&body body)
  `(with-mod *ed-q* ,@body))

(defmacro modr (&body body)
  `(with-mod *ed-r* ,@body))

;; -------------------------------------------------------------------

(defmethod ed-affine ((pt ecc-pt))
  pt)

(defmethod ed-affine ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    (cond ((= 1 z)
           (make-ecc-pt
            :x  x
            :y  y))
          ((have-fast-impl)
           (%ecc-fast-to-affine pt))
          (t
           (with-mod *ed-q*
             (make-ecc-pt
              :x  (m/ x z)
              :y  (m/ y z))))
          )))

(defmethod ed-affine ((pt ecc-cmpr-pt))
  (ed-affine (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-projective ((pt ecc-proj-pt))
  pt)

(defmethod ed-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    (make-ecc-proj-pt
     :x x
     :y y
     :z 1)))
  
#|
(defmethod ed-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y))
             (alpha (field-random *ed-q*)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z alpha)
      )))
|#

(defmethod ed-projective ((pt ecc-cmpr-pt))
  (ed-projective (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-random-projective ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y))
             (alpha (safe-field-random *ed-q*))
             (declare (integer alpha)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z alpha)
      )))

(defmethod ed-random-projective ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z))
             (alpha (safe-field-random *ed-q*))
             (declare (integer alpha)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x (m* alpha x)
       :y (m* alpha y)
       :z (m* alpha z))
      )))

(defmethod ed-random-projective ((pt ecc-cmpr-pt))
  (ed-random-projective (ed-decompress-pt pt)))

;; -------------------------------------------------------------------

(defmethod ed-pt= ((pt1 ecc-pt) pt2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
             (declare (integer x1 y1))
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) (ed-affine pt2))
             (declare (integer x2 y2)))
    (with-mod *ed-q*
      (and (= x1 x2)
           (= y1 y2))
      )))

#|
(defmethod ed-pt= ((pt1 ecc-proj-pt) (pt2 ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y) (z1 z)) pt1)
             (declare (integer x1 y1 z1))
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
             (declare (integer x2 y2)))
    (with-mod *ed-q*
      (and (= (m* z1 x2) x1)
           (= (m* z1 y2) y1))
    )))
|#

(defmethod ed-pt= ((pt1 ecc-proj-pt) pt2)
  (um:bind* ((:struct-accessors ecc-proj-pt ((x1 x) (y1 y) (z1 z)) pt1)
             (declare (integer x1 y1 z1))
             (:struct-accessors ecc-proj-pt ((x2 x) (y2 y) (z2 z)) (ed-projective pt2))
             (declare (integer x2 y2 z2)))
    (with-mod *ed-q*
      (and (= (m* z1 x2) (m* z2 x1))
           (= (m* z1 y2) (m* z2 y1)))
      )))

(defmethod ed-pt= ((pt1 ecc-cmpr-pt) (pt2 ecc-cmpr-pt))
  (= (ecc-cmpr-pt-cx pt1)
     (ecc-cmpr-pt-cx pt2)))

(defmethod ed-pt= ((pt1 ecc-cmpr-pt) pt2)
  (ed-pt= (ed-affine pt1) pt2))

;; -------------------------------------------------------------------

(defmethod ed-satisfies-curve ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
    (with-mod *ed-q*
      (let ((xx (m* x x))
            (yy (m* y y)))
        (= (m+ xx yy)
           (m* *ed-c* *ed-c*
               (m+ 1 (m* *ed-d* xx yy))))
        ))))
  
(defmethod ed-satisfies-curve ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    ;; z^2*(x^2 + y^2) = c^2*(z^4 + d*x^2*y^2)
    (with-mod *ed-q*
      (let ((xx (m* x x))
            (yy (m* y y))
            (zz (m* z z)))
        (= (m* zz (m+ xx yy))
           (m* *ed-c* *ed-c*
               (+ (m* zz zz)
                  (m* *ed-d* xx yy))))
        ))))

(defmethod ed-satisfies-curve ((pt ecc-cmpr-pt))
  (ed-satisfies-curve (ed-projective pt)))

;; -------------------------------------------------------------------

(defun ed-affine-add (pt1 pt2)
  ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
               (declare (integer x1 y1))
               (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
               (declare (integer x2 y2))
               (y1y2  (m* y1 y2))
               (declare (integer y1y2))
               (x1x2  (m* x1 x2))
               (declare (integer x1x2))
               (x1x2y1y2 (m* *ed-d* x1x2 y1y2))
               (declare (integer x1x2y1y2))
               (denx  (m* *ed-c* (1+ x1x2y1y2)))
               (declare (integer denx))
               (deny  (m* *ed-c* (- 1 x1x2y1y2)))
               (declare (integer deny))
               (numx  (+ (m* x1 y2)
                         (m* y1 x2)))
               (declare (integer numx))
               (numy  (- y1y2 x1x2))
               (declare (integer numy)))
      (make-ecc-pt
       :x  (m/ numx denx)
       :y  (m/ numy deny))
      )))

(defun ed-projective-add (pt1 pt2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-proj-pt ((x1 x)
                                               (y1 y)
                                               (z1 z)) pt1)
               (declare (integer x1 y1 z1))
               (:struct-accessors ecc-proj-pt ((x2 x)
                                               (y2 y)
                                               (z2 z)) pt2)
               (declare (integer x2 y2 z2))
               (a  (m* z1 z2))
               (declare (integer a))
               (b  (m* a a))
               (declare (integer b))
               (c  (m* x1 x2))
               (declare (integer c))
               (d  (m* y1 y2))
               (declare (integer d))
               (e  (m* *ed-d* c d))
               (declare (integer e))
               (f  (- b e))
               (declare (integer f))
               (g  (+ b e))
               (declare (integer g))
               (x3 (m* a f (- (m* (+ x1 y1)
                                  (+ x2 y2))
                               c d)))
               (y3 (m* a g (- d c)))
               (z3 (m* *ed-c* f g)))
      (make-ecc-proj-pt
       :x  x3
       :y  y3
       :z  z3)
      )))

(defun ed-add (pt1 pt2)
  ;; contageon to randomized projective coords for added security
  ;; (reset-blinders)
  ;; (tally :ecadd)
  (let ((ppt1 (ed-projective pt1))  ;; projective add is so much faster than affine add
        (ppt2 (ed-projective pt2))) ;; so it pays to make the conversion
    (cond
     ((have-fast-impl)
      (%ecc-fast-add ppt1 ppt2))

     (t 
      ;; since projective add takes about 6 usec, and affine add takes
      ;; about 40 usec, it pays to always convert to projective coords,
      ;; especially since it is so cheap to do so.
      (ed-projective-add ppt1 ppt2))
     )))

;; ----------------------------------------------------------------

(defmethod ed-negate ((pt ecc-pt))
  (um:bind* ((:struct-accessors ecc-pt ((x x) (y y)) pt)
             (declare (integer x y)))
    (with-mod *ed-q*
      (make-ecc-pt
       :x  (m- x)
       :y  y)
      )))

(defmethod ed-negate ((pt ecc-proj-pt))
  (um:bind* ((:struct-accessors ecc-proj-pt ((x x) (y y) (z z)) pt)
             (declare (integer x y z)))
    (with-mod *ed-q*
      (make-ecc-proj-pt
       :x  (m- x)
       :y  y
       :z  z)
      )))

(defmethod ed-negate ((pt ecc-cmpr-pt))
  (ed-negate (ed-projective pt)))

(defun ed-sub (pt1 pt2)
  (ed-add pt1 (ed-negate pt2)))

;; ----------------------------------------------------------------
;; NAF multiplication, 4 bits at a time...
#|
(defun naf4 (k)
  (declare (integer k))
  (labels ((mods (x)
             (declare (integer x))
             (let ((xm (ldb (byte 4 0) x)))
               (declare (fixnum xm))
               (if (>= xm 8)
                   (- xm 16)
                 xm))))
    (um:nlet iter ((k   k)
                   (ans nil))
      (declare (integer k)
               (list ans))
      (if (zerop k)
          ans
        (if (oddp k)
            (let ((di (mods k)))
              (declare (fixnum di))
              (go-iter (ash (- k di) -4) (cons di ans)))
          ;; else
          (go-iter (ash k -1) (cons 0 ans)))
        ))))
        
(defun ed-basic-mul (pt n)
  (declare (integer n))
  (cond ((zerop n) (ed-projective
                    (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt)) pt)
        
        (t (let ((precomp
                  (let* ((r0   (ed-projective pt))
                         (r0x2 (ed-add r0 r0)))
                    (loop for ix fixnum from 1 below 8 by 2
                          for r1 = r0 then (ed-add r1 r0x2)
                          collect (cons ix r1)
                          collect (cons (- ix) (ed-negate r1)))
                    )))
             
             (um:nlet iter ((nns  (naf4 n))
                                 (qans nil))
               (if (endp nns)
                   (or qans (ed-neutral-point))
                 (let ((qsum  (and qans (ed-add qans qans)))
                       (nnhd  (car nns)))
                   (declare (fixnum nnhd))
                   (unless (zerop nnhd)
                     (let ((kpt (cdr (assoc nnhd precomp))))
                       (if qsum
                           (setf qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum qsum)
                                 qsum (ed-add qsum kpt))
                         ;; else
                         (setf qsum kpt))))
                   (go-iter (cdr nns) qsum))))
             ))
        ))
|#

#|
(let ((prf (range-proofs:make-range-proof 1234567890)))
  (time   
   (loop repeat 100 do
       (range-proofs:validate-range-proof prf))))

(time
 (loop repeat 100 do
       (range-proofs:validate-range-proof
        (range-proofs:make-range-proof 1234567890))))
|#
;; -------------------------------------------------------------------
;; 4-bit fixed window method - decent performance, and never more than
;; |r|/4 terms

(defun nibbles (n)
  ;; for debug display to compare with windows4 output
  (let* ((nbits (integer-length n))
         (limt  (* 4 (floor nbits 4))))
    (loop for pos fixnum from limt downto 0 by 4 collect
          (ldb (byte 4 pos) n))))
  
(defun windows (n window-nbits)
  ;; return a big-endian list of balanced bipolar window values for
  ;; each window-nbits nibble in the number. E.g., for window-nbits = 4,
  ;; (windows 123 4) -> (1 -8 -5),
  ;; where each values is in the set (-8, -7, ..., 7)
  (declare (integer n)
           (fixnum window-nbits))
  (let* ((nbits (integer-length n))
         (limt  (* window-nbits (floor nbits window-nbits)))
         (2^wn  (ash 1 window-nbits))
         (wnlim (1- (ash 2^wn -1))))
    (declare (fixnum nbits limt 2^wn wnlim))
    (um:nlet iter ((pos 0)
                   (ans nil)
                   (cy  0))
      (declare (fixnum pos cy))
      (let* ((byt (ldb (byte window-nbits pos) n))
             (x   (+ byt cy)))
        (declare (fixnum byt x))
        (multiple-value-bind (nxt nxtcy)
            (if (> x wnlim)
                (values (- x 2^wn) 1)
              (values x 0))
          (if (< pos limt)
              (go-iter (+ pos window-nbits) (cons nxt ans) nxtcy)
            (list* nxtcy nxt ans)))
        ))))

(defun windows-to-int (wins window-nbits)
  ;; for debugging...
  (let ((ans 0))
    (loop for w in wins do
          (setf ans (+ w (ash ans window-nbits))))
    ans))

(defun ed-projective-double (pt)
  (ed-projective-add pt pt))

(defclass bipolar-window-cache ()
  ((precv  :reader   bipolar-window-cache-precv
           :initarg  :precv)
   (offs   :reader   bipolar-window-cache-offs
           :initarg  :offs)
   (pt*1   :reader   bipolar-window-cache-pt*1
           :initarg  :pt*1)
   (pt*m1  :reader   bipolar-window-cache-pt*m1
           :initarg  :pt*m1)))

(defmethod make-bipolar-window-cache (&key nbits pt)
  (declare (fixnum nbits))
  (let* ((nel    (ash 1 nbits))
         (precv  (make-array nel :initial-element nil))
         (offs   (ash nel -1))
         (pt*1   (ed-projective pt))
         (pt*m1  (ed-negate pt*1)))
    (declare (fixnum nel offs))
    (setf (aref precv (1+ offs)) pt*1      ;; slot ix = 0 never referenced
          (aref precv (1- offs)) pt*m1)
    (make-instance 'bipolar-window-cache
                   :precv  precv
                   :offs   offs
                   :pt*1   pt*1
                   :pt*m1  pt*m1)))

(defmethod get-prec ((wc bipolar-window-cache) (ix integer))
  ;; get cached pt*n, for n = -2^wn, -2^wn+1, ...,-1, 0, 1, ... 2^wn-2, 2^wn-1
  ;; each cached entry computed on demand if necessary
  (declare (fixnum ix))
  (with-accessors ((precv  bipolar-window-cache-precv)
                   (offs   bipolar-window-cache-offs)
                   (pt*1   bipolar-window-cache-pt*1)
                   (pt*m1  bipolar-window-cache-pt*m1)) wc
    (let ((jx (+ ix offs)))
      (declare (fixnum jx))
      (or (aref precv jx)
          (setf (aref precv jx)
                (if (oddp ix)
                    (if (minusp ix)
                        (ed-projective-add pt*m1 (get-prec wc (1+ ix)))
                      (ed-projective-add pt*1 (get-prec wc (1- ix))))
                  ;; else - ix even
                  (ed-projective-double (get-prec wc (ash ix -1))))
                )))))

(defmethod generalized-bipolar-windowed-mul (pt n &key window-nbits)
  ;; ECC point-scalar multiplication using fixed-width bipolar window
  ;; algorithm
  (declare (fixnum window-nbits)
           (integer n))
  (let* ((ws  (windows n window-nbits))
         (wc  (make-bipolar-window-cache
               :nbits window-nbits
               :pt    pt))  ;; affine or projective in...
         (ans nil))
    (loop for w fixnum in ws do
          (when ans
            (loop repeat window-nbits do
                  (setf ans (ed-projective-double ans))))
          (unless (zerop w)
            (let ((pw  (get-prec wc w)))
              (setf ans (if ans
                            (ed-projective-add pw ans)
                          pw)))
            ))
    (or ans  ;; projective out...
        (ed-neutral-point))))

(defmethod generalized-bipolar-windowed-mul ((pt ecc-cmpr-pt) n &key window-nbits)
  (generalized-bipolar-windowed-mul (ed-projective pt) n :window-nbits window-nbits))

;; --------------------------------------------------------------------------------
#|
;; 1-bit NAF form
(defun naf (k)
  (declare (integer k))
  ;; non-adjacent form encoding of integers
  (um:nlet iter ((k k)
                 (ans nil))
    (declare (integer k))
    (if (plusp k)
        (let ((kj (if (oddp k)
                      (- 2 (mod k 4))
                    0)))
          (declare (integer kj))
          (go-iter (ash (- k kj) -1) (cons kj ans)))
      ans)))

(defun ed-basic-mul (pt n)
  ;; this is about 50% faster than not using NAF
  (cond ((zerop n)  (ed-projective
                     (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt))  pt)
        
        (t  (let* ((r0  (ed-random-projective pt)) ;; randomize point
                   (r0n (ed-negate r0))
                   (nns (naf n))
                   (v   r0))
              (loop for nn in (cdr nns) do
                    (setf v (ed-add v v))
                    (case nn
                      (-1  (setf v (ed-add v r0n)))
                      ( 1  (setf v (ed-add v r0)))
                      ))
              v))
        ))
|#

;; --------------------------------------------------------------------------------

(defun ed-mul (pt n)
  #|
  (let* ((alpha  (* *ed-r* *ed-h* (field-random #.(ash 1 48)))))
    (ed-basic-mul pt (+ n alpha)))
  |#
  (declare (integer n))
  ;; (tally :ecmul)
  (let ((nn  (mod n *ed-r*)))
    (declare (integer nn))
    
    (cond ((zerop nn)
           (ed-neutral-point))
          
          ((or (= nn 1)
               (ed-neutral-point-p pt))
           pt)

          ((have-fast-impl)
           (%ecc-fast-mul pt nn))
          
          (t
           (generalized-bipolar-windowed-mul pt nn
                                    :window-nbits 4))
          )))

(defun ed-div (pt n)
  (with-mod *ed-r*
    (ed-mul pt (m/ n))))

(defun ed-nth-proj-pt (n)
  (ed-mul *ed-gen* n))

(defun ed-nth-pt (n)
  (ed-affine (ed-nth-proj-pt n)))

;; ---------------------------------------------------------------
;; conversion between integers and little-endian UB8 vectors

(defun ed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbits *edcurve*
                          (lambda ()
                            (integer-length *ed-q*))))

(defun ed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-nbits) 8))))

;; ----------------------------------------

(defun ed-compressed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbits *edcurve*
                          (lambda ()
                            (1+ (ed-nbits)))))

(defun ed-compressed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-compressed-nbits) 8))))

(defun ed-cmpr/h-sf ()
  (get-cached-symbol-data '*edcurve*
                          :ed-cmpr/h-sf *edcurve*
                          (lambda ()
                            (with-mod *ed-r*
                              (m/ *ed-h*)))))

(defun ed-decmpr*h-fn ()
  (get-cached-symbol-data '*edcurve*
                          :ed-decmpr*h-fn *edcurve*
                          (lambda ()
                            (case *ed-h*
                              (1  'identity)
                              (2  (lambda (pt)
                                    (ed-add pt pt)))
                              (4  (lambda (pt)
                                    (let ((pt2 (ed-add pt pt)))
                                      (ed-add pt2 pt2))))
                              (8  (lambda (pt)
                                    (let* ((pt2 (ed-add pt pt))
                                           (pt4 (ed-add pt2 pt2)))
                                      (ed-add pt4 pt4))))
                              (t (error "No decompression function"))
                              ))))

;; -------------------

(defmethod ed-compress-pt ((pt ecc-cmpr-pt) &key enc)
  (if enc
      (ed-compress-pt (ed-projective pt) :enc enc)
    pt))

(defmethod ed-compress-pt (pt &key enc) ;; :bev, :lev, :base58 or nil
  ;;
  ;; Standard encoding for EdDSA is X in little-endian notation, with
  ;; Odd(Y) encoded as MSB beyond X.
  ;;
  ;; If lev is true, then a little-endian UB8 vector is produced,
  ;; else an integer value.
  ;;
  (um:bind* ((cmpr/h  (ed-cmpr/h-sf))
             (:struct-accessors ecc-pt (x y)
              (ed-affine (ed-mul pt cmpr/h))))
    (let ((val  (dpb (ldb (byte 1 0) y)
                     (byte 1 (ed-nbits)) x)))
      (ecase enc
        ((nil)    (make-ecc-cmpr-pt :cx val))
        (:bev     (bevn val (ed-compressed-nbytes)))
        (:lev     (levn val (ed-compressed-nbytes)))
        (:base58  (base58 (bevn val (ed-compressed-nbytes))))
        ))))

;; --------------------------

(defmethod ed-decompress-pt ((x ecc-pt))
  x)

(defmethod ed-decompress-pt ((x ecc-proj-pt))
  x)

(defmethod ed-decompress-pt ((x ecc-cmpr-pt))
  (ed-decompress-pt (ecc-cmpr-pt-cx x)))

(defmethod ed-decompress-pt (x)
  (ed-decompress-pt (int x)))

(defmethod ed-decompress-pt ((v integer))
  (with-mod *ed-q*
    (let* ((decmpr-fn (ed-decmpr*h-fn))
           (nbits (ed-nbits))
           (sign  (ldb (byte 1 nbits) v))
           (x     (ldb (byte nbits 0) v))
           (y     (ed-solve-y x)))
      (unless (= sign (ldb (byte 1 0) y))
        (setf y (m- y)))
      (funcall decmpr-fn
               (make-ecc-proj-pt
                :x  x
                :y  y
                :z  1))
      )))

(defun ed-solve-y (x)
  (with-mod *ed-q*
    (msqrt (m/ (m* (+ *ed-c* x)
                   (- *ed-c* x))
               (- 1 (m* x x *ed-c* *ed-c* *ed-d*))))))

;; -----------------------------------------------------------------

(defgeneric ed-valid-point-p (pt)
  (:method ((pt ecc-pt))
   (and (ed-satisfies-curve pt)
        (not (or (zerop (ecc-pt-x pt))
                 (zerop (ecc-pt-y pt))))
        pt))
  (:method ((pt ecc-proj-pt))
   (ed-valid-point-p (ed-affine pt)))
  (:method ((pt ecc-cmpr-pt))
   (ed-valid-point-p (ed-affine pt)))
  (:method ((pt integer))
   ;; assume we have integer repr of compressed pt
   ;; -- some integer values cannot be decompressed into ECC pts.
   (ignore-errors
     (ed-valid-point-p (ed-decompress-pt pt))))
  (:method ((pt vector))
   ;; vector should be a BVEC for an integer representing a compressed
   ;; point
   (ignore-errors
     (ed-valid-point-p (int pt)))) )

(defun ed-validate-point (pt)
  (let ((vpt (ed-valid-point-p pt)))
    (assert vpt)
    vpt))

#|
(loop repeat 10000 do
      (let* ((pt (ed-random-generator)))
        (ed-validate-point pt)))

(loop repeat 10000 do
      (let* ((n (random-between 1 *ed-r*))
             (pt (ed-nth-pt n)))
        (ed-validate-point pt)
        (ed-validate-point (ed-mul pt (- n)))))
 |#
;; -----------------------------------------------------------------

#||#
(defmethod hashable ((x ecc-proj-pt))
  (hashable (ed-compress-pt x :enc :lev)))

(defmethod hashable ((x ecc-pt))
  (hashable (ed-projective x)))

(defmethod hashable ((x ecc-cmpr-pt))
  (hashable (ed-projective x)))
#||#

;; -----------------------------------------------------------

(defun hash-to-grp-range (&rest args)
  (apply 'hash-to-range *ed-r* args))

(defun hash-to-pt-range (&rest args)
  (apply 'hash-to-range *ed-q* args))

;; -------------------------------------------------

(defun compute-deterministic-skey (seed &optional (index 0))
  (multiple-value-bind (_ hval)
      (hash-to-grp-range index seed)
    (declare (ignore _))
    hval))

(defun make-deterministic-keys (&rest seeds)
  (multiple-value-bind (_ skey)
      (apply #'hash-to-grp-range seeds)
    (declare (ignore _))
    (values skey (ed-nth-pt skey))))

(defun ed-random-pair ()
  "Select a random private and public key from the curve"
  (make-deterministic-keys :edpt (ctr-drbg 256)))

;; -----------------------------------------------------
;; Hashing onto curve

(defun ed-pt-from-hash (hintval)
  "Hash onto curve. Treat h as X coord, just like a compressed point.
Then if Y is a quadratic residue we are done.
Else re-probe with (X^2 + 1)."
  (let ((cof-fn  (ed-decmpr*h-fn)))
    (with-mod *ed-q*
      (um:nlet iter ((x  hintval))
        (or
         (um:when-let (y (ignore-errors (ed-solve-y x)))
           (let ((pt (funcall cof-fn
                              (make-ecc-pt
                               :x x
                               :y y))))
             ;; Watch out! This multiply by cofactor is necessary
             ;; to prevent winding up in a small subgroup.
             ;;
             ;; we already know the point sits on the curve, but
             ;; it could now be the neutral point if initial
             ;; (x,y) coords were in a small subgroup.
             (and (not (ed-neutral-point-p pt))
                  pt)))
         ;; else - invalid point, so re-probe at x^2+1
         (go-iter (m+ 1 (m* x x)))
         )))))

(defun ed-pt-from-seed (&rest seeds)
  (multiple-value-bind (_ hval)
      (apply 'hash-to-pt-range seeds)
    (declare (ignore _))
    (ed-pt-from-hash hval)))

(defun ed-random-generator (&rest seeds)
  (apply 'ed-pt-from-seed
         (uuid:make-v1-uuid)
         (ctr-drbg (integer-length *ed-q*))
         seeds))

;; ---------------------------------------------------
;; The IETF EdDSA standard as a primitive
;;
;; From IETF specifications
;; (we tend toward BEV values everywhere, but IETF dictates LEV)

(defun compute-schnorr-deterministic-random (msgv k-priv)
  (um:nlet iter ((ix 0))
    ;; randomness r from hash to group range of (CTR | SKEY | MSG)
    ;; to make deterministic to avoid PlayStation attacks, yet
    ;; random because of hash - nothing up my sleeve...
    (let ((r   (int (hash-to-grp-range
                     (levn ix 4)
                     (levn k-priv (integer-length (1- *ed-q*))) ;; we want *ed-q* here to avoid truncating skey
                     msgv))))
      (if (plusp r)
          (values r (ed-nth-pt r) ix)
        (go-iter (1+ ix)))
      )))

(defun ed-dsa (msg skey)
  (let* ((msg-enc   (loenc:encode msg))
         (pkey      (ed-nth-pt skey))
         (pkey-cmpr (ed-compress-pt pkey)))
    ;; r = the random challenge value for Fiat-Shamir sigma proof
    ;; - deterministic, to avoid attacks, yet unpredictable via hash
    ;; - a "nothing up my sleeve" proof value
    (multiple-value-bind (r rpt)
        (compute-schnorr-deterministic-random msg-enc skey)
      (let* ((rpt-cmpr  (ed-compress-pt rpt))
             (nbcmpr    (ed-compressed-nbytes))
             (s         (with-mod *ed-r*
                          (m+ r
                              (m* skey
                                  (int
                                   (hash-to-grp-range
                                    (levn rpt-cmpr  nbcmpr)
                                    (levn pkey-cmpr nbcmpr)
                                    msg-enc))
                                  )))))
        (list
         :msg   msg
         :pkey  pkey-cmpr
         :r     rpt-cmpr
         :s     s)
        ))))

(defun ed-dsa-validate (msg pkey r s)
  ;; pkey should be presented in compressed pt form
  ;; r is likewise a compressed pt
  ;; s is a group scalar
  (let ((nbcmpr (ed-compressed-nbytes)))
    (ed-pt=
     (ed-mul (ed-nth-pt s) *ed-h*)
     (ed-add (ed-mul (ed-decompress-pt r) *ed-h*)
             (ed-mul
              (ed-mul (ed-decompress-pt pkey)
                      (int
                       (hash-to-grp-range
                        (levn r    nbcmpr)
                        (levn pkey nbcmpr)
                        (loenc:encode msg))))
              *ed-h*))
     )))

;; -----------------------------------------------------------
;; VRF on Elliptic Curves
;;
;; Unlike the situation with pairing curves and BLS signatures, we
;; must use a Schnorr-like scheme, with a deterministic
;; nonce-protected random challenge value, to guard the secret key.
;;
;; If you ever happened to use the same random challenge value on a
;; different message seed then it becomes possible to compute the
;; secret key.
;;
;; Adapted from Appendix A of paper:
;;  "CONIKS: Bringing Key Transparency to End Users"
;;   by Melara, Blankstein, Bonneau, Felten, and Freedman

(defun ed-vrf (seed skey)
    (let* ((h    (ed-pt-from-seed seed))
           (vrf  (ed-mul h skey)))
      (ed-compress-pt vrf)))
           

(defun ed-prove-vrf (seed skey)
    (let* ((h    (ed-pt-from-seed seed))
           (vrf  (ed-compress-pt (ed-mul h skey)))
           
           ;; r = the random challenge value for Fiat-Shamir sigma proof
           ;; - deterministic, to avoid attacks, yet unpredictable via hash
           ;; - a "nothing up my sleeve" proof value
           (r    (compute-schnorr-deterministic-random seed skey))
           ;; s = H(g, h, P, v, g^r, h^r)
           (s    (int
                  (hash-to-grp-range
                   (ed-compress-pt *ed-gen*)         ;; g
                   (ed-compress-pt h)                ;; h = H(m)
                   (ed-compress-pt (ed-nth-pt skey)) ;; P = pkey
                   vrf                               ;; v
                   (ed-compress-pt (ed-nth-pt r))    ;; g^r
                   (ed-compress-pt (ed-mul h r)))))  ;; h^r
           (tt   (with-mod *ed-r*                    ;; tt = r - s * skey
                   (m- r (m* s skey)))))

      (list :v vrf   ;; the VRF value (a compressed pt)
            :s s     ;; a check scalar
            :t tt))) ;; a check scalar


(defun ed-check-vrf (seed proof pkey)
  ;; pkey should be presented in compressed pt form
  (let* ((v    (getf proof :v))
         (s    (getf proof :s))
         (tt   (getf proof :t))
         (h    (ed-pt-from-seed seed))
         ;; check s ?= H(g, h, P, v, g^r = g^tt * P^s, h^r = h^tt * v^s)
         (schk (int
                (hash-to-grp-range
                 (ed-compress-pt *ed-gen*)
                 (ed-compress-pt h)
                 pkey
                 v
                 (ed-compress-pt
                  (ed-add
                   (ed-nth-proj-pt tt)
                   (ed-mul (ed-decompress-pt pkey) s)))
                 (ed-compress-pt
                  (ed-add
                   (ed-mul h tt)
                   (ed-mul (ed-decompress-pt v) s)))
                 ))))
    (= schk s)))

;; -----------------------------------------------------------
#|
(let* ((*edcurve* *curve41417*)
       ;; (*edcurve* *curve1174*)
       ;; (*edcurve* *curve-e521*)
       )
  (plt:window 'plt
              :xsize 330
              :ysize 340)
  (plt:polar-fplot 'plt `(0 ,(* 2 pi))
                   (lambda (arg)
                     (let* ((s (sin (+ arg arg)))
                            (a (* *ed-d* *ed-c* *ed-c* s s 1/4))
                            (b 1)
                            (c (- (* *ed-c* *ed-c*))))
                       (sqrt (/ (- (sqrt (- (* b b) (* 4 a c))) b)
                                (+ a a)))))
                   :clear t
                   :aspect 1))

(let* ((ans (loop for ix from 1 to 10000
                  for pt = *ed-gen* then (ed-add pt *ed-gen*)
                  collect (ecc-pt-x (ed-affine pt)))))
  (plt:plot 'raw (mapcar (um:curry #'ldb (byte 8 0)) ans)
            :clear t)
  (plt:histogram 'plt (mapcar (um:curry #'ldb (byte 8 0)) ans)
                 :clear t
                 :cum t))

(loop repeat 1000 do
      (let* ((x   (field-random (* *ed-h* *ed-r*)))
             (pt  (ed-mul *ed-gen* x))
             (ptc (ed-compress-pt pt))
             (pt2 (ed-decompress-pt ptc)))
        (assert (ed-validate-point pt))
        (unless (ed-pt= pt pt2)
          (format t "~%pt1: ~A" (ed-affine pt))
          (format t "~%pt2: ~A" (ed-affine pt2))
          (format t "~%ptc: ~A" ptc)
          (format t "~%  k: ~A" x)
          (assert (ed-pt= pt pt2)))
        ))
 |#

(defun get-shares ()
  (values *ed-sk3* *ed-sk1* *ed-sk2*))

(defun distribute-shares (shares)
  (setf *ed-sk1* (first shares)
        *ed-sk2* (second shares)
        *ed-sk3* (third shares)))

;; ------------------------------------------------------------------------------
;; CORE-CRYPTO:STARTUP and CORE-CRYPTO:SHUTDOWN

(defun startup-edwards ()
  #-:WINDOWS (core-crypto:ensure-dlls-loaded)
  (format t "~%Connecting Edwards Curves")
  (set-ed-curve :curve1174))

(core-crypto:add-to-startups 'startup-edwards)

(defun shutdown-edwards ()
  (format t "~%Disconnecting Edwards Curves")
  (setf *edcurve* nil))

(core-crypto:add-to-shutdowns 'shutdown-edwards)


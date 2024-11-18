
(defpackage :bresenham
  (:use cl)
  (:export
   :sync-resamp
   ))

(in-package :bresenham)

(defun sync-resamp (tbl sr new-sr)
  ;; resample a table with sample rate sr
  ;; to a new table for sample rate new-sr
  ;; table assumed to start from freq zero.
  (let ((nel (length tbl)))
    (labels ((qinterp (ix x)
               ;; quadratic interpolation of uniformly spaced values
               ;; ix labels value just above x
               ;; so interpolates are (ix-2), (ix-1), ix
               ;; and x is fraction of interval from (ix-1) to ix
               ;; --- am1 --- a0 -x- a1 ---
               (when (< ix 2)
                 (decf x (- 2 ix))
                 (setf ix 2))
               (let* ((am1  (aref tbl (- ix 2)))
                      (a0   (aref tbl (1- ix)))
                      (ap1  (aref tbl ix))
                      (b    (/ (- ap1 am1) 2))
                      (c    (- (/ (+ ap1 am1) 2) a0)))
                 (+ a0 (* x (+ b (* x c))))
                 ))
             (linterp (ix x)
               (let ((a0 (aref tbl (- ix 1)))
                     (a1 (aref tbl ix)))
                 (+ a0 (* x (- a1 a0)))))
             (interp (ix x)
               (cond ((zerop ix)
                      (error "Can't happen"))
                     ((< nel 3)
                      (linterp ix x))
                     (t
                      (qinterp ix x))
                     )))
      
      (cond ((zerop nel)
             #())
            (t
             (um:nlet-tail iter ((ix  0)
                                 (new-tbl nil)
                                 (ctr 0))
               (cond ((>= ix nel)
                      (coerce (nreverse new-tbl) 'vector))
                     ((plusp ctr)
                      (iter (1+ ix) new-tbl (- ctr new-sr)))
                     ((zerop ctr)
                      ;; landed right on a table entry - no need to interpolate
                      (iter ix (cons (aref tbl ix) new-tbl) sr))
                     (t ;; ctr < 0
                      (let* ((x       (/ (+ new-sr ctr) new-sr))
                             (new-val (interp ix x)))
                        (iter ix (cons new-val new-tbl) (+ ctr sr))
                        ))
                     )))
            ))))

(defun qinterp (tbl ix x)
  ;; quadratic interpolation of uniformly spaced values
  ;; ix labels value just above x
  ;; so interpolates are y(ix-2), y(ix-1), y(ix)
  ;; and x is fraction of interval from y(ix-1) to y(ix)
  ;; --- am1 --- a0 -x- a1 ---
  (when (< ix 2)
    (decf x (- 2 ix))
    (setf ix 2))
  (let* ((am1  (aref tbl (- ix 2)))
         (a0   (aref tbl (1- ix)))
         (ap1  (aref tbl ix))
         (b    (/ (- ap1 am1) 2))
         (c    (- (/ (+ ap1 am1) 2) a0)))
    (+ a0 (* x (+ b (* x c))))
    ))

#|
(let ((tbl (vm:framp 5)))
  (plt:fplot 'tst '(0 2)
             (lambda (x)
               (multiple-value-bind (w f) (floor x)
                 (qinterp tbl (1+ w) f)))
             :clear t))
|#

(defun euclid-rhythm (n m &key (ph 0))
  (let ((ans (make-array m
                         :initial-element '_)))
    (um:nlet-tail iter ((ctr 0)
                        (ix  (mod ph m))
                        (len 0))
      (if (>= len n)
          ans
        (cond ((plusp ctr)
               (iter (- ctr n) (mod (1+ ix) m) len))
              ((eql 'x (aref ans ix))
               (iter (+ ctr m) ix len))
              (t
               (setf (aref ans ix) 'x)
               (iter (+ ctr m) ix (1+ len)))
              )))))

(defun er (n m &key (ph 0))
  (assert (<= 0 n m))
  (cond ((zerop n)
         (make-list m :initial-element '_))
        ((eql n m)
         (make-list m :initial-element 'x))
        (t
         (let* ((pat (multiple-value-bind (w r) (truncate (- m n) n)
                       (let* ((ns  (make-list w :initial-element '_)))
                         (apply 'append
                                (append
                                 (make-list r :initial-element (append '(x _) ns))
                                 (make-list (- n r) :initial-element (cons 'x ns)) ))) ))
                (start (mod ph m))
                (end   (+ start m)))
           (subseq (append pat pat) start end)))
        ))


#|
(let* ((tbl #(-0.2 -0.25 0.72))
       (x   (/ (* 1000 256) 48000))
       (x   (- x (floor x))))
  (qinterp tbl 2 x))
 |#

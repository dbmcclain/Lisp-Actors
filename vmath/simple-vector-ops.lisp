
(in-package #:com.ral.vector-ops)

;; --------------------------------------------------------
;; Vector Routines

(defun blit (src srcoff dst dstoff nel)
  (loop for ix from 0 below nel do
        (setf (aref dst (+ dstoff ix))
              (aref src (max 0 (+ srcoff ix))))))

(defun vec (arr offs &optional (nel (- (length arr) offs)))
  (if (and (zerop offs)
           (= nel (length arr)))
      arr
    (make-array nel
              :displaced-to arr
              :displaced-index-offset offs
              :element-type (array-element-type arr))))
  
(defun vlog (v)
  (map 'vector #'log v))

(defun vexp (v)
  (map 'vector #'exp v))

(defun vabs (v)
  (map 'vector #'abs v))

(defun vsub (&rest vs)
  (apply #'map 'vector #'- vs))

(defun vadd (&rest vs)
  (apply #'map 'vector #'+ vs))

(defun vmul (&rest vs)
  (apply #'map 'vector #'* vs))

(defun vdiv (&rest vs)
  (apply #'map 'vector #'/ vs))

(defun vscale (sf v)
  (map 'vector (lambda (x)
                 (* sf x))
       v))

(defun voffset (offs v)
  (map 'vector (lambda (x)
                 (+ offs x))
       v))

(defun vmin (v)
  (reduce #'min v))

(defun vmax (v)
  (reduce #'max v))

(defun vsum (v)
  (reduce #'+ v))

(defun vprod (v)
  (reduce #'* v))

(defun vround (v &optional (unit 1))
  (map 'vector (um:rcurry #'round unit) v))

(defun vfloor (v &optional (unit 1))
  (map 'vector (um:rcurry #'floor unit) v))

(defun vceiling (v &optional (unit 1))
  (map 'vector (um:rcurry #'ceiling unit) v))

(defun vsqr (v)
  (map 'vector #'* v v))

(defun vsqrt (v)
  (map 'vector #'sqrt v))

(defun vlog10 (v)
  (map 'vector (um:rcurry #'log 10) v))

(defun vpow10 (v)
  (map 'vector (um:curry #'expt 10) v))

(defun vexpt (v expon)
  (map 'vector (um:rcurry #'expt expon) v))

(defun undot-the-list (lst &optional ans)
  (cond ((null lst)   (nreverse ans))
        ((consp lst)
         (undot-the-list (rest lst) (cons (first lst) ans)))
        (t (nreverse (cons lst (cons '&rest ans))))
        ))

(defmacro destructure-vector ((&rest args) vec &body body)
  (let* ((v   (gensym))
         (ctr 0)
         (udargs     (undot-the-list args))
         (rest-args  (position '&rest udargs))
         (first-args (if rest-args
                         (subseq udargs 0 rest-args)
                       udargs)))
    `(let* ((,v  ,vec)
            ,@(mapcar (lambda (arg)
                        (let ((ct ctr))
                          (incf ctr)
                          `(,arg (aref ,v ,ct))
                          ))
                      first-args)
            ,@(if rest-args
                 (list `(,(nth (1+ rest-args) udargs) (vec ,v ,rest-args)))
               ))
       ,@body)
    ))

#|
;; test it out...
(destructure-vector (a b) vec-expr my-body)
(destructure-vector (a b &rest others) vec-expr my-body)
(destructure-vector (a b . c) vec-expr my-body)
|#

(defun vlimit> (v lim>)
  (map 'vector (um:curry #'max lim>) v))

(defun vlimit< (v lim<)
  (map 'vector (um:curry #'min lim<) v))

(defun clip (x lim< lim>)
  (let ((vmax (max lim< lim>))
        (vmin (min lim< lim>)))
    (max vmin (min vmax x))))

(defun vclip (v lim< lim>)
  (let ((vmax (max lim< lim>))
        (vmin (min lim< lim>)))
    (map 'vector (um:compose (um:curry #'min vmax)
                             (um:curry #'max vmin))
         v)))

(defun wrap (x lim< lim>)
  (let* ((vmax (max lim< lim>))
         (vmin (min lim< lim>))
         (diff (- vmax vmin)))
    (+ vmin (mod (- x vmin) diff))))

(defun vwrap (v lim< lim>)
  (let* ((vmax (max lim< lim>))
         (vmin (min lim< lim>))
         (diff (- vmax vmin)))
    (map 'vector (lambda (x)
                   (+ vmin (mod (- x vmin) diff)))
         v)))

(defun extrema (&rest args)
  (values (apply #'min args)
          (apply #'max args)))

(defun vextrema (v)
  (let* ((vmin  (aref v 0))
         (vmax  vmin))
    (map nil (lambda (x)
               (setf vmin (min vmin x)
                     vmax (max vmax x)))
         v)
    (values vmin vmax)))

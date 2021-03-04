
(in-package :edec)

#|
(let* ((pairs
        (with-ed-curve :curve1174
          (loop for ix from 32 below 64 collect
                (cons ix (mod *ed-r* ix)))))
       (xs    (map 'vector 'car pairs))
       (ys    (map 'vector 'cdr pairs)))
  (plt:plot 'plt xs ys
            :clear t
            :yrange '(0 64)
            :plot-joined t
            :symbol :circle)
  (plt:fplot 'plt '(32 64) 'identity
             :color :red
             :alpha 0.25))
|#

#|
(defun maxpos (lst fn)
  (um:nlet iter ((lst   lst)
                 (mxval nil)
                 (ans   nil))
    (if lst
        (let* ((hd  (car lst))
               (tl  (cdr lst)))
          (cond (mxval
                 (let ((val (funcall fn hd)))
                   (if (> val mxval)
                       (go-iter tl val hd)
                     (go-iter tl mxval ans))))
                (t
                 (go-iter tl (funcall fn hd) hd))
                ))
      (values ans mxval))))
|#

;; --------------------------------------------------------------------------------

(defun probe-params (base wrdsiz)
  #F
   (declare (integer base)
           (fixnum  wrdsiz))
  (let* ((len     (integer-length base))
         (nframes (ceiling len wrdsiz))
         (nlast   (mod len wrdsiz))
         (wrapmul (mod (ash 1 len) base)))
    (declare (fixnum len nframes nlast)
             (integer wrapmul))
    (list :base     base
          :nframes  nframes
          :wordsize wrdsiz
          :lastsize (if (zerop nlast)
                        wrdsiz
                      nlast)
          :wrapmul  wrapmul)))

(defun probe (base)
  #F
  (declare (integer base))
  (let ((wrdsiz nil)
        (maxval nil)
        (len    (integer-length base)))
    (declare (fixnum len))
    (loop for wordsize fixnum from 48 to 56 do
          (let ((val (mod len wordsize)))
            (declare (fixnum val))
            (when (zerop val)
              (setf val len))
            (when (or (null wrdsiz)
                      (>= val (the fixnum maxval)))
              (setf wrdsiz wordsize
                    maxval val))
            ))
    (probe-params base wrdsiz)))

;; --------------------------------------------------------------------------------

(defclass mod-ctrl ()
  ((nframes  :reader mod-ctrl-nframes
             :initarg :nframes)
   (wordsize :reader mod-ctrl-wordsize
             :initarg :wordsize)
   (lastsize :reader mod-ctrl-lastsize
             :initarg :lastsize)
   (wrapmul  :reader mod-ctrl-wrapmul
             :initarg :wrapmul)
   (base     :reader mod-ctrl-base
             :initarg :base)))

(defclass mod-int ()
  ((ctrl     :reader mod-int-ctrl
             :initarg :ctrl)
   (frames   :reader mod-int-frames
             :initarg :frames)))

(defmethod make-mod-ctrl ((base integer))
  #F
  (let ((cache (or (get 'mod-ctrl 'cache)
                   (setf (get 'mod-ctrl 'cache)
                         (make-hash-table)))))
    (or (gethash base cache)
        (setf (gethash base cache)
              (apply 'make-instance 'mod-ctrl
                     (probe base)))
        )))

;; --------------------------------------------------------------------------------

(defmethod make-frames ((ctrl mod-ctrl))
  #F
  (make-array (mod-ctrl-nframes ctrl)
              :element-type 'integer
              :initial-element 0))

(defmethod int-to-frames ((ctrl mod-ctrl) (val integer))
  #F
  (let ((mval   (mod val (mod-ctrl-base ctrl)))
        (wsize  (mod-ctrl-wordsize ctrl)))
    (coerce
     (loop repeat (mod-ctrl-nframes ctrl)
           for pos from 0 by wsize
           collect
           (ldb (byte wsize pos) mval))
     'vector)))

;; --------------------------------------------------------------------------------

(defmethod make-mod-int ((ctrl mod-ctrl) (frames sequence))
  #F
  (make-instance 'mod-int
                 :ctrl   ctrl
                 :frames (coerce frames 'vector)))

(defmethod make-mod-int ((ctrl mod-ctrl) (val integer))
  #F
  (make-mod-int ctrl (int-to-frames ctrl val)))

(defmethod make-mod-int ((base integer) (val integer))
  #F
  (make-mod-int (make-mod-ctrl base) val))

(defmethod mod-int ((x mod-int))
  x)

(defmethod mod-int ((x integer))
  (make-mod-int (mod-base) x))

#|
(inspect 
 (with-ed-curve :curve1174
   (make-mod-int *ed-q* *ed-r*)))
 |#

;; --------------------------------------------------------------------------------

(defmethod mi-normalize ((x mod-int))
  #F
  (let* ((ctrl    (mod-int-ctrl x))
         (frames  (mod-int-frames x))
         (nframes (mod-ctrl-nframes ctrl))
         (ixlast  (1- nframes))
         (wsize   (mod-ctrl-wordsize ctrl))
         (wshift  (- wsize))
         (wsel    (byte wsize 0))
         (lsize   (mod-ctrl-lastsize ctrl))
         (wrapmul (mod-ctrl-wrapmul ctrl))
         (tmp     0))
    (declare (fixnum  nframes ixlast wsize wshift lsize)
             (integer tmp wrapmul)
             ((vector integer) frames))
    (loop for ix fixnum from 0 below ixlast do
          (let ((tmp2 (+ tmp (the fixnum (aref frames ix)))))
            (declare (integer tmp2))
            (setf (aref frames ix) (the fixnum (ldb wsel tmp2))
                  tmp              (the integer (ash tmp2 wshift)))
            ))
    (let ((tmp2 (+ tmp
                   (the integer (aref frames ixlast))
                   )))
      (declare (integer tmp2))
      (setf (aref frames ixlast) (the fixnum  (ldb (byte lsize 0) tmp2)))
      (incf (aref frames 0)      (the integer (* wrapmul
                                                 (the integer (ash tmp2 (- lsize)))
                                                 )))
      )))

;; --------------------------------------------------------------------------------

(defmethod mi-neg ((x mod-int))
  #F
  (make-mod-int (mod-int-ctrl x)
                (map 'vector '- (mod-int-frames x))))


;; --------------------------------------------------------------------------------

(defmethod %mi+ ((a mod-int) (b mod-int))
  #F
  (let ((ctrl    (mod-int-ctrl a)))
    (assert (eql ctrl (mod-int-ctrl b)))
    (make-mod-int ctrl
                  (map 'vector '+ (mod-int-frames a) (mod-int-frames b)))
    ))

(defmethod %mi+ ((a mod-int) (b integer))
  #F
  (let* ((frames (copy-seq (mod-int-frames a))))
    (incf (aref frames 0) b)
    (make-mod-int (mod-int-ctrl a) frames)))

(defmethod %mi+ ((a integer) (b mod-int))
  #F
  (mi+ b a))

(defun mi+ (arg &rest args)
  (dolist (opnd args)
    (setf arg (%mi+ arg opnd)))
  arg)

;; --------------------------------------------------------------------------------

(defmethod %mi- ((a mod-int) (b mod-int))
  #F
  (let ((ctrl    (mod-int-ctrl a)))
    (assert (eql ctrl (mod-int-ctrl b)))
    (make-mod-int ctrl
                  (map 'vector '- (mod-int-frames a) (mod-int-frames b)))
    ))

(defmethod %mi- ((a mod-int) (b integer))
  #F
  (let* ((frames (copy-seq (mod-int-frames a))))
    (decf (aref frames 0) b)
    (make-mod-int (mod-int-ctrl a) frames)))

(defmethod %mi- ((a integer) (b mod-int))
  #F
  (let ((ans (mi-neg b)))
    (incf (aref (mod-int-frames ans) 0) a)
    ans))

(defun mi- (arg &rest args)
  (if args
      (%mi- arg (apply 'mi+ args))
    (mi-neg arg)))

;; --------------------------------------------------------------------------------

(defmethod %mi* ((a mod-int) (b integer))
  #F
  (let* ((ctrl     (mod-int-ctrl a))
         (aframes  (mod-int-frames a))
         (cframes  (make-frames ctrl))
         (ixlast   (1- (mod-ctrl-nframes ctrl)))
         (wsize    (mod-ctrl-wordsize ctrl))
         (wsel     (byte wsize 0))
         (wshift   (- wsize))
         (lsize    (mod-ctrl-lastsize ctrl))
         (wrapmul  (mod-ctrl-wrapmul ctrl))
         (lwrapmul (ash wrapmul (- wsize lsize)))
         (tmp      0))
    (declare (fixnum  ixlast wsize wshift lsize)
             (integer tmp wrapmul lwrapmul)
             ((vector integer) aframes cframes))
    (labels ((stage (ix)
               (declare (fixnum ix))
               (let ((tmp2  (+ tmp
                               (the integer (* b
                                               (the integer (aref aframes ix))
                                               ))
                               )))
                 (declare (integer tmp2))
                 (setf (aref cframes ix) (the fixnum  (ldb wsel tmp2))
                       tmp               (the integer (ash tmp2 wshift))))
               ))
      (stage ixlast)
      (setf tmp (* tmp lwrapmul))
      (loop for ix fixnum from 0 below ixlast do (stage ix))
      (let ((tmp2 (+ tmp
                     (the integer (aref cframes ixlast)))
                  ))
        (declare (integer tmp2))
        (setf (aref cframes ixlast) (the fixnum  (ldb (byte lsize 0) tmp2)))
        (incf (aref cframes 0)      (the integer (* wrapmul
                                                    (the integer (ash tmp2 (- lsize)))
                                                    )))
        (make-mod-int ctrl cframes)
        ))))

(defmethod %mi* ((a integer) (b mod-int))
  #F
  (mi* b a))

(defmethod %mi* ((a integer) (b integer))
  #F
  (* a b))

(defmethod %mi* ((a mod-int) (b mod-int))
  #F
  (let ((ctrl (mod-int-ctrl a)))
    (assert (eql ctrl (mod-int-ctrl b)))
    (let* ((aframes  (mod-int-frames a))
           (bframes  (mod-int-frames b))
           (cframes  (make-frames ctrl))
           (nframes  (mod-ctrl-nframes ctrl))
           (ixlast   (1- nframes))
           (wsize    (mod-ctrl-wordsize ctrl))
           (lsize    (mod-ctrl-lastsize ctrl))
           (wrapmul  (mod-ctrl-wrapmul ctrl))
           (lwrapmul (ash wrapmul (- wsize lsize)))
           (wsel     (byte wsize 0))
           (wshift   (- wsize))
           (tmp      0))
      (declare (fixnum  nframes ixlast wsize lsize wshift)
               (integer wrapmul lwrapmul tmp)
               ((vector integer) aframes bframes cframes))
      (labels ((stage (ix)
                 (declare (fixnum ix))
                 (let ((tmp2 (+ tmp
                                (the integer
                                     (loop for iy fixnum from 0 to ix
                                           for iz fixnum from ix downto 0
                                           sum
                                           (* (the integer (aref aframes iy))
                                              (the integer (aref bframes iz)))))
                                (the integer
                                     (* lwrapmul
                                        (the integer
                                             (let ((ix+1  (1+ ix)))
                                               (loop for iy fixnum from ix+1 to ixlast
                                                     for iz fixnum from ixlast downto ix+1
                                                     sum
                                                     (* (the integer (aref aframes iy))
                                                        (the integer (aref bframes iz))))))
                                        ))
                                )))
                   (declare (integer tmp2))
                   (setf (aref cframes ix) (the fixnum (ldb wsel tmp2))
                         tmp               (the integer (ash tmp2 wshift)))
                   )))
        (stage ixlast)
        (setf tmp (* tmp lwrapmul))
        (loop for ix fixnum from 0 below ixlast do (stage ix))
        (let ((tmp2 (+ tmp
                       (the integer (aref cframes ixlast))
                       )))
          (declare (integer tmp2))
          (setf (aref cframes ixlast) (the fixnum  (ldb (byte lsize 0) tmp2)))
          (incf (aref cframes 0)      (the integer (* wrapmul
                                                      (the integer (ash tmp2 (- lsize)))
                                                      )))
          (make-mod-int ctrl cframes))
        ))))

(defmethod mi* (arg &rest args)
  #F
  (dolist (opnd args)
    (setf arg (%mi* arg opnd)))
  arg)

;; --------------------------------------------------------------------------------

(defmethod mi-sqr ((a mod-int))
  #F
  (%mi* a a))

;; --------------------------------------------------------------------------------

(defmethod mi^ ((x mod-int) (exp integer))
  #F
  (let* ((n     (integer-length exp))
         (prec  (make-array 16
                            :initial-element nil))
         (ans   nil))
    (declare (fixnum n))
    (setf (aref prec 1) x)
    (labels ((get-prec (ix)
               ;; on-demand precomputed x^n, n = 1..15
               (declare (fixnum ix))
               (or (aref prec ix)
                   (setf (aref prec ix)
                         (if (oddp ix)
                             (mi* x (get-prec (1- ix)))
                           (mi-sqr (get-prec (ash ix -1))))
                         ))
               ))
      ;; 4-bit fixed window algorithm
      (loop for pos fixnum from (* 4 (floor n 4)) downto 0 by 4 do
            (when ans
              (setf ans (mi-sqr (mi-sqr (mi-sqr (mi-sqr ans))))))
            (let ((bits (ldb (byte 4 pos) exp)))
              (declare (fixnum bits))
              (unless (zerop bits)
                (let ((y (get-prec bits)))
                  (setf ans (if ans
                                (mi* ans y)
                              y))))
              ))
      ans)))

;; --------------------------------------------------------------------------------

(defmethod mi-inv ((x mod-int))
  #F
  (mi^ x (- (mod-ctrl-base (mod-int-ctrl x))
            2)))

(defmethod %mi/ ((x mod-int) (y mod-int))
  #F
  (%mi* x (mi-inv y)))

(defun mi/ (arg &rest args)
  (if args
      (%mi/ arg (apply 'mi* args))
    (mi-inv arg)))

;; --------------------------------------------------------------------------------

(defmethod to-integer ((x mod-int))
  #F
  (let* ((ctrl   (mod-int-ctrl x))
         (frames (mod-int-frames x)))
    (declare ((vector integer) frames))
    (mod
     (loop for ix    fixnum from 0 below (mod-ctrl-nframes ctrl)
           for shift fixnum from 0 by (mod-ctrl-wordsize ctrl)
           sum
           (ash (the integer (aref frames ix))
                shift))
     (mod-ctrl-base ctrl))))

(defmethod int ((x mod-int))
  #F
  (to-integer x))

;; --------------------------------------------------------------------------------

#|
(setf x 
      (inspect 
       (with-ed-curve :curve1174
         (let ((a (make-mod-int *ed-q* *ed-r*)))
           (mi* a a)))))

(to-integer (with-ed-curve :curve1174
              (let ((a (make-mod-int *ed-q* *ed-r*)))
                (mi* a a))))

(to-integer (with-ed-curve :curve1174
              (let ((a (make-mod-int *ed-q* *ed-r*)))
                (mi-neg (mi* a a)))))
|#              

(defun tst-m (&optional (n 10000))
  (with-mod *ed-q*
    (let* ((xs  (loop repeat n collect (field-random *ed-q*)))
           (exp (- *ed-q* 2))
           (fn  (um:rcurry 'm^ exp)))
      (loop repeat 3 do
            (time (map 'nil fn xs))))))
  
(defun tst-mi (&optional (n 10000))
  (with-mod *ed-q*
    (let ((xs (loop repeat n collect (mod-int (field-random *ed-q*)))))
      (loop repeat 3 do
            (time (map 'nil 'mi/ xs))))))
                  
(defun cgen-mi* (base)
  (let* ((ctrl     (make-mod-ctrl base))
         (nframes  (mod-ctrl-nframes ctrl))
         (ixlast   (1- nframes))
         (wsize    (mod-ctrl-wordsize ctrl))
         (lsize    (mod-ctrl-lastsize ctrl))
         (wrapmul  (mod-ctrl-wrapmul ctrl))
         (lwrapmul (ash wrapmul (- wsize lsize)))
         (wsel     (byte wsize 0))
         (wshift   (- wsize))
         (tmp      0))
    (with-output-to-string (s)
      ;; type declarations and constants
      (format s "~%const uint64_t low~A = 0x~16,'0x;" wsize (1- (ash 1 wsize)))
      (unless (eql wsize lsize)
        (format s "~%const uint64_t low~A = 0x~16,'0x;" lsize (1- (ash 1 lsize))))
      (format s "~%")
      (format s "~%typedef uint64_t frames[~A];" nframes)

      ;; gadd
      (format s "~%")
      (format s "~%void gadd(frames a, frames b, frames c)~%{")
      (loop for ix from 0 below nframes do
            (format s "~%~tc[~A] = a[~A] + b[~A];" ix ix ix))
      (format s "~%}")

      ;; gsub
      (format s "~%")
      (format s "~%void gsub(frames a, frames b, frames c)~%{")
      (loop for ix from 0 below nframes do
            (format s "~%~tc[~A] = a[~A] - b[~A];" ix ix ix))
      (format s "~%}")

      ;; gmuli
      (format s "~%")
      (format s "~%void gmuli(frames a, int b, frames c)~%{")
      (loop for ix from 0 below nframes do
            (format s "~%~tc[~A] = b * a[~A];" ix ix))
      (format s "~%}")

      ;; gmul
      (format s "~%")
      (format s "~%void gmul(frames a, frames b, frames c)~%{")
      (format s "~%~tuint128_t tmp;~%")
      (labels ((stage (ix &key first-time tmpmul)
                 (declare (fixnum ix))
                 (format s "~%~ttmp = ")
                 (unless (or first-time
                             (zerop ix))
                   (format s "(tmp >> ~A) + " wsize))
                 (let ((terms (loop for iy fixnum from 0 to ix
                                    for iz fixnum from ix downto 0
                                    collect
                                    (format nil "a[~A]*b[~A]" iy iz))))
                   (format s "~{~A~^ + ~}" terms))
                 (let ((terms (let ((ix+1  (1+ ix)))
                                (loop for iy fixnum from ix+1 to ixlast
                                      for iz fixnum from ixlast downto ix+1
                                      collect
                                      (format nil "a[~A]*b[~A]" iy iz)))))
                   (when (zerop ix)
                     (push (format nil "(tmp >> ~A)" wsize) terms))
                   (when terms
                     (format s " + ~A*(~{~A~^ + ~})" lwrapmul terms)))
                 (format s ";")
                 (format s "~%~tc[~A] = (uint64_t)(tmp & low~A);" ix wsize)))
      (stage ixlast :first-time t)
      (loop for ix fixnum from 0 below ixlast do (stage ix :tmpmul (and (zerop ix)
                                                                        lwrapmul)))
      (format s "~%~ttmp = (tmp >> ~A) + c[~A];" wsize ixlast)
      (format s "~%~tc[~A] = (uint64_t)(tmp & low~A);" ixlast lsize)
      (format s "~%~tc[0] += (uint64_t)(~A*(tmp >> ~A));" wrapmul lsize)
      (format s "~%}")
      ))))

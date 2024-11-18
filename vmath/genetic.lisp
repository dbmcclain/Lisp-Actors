
(defpackage #:com.ral.genetic
  (:use :cl)
  (:export
   ))

(in-package #:com.ral.genetic)

(defstruct gene-pool
  nchromosomes
  chromosomes
  ngenes
  genes)

(defvar *gene-pool*)

(define-symbol-macro *nchrom*  (gene-pool-nchromosomes *gene-pool*))
(define-symbol-macro *chroms*  (gene-pool-chromosomes  *gene-pool*))
(define-symbol-macro *ngenes*  (gene-pool-ngenes       *gene-pool*))
(define-symbol-macro *genes*   (gene-pool-genes        *gene-pool*))

(defun mutate (n)
  (let* ((apool  (coerce (cdr *chroms*) 'vector)) ;; avoid mutating our thoroghbred
         (npool  (length apool))
         (genes  *genes*)
         (ngenes *ngenes*))
    (loop repeat n do
          (let* ((chrom (aref apool (lw:mt-random npool)))
                 (gene  (lw:mt-random ngenes)))
            (setf (aref chrom gene) (lw:mt-random (ash 1 (aref genes gene))))
            ))
    ))

(defun create-chromosomes (vbits npool)
  (let ((vlen (length vbits)))
    (loop repeat npool collect
          (let ((item (make-array vlen)))
            (loop for ix from 0 below vlen do
                  (setf (aref item ix) (lw:mt-random (ash 1 (aref vbits ix)))))
            item))
    ))

(defvar *progress-fn*     #'lw:do-nothing)
(defvar *progress-count*  0)

(defun select-pool (fn)
  (let* ((upool   (remove-duplicates *chroms* :test #'equalp))
         (subpool (subseq (sort (mapcar (lambda (item)
                                          (list (funcall fn item) item))
                                        upool)
                               #'<
                               :key #'first)
                         0 (truncate (length upool) 8))))
    (when (>= (incf *progress-count*) 1)
      (setf *progress-count* 0)
      (funcall *progress-fn* (first subpool)))
    subpool))

(defun cross-breed (sub-pool)
  (let* ((apool    (coerce sub-pool 'vector))
         (gpool    (coerce *chroms* 'vector))
         (nparents (length apool))
         (nchroms  *nchrom*)
         (ngenes   *ngenes*))
    (format t " Crossbreeding ~A " nparents)
    (setf *chroms* (nconc sub-pool
                          (loop repeat (- *nchrom* nparents) collect
                                ;; let royalty breed with the masses too...
                                (let* ((par1   (aref apool (lw:mt-random nparents)))
                                       (par2   (aref gpool (lw:mt-random nchroms)))
                                       (offspr (copy-seq par1)))
                                  (loop repeat ngenes do ;; (truncate ngenes 4) do
                                        (let* ((par (if (zerop (lw:mt-random 3))
                                                        par2
                                                      par1))
                                               (gene (lw:mt-random ngenes)))
                                          (setf (aref offspr gene) (aref par gene))))
                                  offspr))
                          ))))

(defun find-min (vbits fn &key (niter 100) (npool 1024))
  (let* ((*gene-pool* (make-gene-pool
                       :nchromosomes  npool
                       :chromosomes   (create-chromosomes vbits npool)
                       :ngenes        (length vbits)
                       :genes         vbits) ))
    (loop repeat niter do
          (cross-breed (mapcar #'second (select-pool fn)))
          (mutate (truncate npool 2)))
    (first (select-pool fn))))


(defun assemble-x (vec vbits from to)
  (let ((x 0)
        (tbits (reduce #'+ vbits)))
    (loop for ix from 0 below (length vec) do
          (setf x (+ (ash x (aref vbits ix))
                     (aref vec ix))))
    (+ from
       (* (- to from) 2
          (- (/ x (ash 1 tbits)) 0.5)))
    ))

#|
;; try it out on a minimization problem, where we know the answer
;; Finding the min of (3*x^2 - 4*x + 2) ans: 2/3
(plt:fplot 'plt '(0 1.3) (lambda (x)
                            (+ (* 3 x x)
                               (* -4 x)
                               2))
           :clear t)

(let ((vbits #(3 3 3 3)))
  (find-min vbits (lambda (v)
                    ;; search of (-10,10)
                    (let ((x (assemble-x v vbits -10 10)))
                      (+ (* 3 x x)
                         (* -4 x)
                         2)))))

=>
;; we are searching on a grid over (-10, 10) with 4096 subdivisions
;; answer to 1 part in 20/4096 (= +/-5e-3) should be 0.666
(0.66668225 #(4 2 1 1)) 
(0.66668225 #(4 2 1 1)) 
(0.66668225 #(4 2 1 1)) 
(0.66668225 #(4 2 1 1)) 
(0.66668225 #(4 2 1 1)) 
|#

#|
;; let's try something harder.. the optimum phase shift between harmonics
;; for a white spectrum series of N harmonics of sinewaves (i.e. all equal amplitude)

(defun tst-phases (phs)
  (let* ((nharm  1)
         (vbits  (subseq #(12  12   12  12   12   12   12 12 12) 0 nharm))
         ;; (ampls  (subseq '(0.2  1.0  0.4  0.3  0.5  0.2  0.1  0.1  0.1) 0 nharm))
         (ampls  (subseq '(  1    0    1    0    0    0    0    0    0) 0 nharm))
         (ns     (subseq '(  2    3    4    5    6    7    8    9   10) 0 nharm))
         bank)
    (labels ((ph (val)
               (/ val 4096))
             (osc (n ampl ph)
               (let ((ans (make-array 8192)))
                 (loop for ix from 0 below 8192 do
                       (setf (aref ans ix)
                             (* ampl (sin (* 2 pi (+ (/ (* n ix) 4096) ph)))))
                       )
                 ans))
             (get-osc (n ph)
               (let ((ix (mod (round (* ph 4096) n) 4096)))
                 ;; (subseq (aref bank (1- n)) (- 4096 ix) (- 8192 ix))
                 (subseq (aref bank (1- n)) ix (+ 4096 ix))
                 ))
             (osc-bank (vphs)
               (apply #'map 'vector #'+ (get-osc 1 0)
                      (map 'list #'get-osc ns (map 'list #'ph vphs))))
             (max-abs (a b)
               (max (abs a) (abs b)))
             (min-fn (vphs)
               (reduce #'max-abs (osc-bank vphs)))
             (show-progress (lst)
               (destructuring-bind (score vphs) lst
                 (let ((osc (osc-bank vphs)))
                   (plt:plot 'progress (concatenate 'vector osc osc)
                             :clear t)
                   (print score)))))

      (setf bank (coerce (cons (osc 1 1 0)
                               (mapcar (um:rcurry #'osc 0) ns ampls))
                         'vector))
      (let ((ans (osc-bank (mapcar (lambda (ph)
                                     (round (* ph 4096)))
                                   phs))))
        (plt:plot 'tst (concatenate 'vector ans ans)
                  :clear t)))))

(tst-phases '(0.881 0.259 0.936))
(tst-phases '(0.777 0 0.734))

(tst-phases '(-0.9))

(defun min-phases ()
  (let* ((nharm  9)
         (vbits  (subseq #(12  12   12  12   12   12   12 12 12) 0 nharm))
         ;; (ampls  (subseq '(0.2  1.0  0.4  0.3  0.5  0.2  0.1  0.1  0.1) 0 nharm))
         (ampls  (subseq '(  1    1    1    1    1    1    1    1    1) 0 nharm))
         (ns     (subseq '(  2    3    4    5    6    7    8    9   10) 0 nharm))
         bank)
    (labels ((ph (val)
               (/ val 4096))
             (osc (n ampl ph)
               (let ((ans (make-array 8192)))
                 (loop for ix from 0 below 8192 do
                       (setf (aref ans ix)
                             (* ampl (sin (* 2 pi (+ (/ (* n ix) 4096) ph)))))
                       )
                 ans))
             (get-osc (n ph)
               (let ((ix (mod (round (* ph 4096) n) 4096)))
                 ;; (subseq (aref bank (1- n)) (- 4096 ix) (- 8192 ix))
                 (subseq (aref bank (1- n)) ix (+ 4096 ix))
                 ))
             (osc-bank (vphs)
               (apply #'map 'vector #'+ (get-osc 1 0)
                      (map 'list #'get-osc ns (map 'list #'ph vphs))))
             (max-abs (a b)
               (max (abs a) (abs b)))
             (min-fn (vphs)
               (reduce #'max-abs (osc-bank vphs)))
             (show-progress (lst)
               (destructuring-bind (score vphs) lst
                 (let ((osc (osc-bank vphs)))
                   (plt:plot 'progress (concatenate 'vector osc osc)
                             :clear t)
                   (print score)))))

      (setf bank (coerce (cons (osc 1 1 0)
                               (mapcar (um:rcurry #'osc 0) ns ampls))
                         'vector))
      (setf *progress-count* 0)
      (let* ((*progress-fn* #'show-progress)
             (ans (find-min vbits #'min-fn :npool (* 1 1024))))
        (list (first ans) (map 'vector (um:compose #'float #'ph) (second ans)))
        ))))


(defun make-gf-lagrange-interpolator (shares)
  (labels ((lprod (x xs)
             (reduce (lambda (prod x2)
                       (gf* prod (gf+ x2 x)))
                     xs
                     :initial-value 1)))
    (lambda (x0)
      (labels ((term (sum share)
                 (with-accessors ((x crypto-share-x)
                                  (y crypto-share-y)) share
                   (let ((xs (mapcar #'crypto-share-x (remove share shares))))
                     (gf+ sum
                          (gf* y (gf/ (lprod x0 xs)
                                      (lprod x xs)) )) ))) )
        (reduce #'term shares
                :initial-value 0))) ))

(defun min-max (fn npts)
  ;; try deriving a min-max approximation to a function using npts
  ;; for interpolates. The genetic algorithm needs to find the locations
  ;; of those npts along the real axis covering (-1,1)
  #F
  (let* ((vbits  (make-array npts :initial-element 12))
         (xsamp  (make-array 4096))
         (ysamp  (make-array 4096)))
    (labels ((x-pos (x)
               (- (/ (* 2.0d0 x) 4096.0d0) 1.0d0))
             (make-interpolator (v)
               (let* ((xs (map 'vector #'x-pos (remove-duplicates v)))
                      (ys (map 'vector fn xs))
                      (nel (length xs)))
                 (lambda (x)
                   (do ((ix  0   (1+ ix))
                        (sum 0))
                       ((>= ix nel) sum)
                     (incf sum (do ((jx   0  (1+ jx))
                                    (xi   (aref xs ix))
                                    (prod (aref ys ix)))
                                   ((>= jx nel) prod)
                                 (unless (= jx ix)
                                   (let ((xj (aref xs jx)))
                                     (setf prod (* prod (/ (- x xj)
                                                           (- xi xj))))))
                                 ))))
                 ))
             (err (x interpfn)
               (- (funcall interpfn x)
                  (funcall fn x)))
             (score (v)
               (let* ((interpfn (make-interpolator v)))
                 (loop for x across xsamp maximize
                       (abs (err x interpfn)))))
             (show-progress (lst)
               (destructuring-bind (score v) lst
                 (let* ((interpfn (make-interpolator v))
                        (xs (map 'vector #'x-pos v))
                        (ys (map 'vector (um:rcurry #'err interpfn) xs)))
                   #|
                   (loop for ix from 0
                         for x across xsamp do
                         (setf (aref ysamp ix) (err x interpfn)))
                   |#
                   #||#
                   (plt:fplot 'progress '(-1 1) (um:rcurry #'err interpfn)
                              :clear t)
                   #||#
                   #|
                   (plt:plot 'progress xsamp ysamp
                             ;; :clear t
                             )
                   |#
                   (plt:plot 'progress xs ys
                             :symbol :circle)
                   (print score)))))
      (setf *progress-count* 0)
      (loop for ix from 0 below 4096 do
            (setf (aref xsamp ix) (x-pos ix)))
      (let* ((*progress-fn* #'show-progress)
             (ans (find-min vbits #'score :npool 256)))
        (list (first ans) (map 'vector (um:compose #'float #'x-pos) (second ans)))
        ))))

(min-max #'sin 2)

|#
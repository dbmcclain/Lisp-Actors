
(in-package :edec)


(defun shmul (ans-name x nsh y)
  (with-mod *ed-q*
    (let ((t1  (if (oddp nsh)
                   (m* x x)
                 x))
          t2)
      (loop for ix from 0 below (ash nsh -1) do
              (setf t2 (m* t1 t1)
                    t1 (m* t2 t2)))
      (let ((ans (m* t1 y)))
        (with-standard-io-syntax
          (format t "~%~5A: x=~x, nsh=~d, y=~x => ~x" ans-name x nsh y ans))
        ans)
      )))

(defun ginv (x)
  (with-mod *ed-q*
    (with-standard-io-syntax
      (format t "~%ANS = ~x" (m/ x))))
  (let* ((x2   (shmul 'x2   x      1    x )) ;; x^2 * x = x^3
         (x^5  (shmul 'x^5  x      2    x )) ;; x^4 * x = x^5
         (x3   (shmul 'x3   x2     1    x )) ;; (x^3)^2 * x = x^7
         (x4   (shmul 'x4   x3     1    x )) ;; (x^7)^2 * x = x^15
         (x7   (shmul 'x7   x4     3   x3 ))   
         (x8   (shmul 'x8   x7     1    x )) ;; (x^15)^16 * x^15 = x^255
         (x16  (shmul 'x16  x8     8   x8 )) ;; (x^255)^256 * x^255 = x^65535
         (x23  (shmul 'x23  x16    7   x7 ))
         (x32  (shmul 'x32  x16   16  x16 ))
         (x55  (shmul 'x55  x32   23  x23 ))
         (x64  (shmul 'x64  x32   32  x32 ))
         (x119 (shmul 'x119 x64   55  x55 ))
         (x128 (shmul 'x128 x64   64  x64 ))
         (x247 (shmul 'x247 x128 119 x119 )))
    (shmul 'ans x247 4 x^5)))

(ginv 2)

(defun tst ()
  (with-mod *ed-q*
    (let* ((x    (random *ed-q*))
           (inv  (m/ x))
           (_    (with-standard-io-syntax
                   (format t "~%ANS: ~x" inv)))
           (xinv (ginv x)))
      (assert (= inv xinv)))))

#|
(tst)
|#

(defun cost-of-tree (exp2 &optional (cost 0))
  (cond
   ((eql 1 exp2)
    cost)
   ((oddp exp2)
    (format t "~%~D: odd, try 1-" exp2)
    (cost-of-tree (1- exp2) (+ cost 2)))
   (t
    (let ((n/2  (ash exp2 -1)))
      (format t "~%~D: even, try ~D" exp2 n/2)
      (cost-of-tree n/2 (+ n/2 1 cost))))
   ))

;; for :curve1174 group ed-q inverse
(cost-of-tree 247)
(cost-of-tree-p2 247)

;; for :curve1174 group ed-q sqrt
(cost-of-tree 248)
(cost-of-tree-p2 248)

(defun cost-of-tree-p2 (exp2 &optional seen (cost 0))
  (cond
   ((eql 1 exp2)
    (values cost seen))
   ((member exp2 seen)
    (values cost seen))
   ((oddp exp2)
    (let* ((ne (um:floor-pwr2 exp2))
           (no (- exp2 ne)))
      (format t "~%~D: odd, try ~D,~D" exp2 ne no)
      (multiple-value-bind (ce se)
          (cost-of-tree-p2 ne seen 0)
        (cost-of-tree-p2 no (list* exp2 ne se) (+ cost no 1 ce)))
      ))
   (t
    (let ((n/2  (ash exp2 -1)))
      (format t "~%~D: even, try ~D" exp2 n/2)
      (cost-of-tree-p2 n/2 (cons exp2 seen) (+ n/2 1 cost))))
   ))

;; for :curve-e521f group ed-q inverse 
(cost-of-tree 519)
(cost-of-tree-p2 519)

;; for :curve-e521f group ed-q sqrt
(cost-of-tree 519)
(cost-of-tree-p2 519)


(defun gen-tree (exp2 &optional (cost 0) seen nodes)
  (cond
   ((eql 1 exp2)
    (values cost
            (with-output-to-string (s)
              ;; (format s "~%coord_t ~{~a~^, ~};" seen)
              (format s "~%coord_t w;")
              (terpri s)
              (mapc (lambda (ln)
                      (format s "~%~a" ln))
                    nodes)
              (terpri s))
            ))
   ((oddp exp2)
    (gen-tree (1- exp2) (+ cost 2)
              (cons
               (format nil "x~D" exp2)
               seen)
              (cons
               ;; (format nil "shmul(x~d,~t1,~tx,~tx~d);" (1- exp2) exp2)
               (format nil "shmul(w,   1, x, w); // ~3d-bits" exp2)
               nodes)))
   (t
    (let ((n/2  (ash exp2 -1)))
      (gen-tree n/2 (+ n/2 1 cost)
                (cons
                 (format nil "x~d" exp2)
                 seen)
                (cons
                 (if (= 1 n/2)
                     ;; (format nil "shmul(x,~t1,~tx,~tx2);")
                     (format nil "shmul(x,   1, x, w); //   2-bits")
                   ;; (format nil "shmul(x~d,~t~d,~tx~d,~tx~d);" n/2 n/2 n/2 exp2)
                   (format nil "shmul(w, ~3d, w, w); // ~3d-bits" n/2 exp2))
                 nodes))))
   ))

(gen-tree 247) ;; :curve1174 inverse
(gen-tree 248) ;; :curve1174 sqrt

(gen-tree 519) ;; :curve-e521 inverse


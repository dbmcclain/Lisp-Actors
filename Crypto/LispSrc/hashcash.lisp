
(in-package :edec)

(defun hc-delay (seed n)
  (declare (fixnum n))
  (um:nlet iter ((ix 0))
    (let* ((h     (hash/256 seed ix))
           (bits  (int h)))
      (if (zerop (ldb (byte n 0) bits))
          (list :nbits n
                :ctr   ix
                :seed  seed
                :hash  h
                :niter (1+ ix))
        ;; else
        (go-iter (1+ ix))))))

(defvar *mn*  nil)
(defvar *sd*  nil)
(defvar *mni*  nil)
(defvar *sdi*  nil)

(defun gather-stats (nmax &key (nsamp 4))
  (let* ((mn  (make-array nmax :initial-element 0))
         (sd  (make-array nmax :initial-element 0))
         (mni  (make-array nmax :initial-element 0))
         (sdi  (make-array nmax :initial-element 0))
         (samp (make-array nsamp :initial-element 0))
         (isamp (make-array nsamp :initial-element 0)))
    (dotimes (n nmax)
      (dotimes (ix nsamp)
        (let* ((start (usec:get-time-usec))
               (ans   (hc-delay (random-between 0 *ed-r*) n))
               (stop  (usec:get-time-usec)))
          (setf (aref samp ix) (* 1e-6 (- stop start))
                (aref isamp ix) (getf ans :niter))))
      (setf (aref mn n) (vm:median samp)
            (aref mni n) (vm:median isamp)))

    (setf *mn* mn
          *mni* mni
          *sd* sd
          *sdi* sdi)
    (show-stats nmax mn sd)))

(defun show-stats (nmax mn sd)    
    (plt:plot 'plt mn :clear t
              :symbol :circle
              :ylog   t
              :title  "HashCash Timings"
              :xtitle "Nbr Bits"
              :ytitle "Delay [sec]"
              :logo    nil
              :cright1 nil
              :cright2 nil)
    #|
    (plt:plot-bars 'plt
                   (list (map 'vector '- mn sd)
                         (map 'vector '+ mn sd)))
    |#
    (multiple-value-bind (xmn ywmn slope wsigma niter)
        (linfit:regression (coerce (um:range 0 nmax) 'vector)
                           (map 'vector (um:rcurry 'log 10) mn)
                           1)
      (plt:fplot 'plt `(0 ,nmax) (lambda (x)
                                   (expt 10 (+ ywmn (* slope (- x xmn)))))
                 :color :red
                 :alpha 0.75)
      (plt:draw-text-box 'plt (list
                               (format nil "slope = ~,3F" slope)
                               (format nil "xmn = ~F" xmn)
                               (format nil "ymn = ~,3F" (expt 10 ywmn))
                               (format nil "Delay = ~,2F*10^(~,2F*(nbits - ~F))" (expt 10 ywmn) slope xmn))
                         '(:frac 0.1)
                         '(:frac 0.9)
                         :filled t
                         :alpha  0.75)
      (list :xmn   xmn
            :ywmn  (expt 10 ywmn)
            :slope slope)))

(gather-stats 16 :nsamp 9)

(gather-stats 20 :nsamp 5)

(let* ((xs (um:range 0 10))
       (ym (map 'vector (um:curry '* 2) xs))
       (ys (map 'vector (constantly 0.5) xs)))
  (plt:plot 'plt xs ym :clear t
            :symbol :circle)
  (plt:plot-bars 'plt
                 (list (map 'vector '- ym ys) (map 'vector '+ ym ys))))
      
;; -------------------------------------------------------------------

(defvar *pkey* (ed-random-generator))
(defvar *a*    (random-between 0 *ed-r*))

(defun ecdly (p)
  (um:nlet itera ((a 0))
    (when (< a 1000)
      (let ((a (+ a *a*)))
        (um:nlet itern ((n 1)
                             (px *pkey*))
          (when (< n 1000)
            (let ((new-px (ed-add (ed-mul px a) *ed-gen*)))
              (if (ed-pt= p new-px)
                  (return-from ecdly (list a n))
                (go-itern (1+ n) new-px)))))
        ))
    (go-itera (1+ a)))
  (error "Can't find a solution"))

(with-mod *ed-r*
  (let* ((a (+ 251 *a* ))
         (n 294)
         (p (ed-add
             (ed-mul *pkey* (m^ a n))
             (ed-mul *ed-gen* (m/ (m- (m^ a n) 1)
                                  (m- a 1))))))
    (time (ecdly p))))

;; ---------------------------------------------------------------------------------
#|
;; from Rust implementation
;; Debug Timings
(let* ((str #>.end
Delay for 1 bits: 51탎
Delay for 2 bits: 50탎
Delay for 3 bits: 162탎
Delay for 4 bits: 153탎
Delay for 5 bits: 1.827ms
Delay for 6 bits: 554탎
Delay for 7 bits: 964탎
Delay for 8 bits: 1.355ms
Delay for 9 bits: 6.955ms
Delay for 10 bits: 11.335ms
Delay for 11 bits: 40.417ms
Delay for 12 bits: 118.476ms
Delay for 13 bits: 236.791ms
Delay for 14 bits: 587.896ms
Delay for 15 bits: 613.086ms
Delay for 16 bits: 1.320912s
Delay for 17 bits: 4.568054s
Delay for 18 bits: 10.247565s
Delay for 19 bits: 11.892412s
Delay for 20 bits: 19.648837s
.end)
      (lines (um:split-string str :delims (list #\newline)))
      (pairs (mapcar (lambda (line)
                       (multiple-value-bind (start end vstart vend)
                           (#~m"^Delay for ([0-9]+) bits: ([0-9]+[\\.[0-9]*]?)" line)
                         (let ((units (case (aref line (aref vend 1))
                                        (#\s 1)
                                        (#\m 1e-3)
                                        (t   1e-6))))
                           (list (read-from-string line t nil :start (aref vstart 0) :end (aref vend 0))
                                 (* units (read-from-string line t nil :start (aref vstart 1) :end (aref vend 1)))))))
                     lines))
      (nbits        (mapcar 'first pairs))
      (med3-timings (mapcar 'second pairs)))
  (plt:plot 'plt nbits med3-timings
            :clear t
            :symbol :circle
            :ylog   t
            :title  "Rust HashCash Debug Timings"
            :xtitle "Nbr Bits"
            :ytitle "(Median of 3) Delay [sec]"
            :logo    nil
            :cright1 nil
            :cright2 nil)
  (multiple-value-bind (xmn ywmn slope wsigma niter)
      (linfit:regression (coerce nbits 'vector)
                         (map 'vector (um:rcurry 'log 2) med3-timings)
                         1)
    (plt:fplot 'plt `(1 ,20) (lambda (x)
                                 (expt 2 (+ ywmn (* slope (- x xmn)))))
               :color :red
               :alpha 0.75)
    (plt:draw-text-box 'plt (list
                             (format nil "slope = ~,3F" slope)
                             (format nil "xmn = ~F" xmn)
                             (format nil "ymn = ~,3F" (expt 2 ywmn))
                             (format nil "Delay = ~,2F*2^(~,2F*(nbits - ~F))" (expt 2 ywmn) slope xmn))
                       '(:frac 0.1)
                       '(:frac 0.9)
                       :filled t
                       :alpha  0.75)
    ))
            
;; Release Timings
(let* ((str #>.end
Delay for 1 bits: 1탎
Delay for 2 bits: 1탎
Delay for 3 bits: 5탎
Delay for 4 bits: 13탎
Delay for 5 bits: 44탎
Delay for 6 bits: 5탎
Delay for 7 bits: 100탎
Delay for 8 bits: 75탎
Delay for 9 bits: 276탎
Delay for 10 bits: 426탎
Delay for 11 bits: 1.379ms
Delay for 12 bits: 398탎
Delay for 13 bits: 1.429ms
Delay for 14 bits: 1.531ms
Delay for 15 bits: 5.361ms
Delay for 16 bits: 85.641ms
Delay for 17 bits: 37.981ms
Delay for 18 bits: 97.96ms
Delay for 19 bits: 82.564ms
Delay for 20 bits: 707.936ms
.end)
      (lines (um:split-string str :delims (list #\newline)))
      (pairs (mapcar (lambda (line)
                       (multiple-value-bind (start end vstart vend)
                           (#~m"^Delay for ([0-9]+) bits: ([0-9]+[\\.[0-9]*]?)" line)
                         (let ((units (case (aref line (aref vend 1))
                                        (#\s 1)
                                        (#\m 1e-3)
                                        (t   1e-6))))
                           (list (read-from-string line t nil :start (aref vstart 0) :end (aref vend 0))
                                 (* units (read-from-string line t nil :start (aref vstart 1) :end (aref vend 1)))))))
                     lines))
      (nbits        (mapcar 'first pairs))
      (med3-timings (mapcar 'second pairs)))
  (plt:plot 'plt nbits med3-timings
            :clear t
            :symbol :circle
            :ylog   t
            :title  "Rust HashCash Release Timings"
            :xtitle "Nbr Bits"
            :ytitle "(Median of 3) Delay [sec]"
            :logo    nil
            :cright1 nil
            :cright2 nil)
  (multiple-value-bind (xmn ywmn slope wsigma niter)
      (linfit:regression (coerce nbits 'vector)
                         (map 'vector (um:rcurry 'log 2) med3-timings)
                         1)
    (plt:fplot 'plt `(1 ,20) (lambda (x)
                                 (expt 2 (+ ywmn (* slope (- x xmn)))))
               :color :red
               :alpha 0.75)
    (plt:draw-text-box 'plt (list
                             (format nil "slope = ~,3F" slope)
                             (format nil "xmn = ~F" xmn)
                             (format nil "ymn = ~,4F" (expt 2 ywmn))
                             (format nil "Delay = ~,4F*2^(~,2F*(nbits - ~F))" (expt 2 ywmn) slope xmn))
                       '(:frac 0.1)
                       '(:frac 0.9)
                       :filled t
                       :alpha  0.75)
    ))
            
|#

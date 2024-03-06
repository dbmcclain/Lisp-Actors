
(in-package :edec-ff)

;; -----------------------------------------------------------------------------
;; Elligator encoding of curve points

(defun elligator-limit ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-limit *edcurve*
                          (lambda ()
                            (with-embedding-field
                              (floor (1+ (field-base)) 2.)))))

(defun elligator-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbits *edcurve*
                          (lambda ()
                            (integer-length (1- (elligator-limit))))))

(defun elligator-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbytes *edcurve*
                          (lambda ()
                            (ceiling (elligator-nbits) 8.))))

(defun elligator-int-padding ()
  ;; generate random padding bits for an elligator int
  (let* ((enb   (elligator-nbits))
         (nbits (mod enb 8)))
    (if (zerop nbits)
        0
      (ash (ctr-drbg-int (- 8. nbits)) enb))
    ))

(defun elligator-body (enc)
  (let ((enb  (elligator-nbits)))
    (ldb (byte enb 0) enc)))

(defun refresh-elligator (enc)
  (logior (elligator-body enc) (elligator-int-padding)))

(defun compute-csr ()
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (let* ((dp1  (ff+ *ed-d* 1))
         (dm1  (ff- *ed-d* 1))
         (dsqrt (ff* 2. (ffsqrt (ff- *ed-d*))))
         (c    (ff/ (ff+ dsqrt dm1) dp1))
         (c    (if (ff-quadratic-residue-p c)
                   c
                 (ff/ (ff- dsqrt dm1) dp1)))
         (r    (ff+ c (ff/ c)))
         (s    (ffsqrt (ff/ 2. c))))
    (list c s r )
    ))

(defun csr ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator-csr *edcurve*
                          'compute-csr))

(defun to-elligator-range (x)
  (ldb (byte (elligator-nbits) 0) (int x)))

(defun elligator-decode (z)
  ;; z in (1,2^(floor(log2 *ed-q*/2)))
  ;; good multiple of bytes for curve-1174 is 248 bits = 31 bytes
  ;;                            curve-E382    376        47
  ;;                            curve-41417   408        51
  ;;                            curve-E521    520        65
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (with-embedding-field
    (let ((z (to-elligator-range z)))
      (declare (integer z))
      (cond ((= z 1)
             (ed-neutral-point))
          
            (t
             (um:bind* (((c s r) (csr))
                        (u     (ff/ (ff- 1 z) (ff+ 1 z)))
                        (u^2   (ff* u u))
                        (c^2   (ff* c c))
                        (u2c2  (ff+ u^2 c^2))
                        (u2cm2 (ff+ u^2 (ff/ c^2)))
                        (v     (ff* u u2c2 u2cm2))
                        (chiv  (ff-chi v))
                        (xx    (ff* chiv u))
                        (yy    (ff* (ff^ (ff* chiv v) (truncate (1+ (field-base)) 4))
                                    chiv
                                    (ff-chi u2cm2)))
                        (1+xx  (ff+ 1 xx))
                        (x     (ff/ (ff* (ff- c 1)
                                         s
                                         xx
                                         1+xx)
                                    yy))
                        (y     (ff/ (ff- (ff* r xx)
                                         (ff* 1+xx 1+xx))
                                    (ff+ (ff* r xx)
                                         (ff* 1+xx 1+xx))))
                        (pt    (make-ecc-pt
                                :x  (int x)
                                :y  (int y))))
               ;; (assert (ed-satisfies-curve pt))
               pt
               ))
            ))))
 
(defun elligator-encode (pt)
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  ;; return encoding tau for point pt, or nil if pt not in image of phi(tau)
  (if (ed-neutral-point-p pt)
      (logior 1 (elligator-int-padding))
    ;; else
    (with-embedding-field
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 (yp1  (ff+ 1 y)))
        (unless (ff= 0 yp1)
          (um:bind* (((c s r) (csr))
                     (etar       (ff* r (ff/ (ff- y 1) (ff* 2 yp1))))
                     (etarp1     (ff+ 1 etar))
                     (etarp1sqm1 (ff- (ff* etarp1 etarp1) 1))
                     (scm1       (ff* s (ff- c 1))))
            (when (and (ff-quadratic-residue-p etarp1sqm1)
                       (or (not (ff= 0 (ff+ etar 2.)))
                           (ff= x (ff/ (ff* 2 scm1 (ff-chi c))
                                       r))))
              (um:bind* ((xx    (ff- (ff^ etarp1sqm1 (floor (1+ (field-base)) 4.))
                                     etarp1))
                         (z     (ff-chi (ff* scm1
                                             xx
                                             (ff+ 1 xx)
                                             x
                                             (ff+ (ff* xx xx) (ff/ (ff* c c))))))
                         (u     (ff* z xx))
                         (enc   (ff/ (ff- 1 u) (ff+ 1 u)))
                         (tau   (min (int enc) (int (ff- enc)))))
                ;; (assert (ed-pt= pt (elligator-decode enc))) ;; check that pt is in the Elligator set
                ;; (assert (< tau (elligator-limit)))
                (logior tau (elligator-int-padding))
                ))
            ))))
    ))

;; -------------------------------------------------------

(defun compute-deterministic-elligator-skey (&rest seeds)
  ;; compute a private key from the seed that is safe, and produces an
  ;; Elligator-capable public key.
  (labels ((helper (seed index)
             (let* ((skey (compute-deterministic-skey seed index))
                    (pkey (ed-nth-pt skey))
                    #|
                     (_    (assert (not (and (ff= 0 (ecc-pt-y pkey))     ;; for tracking down
                                             (ff= 0 (ecc-pt-x pkey)))))) ;; problem with curve-e521f
                     |#
                    (tau  (elli2-encode pkey)))
               (declare (ignore _))
               (if tau
                   (values skey tau index)
                 (helper seed (1+ index)))
               )))
    (helper seeds 0)
    ))

(defun compute-elligator-summed-pkey (sum-pkey)
  ;; post-processing step after summing public keys. This corrects the
  ;; summed key to become an Elligator-capable public key. Can only be
  ;; used on final sum, not on intermediate partial sums.
  (um:nlet iter ((ix 0))
    (let ((p  (ed-add sum-pkey (ed-nth-pt ix))))
      (or (elli2-encode p)
          (go-iter (1+ ix))))))
#|
(multiple-value-bind (skey1 pkey1) (compute-elligator-skey :dave)
  (multiple-value-bind (skey2 pkey2) (compute-elligator-skey :dan)
    (let ((p  (ed-add pkey1 pkey2)))
      (compute-elligator-summed-pkey p))))

(defun tst (nel)
  (let ((ans nil)
        (dict (make-hash-table)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (skey tau ct)
              (compute-elligator-skey (ed-convert-int-to-lev ix 4))
            (if (gethash skey dict)
                (print "Collision")
              (setf (gethash skey dict) tau))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#
             
(defun compute-elligator-schnorr-deterministic-random (msgv k-priv)
  (um:nlet iter ((ix 0))
    (let* ((r     (with-curve-field
                    (normalize
                   (hash/512
                    (levn ix 4.)
                    (levn k-priv (elligator-nbytes))
                      msgv))))
           (rpt   (ed-nth-pt r))
           (tau-r (elli2-encode rpt)))
      (if (and (plusp r) tau-r)
          (values r tau-r ix)
        (go-iter (1+ ix)))
      )))

#|
(defun tst (nel)
  (let ((ans  nil)
        (skey (compute-elligator-skey :dave)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (r rpt ct)
              (compute-elligator-schnorr-deterministic-random
               (ed-convert-int-to-lev ix 4) skey)
            (declare (ignore r rpt))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#

(defun elligator-ed-dsa (msg k-priv)
  (let ((msg-enc (lev (loenc:encode msg)))
        (tau-pub (elli2-encode (ed-nth-pt k-priv))))
    (unless tau-pub
      (error "Not an Elligator key"))
    (multiple-value-bind (r tau-r)
        (compute-elligator-schnorr-deterministic-random msg-enc k-priv)
      (let* ((nbytes (elligator-nbytes))
             (s      (with-curve-field
                       (int
                        (ff+ r
                             (ff* k-priv
                                  (ff-normalize
                                   (hash/512
                                    (levn tau-r nbytes)
                                    (levn tau-pub nbytes)
                                    msg-enc))
                                  ))))))
        (list
         :msg     msg
         :tau-pub tau-pub
         :tau-r   tau-r
         :s       s)
        ))))

(defun elligator-ed-dsa-validate (msg tau-pub tau-r s)
  (let ((nbytes (elligator-nbytes)))
    (ed-pt=
     (ed-nth-pt s)
     (ed-add (elli2-decode tau-r)
             (ed-mul (elli2-decode tau-pub)
                     (int
                      (hash/512
                       (levn tau-r   nbytes)
                       (levn tau-pub nbytes)
                       (lev (loenc:encode msg))))
                     )))))

;; ------------------------------------------------------------

(defun do-elligator-random-pt (fn-gen)
  ;; search for a random multiple of *ed-gen*
  ;; such that the point is in the Elligator set.
  ;; Return a property list of
  ;;  :r   = the random integer in [1,q)
  ;;  :pt  = the random point in projective form
  ;;  :tau = the Elligator encoding of the random point
  ;;  :pad = bits to round out the integer length to multiple octets
  (um:nlet iter ()
    (multiple-value-bind (skey pkey) (ed-random-pair)
      (let ((tau  (and (plusp skey)
                       (funcall fn-gen pkey)))) ;; elligatorable? - only about 50% are
        (if tau
            (list :r   skey
                  :tau tau)
          (go-iter))
        ))))

(defun elligator-tau-vector (tau)
  ;; lst should be the property list returned by elligator-random-pt
  (levn tau (elligator-nbytes)))

(defun do-elligator-schnorr-sig (msg tau-pub k-priv fn-gen)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:nlet iter ()
    (let* ((lst   (funcall fn-gen))
           (vtau  (elligator-tau-vector (getf lst :tau)))
           (h     (int
                   (hash/512
                    vtau
                    (elligator-tau-vector tau-pub)
                    msg)))
           (r     (getf lst :r))
           (s     (with-curve-field
                    (int (ff+ r (ff* h k-priv)))))
           (smax  (elligator-limit)))
      (if (>= s smax)
          (progn
            ;; (print "restart ed-schnorr-sig")
            (go-iter))
        (let* ((spad  (logior s (elligator-int-padding)))
               (svec  (elligator-tau-vector spad)))
          (list vtau svec))
        ))))

(defun do-elligator-schnorr-sig-verify (msg tau-pub sig fn-decode)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:bind* (((vtau svec) sig)
             (pt-pub (funcall fn-decode tau-pub))
             (pt-r   (funcall fn-decode (int vtau)))
             (h      (int
                      (hash/512
                       vtau
                       (elligator-tau-vector tau-pub)
                       msg)))
             (s      (to-elligator-range (int svec)))
             (pt     (ed-nth-pt s))
             (ptchk  (ed-add pt-r (ed-mul pt-pub h))))
    (ed-pt= pt ptchk)))

;; -------------------------------------------------------

(defun elligator-random-pt ()
  (do-elligator-random-pt #'elligator-encode))

(defun ed-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elligator-random-pt))

(defun ed-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elligator-decode))

;; -------------------------------------------------------

#|
(defun chk-elligator ()
  (loop repeat 1000 do
        ;; ix must be [0 .. (q-1)/2]
        (let* ((ix (random-between 0 (floor (1+ *ed-q*) 2)))
               (pt (elligator-decode ix))
               (jx (elligator-encode pt)))
          (assert (= ix jx))
          )))
(chk-elligator)

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (ed-schnorr-sig msg tau-pub k-priv)))
   (ed-schnorr-sig-verify msg tau-pub sig))

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (elli2-schnorr-sig msg tau-pub k-priv)))
   (elli2-schnorr-sig-verify msg tau-pub sig))

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elligator-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )
        
                      
|#

#|
(with-mod *ed-q*
  (let* ((c4d  (m* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
         (a    (m/ (m* 2 (1+ c4d)) (- c4d 1)))
         (b    1))
    (m* a b (- (m* a a) (m* 4 b))))) ;; must not be zero
|#

;; --------------------------------------------------------

(defun find-quadratic-nonresidue ()
  (um:nlet iter ((n  -1))
    (if (ff-quadratic-residue-p n)
        (go-iter (1- n))
      n)))

(defun compute-elli2-ab ()
  ;; For Edwards curves:  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  ;; x --> -2*c*u/v
  ;; y --> c*(1+u)/(1-u)
  ;; to get Elliptic curve: v^2 = (c^4*d -1)*(u^3 + A*u^2 + B*u)
  ;; v --> w*Sqrt(c^4*d - 1)
  ;; to get: w^2 = u^3 + A*u^2 + B*u
  ;; we precompute c4d = c^4*d, A = 2*(c^4*d+1)/(c^4*d-1), and B = 1
  ;; must have: A*B*(A^2 - 4*B) != 0
  (let* ((c4d        (ff* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
         (c4dm1      (ff- c4d 1))
         (sqrt-c4dm1 (ffsqrt c4dm1)) ;; used during coord conversion
         (a          (ff/ (ff* 2. (ff+ c4d 1)) c4dm1))
         (b          1)
         (u          (find-quadratic-nonresidue))
         (dscr       (ff- (ff* a a) (ff* 4 b))))
    ;; (assert (not (ff-quadratic-residue-p (field-base) dscr)))
    (assert (not (ff= 0 (ff* a b dscr))))
    (list sqrt-c4dm1 a b u)))
  
(defun elli2-ab ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator2-ab *edcurve*
                          'compute-elli2-ab))

(defun montgy-pt-to-ed (pt)
  ;; v = w * Sqrt(c^4*d-1)
  ;; x = -2*c*u/v
  ;; y = c*(1+u)/(1-u)
  ;; w^2 = u^3 + A*u^2 + B =>  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (with-embedding-field
    (um:bind* ((:struct-accessors ecc-pt ((xu x)
                                          (yw y)) pt))
      (if (and (ff= 0 xu)
               (ff= 0 yw))
          (ed-neutral-point)
        ;; else
        (destructuring-bind (sqrt-c4dm1 a b u) (elli2-ab)
          (declare (ignore a b u))
          (let* ((yv   (ff* sqrt-c4dm1 yw))
                 (x    (ff/ (ff* -2. *ed-c* xu) yv))
                 (y    (ff/ (ff* *ed-c* (ff+ 1 xu)) (ff- 1 xu)))
                 (pt   (make-ecc-pt
                        :x  (int x)
                        :y  (int y))))
            (assert (ed-satisfies-curve pt))
            pt ))
        ))))
              
(defun elli2-decode (r)
  ;; protocols using the output of elli2-decode must validate the
  ;; results. All output will be valid curve points, but many
  ;; protocols must avoid points of low order and the neutral point.
  ;;
  ;; Elli2-encode will provide a value of 0 for the neutral point.
  ;; But it will refuse to generate a value for the other low order
  ;; torsion points.
  ;;
  ;; However, that doesn't prevent an attacker from inserting values
  ;; for them.  There are no corresponding values for the low order
  ;; torsion points, apart from the neutral point. But certain random
  ;; values within the domain [0..(q-1)/2] are invalid.
  ;;
  (let ((r (to-elligator-range r))) ;; mask off top random
    (declare (integer r))
    (cond  ((zerop r)  (ed-neutral-point))
           (t
            (with-embedding-field
              (um:bind* (((sqrt-c4dm1 a b u) (elli2-ab))
                         (u*r^2   (ff* u r r))
                         (1+u*r^2 (ff+ 1 u*r^2)))
                
                ;; the following error can never trigger when fed with
                ;; r from elli2-encode. But random values fed to us
                ;; could cause it to trigger the error.
                
                (when (or (ff= 0 1+u*r^2)     ;; this could happen for r^2 = -1/u
                          (ff= (ff* a a u*r^2)   ;; this can never happen: B=1 so RHS is square
                               (ff* b 1+u*r^2 1+u*r^2)))  ;; and LHS is not square.
                  (error "invalid argument"))
                
                (let* ((v    (ff- (ff/ a 1+u*r^2)))
                       (eps  (ff-chi (ff+ (ff* v v v)
                                          (ff* a v v)
                                          (ff* b v))))
                       (xu   (ff- (ff* eps v)
                                  (ff/ (ff* (ff- 1 eps) a) 2.)))
                       (rhs  (ff* xu
                                  (ff+ (ff* xu xu)
                                       (ff* a xu)
                                       b)))
                       (yw   (ff- (ff* eps (ffsqrt rhs))))
                       ;; now we have (xu, yw) as per Bernstein: yw^2 = xu^3 + A*xu^2 + B*xu
                       ;; Now convert- to our Edwards coordinates:
                       ;;   (xu,yw) --> (x,y): x^2 + y^2 = c^2*(1 + d*x^2*y^2)
                       (yv   (ff* sqrt-c4dm1 yw))
                       (x    (ff/ (ff* -2. *ed-c* xu) yv))
                       (y    (ff/ (ff* *ed-c* (ff+ 1 xu)) (ff- 1 xu)))
                       (pt   (make-ecc-pt
                              :x  (int x)
                              :y  (int y))))
                  #|
                  (assert (ed-satisfies-curve pt)) ;; true by construction
                  (assert (not (ff= 0 (ff* v eps xu yw)))) ;; true by construction
                  (assert (ff= (ff* yw yw) rhs))     ;; true by construction
                  |#
                  pt
                  ))))
           )))
  
(defun ed-pt-to-montgy (pt)
  ;; u = (y - c)/(y + c)
  ;; v = -2 c u / x
  ;; w = v / sqrt(c^4 d - 1)
  ;; montgy pt (u,w) in: w^2 = u^3 + A u^2 + B u
  (if (ed-neutral-point-p pt)
      (make-ecc-pt
       :x 0
       :y 0)
    ;; else
    (with-embedding-field
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 ((sqrt-c4dm1 a b u) (elli2-ab))
                 (declare (ignore u))
                 (xu   (ff/ (ff- y *ed-c*) (ff+ y *ed-c*)))
                 (yv   (ff/ (ff* -2. *ed-c* xu) x ))
                 (yw   (ff/ yv sqrt-c4dm1)))
        (assert (ff= (ff* yw yw)
                     (ff+ (ff* xu xu xu)
                          (ff* a xu xu)
                          (ff* b xu))))
        (make-ecc-pt
         :x (int xu)
         :y (int yw))
        ))))
             
(defun elli2-encode (pt)
  ;; Elligator2 mapping of pt to Zk
  ;; return Zk or nil
  (cond ((ed-neutral-point-p pt)
         (elligator-int-padding))
        (t
         (with-embedding-field
           (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                      ((sqrt-c4dm1 a b u) (elli2-ab))
                      (declare (ignore b))
                      ;; convert our Edwards coords to the form needed by Elligator-2
                      ;; for Montgomery curves
                      (xu   (ff/ (ff- y *ed-c*) (ff+ y *ed-c*)))
                      (yv   (ff/ (ff* -2. *ed-c* xu) x ))
                      (yw   (ff/ yv sqrt-c4dm1))
                      (xu+a (ff+ xu a)))
             ;; now we have (x,y) --> (xu,yw) for:  yw^2 = xu^3 + A*xu^2 + B*xu
             #|
             (labels ((esqrt (x)
                        (cond ((= 3 (mod *ed-q* 4))
                               (m^ x (floor (1+ *ed-q*) 4)))
                              ((= 5 (mod *ed-q* 8))
                               (m^ x (floor (+ 3 *ed-q*) 8)))
                              (t
                               (error "NYI"))
                              )))
               (cond ((zerop xu)
                      (elligator-int-padding))
                     ((zerop yw)
                      (elligator-int-padding))
                     ((zerop xu+a)
                      (elligator-int-padding))
                     ((quadratic-residue-p yw)
                      (let ((r (esqrt (- (m/ xu xu+a u)))))
                        (logior (min r (m- r))
                                (elligator-int-padding))))
                     (t
                      (let ((r (esqrt (- (m/ xu+a xu u)))))
                        (logior (min r (m- r))
                                (elligator-int-padding))))
                     |#
                     #||#
                  (when (and (not (ff= 0 xu+a))
                             (or (not (ff= 0 yw))
                                 (ff= 0 xu))
                             (ff-quadratic-residue-p (ff- (ff* u xu xu+a))))
                    (let* ((e2    (if (ff-quadratic-residue-p yw)
                                      (ff/ xu xu+a)
                                    (ff/ xu+a xu)))
                           (enc   (ffsqrt (ff/ e2 (ff- u))))
                           (tau   (min (int enc) (int (ff- enc))))
                           ;; (ur2   (m* u tau tau))
                           ;; (1pur2 (1+ ur2))
                           )
                      
                      ;; (assert (< tau (elligator-limit)))
                      #|
                     (when (zerop 1pur2) ;; never happens, by construction
                       (format t "~%Hit magic #1: tau = ~A" tau))
                     (when (= (m* a a ur2) ;; never happens, by construction
                              (m* b 1pur2 1pur2))
                       (format t "~%Hit magic #2: tau = ~A" tau))
                     
                     (unless (or (zerop 1pur2) ;; never happens, by construction
                                 (= (m* a a ur2)
                                    (m* b 1pur2 1pur2)))
                       tau)
                     |#
                      (logior tau (elligator-int-padding))
                      ))
                  #||#
                  )))))

(defun elli2-random-pt ()
  (do-elligator-random-pt #'elli2-encode))

(defun elli2-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elli2-random-pt))

(defun elli2-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elli2-decode))

;; ------------------------------------------------------------------------------
;; General scheme for creating private / public keys with Elligator encodings...
;; (elli2-random-pt) => property list with (getf ans :r) = private key integer
;;                                         (+ (getf ans :tau)
;;                                            (getf ans :padding)) = public Elligator integer
;; ------------------------------------------------------------------------------
#|
(defun chk-elli2 ()
  (loop repeat 100 do
        (let* ((lst (elli2-random-pt))
               (pt  (ed-affine (getf lst :pt)))
               (tau (getf lst :tau))
               (pt2 (ed-affine (elli2-decode tau))))
          (assert (ed-pt= pt pt2))
          )))
(chk-elli2)

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elli2-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )

;; pretty darn close to 50% of points probed
;; result in successful Elligator-2 encodings
(let ((cts 0))
  (loop repeat 1000 do
        (let* ((ix (field-random (* *ed-h* *ed-r*)))
               (pt (ed-mul *ed-gen* ix)))
          (when (elli2-encode pt)
            (incf cts))))
  (/ cts 1000.0))
|#
             

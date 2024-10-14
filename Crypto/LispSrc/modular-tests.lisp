
(in-package :finite-field)

(inspect '*field*)

(define-ffield curve1174-embedding-field edec:*ed-q*)

(with-standard-io-syntax
  (print (make-load-form (ffld-base (curve1174-embedding-field 15))))
  (values))

(hash:hash/256 (ffld-base (curve1174-embedding-field 15)))

(setf *print-base* 16.)
(setf *print-base* 10.)
(setf um:*print-bignum-abbrev* nil)
(setf um:*print-bignum-abbrev* t)
(setf edec-ff::*use-fast-impl* nil)
(setf edec-ff::*use-fast-impl* t)


(let* ((x    15)
       (pt   (edec:ed-mul edec:*ed-gen* x))
       (p2   (edec:ed-affine (edec:ed-div pt x))))
  (edec:ed-pt= p2 edec:*ed-gen*)))
    
(let* ((x    15)
       (pt   (edec:ed-mul edec:*ed-gen* x))
       (p1   (edec:ed-add pt pt))
       (p2   (edec:ed-affine (edec:ed-mul pt 2))))
  (edec:ed-pt= p1 p2))
    
(with-field 'curve1174-embedding-field
  (ff^ (hash:hash/256 :test) 13))

(with-field 'curve1174-embedding-field
  (ff* 2 (ff/ 2)))

(with-field 'curve1174-embedding-field
  (let ((x (ff^ 4 (ash (1+ edec:*ed-q*) -2))))
    (ffsqrt 16)))

(with-field 'curve1174-embedding-field
  (ffsqrt (ash 1 10240)))


(with-field 'curve1174-embedding-field
  (um:without-abbrev
    (print
     ;; (ffsqr (ff^ 2 (ash (1+ edec:*ed-q*) -2)))
     (ffsqrt 2))
    (values)))

(with-field 'curve1174-embedding-field
  (um:without-abbrev (print (ff* (ff-to-montgy 2) 2))
    (values))) ;; (- edec:*ed-q* 2)))

(with-field 'curve1174-embedding-field
  (um:without-abbrev (print (ffinv 2)) (values)))

(with-field 'curve1174-embedding-field
  (inspect (copy-ffld 2)))

(with-field 'curve1174-embedding-field
  (let* ((x  (copy-ffld -1)))
  (um:without-abbrev
    (print (ffsqrt (ff+ x x x x x x x x x x x x x x x x x x x)))
    (values))))


(with-field 'curve1174-embedding-field
  (let ((range (* edec:*ed-q* edec:*ed-q*))
        (niter 1_000_000))
    (print "Test elaborate normalize")
    (time
     (loop repeat niter do
             (let ((x (random range)))
               (basic-normalize *field* x))))
    (print "Test basic normalize")
    (time
     (loop repeat niter do
           (let ((x (random range)))
             (mod x (ffbase-base *field*)))))
    (print "Time the overhead")
    (time
     (loop repeat niter do
           (let ((x (random range)))
             (lw:do-nothing x *field*))))
    ))

;; ---------------------------------------------

(edec:ed-affine (edec:ed-decompress-pt (edec:ed-compress-pt edec:*ed-gen*)))

(let ((tau (edec:elligator-encode edec:*ed-gen*)))
  (edec:elligator-decode tau))

(let ((tau (edec:elli2-encode edec:*ed-gen*)))
  (edec:elli2-decode tau))

(let+ ((:mvb (skey tau) (edec:compute-deterministic-elligator-skey :test)))
  (edec:elli2-decode tau))

;; -----------------------------------------------

(with-field 'curve1174-embedding-field
  (let* ((base  (field-base))
         (nbits (integer-length base))
         (bits  (byte nbits 0))
         (r     (random base))
         (rsq   (* r r)))
    (multiple-value-bind (gcd ninv rinv)
        (ff-bezout base (ash 1 nbits))
      (let* ((lo  (ldb bits rsq))
             (q   (ldb bits (* lo ninv)))
             (qb  (* q base))
             (diff (- rsq qb))
             (sh   (ash diff (- nbits))))
        (with-standard-io-syntax
          (let ((*print-base* 16.))
            (pprint (list
                    :r    r
                    :ninv ninv
                    :lo   lo
                    :q    q
                    :rsq  rsq
                    :qb   qb
                    :diff diff
                    :sh   sh))))
        (values)
        ))))


Bezout Identity:   There exist integers, a & b, such that: a*r + b*q = gcd(r,q)
If r = 2^N, and q is prime: 2^(N-1) < q < 2^N, then gcd(r,q) = 1.
a*2^N + b*q = 1, so a = 1/(2^N) mod q, and b = 1/q mod 2^N.

In Modulo q, for some x = k + n*q, 0 <= k < q, and n arbitrary:
x mod q = k
x mod 2^N = k mod 2^N + n*q mod 2^N. But since k < q, and q < 2^N, k mod 2^N = k.
x mod 2^N = k + n*q mod 2^N.
But we now from Bezout that b*q mod 2^N = 1

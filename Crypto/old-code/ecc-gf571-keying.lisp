;; ecc-gf571-keying.lisp -- NIST B-571 Elliptic Curve Crypto
;; DM/Acudora  11/11
;;------------------------------------------------
;; ECC over the Field F(2^571)
;; based on underlying polynomial field GF(2^571)

(defun gf+ (&rest args)
  (apply #'logxor args))

(defvar *nbits* 571)

(defvar *gf-order* (1- (ash 1 *nbits*)))

(defvar $prim
  ;; B571: t^571 + t^10 + t^5 + t^2 + 1
  (logior (ash 1   *nbits*)
          (ash 1   10)
          (ash 1   5)
          (ash 1   2)
          1))

#|
 ;; for testing
(defun make-gf8 (prim)
  (setf *nbits* 8
        $prim   prim
        *gf-order* (1- (ash 1 8))
        *gf-inv-order* (1- *gf-order*)))
|#

(defun step-state (state)
  (let ((new-state (ash state 1)))
    (if (logbitp *nbits* new-state)
        (logxor new-state $prim)
      new-state)))

(defun gf* (a b)
  (do ((ans  0)
       (x    a   (step-state x))
       (limit (integer-length b))
       (mask 0   (1+ mask)))
      ((>= mask limit) ans)
    (when (logbitp mask b)
      (setf ans (logxor ans x))) ))

(defun gf^ (x n)
  (labels ((expt (n)
             (do ((ans   1)
                  (limit (integer-length n))
                  (expon 0  (1+ expon))
                  (mul   x  (gf* mul mul)))
                 ((>= expon limit) ans)
               (when (logbitp expon n)
                 (setf ans (gf* ans mul))))))
    (if (minusp n)
        (gf1/x (expt (- n)))
      (expt n))))
      

(defun gf1/x (x)
  (gf^ x #.(1- *gf-order*)))

(defun gf/ (a b)
  (gf* a (gf1/x b)))

(defun gf^2 (x)
  (gf* x x))

;; -----------------------------------------

;; -----------------------------------------

(defmethod convert-text-to-int8-array ((str string))
  (let ((ans (make-array (length str)
                         :element-type '(unsigned-byte 8))))
    (map-into ans #'char-code str)
    ans))

(defmethod convert-text-to-int8-array (x)
  x)

(defun convert-text-to-int571 (str)
  (convert-vector-to-int
   (convert-text-to-int8-array str)))

(defun convert-vector-to-int (bytes)
  (let* ((len (length bytes))
         (ans 0))
    (loop for ix from 0 below len
          do
          (setf ans (logior (ash ans 8) (aref bytes ix))))
    ans))

(defun convert-int571-to-int8-array (x)
  (coerce
   (loop for ix from 71 downto 0 collect
         (ldb (byte 8 (* 8 ix)) x))
   '(vector (unsigned-byte 8) *)))

(defun hashn (arr n)
  (let ((ans (make-array 32
                         :initial-element 0
                         :element-type '(unsigned-byte 8))))
    (loop repeat n do
          (let ((dig (ironclad:make-digest :sha256)))
            (ironclad:update-digest dig ans)
            (ironclad:update-digest dig arr)
            (setf ans (ironclad:produce-digest dig))))
    ans))

#|
(defun make-dummy-key ()
  (convert-vector-to-int (hashn (uuid:uuid-to-byte-array (uuid:make-v1-uuid)) 8192)))
|#

;; ---------------------------------------------------------------------------------
;; ECC Routines over NIST B-571

(defvar *ecc-gen*
  (list
   #x303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19
   #x37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b ))

(defvar *ecc-a*  1)

(defvar *ecc-b*
  #x2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a )

(defvar *ecc-r*
  ;; prime order of the additive group  (r * pt = infinity, for all pt in group)
  3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285703 )

(defun ecc-negate (pt)
  (destructuring-bind (x y) pt
    (list x (gf+ x y))))

(defun ecc-infinite-p (pt)
  (zerop (car pt)))

(defun ecc-double (pt)
  (if (ecc-infinite-p pt)
      pt
  (destructuring-bind (x1 y1) pt
      (let* ((s  (gf+ x1 (gf/ y1 x1)))
             (x2 (gf+ (gf^2 s) s *ecc-a*))
             (y2 (gf+ (gf^2 x1) (gf* (gf+ s 1) x2))))
        (list x2 y2)) )))
            
(defun ecc-add (a b)
  (labels
      ((ecc-add-distinct (a b)
         (destructuring-bind (x1 y1) a
           (destructuring-bind (x2 y2) b
             (let* ((s   (gf/ (gf+ y1 y2)
                              (gf+ x1 x2)))
                    (x3  (gf+ (gf^2 s) s x1 x2 *ecc-a*))
                    (y3  (gf+ (gf* s (gf+ x1 x3)) x3 y1)))
               (list x3 y3)))) ))
    
    (destructuring-bind (x1 y1) a
      (destructuring-bind (x2 y2) b
        (cond
         ((and (zerop x1)
               (zerop y1))
          b)
         
         ((and (zerop x2)
               (zerop y2))
          a)
         
         ((= x1 x2)
          (cond
           ((= y1 y2)
            (ecc-double a))
           
           ((or (= (gf+ x1 y1) y2)
                (= (gf+ x2 y2) y1))
            (list 0 0))
           
           (t (error "Can't happen")
              (ecc-add-distinct a b))
           ))
         
         (t (ecc-add-distinct a b))
         ))) ))

(defun ecc-sub (a b)
  (ecc-add a (ecc-negate b)))

;;
;; Lopez-Dahab (LD) Projective Coordinates
;; y^2 + x.y = x^3 + a.x^2 + b, a in {0,1}
;;

(defun ecc-projective-double (pt)
  (destructuring-bind (x1 y1 z1) pt
    (if (zerop z1)
        pt
      (let* ((x1sq    (gf^2 x1))
             (z1sq    (gf^2 z1))
             (bz1sqsq (gf* *ecc-b* (gf^2 z1sq)))
             (z3      (gf* x1sq z1sq))
             (x3      (gf+ (gf^2 x1sq) bz1sqsq))
             (y3      (gf+ (gf* bz1sqsq z3)
                           (gf* x3
                                (gf+ (gf* *ecc-a* z3)
                                     (gf^2 y1)
                                     bz1sqsq)))))
        (list x3 y3 z3)))))
           
(defun ecc-projective-add (pt1 pt2)
  (destructuring-bind (x1 y1 z1) pt1
    (destructuring-bind (x2 y2 z2) pt2
      (declare (ignore z2)) ;; always 1
      (if (zerop z1)
          pt2
        (let* ((t1    (gf* z1 x2))
               (t2    (gf^2 z1))
               (x3    (gf+ x1 t1))
               (t1    (gf* z1 x3))
               (t3    (gf* t2 y2))
               (y3    (gf+ y1 t3)))
          (if (zerop x3)
              (if (zerop y3)
                  (ecc-projective-double pt2)
                (list 1 0 0))
            (let* ((z3   (gf^2 t1))
                   (t3   (gf*  t1 y3))
                   (t1   (if (= *ecc-a* 1)
                             (gf+ t1 t2)
                           t1))
                   (t2   (gf^2 x3))
                   (x3   (gf* t2 t1))
                   (t2   (gf^2 y3))
                   (x3   (gf+ x3 t2))
                   (x3   (gf+ x3 t3))
                   (t2   (gf* x2 z3))
                   (t2   (gf+ t2 x3))
                   (t1   (gf^2 z3))
                   (t3   (gf+ t3 z3))
                   (y3   (gf* t3 t2))
                   (t2   (gf+ x2 y2))
                   (t3   (gf* t1 t2))
                   (y3   (gf+ y3 t3)))
              (list x3 y3 z3)) ))))))

(defun ecc-projective (pt)
  (destructuring-bind (x y) pt
    (list x y 1)))

(defun ecc-affine (pt)
  (destructuring-bind (x y z) pt
    (if (zerop z)
        (list 0 0)
      (list (gf/ x z)
            (gf/ y (gf^2 z))) )))

(defun ecc-projective-mul (pt n)
  ;; left-to-right algorithm
  ;; keeps second operand of addition as (x2 y2 1)
  ;; for projective add
  (if (zerop n)
      (list 0 0)
    (destructuring-bind (x y) pt
      (if (and (zerop x)
               (zerop y))
          pt
        (let* ((r0  (ecc-projective pt))
               (r1  r0)
               (l   (1- (integer-length n))))
          (loop repeat l do
                (setf r0 (ecc-projective-double r0))
                (decf l)
                (when (logbitp l n)
                  (setf r0 (ecc-projective-add r0 r1))))
          (ecc-affine r0))) )))
            
(defun ecc-affine-mul (pt n)
  (do ((ans  (list 0 0))
       (p    pt   (ecc-double p))
       (limit (integer-length n))
       (mask  0  (1+ mask)))
      ((>= mask limit) ans)
    (when (logbitp mask n)
      (setf ans (ecc-add ans p))) ))

(defun ecc-mul (pt n)
  (ecc-projective-mul pt n))

;; --------------------------------------------------------
;;

(defun random-between (lo hi)
  ;; random number in interval [lo,hi)
  (+ lo (lw:mt-random (- hi lo))))

(defun ecc-random-key ()
  (logxor (random-between 1 *ecc-r*)
          (uuid:uuid-to-integer (uuid:make-v1-uuid))))

;; -------------------------------------------------

(defvar *ecc-acudora-private-key*
  ;; (ecc-random-key)
  #x2E210534C1B3BC291B567D9451766B9E702113DBF895EE238F634CB400F783544F50D3F3125BEEF93A90F31D478567A5E96E5B9D160212414CC5F8D2D2761FC52A17B9E159E83C8)

(defvar *ecc-acudora-public-key*
  (ecc-mul *ecc-gen* *ecc-acudora-private-key*))

;; ---------------------------------------------------------------------------------

(defun hex (x)
  (let ((*print-length* nil))
    (write x :base 16)))

#|
(defun find-dense-primitive ()
  (let ((x (uuid:uuid-to-byte-array (uuid:make-v1-uuid))))
    (labels ((tst (n)
               (= #.(ash 1 255)
                  (g256expt n #.(1- (ash 1 256))))))
      (um:nlet-tail iter ((x (hashn x 8192)))
        (let* ((y (convert-text-to-int256 x))
               (z (logior #.(ash 1 255) (ash y -1)))
               ($prim z))
          (if (um:nlet-tail inner ((n 50))
                (or (zerop n)
                    (and (tst (random-between 1 #.(ash 1 256)))
                         (inner (1- n)))))
              (progn
                (hex z)
                (list z (float (/ (logcount z) (integer-length z)))))
            (iter (hashn x 1))) )))))
|#

(defun print-c-array (arr)
  (format t "{ ~{0x~2,'0x~^, ~} };" (coerce arr 'list)))

(defun print-prim (&optional (prim $prim))
  (print-c-array (convert-int571-to-int8-array prim)))

;; ---------------------------------------------------------------------------------

(defvar *ecc-vtuning-product-private-key* ;; private
  #x2AA311AA2A18F9B330C2E40F2CC8EA3149DF58DDCB8389C3223702A7AE60F68AD14940D5E0346EF134248FF1F801A0E8FD4D0EB7498D05D7753AC50EC8F0E9ABB548BCE542F6B1D)

(defvar *enckey*
  (ecc-mul *ecc-acudora-public-key* *ecc-vtuning-product-private-key*))


;; ---------------------------------------------------------------------------------

(defvar *output-path*
  #+:MSWINDOWS
  #P"C:/Program Files (x86)/Steinberg/Cubase LE 4/VSTPlugins/VTuning/"
  #+:MAC
  (make-pathname
   :directory '(:relative "VTuning" "crypto")))

(defvar *fmem-path*
  (merge-pathnames "fmem-array.dat" *output-path*))

(defvar *fcoffs-path*
  (merge-pathnames "fcoffs" *output-path*))


;; -------------------------------------------------
;; String Encryption

(defvar *ecc-strings-private-key*
  #x781008A267866C1B9B4653379E408A582F3BB75FD33D21F6182102536036FC27D88E9FA89DF3B6A7831762A23FA0A52743CC7D10E87379B2B3B61AB0438A04F1972370C8CB62DB)

(defvar *ecc-strings-public-key*
  (ecc-mul *ecc-gen* *ecc-strings-private-key*))

;; -------------------------------------------------

(defun convert-number-to-16bytes (x)
  (let ((ans (make-array 16
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (loop for ix from 15 downto 0 do
          (setf (aref ans ix) (ldb (byte 8 0) x)
                x             (ash x -8)))
    ans))

(defun encrypt-string (str &key (key *ecc-strings-public-key*))
  ;; take a string < 64 chars long and convert to
  ;; encrypted form 80 bytes long
  (assert (< (Length str) 64))
  (let* ((len (length str))
         (str (concatenate 'string str
                           (make-string (- 64 len)
                                        :initial-element (code-char 0))))
         (cnt  (uuid:uuid-to-integer (uuid:make-v1-uuid)))
         (enc  (logxor (convert-text-to-int571 str)
                       (car (ecc-mul key cnt))))
         (encv (convert-int571-to-int8-array enc)))
    ;; first 16-bytes is the string UUID
    (concatenate 'vector (convert-number-to-16bytes cnt) (subseq encv 8))))
    
(defun decrypt-string (arr &key (key *ecc-strings-public-key*))
  ;; take an 80 byte vector and decrypt to a C string < 64 bytes
  (let* ((cnt (convert-vector-to-int (subseq arr 0 16)))
         (enc (logxor (convert-vector-to-int (subseq arr 16))
                      (car (ecc-mul key cnt))))
         (encv (convert-int571-to-int8-array enc))
         (strv (subseq encv 8))
         (pos  (position 0 strv)))
    (map 'string #'code-char (subseq strv 0 pos))))

#|
(progn
  (terpri)
  (dolist (str '("fcoffs"
                 "VTuningLicense.txt"
                 "receptor.jpg"))
    (print-c-array (encrypt-string str))
    (terpri)))

(let ((arrs (mapcar #'encrypt-string '("fcoffs"
                                       "VTuningLicense.txt"
                                       "receptor.jpg"))))
  (terpri)
  (dolist (arr arrs)
    (print (decrypt-string arr))))
|#

(defvar *encrypted-strings-path*
  (merge-pathnames "encrypted-strings" *output-path*))

(defvar *scoffs-path*
  (merge-pathnames "scoffs" *output-path*))

;; -------------------------------------------------
;; Create the encrypted strings file

(defun create-strings-file ()
  (let ((strs (mapcar #'encrypt-string '("scoffs"
                                         "coffs"
                                         "fcoffs"
                                         "receptor.jpg"
                                         "VTuningLicense.txt"))))
    (with-open-file (f *encrypted-strings-path*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
                    
      (write-sequence (convert-int571-to-int8-array
                       (car *ecc-strings-public-key*))
                      f)
      (write-sequence (convert-int571-to-int8-array
                       (cadr *ecc-strings-public-key*))
                      f)
      (dolist (str strs)
        (write-sequence str f)))))

#|
(create-strings-file)
|#

;; -------------------------------------------------
;; AES-256/CBC Encrypted Payloads

(defvar *prog-path*
  (merge-pathnames "VTuning.dll" *output-path*))

(defparameter *progkey*
  (let ((dig (ironclad:make-digest :sha256)))
    (convert-vector-to-int (ironclad:digest-file dig *prog-path*))))

(defun convert-hashint-to-32bytes (x)
  (let ((ans (make-array 32
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (loop for ix from 31 downto 0 do
          (setf (aref ans ix) (ldb (byte 8 0) x)
                x             (ash x -8)))
    ans))

;; -------------------------------------------------
;; Create the FCOFFS and SCOFFS files

(defun encrypt-payloads ()
  (labels ((prep (key)
             (coerce
              (convert-int571-to-int8-array (car key))
              'list)))
    
    ;; Encrypt the fmem array
    (let ((enckey (prep *enckey*)))
      (sys:call-system-showing-output
       (format nil "aescrypt 0 ~W ~W hex:~{~2,'0X~}"
               (namestring *fmem-path*)
               (namestring *fcoffs-path*)
               enckey)))
    
    ;; encrypt the strings
    (let ((enckey (prep (ecc-mul *ecc-gen* *progkey*))))
      (sys:call-system-showing-output
       (format nil "aescrypt 0 ~W ~W hex:~{~2,'0X~}"
               (namestring *encrypted-strings-path*)
               (namestring *scoffs-path*)
               enckey))) ))

(encrypt-payloads)

         
;; ---------------------------------------------------------------------------------

(defvar *img-path*
  #+:MSWINDOWS "c:/Windows/receptor.jpg"
  #+:MAC     (merge-pathnames "receptor.jpg" *output-path*))

(defvar *txt-path*
  (merge-pathnames "VTuningLicense.txt" *output-path*))

(defvar *coffs-path*
  (merge-pathnames "coffs" *output-path*))

(defparameter *imgkey*
  (let* ((dig (ironclad:make-digest :sha256)))
    (convert-vector-to-int (ironclad:digest-file dig *img-path*))))

(defparameter *fckey*
  (let ((dig (ironclad:make-digest :sha256)))
    (convert-vector-to-int (ironclad:digest-file dig *fcoffs-path*))))

(defparameter *sckey*
  (let ((dig (ironclad:make-digest :sha256)))
    (convert-vector-to-int (ironclad:digest-file dig *scoffs-path*))))

(defparameter *txtkey*
  (let ((dig (ironclad:make-digest :sha256)))
    (convert-vector-to-int (ironclad:digest-file dig *txt-path*))))

(defparameter *totalkey*
  (let ((dig (ironclad:make-digest :sha256)))
    (dolist (key (list *progkey*
                       *imgkey*
                       *fckey*
                       *sckey*
                       *txtkey*))
      (ironclad:update-digest dig (convert-hashint-to-32bytes key)))
    (convert-vector-to-int (ironclad:produce-digest dig))))


(defparameter *compkey*
  (ecc-sub *enckey*
           (ecc-mul *ecc-gen* *totalkey*)))

;; -------------------------------------------------
;; Generate COFFS file

(with-open-file (f *coffs-path*
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
  (write-sequence (convert-int571-to-int8-array
                   (car *compkey*))
                  f)
  (write-sequence (convert-int571-to-int8-array
                   (cadr *compkey*))
                  f))

;; -------------------------------------------------

(defun auth ()
  (equalp
   *enckey*
   (ecc-add *compkey*
    (ecc-mul *ecc-gen* *totalkey*))))
(unless (auth)
  (error "Something is really wrong here!"))


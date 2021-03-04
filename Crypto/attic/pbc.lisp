;; pbc.lisp -- PBC (Pairing Based Crypto) in Lisp
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
;;; THIS FILE IS OBSOLETE. USE PBC-CFFI.LISP INSTEAD.

#|
Copyright (c) 2018 Emotiq AG

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

(defpackage :pbc-interface
  (:use :common-lisp
        :core-crypto
        :vec-repr
        :hash)
  (:nicknames :pbc)
  (:export
   ;; classes and their slot readers
   :crypto-val
   :crypto-val-vec
   :g1-cmpr
   :g1-cmpr-pt
   :g2-cmpr
   :g2-cmpr-pt
   :zr
   :zr-val
   :gt
   :gt-val
   :public-key
   :public-key-val
   :secret-key
   :secret-key-val
   :signature
   :signature-val
   :pairing
   :pairing-val
   :crypto-text
   :crypto-text-vec
   
   :init-pairing
   :set-generator  ;; 1 each for G1, and G2 groups
   
   :make-key-pair
   :check-public-key

   :set-secret-key
   :set-public-key

   :make-public-subkey
   :make-secret-subkey
   :ibe-encrypt
   :ibe-decrypt
   
   :sign-message       ;; BLS Sigs
   :check-message
   :combine-signatures ;; for BLS MultiSigs

   :with-crypto
   :ask-crypto
   
   :mul-pts  ;; bent nomenclature for ECC
   :add-zrs
   :inv-zr
   :expt-pt-zr

   :keying-triple
   :keying-triple-pkey
   :keying-triple-sig
   :keying-triple-skey
   
   :signed-message
   :signed-message-msg
   :signed-message-sig
   :signed-message-pkey
   ))

(in-package :pbc-interface)

;; -----------------------------------------------------------------------

(fli:disconnect-module :pbclib
                       :remove t)

(fli:register-module :pbclib
		     :dlopen-flags t
		     :real-name
		     #+:macosx
                     (concatenate 'string 
                                  (namestring (asdf:system-relative-pathname 'emotiq "../var/local/lib"))
                                  "/libLispPBCIntf.dylib")
		     #+:linux
                     (concatenate 'string 
                                  (namestring (asdf:system-relative-pathname 'emotiq "../var/local/lib"))
                                  "/libLispPBCIntf.so")
		     )

;; -----------------------------------------------------------------------
;; for initial test of strings transfer to/from C/Lisp

(fli:define-foreign-function (_echo "echo" :source)
    ((nel     :long)
     (msg     (:pointer (:unsigned :char)))
     (ret-msg (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(defun echo ()
  (let ((msg "Hello Dave!"))
    (fli:with-dynamic-foreign-objects ()
      (let ((buf (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems 1024)))
        (let ((nel  (_echo (length msg)
                           (fli:convert-to-dynamic-foreign-string
                            msg
                            :external-format :ASCII)
                           buf)))
          (list nel (subseq (fli:convert-from-foreign-string
                             buf
                             :external-format :ASCII)
                            0 nel))
          )))))

;; -------------------------------------------------
;; Init interface - this must be performed first

(fli:define-foreign-function (_init-pairing "init_pairing" :source)
    ((param-text  (:pointer (:unsigned :char)))
     (ntext       :long)
     (psizes      (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Query interface

(fli:define-foreign-function (_get-g2 "get_g2" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-g1 "get_g1" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Setter interface

(fli:define-foreign-function (_set-g2 "set_g2" :source)
    ((pbuf   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_set-g1 "set_g1" :source)
    ((pbuf   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Keying interface

(fli:define-foreign-function (_make-key-pair "make_key_pair" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nel    :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_make-public-subkey "make_public_subkey" :source)
    ((abuf   (:pointer (:unsigned :char)))
     (pbuf   (:pointer (:unsigned :char)))
     (hbuf   (:pointer (:unsigned :char)))
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_make-secret-subkey "make_secret_subkey" :source)
    ((abuf   (:pointer (:unsigned :char)))
     (sbuf   (:pointer (:unsigned :char)))
     (hbuf   (:pointer (:unsigned :char)))
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-encrypt "sakai_kasahara_encrypt" :source)
    ;; aka SAKKE, IETF RFC 6508
    ((rbuf   (:pointer (:unsigned :char))) ;; returned R point in G2
     (pbuf   (:pointer (:unsigned :char))) ;; returned pairing for encryption in GT
     (pkey   (:pointer (:unsigned :char))) ;; public subkey in G2
     (hbuf   (:pointer (:unsigned :char))) ;; hash(ID, msg)
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-decrypt "sakai_kasahara_decrypt" :source)
    ;; aka SAKKE, IETF RFC 6508
    ((pbuf   (:pointer (:unsigned :char)))  ;; returned pairing for decryptin in GT
     (rbuf   (:pointer (:unsigned :char)))  ;; R point in G2
     (skey   (:pointer (:unsigned :char)))) ;; secret subkey in G1
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-check "sakai_kasahara_check" :source)
    ;; aka SAKKE, IETF RFC 6508
    ((rbuf   (:pointer (:unsigned :char)))  ;; R point in G2
     (pkey   (:pointer (:unsigned :char)))  ;; public subkey in G2
     (hbuf   (:pointer (:unsigned :char)))  ;; hash(ID, msg)
     (hlen   :long))
  :result-type :long
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-secret-key "get_secret_key" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-public-key "get_public_key" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_set-secret-key "set_secret_key" :source)
    ((sbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_set-public-key "set_public_key" :source)
    ((pbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------
;; BLS Signatures

(fli:define-foreign-function (_sign-hash "sign_hash" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nbuf   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-signature "get_signature" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_check-signature "check_signature" :source)
    ((psig   (:pointer (:unsigned :char)))
     (phash  (:pointer (:unsigned :char)))
     (nhash  :long)
     (pkey   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------

(fli:define-foreign-function (_mul-g1-pts "mul_G1_pts" :source)
    ((p1    (:pointer (:unsigned :char)))
     (p2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_mul-g2-pts "mul_G2_pts" :source)
    ((p1    (:pointer (:unsigned :char)))
     (p2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_add-zr-vals "add_Zr_vals" :source)
    ((z1    (:pointer (:unsigned :char)))
     (z2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_inv-zr-val "inv_Zr_val" :source)
    ((z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_exp-G1z "exp_G1z" :source)
    ((g     (:pointer (:unsigned :char)))
     (z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_exp-G2z "exp_G2z" :source)
    ((g     (:pointer (:unsigned :char)))
     (z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_compute-pairing "compute_pairing" :source)
    ((gtbuf (:pointer (:unsigned :char)))  ;; result returned here
     (hbuf  (:pointer (:unsigned :char)))
     (gbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------
;; Abstract superclass for crypto objects. These are just wrappers
;; around UB8V objects. All subclasses share the same immutable slot.

(defclass crypto-val (ub8v-repr)
  ((val  :reader   crypto-val-vec
         :initarg  :value)))

(defmethod ub8v-repr ((x crypto-val))
  (crypto-val-vec x))

;; -------------------------------------------------
;; Useful subclasses

(defclass g1-cmpr (crypto-val)
  ((val :reader g1-cmpr-pt
       :initarg  :pt)
   ))

(defclass signature (g1-cmpr)
  ((val :reader signature-val
       :initarg  :val)))

(defclass g2-cmpr (crypto-val)
  ((val  :reader g2-cmpr-pt
        :initarg  :pt)
   ))

(defclass public-key (g2-cmpr)
  ((val  :reader public-key-val
        :initarg  :val)))

(defclass gt (crypto-val)
  ((val  :reader  gt-val
         :initarg   :val)))

(defclass pairing (gt)
  ((val  :reader pairing-val
         :initarg  :val)))

(defclass zr (crypto-val)
  ((val  :reader  zr-val
         :initarg   :val)))

(defclass secret-key (zr)
  ((val :reader secret-key-val
        :initarg  :val)))

(defclass crypto-text (crypto-val)
  ((val  :reader  crypto-text-vec
         :initarg :vec)))

;; -------------------------------------------------

(defstruct curve-params
  pairing-text
  g1 g2)

;; from: genfparam 256
(defparameter *curve-fr256-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 16283262548997601220198008118239886027035269286659395419233331082106632227801
r 16283262548997601220198008118239886026907663399064043451383740756301306087801
b 10476541659213232777352255224319706265440471807344192411073251777589416636392
beta 2588849289436542488537732220497504302700946308066126767616133606209888506551
alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
.end
   :g1  (make-instance 'g1-cmpr
         :pt (make-instance 'base58
              :str "3CC2P9iZp4DguyULARTeE6LHc1jCLhEhm1kY8hLz48BgU"))
   :g2  (make-instance 'g2-cmpr
         :pt (make-instance 'base58
              :str "SiVvDVXDAzEFNYLqwboyPGuggSF7o4K6Hnhr4tQbFnkPcSHyDpaLKEejzpscLP39dMHxCF9W7VtYXKe8drDrCehu"))
   ))

(defparameter *curve-default-ar160-params*
  (make-curve-params
   :pairing-text
   #>.end
type a
q 8780710799663312522437781984754049815806883199414208211028653399266475630880222957078625179422662221423155858769582317459277713367317481324925129998224791
h 12016012264891146079388821366740534204802954401251311822919615131047207289359704531102844802183906537786776
r 730750818665451621361119245571504901405976559617
exp2 159
exp1 107
sign1 1
sign0 1
.end
   :g1  (make-instance 'g1-cmpr
         :pt (make-instance 'base58
              :str "BirBvAoXsqMYtZCJ66wwCSFTZFaLWrAEhS5GLFrd96DGojc9xfp7beyDPxC5jSuta3yTMXQt7BXLTpam9dj1MVf7m"))
   :g2  (make-instance 'g2-cmpr
         :pt (make-instance 'base58
              :str "FXJJmcVJsYYG8Y89AZ9Z51kjVANBD68LQi7pD28EG92dxFoWijrcrDaVVYUgiB9yv4GazAAGg7ARg6FeDxCxetkY8"))
   ))

(defparameter *curve*    nil)
(defparameter *G1-size*  nil) ;; G1 corresponds to the h curve
(defparameter *G2-size*  nil) ;; G2 corresponds to the g curve for keying
(defparameter *Zr-size*  nil) ;; Zr corresponds to the secret-key
(defparameter *GT-size*  nil) ;; GT corresponds to the pairings

(defparameter *g2-init*  nil) ;; t if we have done set-generator
(defparameter *zr-init*  nil) ;; t if we have done set-secret-key

;; -------------------------------------------------

(defun need-pairing ()
  (unless *curve*
    (error "Pairing not initialized. Use INIT-PAIRING.")))

(defun need-keying ()
  (need-pairing)
  (unless *zr-init*
    (error "Keying not established. Use SET-SECRET-KEY.")))

(defun need-generator ()
  (need-pairing)
  (unless *g2-init*
    (error "Generator not established. Use SET-GENERATOR.")))

;; -------------------------------------------------

(defun init-pairing (&optional (params *curve-fr256-params*))
  (fli:with-dynamic-foreign-objects ()
    (setf *G1-size* nil
          *G2-size* nil
          *Zr-size* nil
          *GT-size* nil
          
          *G2-init* nil
          *Zr-init* nil
          *curve*   nil)
    (um:bind* ((:struct-accessors curve-params ((txt pairing-text)
                                                (g1  g1)
                                                (g2  g2)) params)
               (ntxt   (length txt))
               (ansbuf (fli:allocate-dynamic-foreign-object
                        :type :long :nelems 4)))
      (assert (zerop (_init-pairing (fli:convert-to-dynamic-foreign-string
                                     txt
                                     :external-format :ASCII)
                                    ntxt
                                    ansbuf)))
      (setf *curve* params
            *g1-size*  (fli:dereference ansbuf :index 0)
            *g2-size*  (fli:dereference ansbuf :index 1)
            *gt-size*  (fli:dereference ansbuf :index 2)
            *zr-size*  (fli:dereference ansbuf :index 3))
      (if g1
          (set-generator g1)
        (setf (curve-params-g1 params) (get-g1)))
      (if g2
          (set-generator g2)
        (setf (curve-params-g2 params) (get-g2)))
      (values))))

;; -------------------------------------------------
;; PBC lib expects all values as big-endian
;; We work internally with little-endian values

(defun raw-bytes (x)
  ;; used only in encrypt/decrypt where message is stored as BEV
  (bev-vec (bev x)))

(defun construct-bev (vec)
  ;; careful here... this assumes vec is UB8-VECTOR
  ;; more efficient internally when we know vec.
  (make-instance 'bev
                 :vec vec))

(defun xfer-foreign-to-lisp (fbuf nel)
  (let ((lbuf (make-ub8-vector nel)))
    (fli:replace-foreign-array lbuf fbuf
                               :end2 nel)
    (construct-bev lbuf)))

(defun make-fli-buffer (nb &optional initial-contents)
  ;; this must only be called from inside of a WITH-DYNAMIC-FOREIGN-OBJECTS
  ;; initial-contents should be in little-endian order
  (let ((bytes (and initial-contents
                    (bev-vec (bevn initial-contents nb))))) 
    (fli:allocate-dynamic-foreign-object
     :type   '(:unsigned :char)
     :nelems nb
     :initial-contents (coerce bytes 'list))))

(defmacro with-fli-buffers (buffers &body body)
  `(fli:with-dynamic-foreign-objects ()
     (let ,(mapcar #`(,(first a1) (make-fli-buffer ,(second a1) ,(third a1))) buffers)
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1)

;; -------------------------------------------------
;; GET-ELEMENT -- read a (possibly compressed) element from the C
;; layer.  On first attempt, we generally don't know the size
;; involved, so a first call is made with a null buffer pointer. The C
;; world returns the actual neededd lenght to us, and stores that into
;; the :LONG count buffer. Then we proceed as normal.

(defun get-element (nb-sym get-fn)
  (need-pairing)
  (let ((nb (symbol-value nb-sym)))
    (unless nb
      ;; size unknown - query first
      (setf nb
            (setf (symbol-value nb-sym)
                  (funcall get-fn fli:*null-pointer* 0))))
    (with-fli-buffers ((buf nb))
      (assert (eql nb (funcall get-fn buf nb)))
      (xfer-foreign-to-lisp buf nb))
    ))
              
;; -------------------------------------------------

(defun get-g1 ()
  (make-instance 'g1-cmpr
   :pt (get-element '*g1-size* '_get-g1)))

(defun get-g2 ()
  (make-instance 'g2-cmpr
   :pt (get-element '*g2-size* '_get-g2)))

(defun get-signature ()
  (make-instance 'signature
   :val (get-element '*g1-size* '_get-signature)))

(defun get-public-key ()
  (make-instance 'public-key
   :val (get-element '*g2-size* '_get-public-key)))

(defun get-secret-key ()
  (make-instance 'secret-key
   :val (get-element '*zr-size* '_get-secret-key)))

(defun get-order ()
  ;; retuns an integer
  (let ((txt (curve-params-pairing-text *curve*)))
    (read-from-string txt t nil
                      :start (+ (search "r " txt
                                        :test 'string-equal)
                                2))))

;; -------------------------------------------------

(defmethod set-element ((x crypto-val) set-fn nb)
  ;; internal routine
  (need-pairing)
  (let ((bytes (crypto-val-vec x)))
    (with-fli-buffers ((buf nb bytes))
      (funcall set-fn buf))))

(defmethod set-generator ((g1 g1-cmpr))
  (set-element g1 '_set-g1 *g1-size*))

(defmethod set-generator ((g2 g2-cmpr))
  (set-element g2 '_set-g2 *g2-size*)
  (setf *g2-init* t))

(defmethod set-public-key ((pkey public-key))
  (set-element pkey '_set-public-key *g2-size*))

(defmethod set-secret-key ((skey secret-key))
  (need-generator)
  (set-element skey '_set-secret-key *zr-size*)
  (setf *zr-init* t))

;; -------------------------------------------------

(defmethod sign-hash ((hash hash))
  ;; hash-bytes is UB8V
  (need-keying)
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((hbuf nhash hash))
      (_sign-hash hbuf nhash)
      (get-signature)
      )))

(defmethod check-hash ((hash hash) (sig signature) (pkey public-key))
  ;; hash-bytes is UB8V
  (need-generator)
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((sbuf *g1-size*  sig)
                       (hbuf nhash      hash)
                       (pbuf *g2-size*  pkey))
      (zerop (_check-signature sbuf hbuf nhash pbuf))
      )))

;; --------------------------------------------------------------
;; BLS Signatures on Messages - result is a triple (MSG, SIG, PKEY)

(defclass signed-message ()
  ((msg   :reader  signed-message-msg
          :initarg :msg)
   (sig   :reader  signed-message-sig
          :initarg :sig)
   (pkey  :reader  signed-message-pkey  ;; who signed it
          :initarg :pkey)
   ))

(defun sign-message (msg)
  (make-instance 'signed-message
                 :msg  msg
                 :sig  (sign-hash (hash/256 msg))
                 :pkey (get-public-key)))

(defmethod check-message ((sm signed-message))
  (check-hash (hash/256 (signed-message-msg sm))
              (signed-message-sig       sm)
              (signed-message-pkey      sm)))

;; --------------------------------------------------------------
;; Keying - generate secret and authenticated public keys

(defclass keying-triple ()
  ((pkey  :reader keying-triple-pkey
          :initarg :pkey)
   (sig   :reader keying-triple-sig
          :initarg :sig)
   (skey  :reader keying-triple-skey
          :initarg :skey)))

(defun make-key-pair (seed)
  ;; seed can be literally anything at all...
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((hbuf hlen hsh))
      (_make-key-pair hbuf hlen)
      (setf *zr-init* t)
      (let* ((pkey (get-public-key)) ;; public key
             (skey (get-secret-key)) ;; secret key
             (sig  (sign-hash (hash/256 pkey)))) ;; signature on public key
        ;; return 3 values: public key, signature on public key, secret key
        (make-instance 'keying-triple
                       :pkey pkey
                       :sig  sig
                       :skey skey)
        ))))

(defmethod check-public-key ((pkey public-key) (psig signature))
  (check-hash (hash/256 pkey)
              psig
              pkey))

;; -----------------------------------------------------------------------
;; Sakai-Haskara Encryption

(defmethod make-public-subkey ((pkey public-key) seed)
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (pbuf *g2-size* (public-key-val pkey))
                       (abuf *g2-size*))
      (_make-public-subkey abuf pbuf hbuf hlen)
      (make-instance 'public-key
                     :val (xfer-foreign-to-lisp abuf *g2-size*)))))

(defmethod make-secret-subkey ((skey secret-key) seed)
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (sbuf *zr-size* (secret-key-val skey))
                       (abuf *g1-size*))
      (_make-secret-subkey abuf sbuf hbuf hlen)
      (make-instance 'secret-key
                     :val (xfer-foreign-to-lisp abuf *g1-size*)))))

;; --------------------------------------------------------------
;; SAKKE - Sakai-Kasahara Pairing Encryption

(defclass crypto-packet ()
  ((pkey   :reader  crypto-packet-pkey     ;; public key of intended recipient
           :initarg :pkey)
   (id     :reader  crypto-packet-id       ;; ID used in IBE for this message
           :initarg :id)
   (tstamp :reader  crypto-packet-tstamp   ;; timestamp of encryption
           :initarg :tstamp)
   (rval   :reader  crypto-packet-rval     ;; R value of encryption
           :initarg :rval)
   (cmsg   :reader  crypto-packet-cmsg     ;; cryptotext of encrypted message
           :initarg :cmsg)
   ))

(defmethod ibe-encrypt (msg (pkey public-key) id)
  ;; msg can be anything and of any length. (we use LOENC:ENCODE)
  ;; Asymmetric encryption is intended only for short messages, like
  ;; keying material. Use symmetric encryption for bulk message
  ;; encryption. But this will work regardless.
  (need-generator)
  (let* ((pkid      (make-public-subkey pkey id))
         (tstamp    (construct-bev (uuid:uuid-to-byte-array
                                    (uuid:make-v1-uuid))))
         (msg-bytes (loenc:encode msg))
         (nmsg      (length msg-bytes))
         (xlen      (* 32 (ceiling nmsg 32)))
         (xbytes    (if (< nmsg xlen)
                        (let ((cloaked (make-ub8-vector xlen)))
                          (replace cloaked msg-bytes)
                          (fill cloaked 0 :start nmsg)
                          cloaked)
                      ;; else
                      msg-bytes))
         (xmsg      (construct-bev xbytes))
         (rhsh      (hash/256 id tstamp xmsg)))
    (with-fli-buffers ((hbuf  32         rhsh)   ;; hash value
                       (pbuf  *gt-size*)         ;; returned pairing
                       (kbuf  *g2-size*  pkid)   ;; public key
                       (rbuf  *g2-size*))        ;; returned R value
      (_sakai-kasahara-encrypt rbuf pbuf kbuf hbuf 32)
      (let* ((pval (get-hash-nbytes xlen (xfer-foreign-to-lisp pbuf *gt-size*)))
             (cmsg (make-instance 'crypto-text
                                  :vec (construct-bev
                                        (map-into pval 'logxor pval xbytes))))
             (rval (make-instance 'g2-cmpr
                                  :pt (xfer-foreign-to-lisp rbuf *g2-size*))))
        (make-instance 'crypto-packet
                       :pkey   pkey
                       :id     id
                       :tstamp tstamp
                       :rval   rval
                       :cmsg   cmsg)
        ))))
                             
(defmethod ibe-decrypt ((cx crypto-packet) (skey secret-key))
  (need-generator)
  (with-accessors ((pkey   crypto-packet-pkey)
                   (id     crypto-packet-id)
                   (tstamp crypto-packet-tstamp)
                   (rval   crypto-packet-rval)
                   (cmsg   crypto-packet-cmsg)) cx
    (let ((skid (make-secret-subkey skey id))
          (pkey (make-public-subkey pkey id)))
      (with-fli-buffers ((pbuf  *gt-size*)
                         (kbuf  *g1-size*  skid)
                         (rbuf  *g2-size*  rval))
        (_sakai-kasahara-decrypt pbuf rbuf kbuf)
        (let* ((cmsg-bytes (raw-bytes cmsg))
               (nb         (length cmsg-bytes))
               (pval (get-hash-nbytes nb (xfer-foreign-to-lisp pbuf *gt-size*)))
               (msg  (construct-bev
                      (map-into pval 'logxor pval cmsg-bytes)))
               (hval (hash/256 id tstamp msg)))
          (with-fli-buffers ((hbuf 32        hval)
                             (kbuf *g2-size* pkey))
            (when (zerop (_sakai-kasahara-check rbuf kbuf hbuf 32))
              (loenc:decode (raw-bytes msg))))
          )))))
    
;; -----------------------------------------------

(defmethod compute-pairing ((hval g1-cmpr) (gval g2-cmpr))
  (need-pairing)
  (with-fli-buffers ((hbuf  *g1-size*  hval)
                     (gbuf  *g2-size*  gval)
                     (gtbuf *gt-size*))
    (_compute-pairing gtbuf hbuf gbuf)
    (make-instance 'pairing
                   :val (xfer-foreign-to-lisp gtbuf *gt-size*))))

;; --------------------------------------------------------
;; Curve field operations -- to match academic papers, we utilize the
;; "bent" nomenclature where curve point addition is denoted by group
;; multiplication, curve point scalar multiplication is denoted as
;; group exponentiation.

(defun binop (op a b a-siz b-siz final)
  ;; operate on operands a, b, returning in the buffer for a
  ;; it is assumed that the a-siz is also the size of result.
  (need-pairing)
  (with-fli-buffers ((a-buf  a-siz  a)
                     (b-buf  b-siz  b))
    (funcall op a-buf b-buf) ;; result returned in first arg buffer
    (funcall final (xfer-foreign-to-lisp a-buf a-siz))))

(defun make-g1-ans (ans)
  (make-instance 'g1-cmpr
                 :pt  ans))

(defun make-g2-ans (ans)
  (make-instance 'g2-cmpr
   :pt ans))

(defun make-zr-ans (ans)
  (make-instance 'zr
   :val ans))

;; -------------------------------

(defmethod mul-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; multiply two elements from G1 field (always the shorter field
  ;; rep)
  ;;
  ;; (should be obvious, but you can't mix G1 with G2, except by
  ;; pairing operations)
  ;;
  (binop '_mul-g1-pts pt1 pt2
         *g1-size* *g1-size* 'make-g1-ans))

(defmethod mul-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; multiply two elements from G2 field
  (binop '_mul-g2-pts pt1 pt2
         *g2-size* *g2-size* 'make-g2-ans))

(defmethod add-zrs ((z1 zr) (z2 zr))
  ;; add two elements from Zr ring
  (binop '_add-zr-vals z1 z2
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod inv-zr ((z zr))
  ;; compute inverse of z in ring Zr
  (need-pairing)
  (with-fli-buffers ((z-buf  *zr-size* z))
    (_inv-zr-val z-buf)
    (make-instance 'zr
                   :val (xfer-foreign-to-lisp z-buf *zr-size*))))

(defmethod expt-pt-zr ((g1 g1-cmpr) (z zr))
  ;; exponentiate an element of G1 by element z of ring Zr
  (binop '_exp-G1z g1 z
         *g1-size* *zr-size* 'make-g1-ans))

(defmethod expt-pt-zr ((g2 g2-cmpr) (z zr))
  ;; exponentiate an element of G2 by element z of ring Zr
  (binop '_exp-G2z g2 z
         *g2-size* *zr-size* 'make-g2-ans))

;; --------------------------------------------------------
;; BLS MultiSignatures

(defmethod combine-signatures ((sm1 signed-message) (sm2 signed-message))
  ;; BLS multi-signature is the product of the G1 and G2 elements
  ;; between them
  (with-accessors ((msg1   signed-message-msg)
                   (sig1   signed-message-sig)
                   (pkey1  signed-message-pkey)) sm1
    (with-accessors ((sig2  signed-message-sig)
                     (pkey2 signed-message-pkey)) sm2
      ;; no point combining signatures unless the message was the
      ;; same for both...
      (make-instance 'signed-message
                     :msg  msg1
                     :sig  (change-class (mul-pts sig1 sig2)   'signature)
                     :pkey (change-class (mul-pts pkey1 pkey2) 'public-key))
      )))

;; --------------------------------------------------------
;; (init-pairing *curve-default-ar160-params*)
(init-pairing)
;; --------------------------------------------------------
#|
(init-pairing)

;; check BLS Signatures
(multiple-value-bind (pkey psig skey) (make-key-pair :dave)
  (let* ((msg  :hello-dave)
         (hash (hash/256 msg)))
    (sign-hash hash)
    (let* ((sig (get-signature)))
      (check-hash hash sig pkey))))

;; verify multi-sig BLS -- YES!!
(multiple-value-bind (pkey psig skey) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (smsg1  (sign-message msg)))
    (assert (check-message smsg1))
    (multiple-value-bind (pkey2 psig skey2) (make-key-pair :key2)
      (let ((smsg2 (sign-message msg)))
        (assert (check-message smsg2))
        (check-message (combine-signatures smsg1 smsg2))
        ))))

;; verify multi-sig BLS -- YES!!
(multiple-value-bind (pkey1 psig skey1) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (hsh    (hash/256 msg))
         (sig1   (sign-hash hsh)))
    (assert (check-hash hsh sig1 pkey1))
    (multiple-value-bind (pkey2 psig skey2) (make-key-pair :key2)
      (let ((sig2 (sign-hash hsh)))
        (assert (check-hash hsh sig2 pkey2))
        (let ((sig   (mul-g1-pts sig1 sig2))
              (pkey  (mul-g2-pts pkey1 pkey2)))
          (check-hash hsh sig pkey)
          )))))

;; check that p1 + p2 = (s1 + s2)*g
(multiple-value-bind (pk1 psig sk1) (make-key-pair :key1)
  (multiple-value-bind (pk2 psig sk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2)))
      (set-secret-key zsum)
      (let ((pk (get-public-key)))
        (list pksum pk)))))

;; check that sig(sum) = sum(sig)
(multiple-value-bind (pk1 psig sk1) (make-key-pair :key1)
  (let* ((msg  :testing)
         (hsh  (hash/256 msg))
         (sig1 (sign-hash hsh)))
  (multiple-value-bind (pk2 psig sk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2))
           (sig2  (sign-hash hsh))
           (ssum  (mul-g1-pts sig1 sig2)))
      (check-hash hsh ssum pksum)
      (set-secret-key zsum)
      (list (sign-hash hsh) ssum)
      ))))

|#

;; ------------------------------------------------------
;;
;; The PBC & GNU MP libs are possibly not thread-safe nor reentrant.
;; The PBC interface in LibLispPBCInterface is certainly not
;; reentrant, as it depends on C global values. And it is not
;; thread-safe since those globals could change from activities in
;; other threads.
;;
;; Hence we need a way to ensure that all use of it goes through a
;; single thread. Actors to the resuce...
;;
;; Note that because of activities requested by other client code, the
;; values of PBC elements may be arbitrary. If you need a known
;; environment, you should reestablish it on entry. This applies
;; in particular to the secret and public keying in effect.

(defun do-with-pbc-exclusive (fn skey pkey)
  (um:critical-section
    (when skey
      (set-secret-key skey))
    (when pkey
      (set-public-key pkey))
    (funcall fn)))

(defun ado-with-pbc-exclusive (fn skey pkey)
  ;; support parallelism better with an Actor service
  ;; instead of blocking-wait lock
  (let ((handler (load-time-value
                  (ac:make-actor 'do-with-pbc-exclusive))))
    (ac:send handler fn skey pkey)))

(defmacro with-crypto ((&key skey pkey) &body body)
  `(ado-with-pbc-exclusive (lambda ()
                            ,@body)
                          ,skey ,pkey))

#+:LISPWORKS
(editor:setup-indent "with-crypto" 1)


(defun do-ask-crypto (fn skey pkey)
  (let ((mbox (mpcompat:make-mailbox)))
    (with-crypto (:skey skey
                  :pkey pkey)
      (mpcompat:mailbox-send mbox
                       (um:capture-ans-or-exn fn)))
    (um:recover-ans-or-exn (mpcompat:mailbox-read mbox))))

(defmacro ask-crypto ((&key skey pkey) &body body)
  `(do-ask-crypto (lambda ()
                    ,@body)
                  ,skey ,pkey))

#+:LISPWORKS
(editor:setup-indent "ask-crypto" 1)

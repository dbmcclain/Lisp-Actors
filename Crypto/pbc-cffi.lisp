;; pbc-cffi.lisp -- PBC (Pairing Based Crypto) in Lisp using CFFI
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
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

(in-package :pbc-interface)

;; -----------------------------------------------------------------------
;; Test the Lisp/C connection
#|
(cffi:defcfun ("echo" _echo) :uint64
  (nel     :uint64)
  (txt-out :pointer :char)
  (txt-in  :pointer :char))

(defun echo (&optional (str "Hello There!"))
  (cffi:with-foreign-string ((ctxt-in ntxt) str
                             :encoding :ASCII)
    (cffi:with-foreign-pointer (ctxt-out 80)
      (let ((ans (_echo ntxt ctxt-in ctxt-out))
            (out (make-array 80 :element-type '(signed-byte 8))))
        (loop for ix from 0 below 80 do
              (setf (aref out ix) (cffi:mem-aref ctxt-out :char ix)))
        (print (list ans out (map 'string 'code-char (subseq out 0 ans))))
        (force-output)))))
|#
;; -----------------------------------------------------------------------
;; Init interface - this must be performed first

(cffi:defcfun ("init_pairing" _init-pairing) :int64
  (context     :uint64)
  (param-text  :pointer :char)
  (ntext       :uint64)
  (psizes      :pointer :uint64))

;; -------------------------------------------------
;; Query interface

(cffi:defcfun ("get_g2" _get-g2) :uint64
  (context     :uint64)
  (pbuf        :pointer :void)
  (nbuf        :uint64))

(cffi:defcfun ("get_g1" _get-g1) :uint64
  (context     :uint64)
  (pbuf        :pointer :void)
  (nbuf        :uint64))

;; -------------------------------------------------
;; Setter interface

(cffi:defcfun ("set_g2" _set-g2) :int64
  (context   :uint64)
  (pbuf      :pointer :uint8))

(cffi:defcfun ("set_g1" _set-g1) :int64
  (context   :uint64)
  (pbuf      :pointer :uint8))

;; -------------------------------------------------
;; Keying interface

(cffi:defcfun ("make_key_pair" _make-key-pair) :void
  (context     :uint64)
  (skbuf  :pointer :uint8)
  (pkbuf  :pointer :uint8)
  (hbuf   :pointer :uint8)
  (nhash  :uint64))

(cffi:defcfun ("make_public_subkey" _make-public-subkey) :void
  (context     :uint64)
  (abuf   :pointer :uint8)
  (pbuf   :pointer :uint8)
  (hbuf   :pointer :uint8)
  (hlen   :uint64))

(cffi:defcfun ("make_secret_subkey" _make-secret-subkey) :void
  (context     :uint64)
  (abuf   :pointer :uint8)
  (sbuf   :pointer :uint8)
  (hbuf   :pointer :uint8)
  (hlen   :uint64))

(cffi:defcfun ("sakai_kasahara_encrypt" _sakai-kasahara-encrypt) :void
  ;; aka SAKKE, IETF RFC 6508
  (context     :uint64)
  (rbuf   :pointer :uint8) ;; returned R point in G2
  (pbuf   :pointer :uint8) ;; returned pairing for encryption in GT
  (pkey   :pointer :uint8) ;; public subkey in G2
  (hbuf   :pointer :uint8) ;; hash(ID, msg)
  (hlen   :uint64))

(cffi:defcfun ("sakai_kasahara_decrypt" _sakai-kasahara-decrypt) :void
  ;; aka SAKKE, IETF RFC 6508
  (context     :uint64)
  (pbuf   :pointer :uint8)  ;; returned pairing for decryptin in GT
  (rbuf   :pointer :uint8)  ;; R point in G2
  (skey   :pointer :uint8)) ;; secret subkey in G1

(cffi:defcfun ("sakai_kasahara_check" _sakai-kasahara-check) :uint64
  ;; aka SAKKE, IETF RFC 6508
  (context     :uint64)
  (rbuf   :pointer :uint8)  ;; R point in G2
  (pkey   :pointer :uint8)  ;; public subkey in G2
  (hbuf   :pointer :uint8)  ;; hash(ID, msg)
  (hlen   :uint64))

;; -------------------------------------------------
;; BLS Signatures

(cffi:defcfun ("sign_hash" _sign-hash) :void
  (context     :uint64)
  (sig    :pointer :uint8)
  (skbuf  :pointer :uint8)
  (pbuf   :pointer :uint8)
  (nbuf   :uint64))

(cffi:defcfun ("check_signature" _check-signature) :int64
  (context     :uint64)
  (psig   :pointer :uint8)
  (phash  :pointer :uint8)
  (nhash  :uint64)
  (pkey   :pointer :uint8))

;; -------------------------------------------------
;; Curve math ops

(cffi:defcfun ("add_G1_pts" _add-g1-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("sub_G1_pts" _sub-g1-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("mul_G1_pts" _mul-g1-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("div_G1_pts" _div-g1-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("neg_G1_pt" _neg-g1-pt) :void
  (context     :uint64)
  (p1    :pointer :uint8))

(cffi:defcfun ("inv_G1_pt" _inv-g1-pt) :void
  (context     :uint64)
  (p1    :pointer :uint8))

(cffi:defcfun ("add_G2_pts" _add-g2-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("sub_G2_pts" _sub-g2-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("mul_G2_pts" _mul-g2-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("div_G2_pts" _div-g2-pts) :void
  (context     :uint64)
  (p1    :pointer :uint8)
  (p2    :pointer :uint8))

(cffi:defcfun ("neg_G2_pt" _neg-g2-pt) :void
  (context     :uint64)
  (p1    :pointer :uint8))

(cffi:defcfun ("inv_G2_pt" _inv-g2-pt) :void
  (context     :uint64)
  (p1    :pointer :uint8))

(cffi:defcfun ("add_Zr_vals" _add-zr-vals) :void
  (context     :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("sub_Zr_vals" _sub-zr-vals) :void
  (context     :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("mul_Zr_vals" _mul-zr-vals) :void
  (context     :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("div_Zr_vals" _div-zr-vals) :void
  (context     :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("exp_Zr_vals" _exp-zr-vals) :void
  (context     :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("neg_Zr_val" _neg-zr-val) :void
  (context     :uint64)
  (z     :pointer :uint8))

(cffi:defcfun ("inv_Zr_val" _inv-zr-val) :void
  (context     :uint64)
  (z     :pointer :uint8))

(cffi:defcfun ("mul_G1z" _mul-G1z) :void
  (context :uint64)
  (g     :pointer :uint8)
  (z     :pointer :uint8))

(cffi:defcfun ("exp_G1z" _exp-G1z) :void
  (context     :uint64)
  (g     :pointer :uint8)
  (z     :pointer :uint8))

(cffi:defcfun ("mul_G2z" _mul-G2z) :void
  (context     :uint64)
  (g     :pointer :uint8)
  (z     :pointer :uint8))

(cffi:defcfun ("exp_G2z" _exp-G2z) :void
  (context     :uint64)
  (g     :pointer :uint8)
  (z     :pointer :uint8))

(cffi:defcfun ("compute_pairing" _compute-pairing) :void
  (context     :uint64)
  (gtbuf :pointer :uint8)  ;; result returned here
  (hbuf  :pointer :uint8)
  (gbuf  :pointer :uint8))

(cffi:defcfun ("mul_GT_vals" _mul-GT-vals) :void
  (context :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("div_GT_vals" _div-GT-vals) :void
  (context :uint64)
  (z1    :pointer :uint8)
  (z2    :pointer :uint8))

(cffi:defcfun ("exp_GTz" _exp-GTz) :void
  (context :uint64)
  (g     :pointer :uint8)
  (z     :pointer :uint8))

(cffi:defcfun ("inv_GT_val" _inv-GT-val) :void
  (context :uint64)
  (g     :pointer :uint8))

;; ----------------------------------------------------

(cffi:defcfun ("get_G1_from_hash" _get-g1-from-hash) :void
  (context     :uint64)
  (g1buf :pointer :uint8)
  (hbuf  :pointer :uint8)
  (nhash :uint64))

(cffi:defcfun ("get_G2_from_hash" _get-g2-from-hash) :void
  (context     :uint64)
  (g2buf :pointer :uint8)
  (hbuf  :pointer :uint8)
  (nhash :uint64))

(cffi:defcfun ("get_Zr_from_hash" _get-zr-from-hash) :void
  (context     :uint64)
  (zrbuf :pointer :uint8)
  (hbuf  :pointer :uint8)
  (nhash :uint64))

;; -------------------------------------------------
;; Abstract superclass for crypto objects. These are just wrappers
;; around UB8V objects. All subclasses share the same immutable slot.

(defclass crypto-val (bev)
  ((val  :reader crypto-val-vec
         :initarg :vec))
  (:documentation "Base class for objects used in pairing crypto"))

;; -------------------------------------------------
;; Useful subclasses

(defclass g1-cmpr (crypto-val)
  ((val :reader g1-cmpr-pt
        :initarg  :pt))
  (:documentation "Wrapper for compressed points from G1 group"))

(defclass signature (g1-cmpr)
  ((val :reader signature-val
        :initarg  :val))
  (:documentation "Wrapper for signatures computed in G1"))

(defclass secret-subkey (g1-cmpr)
  ((val :reader secret-subkey-val
        :initarg  :val))
  (:documentation "Wrapper for secret subkeys in G1"))

(defclass g2-cmpr (crypto-val)
  ((val  :reader g2-cmpr-pt
         :initarg  :pt))
  (:documentation "Wrapper for compressed points in G2 group"))

(defclass public-key (g2-cmpr)
  ((val  :reader public-key-val
         :initarg  :val))
  (:documentation "Wrapper for public keys in G2 group"))

(defclass public-subkey (g2-cmpr)
  ((val  :reader public-subkey-val
         :initarg  :val))
  (:documentation "Wrapper for public subkeys in G2 group"))

(defclass gt (crypto-val)
  ((val  :reader  gt-val
         :initarg   :val))
  (:documentation "Wrapper for field values from GT"))

(defclass pairing (gt)
  ((val  :reader pairing-val
         :initarg  :val))
  (:documentation "Wrapper for pairing field values"))

(defclass zr (crypto-val)
  ((val  :reader  zr-val
         :initarg   :val))
  (:documentation "Wrapper for field values from Zr"))

(defclass secret-key (zr)
  ((val :reader secret-key-val
        :initarg  :val))
  (:documentation "Wrapper for secret keys in Zr"))

(defclass crypto-text (crypto-val)
  ((val  :reader  crypto-text-vec
         :initarg :vec))
  (:documentation "Wrapper for cryptotext values. Arbitrary sized UB8V
not belonging to any field"))

;; -------------------------------------------------

(defstruct pairing-params
  name
  pairing-text
  g1 g2)

(defstruct (full-pairing-params
            (:include pairing-params))
  ;; cached values filled in at init
  context
  order g1-len g2-len zr-len gt-len)

;; ---------------------------------------------------------------------------------------

;; from modified genfparam 449
(defparameter *pairing-fr449-params*
  (make-pairing-params
   :name :pairing-fr449
   :pairing-text
   (sbs
   #>.end
type f
q 1453677448591213781098647615517727737801456574135793739359641814210133565958086561658399625709718948307085772504735487548743943171189343
r 1453677448591213781098647615517727737801456574135793739359641814210095438835869021901087559228486442136979933658464610817282321701858857
b 3
beta 1229750151499227825328472831094691662740855014468882576320227933811268457325811389491594663312116221111048905113135054099364147490316815
alpha0 298900426690285755283106923224136990858821502781746907684148252712220485095267046905800379160959798842713091253816223767728475099528772
alpha1 94767155804077352517678038114326012034615203665857146478293396108661642277407951463015818396017138542636648537581758284300101921434637
.end
)
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "01e4c2281e669cff6761156a9f3e1e5a162f191ebfe60b33544bbd561984114353f9ea193cd2e768ca4d692f0f26b2a04298a726c5328b83b001"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "00e4d7941297819a73c6218cf286aa008015aa7d5705d174aa2b60fe2f264ca7bf36d74aa9398921d16d332636cbe79b188812a2dc2d268c5a01db8f3931b3303a5fddeb4b75064a0172f8c26c40066e83be75a6638a04249df3a86999f311d55b4c8eadf4527de05923aeb0ea434a57ca8700")))

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^449, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.
Algorithm from 'Pairing-Friendly Elliptic Curves of Prime Order' by
Barreto and Naehrig.
Size of q^12 is 5388 bits.
Was shooting for q ~ 2^448, but Lynn's library chokes on that. Works fine on 2^449.")

(defparameter *chk-pairing-fr449-params*
  ;; = (hex-str (hash/256 *pairing-fr449-params*))
  "667562D01F0735EA6853A2D3634EE480DF8F70DDA0D1D35DE424E58F3D4E4409")

;; ---------------------------------------------------------------------------------------
;; from modified genfparam 256
(defparameter *pairing-fr256-params*
  (make-pairing-params
   :name :pairing-fr256
   :pairing-text
   (sbs
   #>.end
type f
q 115792089237314936872688561244471742058375878355761205198700409522629664518163
r 115792089237314936872688561244471742058035595988840268584488757999429535617037
b 3
beta 76600213043964638334639432839350561620586998450651561245322304548751832163977
alpha0 82889197335545133675228720470117632986673257748779594473736828145653330099944
alpha1 66367173116409392252217737940259038242793962715127129791931788032832987594232
.end
)
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01")))

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^256, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 92 trillion hash/256.
Algorithm from 'Pairing-Friendly Elliptic Curves of Prime Order' by
Barreto and Naehrig
Size of q^12 is 3072 bits.")

(defparameter *chk-pairing-fr256-params*
  ;; = (hex-str (hash/256 *pairing-fr256-params*))
  "116C13BB591E22B3075150DA151171F91E7AB7C5B4403F231C2A04CC79A45A4A")

;; ---------------------------------------------------------------------------------------
;; from modified genfparam 255
(defparameter *pairing-fr255-params*
  (make-pairing-params
   :name  :pairing-fr255
   :pairing-text
   (sbs
   #>.end
type f
q 57896044618657242796275912003089040872670005837955836828594514493287093416899
r 57896044618657242796275912003089040872429389868787834093544968723639815668573
b 3
beta 55803917036574816430082368241718705273146220885597405985627421222965120451030
alpha0 36745065291682366075254502967669756043233951020839323524243711945517092183438
alpha1 48463065091351977226261302315306999962633968949407617433093635561838515781540
.end
)
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500"))
   :g2  (make-instance 'g2-cmpr
          :pt (bev-vec
               "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01"))
|#
)

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^255, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 68 trillion hash/256,
truncated to 255 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")

;; ---------------------------------------------------------------------------------------
;; from: genfparam 250
(defparameter *pairing-fr250-params*
  (make-pairing-params
   :name  :pairing-fr250
   :pairing-text
   (sbs
   #>.end
type f
q 1809251394332986959257939850161114686612631097102842638593643553811817328791
r 1809251394332986959257939850161114686570095801237726254523530744446827763441
b 3
beta 1437141908251968146817076402274864795341362088762910734010474370732118688574
alpha0 536271594856618124639488944906714824552130805762113454417772030647384157770
alpha1 28425959349375493106220488310181159417866476731095721227729238529329201869
.end
)
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
                "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01"))
|#
)

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^250, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 23 trillion hash/256,
truncated to 250 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")


;; ---------------------------------------------------------------------------------------
;; from: genfparam 248
(defparameter *pairing-fr248-params*
  (make-pairing-params
   :name :pairing-fr248
   :pairing-text
   (sbs
   #>.end
type f
q 452312848583254953884744365750739939688865847938171536927600539923801802599
r 452312848583254953884744365750739939667598200005613151790322367567049477473
b 3
beta 397249851777460990708985571352217434120562840764162776983847378519420311973
alpha0 304231671163681708906096514291872830602548274253692921780966369109574403653
alpha1 345706227803693509060549771291435615772039886420137687701437165115462066273
.end
)
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01"))
|#
)

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^248, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 40 trillion hash/256,
truncated to 248 bits..  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")


;; ---------------------------------------------------------------------------------------
;; from: genfparam 247
(defparameter *pairing-fr247-params*
  (make-pairing-params
   :name :pairing-fr247
   :pairing-text
   (sbs
   #>.end
type f
q 226156424291628771614785038744124807180463434874498764847568283250641725631
r 226156424291628771614785038744124807165424936801498629912928666264952945481
b 3
beta 157581236914492743008411029886095484341247996949025327085235169276682226769
alpha0 102203583261777748687691191922467678782639793054014524719597751476114479682
alpha1 183476226787262761591357671373987444584218941387482623829885679422372771068
.end
)
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01"))
|#
)

  "Pairing parameters adapted to ensure q is as large as possible within
the constraints that q < 2^247, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 51 trillion hash/256,
truncated to 247 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")

;; ---------------------------------------------------------------------------------------
;; from: genfparam 256
(defparameter *pairing-fr256-params-old*
  (make-pairing-params
   :pairing-text
   (sbs
   #>.end
type f
q 16283262548997601220198008118239886027035269286659395419233331082106632227801
r 16283262548997601220198008118239886026907663399064043451383740756301306087801
b 10476541659213232777352255224319706265440471807344192411073251777589416636392
beta 2588849289436542488537732220497504302700946308066126767616133606209888506551
alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
.end
)
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "0761cf30e9ce29716b7c5b7bb25b62371b64f73bb1515487de78beeda041f98f01"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "05063635c1a668e13ff75dc50e3ee70691956c1e3a7a1aa753949bfc5a2c64b1089295808a7b287851ed003e0c03de12be1ab149825c21c909f0c440e145d0b000")))

  "Ben Lynn's quick and dirty F-type generation. This curve will wrap
3 out of 4 hash/256")

(defparameter *chk-pairing-fr256-params-old*
  ;; = (hex-str (hash/256 *pairing-fr256-params-old*)) 
  "6F50560941366EF498B658FB1D8FC80F842E444412E06B8C0E2F93598F06E796")

;; ---------------------------------------------------------------------------------------
(defparameter *pairing-default-ar160-params*
  (make-pairing-params
   :name :pairing-ar160
   :pairing-text
   (sbs
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
)
   :g1  (make-instance 'g1-cmpr
         :pt (bev-vec
              "797EF95B4B2DED79B0F5E3320D4C38AE2617EB9CD8C0C390B9CCC6ED8CFF4CEA4025609A9093D4C3F58F37CE43C163EADED39E8200C939912B7F4B047CC9B69300"))
   :g2  (make-instance 'g2-cmpr
         :pt (bev-vec
              "A4913CAB767684B308E6F71D3994D65C2F1EB1BE4C9E96E276CD92E4D2B16A2877AA48A8A34CE5F1892CD548DE9106F3C5B0EBE7E13ACCB8C41CC0AE8D110A7F01")))
"Ben Lynn's favorite default - 160 bits symmetric pairing.
Unfortunately, we are a decade later than when these curves were
developed and 80-bit security is no longer sufficient. But this curve
serves as a check on our implementation with his pbc-calc for
comparison.")

(defparameter *chk-pairing-default-ar160-params*
  ;; = (hex-str (hash/256 *pairing-default-ar160-params*))
  "4DD3AB6DF2424BEE2A05885EB6D1DD2C147B8945BF440C7A45CECB6B41BA568F")

;; ---------------------------------------------------------------------------------------

(defparameter *pairing*  nil)

(define-symbol-macro *pairing-name*  (pairing-params-name         *pairing*)) ;; symbolic name of pairing
(define-symbol-macro *g1*            (pairing-params-g1           *pairing*)) ;; generator for G1
(define-symbol-macro *g2*            (pairing-params-g2           *pairing*)) ;; generator for G2

(define-symbol-macro *context*       (full-pairing-params-context *pairing*)) ;; which context of PBC? (0..15)
(define-symbol-macro *pairing-order* (full-pairing-params-order   *pairing*)) ;; order of all groups (G1,G2,Zr,Gt)
(define-symbol-macro *g1-size*       (full-pairing-params-g1-len  *pairing*)) ;; G1 corresponds to the h curve
(define-symbol-macro *g2-size*       (full-pairing-params-g2-len  *pairing*)) ;; G2 corresponds to the g curve for keying
(define-symbol-macro *zr-size*       (full-pairing-params-zr-len  *pairing*)) ;; Zr corresponds to the secret-key
(define-symbol-macro *gt-size*       (full-pairing-params-gt-len  *pairing*)) ;; GT corresponds to the pairings

(defvar *contexts* (make-array 16 ;; holds copies of *pairing* parameters from init-pairing
                               :initial-element nil))

;; -------------------------------------------------

(defun list-all-pairings ()
  '(:pairing-fr449 :pairing-fr256 :pairing-fr255 :pairing-fr256 :pairing-fr248 :pairing-fr247 :pairing-ar160))

(defmethod %locate-raw-pairing ((name symbol))
  (ecase name
    (:pairing-fr449  *pairing-fr449-params*)
    (:pairing-fr256  *pairing-fr256-params*)
    (:pairing-fr255  *pairing-fr255-params*)
    (:pairing-fr250  *pairing-fr250-params*)
    (:pairing-fr248  *pairing-fr248-params*)
    (:pairing-fr247  *pairing-fr247-params*)
    (:pairing-ar160  *pairing-default-ar160-params*)))

(defmethod %locate-raw-pairing ((pairing pairing-params))
  pairing)

(defvar *crypto-lock*  (mpcompat:make-lock)
  "Used to protect internal startup routines from multiple access")

(defun init-pairing (&key (params nil params-supplied-p)
                          (context (position nil *contexts*)))
  "Initialize the pairings lib.

  If params not specified and we haven't been called yet, or specified
as nil, then use default parameters for 449-bit G1 BN-curve. If params
not specified and we have already been called, just skip doing
anything. Specified params forces a cryptosystem state change.

  Returns previous parameters in case you need to call again to reset
state to prior cryptosystem.

  We protect with a lock because this mutates the state of the
library, and we don't want inconsistent state. Calls to SET-GENERATOR
also mutate the state of the lib, and so are similarly protected from
SMP access. Everything else should be SMP-safe."
  (check-type context (integer 0 15))
  (mpcompat:with-lock (*crypto-lock*)
    (let ((prev   *pairing*)
          (cparams (or (and params
                            (%locate-raw-pairing params))
                       *pairing-fr449-params*)))
      ;; If params not specified, and we have already been called,
      ;; just skip doing anything.
      (when (or params-supplied-p
                (null *pairing*))
        (setf *pairing* nil) ;; in case we fail
        (with-accessors ((txt  pairing-params-pairing-text)
                         (g1   pairing-params-g1)
                         (g2   pairing-params-g2)) cparams
          (cffi:with-foreign-pointer (ansbuf #.(* 4 (cffi:foreign-type-size :uint64)))
            (cffi:with-foreign-string ((ctxt ntxt) txt
                                       :encoding :ASCII)
              (assert (zerop (_init-pairing context ctxt ntxt ansbuf)))

              (setf *pairing* (make-full-pairing-params
                             :name          (pairing-params-name         cparams)
                             :pairing-text  (pairing-params-pairing-text cparams)
                             :g1            (pairing-params-g1           cparams)
                             :g2            (pairing-params-g2           cparams)
                             :context       context)
                    *g1-size*  (cffi:mem-aref ansbuf :uint64 0)
                    *g2-size*  (cffi:mem-aref ansbuf :uint64 1)
                    *gt-size*  (cffi:mem-aref ansbuf :uint64 2)
                    *zr-size*  (cffi:mem-aref ansbuf :uint64 3)
                    *pairing-order* nil
                    (aref *contexts* context) *pairing*)
              (get-order) ;; fills in *pairing-order* cached value
              ;; A cryptosystem is specified by pairing params and
              ;; specific values for the G1 and G2 generators.
              ;; By default these are randomly generated in the above call
              (if g1
                  (set-generator g1)
                (get-g1)) ;; fill in cached value
              (if g2
                  (set-generator g2)
                (get-g1)) ;; fill in cached value
              ))) )
      prev))) ;; return previous *pairing*

;; -------------------------------------------------

(defmethod %locate-pairing ((name symbol))
  ;; if a symbol name is given - typically a keyword name, like
  ;; :PAIRING-FR449
  (or (find-if (lambda (slot)
                 (and slot
                      (eq (pairing-params-name slot) name)))
               *contexts*)
      (let ((*pairing* nil))
        (init-pairing :params name)
        *pairing*)
      (error "No pairing named ~S in context cache" name)))

(defmethod %locate-pairing ((pairing full-pairing-params))
  ;; if *pairing* was saved after an init-pairing call
  pairing)

(defun do-with-pairing (name fn)
  (let ((*pairing* (%locate-pairing name)))
    (funcall fn)))

(defmacro with-pairing (pairing-name &body body)
  ;; use a name keyword like :PAIRING-FR449 or :PAIRING-FR256
  `(do-with-pairing ,pairing-name (lambda () ,@body)))

(defun set-pairing (pairing)
  (setf *pairing* (%locate-pairing pairing)))

;; -------------------------------------------------
;; PBC lib expects all values as big-endian
;; We work internally with little-endian values

(defun raw-bytes (x)
  ;; used only in encrypt/decrypt where message is stored as BEV
  (bev-vec x))

(defun xfer-foreign-to-lisp (fbuf nel)
  (let ((lbuf (make-ub8-vector nel)))
    (dotimes (ix nel)
      (setf (aref lbuf ix) (cffi:mem-aref fbuf :uint8 ix)))
    lbuf))

(defun ensure-bevn (buf nel)
  (bev-vec (bevn buf nel)))

(defun xfer-lisp-to-foreign (lbuf fbuf nel)
  (let ((llbuf  (ensure-bevn lbuf nel)))
    (dotimes (ix nel)
      (setf (cffi:mem-aref fbuf :uint8 ix) (aref llbuf ix)))
    ))

(defmacro with-fli-buffers (buffers &body body)
  (if (endp buffers)
      `(progn
         ,@body)
    (destructuring-bind (name size &optional lisp-buf) (car buffers)
      (if lisp-buf
          `(cffi:with-foreign-pointer (,name ,size)
             (xfer-lisp-to-foreign ,lisp-buf ,name ,size)
             (with-fli-buffers ,(cdr buffers) ,@body))
        `(cffi:with-foreign-pointer (,name ,size)
           (with-fli-buffers ,(cdr buffers) ,@body))
        ))))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1)

;; -------------------------------------------------

(defun get-element (nb get-fn)
  "Internal routine to read a (possibly compressed) element from the C
library."
  (with-fli-buffers ((buf nb))
    (assert (eql nb (funcall get-fn *context* buf nb)))
    (xfer-foreign-to-lisp buf nb)))
              
;; -------------------------------------------------

(defun get-g1 ()
  "Return the G1 generator for the cryptosystem"
  (or *g1*
      (setf *g1*
            (make-instance 'g1-cmpr
                           :pt (get-element *g1-size* '_get-g1))
            )))

(defun get-g2 ()
  "Return the G2 generator for the cryptosystem"
  (or *g2*
      (setf *g2*
            (make-instance 'g2-cmpr
                           :pt (get-element *g2-size* '_get-g2))
            )))

(defun parse-order-from-init-text ()
  (let ((txt (pairing-params-pairing-text *pairing*)))
    (multiple-value-bind (start end gstart gend)
        ;; assumes the "r ..." line is preceded and followed by
        ;; newline chars.  This will always be true for the prefix,
        ;; but if the r line is last, you must ensure that a newline
        ;; follows. This may be needed by the C lib too.
        (#~m/\nr ([0-9]+).*\n/ txt)
      (declare (ignore end))
      (when start
        (read-from-string (subseq txt (aref gstart 0) (aref gend 0)))
        ))))

(defun get-order ()
  "Return the integer value that represents the field and group orders."
  (or *pairing-order*
      (setf *pairing-order* (parse-order-from-init-text))))

;; -------------------------------------------------
;; NOTE: Mapping hash values to Elliptic curves by first mapping to
;; the finite field, then multiplying by a curve generator is
;; *COMPLETELY UNSAFE* for signature generation. Anyone could forge a
;; BLS signature on any message.
;;
;; It is also unsfe in that the discrete log of the point would become
;; known to anyone who can compute the hash value.
;;
;; In general, hash values are mapped to X coordinates with
;; pseudo-random reprobing if needed.
;;
;; For asymmetric pairings it is still unsafe in that the discrete log
;; of the point would become known to anyone who can compute the hash
;; value.
;;
;; In general, hash values are mapped to X coordinates with
;; pseudo-random reprobing if needed.
;;

(defmethod g1-from-hash ((hash hash:hash))
  "Return the hash value mapped into G1"
  (let ((nb  (hash-length hash)))
    (with-fli-buffers ((ptbuf  *g1-size*)
                       (hbuf   nb  hash))
      (_get-g1-from-hash *context* ptbuf hbuf nb)
      (make-instance 'g1-cmpr
                     :pt  (xfer-foreign-to-lisp ptbuf *g1-size*))
      )))
                       
(defmethod g2-from-hash ((hash hash:hash))
  "Return the hash value mapped into G2"
  (let ((nb (hash-length hash)))
  (with-fli-buffers ((ptbuf  *g2-size*)
                     (hbuf   nb  hash))
    (_get-g2-from-hash *context* ptbuf hbuf nb)
    (make-instance 'g2-cmpr
                   :pt  (xfer-foreign-to-lisp ptbuf *g2-size*))
    )))

(defmethod %zr (zbuf)
  ;; we take care here to keep %zr an internal-use-only function.
  ;; we can't accept just any old BEV argument.
  (make-instance 'zr
                 :val zbuf))

(defmethod zr-from-hash ((hash hash:hash))
  "Return the hash value mapped into Zr"
  (let ((nb (hash-length hash)))
    (with-fli-buffers ((zbuf   *zr-size*)
                       (hbuf   nb  hash))
      (_get-zr-from-hash *context* zbuf hbuf nb)
      (%zr (xfer-foreign-to-lisp zbuf *zr-size*))
      )))
                       
;; -------------------------------------------------

(defmethod %set-element ((x crypto-val) set-fn nb)
  ;; internal routine
  (mpcompat:with-lock (*crypto-lock*)
    (let ((bytes (crypto-val-vec x)))
      (with-fli-buffers ((buf nb bytes))
        (funcall set-fn *context* buf)))))

(defmethod set-generator ((g1 g1-cmpr))
  "Set the cryptosystem G1 generator"
  (%set-element g1 '_set-g1 *g1-size*)
  (setf *g1* g1))

(defmethod set-generator ((g2 g2-cmpr))
  "Set the cryptosystem G2 generator"
  (%set-element g2 '_set-g2 *g2-size*)
  (setf *g2* g2))

;; -------------------------------------------------

(defun hash-to-pbc-range (&rest args)
  (apply 'hash-to-range (get-order) args))

(defun pbc-hash (&rest args)
  (apply 'hash-to-pbc-range args))

;; -------------------------------------------------

(defmethod sign-hash ((hash hash:hash) (skey secret-key))
  "Bare-bones BLS Signature"
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((sigbuf *g1-size*)
                       (skbuf  *zr-size* skey)
                       (hbuf nhash hash))
      (_sign-hash *context* sigbuf skbuf hbuf nhash)
      (make-instance 'signature
                     :val (xfer-foreign-to-lisp sigbuf *g1-size*))
      )))

(defmethod sign-hash (msg (skey secret-key))
  (sign-hash (hash/256 msg) skey))


(defmethod check-hash ((hash hash:hash) (sig signature) (pkey public-key))
  "Check bare-bones BLS Signature"
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((sbuf *g1-size*  sig)
                       (hbuf nhash      hash)
                       (pbuf *g2-size*  pkey))
      (zerop (_check-signature *context* sbuf hbuf nhash pbuf))
      )))

(defmethod check-hash (msg (sig signature) (pkey public-key))
  (check-hash (hash/256 msg) sig pkey))

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

(defun sign-message (msg pkey skey)
  "BLS Signature packet"
  (make-instance 'signed-message
                 :msg  msg
                 :sig  (sign-hash (hash-to-pbc-range msg) skey)
                 :pkey pkey))

(defmethod check-message ((sm signed-message))
  "Check BLS Signature"
  (check-hash (hash-to-pbc-range (signed-message-msg sm))
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
  "Return a certified keying pair. Seed can be literally anything.
Certification includes a BLS Signature on the public key."
  (multiple-value-bind (hsh hlen) (hash-to-pbc-range seed)
    (with-fli-buffers ((sbuf *zr-size*)
                       (pbuf *g2-size*)
                       (hbuf hlen hsh))
      (_make-key-pair *context* sbuf pbuf hbuf hlen)
      (let* ((pkey (make-instance 'public-key
                                  :val (xfer-foreign-to-lisp pbuf *g2-size*)))
             (skey (make-instance 'secret-key
                                  :val (xfer-foreign-to-lisp sbuf *zr-size*)))
             (sig  (sign-hash (hash-to-pbc-range pkey) skey))) ;; signature on public key
        ;; return 3 values: public key, signature on public key, secret key
        (make-instance 'keying-triple
                       :pkey pkey
                       :sig  sig
                       :skey skey)
        ))))


(defmethod check-public-key ((pkey public-key) (psig signature))
  "Validate a public key from its BLS Signature"
  (check-hash (hash-to-pbc-range pkey)
              psig
              pkey))

;; -----------------------------------------------------------------------
;; Sakai-Haskara Encryption

(defmethod make-public-subkey ((pkey public-key) seed)
  (multiple-value-bind (hsh hlen) (hash-to-pbc-range seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (pbuf *g2-size* (public-key-val pkey))
                       (abuf *g2-size*))
      (_make-public-subkey *context* abuf pbuf hbuf hlen)
      (make-instance 'public-subkey
                     :val (xfer-foreign-to-lisp abuf *g2-size*)))))


(defmethod make-secret-subkey ((skey secret-key) seed)
  (multiple-value-bind (hsh hlen) (hash-to-pbc-range seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (sbuf *zr-size* (secret-key-val skey))
                       (abuf *g1-size*))
      (_make-secret-subkey *context* abuf sbuf hbuf hlen)
      (make-instance 'secret-subkey
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
  (let* ((pkid      (make-public-subkey pkey id))
         (tstamp    (uuid:uuid-to-byte-array
                     (uuid:make-v1-uuid)))
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
         (xmsg      (coerce xbytes 'ub8-vector))
         (rhsh      (hash-to-pbc-range id tstamp xmsg)))
    (with-fli-buffers ((hbuf  32         rhsh)   ;; hash value
                       (pbuf  *gt-size*)         ;; returned pairing
                       (kbuf  *g2-size*  pkid)   ;; public key
                       (rbuf  *g2-size*))        ;; returned R value
      (_sakai-kasahara-encrypt *context* rbuf pbuf kbuf hbuf 32)
      (let* ((pval (get-raw-hash-nbytes xlen (xfer-foreign-to-lisp pbuf *gt-size*)))
             (cmsg (make-instance 'crypto-text
                                  :vec (map-into pval 'logxor pval xbytes)))
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
        (_sakai-kasahara-decrypt *context* pbuf rbuf kbuf)
        (let* ((cmsg-bytes (raw-bytes cmsg))
               (nb         (length cmsg-bytes))
               (pval (get-raw-hash-nbytes nb (xfer-foreign-to-lisp pbuf *gt-size*)))
               (msg  (map-into pval 'logxor pval cmsg-bytes))
               (hval (hash-to-pbc-range id tstamp msg)))
          (with-fli-buffers ((hbuf 32        hval)
                             (kbuf *g2-size* pkey))
            (when (zerop (_sakai-kasahara-check *context* rbuf kbuf hbuf 32))
              (loenc:decode msg)))
          )))))

;; -----------------------------------------------

(defmethod compute-pairing ((hval g1-cmpr) (gval g2-cmpr))
  (with-fli-buffers ((hbuf  *g1-size*  hval)
                     (gbuf  *g2-size*  gval)
                     (gtbuf *gt-size*))
    (_compute-pairing *context* gtbuf hbuf gbuf)
    (make-instance 'pairing
                   :val (xfer-foreign-to-lisp gtbuf *gt-size*))))

;; --------------------------------------------------------
;; Curve field operations -- to match academic papers, we utilize the
;; "bent" nomenclature where curve point addition is denoted by group
;; multiplication, curve point scalar multiplication is denoted as
;; group exponentiation.

(defun unop (op x x-siz final)
  ;; operate on operands a, b, returning in the buffer for a
  ;; it is assumed that the a-siz is also the size of result.
  (with-fli-buffers ((x-buf  x-siz  x))
    (funcall op *context* x-buf) ;; result returned in first arg buffer
    (funcall final (xfer-foreign-to-lisp x-buf x-siz))))

(defun binop (op a b a-siz b-siz final)
  ;; operate on operands a, b, returning in the buffer for a
  ;; it is assumed that the a-siz is also the size of result.
  (with-fli-buffers ((a-buf  a-siz  a)
                     (b-buf  b-siz  b))
    (funcall op *context* a-buf b-buf) ;; result returned in first arg buffer
    (funcall final (xfer-foreign-to-lisp a-buf a-siz))))

(defun make-g1-ans (ans)
  (make-instance 'g1-cmpr
                 :pt  ans))

(defun make-g2-ans (ans)
  (make-instance 'g2-cmpr
                 :pt ans))

(defun make-zr-ans (ans)
  (%zr (bev-vec ans)))

(defun make-gT-ans (ans)
  (make-instance 'gt
                 :val ans))

;; -------------------------------

(defmethod pbc= ((a crypto-val) (b crypto-val))
  (vec= a b))

(defmethod pbc= ((a hash) (b hash))
  (hash= a b))

(defmethod pbc= (a b)
  (int= a b))

;; -------------------------------

(defmethod add-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; add two elements from G1 field (always the shorter field
  ;; rep)
  ;;
  ;; (should be obvious, but you can't mix G1 with G2, except by
  ;; pairing operations)
  ;;
  (binop '_add-g1-pts pt1 pt2
         *g1-size* *g1-size* 'make-g1-ans))

(defmethod sub-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; subtract two elements from G1 field (always the shorter field
  ;; rep)
  (binop '_sub-g1-pts pt1 pt2
         *g1-size* *g1-size* 'make-g1-ans))

(defmethod mul-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; multiply two elements from G1 field (always the shorter field
  ;; rep)
  (binop '_mul-g1-pts pt1 pt2
         *g1-size* *g1-size* 'make-g1-ans))

(defmethod div-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; divide (bent nom) two elements from G1 field (always the shorter field
  ;; rep)
  (binop '_div-g1-pts pt1 pt2
         *g1-size* *g1-size* 'make-g1-ans))

(defmethod neg-pt ((pt g1-cmpr))
  ;; compute negation of pt in group G1
  (unop '_neg-G1-pt pt *g1-size* 'make-g1-ans))

(defmethod inv-pt ((pt g1-cmpr))
  ;; compute inverse of pt in group G1
  (unop '_inv-G1-pt pt *g1-size* 'make-g1-ans))

;;; ---------------------------

(defmethod add-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; add two elements from G2 field
  (binop '_add-g2-pts pt1 pt2
         *g2-size* *g2-size* 'make-g2-ans))
        
(defmethod sub-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; subtract two elements from G2 field
  (binop '_sub-g2-pts pt1 pt2
         *g2-size* *g2-size* 'make-g2-ans))
        
(defmethod mul-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; multiply (bent nom) two elements from G2 field
  (binop '_mul-g2-pts pt1 pt2
         *g2-size* *g2-size* 'make-g2-ans))
        
(defmethod div-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; divide (bent nom) two elements from G2 field
  (binop '_div-g2-pts pt1 pt2
         *g2-size* *g2-size* 'make-g2-ans))

(defmethod neg-pt ((pt g2-cmpr))
  ;; compute negation of pt in group G2
  (unop '_neg-G2-pt pt *g2-size* 'make-g2-ans))

(defmethod inv-pt ((pt g2-cmpr))
  ;; compute inverse of pt in group G2
  (unop '_inv-G2-pt pt *g2-size* 'make-g2-ans))

;; ---------------------------

(defmethod zr ((val zr))
  val)

(defmethod zr ((val integer))
  ;; for ring Zr, any integer value (0 <= z < order) is valid
  ;;
  ;; Zr is the only group for which arbitrary integer values are
  ;; valid. All others must be derived from extant group (subgroup)
  ;; members.
  (%zr (bev-vec (mod val (get-order)))))

(defmethod add-zrs (z1 z2)
  ;; add two elements from Zr ring
  (binop '_add-zr-vals (zr z1) (zr z2)
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod sub-zrs (z1 z2)
  ;; add two elements from Zr ring
  (binop '_sub-zr-vals (zr z1) (zr z2)
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod mul-zrs (z1 z2)
  ;; mult two elements from Zr ring
  (binop '_mul-zr-vals (zr z1) (zr z2)
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod div-zrs (z1 z2)
  ;; mult two elements from Zr ring
  (binop '_div-zr-vals (zr z1) (zr z2)
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod exp-zrs (z1 z2)
  ;; mult two elements from Zr ring
  ;; Careful here... z^(q-1) = 1, z^q = z
  (binop '_exp-zr-vals
         (zr z1)
         (%zr (bev-vec (mod (int z2) (1- (get-order)))))
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod neg-zr (zr)
  ;; compute negation of pt in group Zr
  (unop '_neg-Zr-val zr *zr-size* 'make-zr-ans))

(defmethod inv-zr (zr)
  ;; compute inverse of pt in group Zr
  (unop '_inv-Zr-val zr *zr-size* 'make-zr-ans))

;; --------------------------------------------------

(defmethod mul-pt-zr ((g1 g1-cmpr) z)
  ;; exponentiate an element of G1 by element z of ring Zr
  (binop '_mul-G1z g1 (zr z)
         *g1-size* *zr-size* 'make-g1-ans))

(defmethod expt-pt-zr ((g1 g1-cmpr) z)
  ;; exponentiate an element of G1 by element z of ring Zr
  (binop '_exp-G1z g1 (zr z)
         *g1-size* *zr-size* 'make-g1-ans))


(defmethod mul-pt-zr ((g2 g2-cmpr) z)
  ;; exponentiate an element of G2 by element z of ring Zr
  (binop '_mul-G2z g2 (zr z)
         *g2-size* *zr-size* 'make-g2-ans))

(defmethod expt-pt-zr ((g2 g2-cmpr) z)
  ;; exponentiate an element of G2 by element z of ring Zr
  (binop '_exp-G2z g2 (zr z)
         *g2-size* *zr-size* 'make-g2-ans))

;; --------------------------------------------------

(defmethod expt-GT-zr ((gT gT) z)
  ;; exponentiate an element of subgroup GT by element z of ring Zr
  ;;
  ;; Careful here... GT is a prime order subgroup of a large group of
  ;; composite order. No reason to expect x^q = x, for x in GT, q =
  ;; prime order.
  (binop '_exp-GTz gT (zr z)
         *gT-size* *zr-size* 'make-gT-ans))

(defmethod mul-gts ((gt1 gt) (gt2 gt))
  (binop '_mul-gt-vals gt1 gt2
         *gt-size* *gt-size* 'make-gt-ans))
  
(defmethod div-gts ((gt1 gt) (gt2 gt))
  (binop '_div-gt-vals gt1 gt2
         *gt-size* *gt-size* 'make-gt-ans))

(defmethod inv-GT ((gt gt))
  ;; compute inverse of gt in group GT
  (unop '_inv-GT-val gt *gt-size* 'make-gt-ans))

;; --------------------------------------------------------
;; BLS MultiSignatures

(defmethod add-sigs ((sig1 signature) (sig2 signature))
  (change-class (mul-pts sig1 sig2) 'signature))

(defmethod add-pkeys ((pkey1 public-key) (pkey2 public-key))
  (change-class (mul-pts pkey1 pkey2) 'public-key))

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
                     :sig  (add-sigs sig1 sig2)
                     :pkey (add-pkeys pkey1 pkey2))
      )))

;; ------------------------------------------------------
;; VRF - Publicly Verifiable Random Functions
;;
;; Produce a deterministic 1:1 random mapping from input seeds of low
;; entropy that cannot be attacked with pre-images. This prevents an
;; attacker from trying every seed item from a set of limited
;; cardinality to locate the associated randomness value. Also prevent
;; attackers from forging or predicting the randomness associated with
;; new seeds.
;;
;; Can serve as unpredictable deterministic mappings of sensitive
;; database information.
;;
;; Anyone can who has the proof can verify the randomness as computed
;; properly, even without knowledge of the generator's public key.
;;
;; If you know the seed, along with the proof and public key, you can
;; verify the mapping.
;;
;; So users could ask for the randomness that represents, e.g., their
;; name, assuming they proved that they have the right to this
;; information. Nobody else could discover that mapping, and so others
;; would just see a consistently used random value in place of a name.
;;
;; So, imagine a publicly viewable database containing cloaked
;; information. Each sensitive item is replaced by some deterministic
;; random value so that nobody can discern what it says. The sensitive
;; items might come from a set of limited cardinality.
;;
;; Now imagine trying to reverse engineer this data, assuming you know
;; the elements of the original set of limited cardinality. What we
;; know is that the field is a random value that cannot be simply
;; constructed by guessing the data value and hashing to see if it
;; yields the same random value.
;;
;; But if we guess the seed value, and also had a list of proofs, we
;; might be able to find a match by looking for when:
;;
;;   e(Proof, x*V + Pkey) = e(U,V) = g
;;
;;   where x = hash of seed, U is generator for G1, and V is generator
;;   for G2, and g is the corresponding generator pairing in GT.
;;
;; So, for safety, the databse cannot be showing proof values, when
;; presumably the PKey of the database writer is public knowledge.
;;
;; Instead, the database must show only random values detached from
;; proofs:
;;
;;  r = e(Proof, V) = e(U,V)^(1/(x + skey)) = g^(1/(x + skey))
;;
;; But g is a large (e.g., 5,000+ bit) value. We could as well use the
;; hash of the randomness values for a more limited range.
;;
;; Note that, even with knowledge of a seed, its proof, the public key
;; of the encrypter, and the resulting randomness, we could not
;; produce randomness associated with any other seed. (by difficulty
;; of ECDLP, and DLP in large paoiring field)
;;
;; Code adapted from descriptions in:
;;   "A Verifiable Random Function With Short Proofs and Keys"
;;  by Yevgeniy Dodis, and Aleksandr Yampolskiy

(defstruct vrf
  seed x y proof)

(defmethod compute-vrf (seed (skey secret-key)
                             &key
                             (seed-hash 'hash-to-pbc-range)
                             randomness-hash)
  (let* ((x      (zr-from-hash (funcall seed-hash seed)))
         (1/x+s  (with-mod (get-order)
                   (m/ (m+ (int x) (int skey)))))
         (g1     (mul-pt-zr (get-g1) 1/x+s))
         (y      (compute-pairing g1 (get-g2))))
    ;; NOTE: while we return everything to caller, it is expected that
    ;; caller will make only select items available to the public.
    (make-vrf
     :seed  seed   ;; seed -> x via hash, both generally kept secret
     :x     x    
     :y     (if randomness-hash  ;; y is public randomness produced by skey = e(U,V)^(1/(x+skey))
                (funcall randomness-hash y)
              y)
     :proof g1)))  ;; public proof that we generated randomness = U^(1/(x+skey))


(defmethod validate-vrf ((proof g1-cmpr) (randomness gt) &key &allow-other-keys)
  ;; verify randomness y = e(Proof, V), i.e., that it was computed properly
  (int= (compute-pairing proof (get-g2))
        randomness))

(defmethod validate-vrf ((proof g1-cmpr) (randomness hash:hash)
                         &key
                         (randomness-hash 'pbc-hash)
                         &allow-other-keys)
  ;; randomness could be a hash value or an integer
  (let ((hashfn (or (hash-function-of-hash randomness)
                    randomness-hash)))
    (hash= (funcall hashfn (compute-pairing proof (get-g2)))
           randomness)))

(defmethod validate-vrf ((proof g1-cmpr) (randomness integer)
                         &key
                         (randomness-hash 'hash-to-pbc-range)
                         &allow-other-keys)
  ;; randomness could be a hash value or an integer
  (int= (funcall randomness-hash (compute-pairing proof (get-g2)))
        randomness))


(defmethod validate-vrf-mapping (seed (proof g1-cmpr) (pkey public-key) randomness
                                      &rest args
                                      &key
                                      (seed-hash 'hash-to-pbc-range)
                                      &allow-other-keys)
  ;; verify that e(Proof, V) = g^(1/(x + skey)) = y randomness
  ;; and verify that e(Proof, x*V + PKey) = e(U,V), i.e., that skey generated it
  ;; seed -> x, via hashing,
  ;;
  ;; Supposing that you have the right to know, you offer your seed,
  ;; the proof and public key provided to you, and the associated
  ;; randomness, and then verify that the randomness is in fact
  ;; associated with your seed, and that the randomness was generated
  ;; in the prescribed manner.
  ;;
  (and (apply 'validate-vrf proof randomness args) ;; validate mapping from proof to randomness
       (let* ((x   (zr-from-hash (funcall seed-hash seed)))
              (g2  (add-pts (mul-pt-zr (get-g2) x) pkey))
              (c   (compute-pairing proof g2))
              (chk (compute-pairing (get-g1) (get-g2))))
         (int= c chk)))) ;; check the proof for having been derived from the seed

;; --------------------------------------------------------
;; Confidential Purchases - cloak a purchase by providing a
;; crypto-proof of sufficient amount. The indifividual terms
;; will still need accompanying range proofs.

(defstruct confidential-purchase
  pbuy psell tbuy rsell)

(defmethod confidential-purchase ((paid integer) (change integer)
                                  (pkey public-key) (skey secret-key)
                                  &optional (pkey-vendor (get-g2)))
  "Form a cloaked purchase by hiding (paid - change) so that vendor
can use his (cost + fees) in a pairing relation and verify that,
indeed the person with claimed public key actually did send the
transaction, and that sufficient currency has been forwarded.

The purchase is a binding commitment. If pkey-vendor is not specified,
then this becomes a cloaked transaction that anyone can verify.
Otherwise, only the vendor can verify.

Purchase is represented by triple (Tbuy, Rsell, Pbuy)
where P_buy  = public key of purchaser, in G_2
      s_buy  = secret key of purchaser, in Z_r
      T_buy  = G_1 value used in verification
      R_sell = G_2 value used in verification
      P_sell = public key of vendor, or generator V in G_2
      s_sell = secret key of vendor, or 1, in Z_r
      k_rand = random blinding factor, in Z_r
      U      = generator for G_1
      V      = generator for G_2

   TBuy  = (k_rand * s_buy) / (paid - change) * U  in G_1
   Rsell = P_sell / k_rand                         in G_2

Proof is by computing:

   C_sell = (cost + fees)/s_sell * Rsell;

and then checking the pairing relation:

   e(T_buy, C_sell) = e(U, P_buy)  in G_T
"
  (assert (typep pkey-vendor 'g2-cmpr))
  (with-mod (get-order)
    (let* ((krand  (safe-field-random (get-order)))
           (tbuy   (mul-pt-zr (get-g1)
                              (m/ (m* krand (int skey))
                                  (m- paid change))))
           (rsell  (mul-pt-zr pkey-vendor (m/ krand))))
      (make-confidential-purchase
       :pbuy  pkey
       :psell pkey-vendor
       :tbuy  tbuy
       :rsell rsell))))

(defmethod check-confidential-purchase ((purch confidential-purchase)
                                        (cost integer)
                                        (fees integer)
                                        &optional (skey 1))
  (with-accessors ((pbuy  confidential-purchase-pbuy)
                   (tbuy  confidential-purchase-tbuy)
                   (rsell confidential-purchase-rsell)) purch
    (with-mod (get-order)
      (let* ((csell (mul-pt-zr rsell (m/ (m+ cost fees) (int skey))))
             (p1    (compute-pairing tbuy csell))
             (p2    (compute-pairing (get-g1) pbuy)))
        (= (int p1) (int p2))
        ))))

;; --------------------------------------------------------
;; (init-pairing *pairing-default-ar160-params*)
;(init-pairing)
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
#|
(defun tst (&optional (n 100))
  "test reentrancy of PBC lib. If the lib isn't reentrant, we should
likely see an assertion failure"
  (labels ((doit (name msg)
             (let ((k (make-key-pair name)))
               (dotimes (ix n)
                 (let ((sig (sign-message msg
                                          (keying-triple-pkey k)
                                          (keying-triple-skey k))))
                   (assert (check-message sig)))))))
    (ac:=bind (ans)
        (ac:par
          (doit :one   :okay#1)
          (doit :two   :okay#2)
          (doit :three :okay#3)
          (doit :four  :okay#4)
          (doit :five  :okay#5)
          (doit :six   :okay#6)
          (doit :seven :okay#7)
          (doit :eight :okay#8))
      (ac:pr :done ans))))
|#
;; ------------------------------------------------------------------------------
;; CORE-CRYPTO:STARTUP and CORE-CRYPTO:SHUTDOWN

(defun startup-pbc ()
  (core-crypto:ensure-dlls-loaded)
  (format t "~%Connecting PBC lib")
  (init-pairing :params *pairing-fr256-params* :context 0)
  (init-pairing :params *pairing-fr449-params* :context 1)
  (set-pairing :pairing-fr256))

(core-crypto:add-to-startups 'startup-pbc)

(defun shutdown-pbc ()
  (format t "~%Disconnecting PBC lib")
  (setf *pairing* nil)
  (fill *contexts* nil))

(core-crypto:add-to-shutdowns 'shutdown-pbc)


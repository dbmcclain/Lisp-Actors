;; lattice-key-exchange.lisp
;;
;; DM/RAL  2023/03/24 03:26:29
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-key-exchange
  (:use #:common-lisp #:lattice #:vec-repr #:hash #:ac)
  (:export
   #:lattice-skey
   #:lattice-pkey
   #:random-key
   #:make-aes-packet
   #:decode-aes-packet
   #:make-connection-to-server-packet
   #:make-connection-to-client-packet
   #:decode-server-connection-packet
   #:decode-client-connection-packet

   #:cnx-to-server-packet-maker
   #:cnx-to-client-packet-maker
   #:client-connection-packet-decoder
   #:server-connection-packet-decoder
   ))

(in-package #:com.ral.crypto.lattice-key-exchange)

;; ----------------------------------

(deflex lattice-skey
  (create
   (lambda (cust)
     (send kvdb:kvdb cust :find :lat2-syzygy))
   ))

(defun node-to-kw (node-name &key (prefix "") (suffix ""))
  (intern (string-upcase (concatenate 'string prefix node-name suffix))
          (find-package :keyword)))

(deflex lattice-pkey
  (create
   (lambda (cust node)
     (β (pkey)
         (send kvdb:kvdb β :find (node-to-kw node :prefix "lat2-pkey-"))
       (unless pkey
         (error "No PKEY for node: ~A" node))
       (send cust pkey)))
   ))

(deflex lat2-encoder
  (create
   (lambda (cust pkey v)
     (β (sys)
         (send lattice-system β)
       (send cust (lat2-encode pkey v sys))
       ))
   ))

(deflex lat2-decoder
  (create
   (lambda (cust cs)
     (β (sys)
         (send lattice-system β)
       (β (skey)
           (send lattice-skey β)
         (send cust (lat2-decode skey cs sys))
         )))
   ))

;; ------------------------------------------------------
;; AES-256/CTR Encryption/Decryption

(defun random-key ()
  (vec (hash/256 :random-key (uuid:make-v1-uuid)
                 (prng:ctr-drbg 256))))

(defun aes-enc/dec (key iv vsrc)
  (let ((cipher (ironclad:make-cipher :aes
                                      :mode :ctr
                                      :key  key
                                      :initialization-vector iv)))
    (ironclad:encrypt-in-place cipher vsrc)
    vsrc))

(defun make-auth-chk (key iv cdata)
  (vec (hash/256 :chk key iv cdata)))

(defun make-iv (key)
  (subseq
   (vec (hash/256 :iv key (uuid:make-v1-uuid)))
   0 16))

(defun make-aes-packet (key &rest data)
  (let* ((vdata  (loenc:encode (coerce data 'vector)))
         (iv     (make-iv key))
         (cdata  (aes-enc/dec key iv vdata))
         (chk    (make-auth-chk key iv cdata)))
    (list iv cdata chk)
    ))

(defun decode-aes-packet (key packet)
  (destructuring-bind (iv cdata chk) packet
    (let ((chkx (make-auth-chk key iv cdata)))
      (unless (equalp chkx chk)
        (error "Invalid packet")))
    (let* ((vdata  (aes-enc/dec key iv cdata)))
      (values-list (coerce
                    (loenc:decode vdata)
                    'list))
      )))

;; -----------------------------------------------------
;; Secure Key Exchange
;;
;;    client                                server
;;    --------------------------            ---------------------------
;;    Random Kc
;;    Ps = Lookup(SrvID)
;;    LatEnc(Ps, Kc), AES(Kc,CliID)  -->    Check CliID for membership
;;                                          Pc = Lookup(CliID)
;;                                          Random Ks
;;                                          Kses = H(Ks, Kc)
;;                                   <--    LatEnc(Pc, Ks)
;;    Kses = H(Ks, Kc)
;;
;;    AES(Kses, Data)                -->
;;                                   <--    AES(Kses, Reply)
;;    ...
;;

(deflex client-connection-packet-decoder
  (create
   (lambda (cust latcrypt)
     ;; At the client, we decode the random server key.
     (send lat2-decoder cust latcrypt))
   ))

(deflex server-connection-packet-decoder
  (create
   (lambda (cust packet)
     ;; At the server, we decode the random client key and their Lattice
     ;; ID.
     (destructuring-bind (latcrypt aescrypt) packet
       (β (rkey)
           (send client-connection-packet-decoder β latcrypt)
         (let ((id (decode-aes-packet rkey aescrypt)))
           (send cust rkey id)
           ))))
   ))

;; ----------------------------------------------------
;; For Actors-based code, using parallel Lattice encryption

(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust srv-node)
     (β (pkey)
         (send lattice-pkey β srv-node)
       (let ((rkey  (random-key))
             (my-id (machine-instance)))
         (β (lat-enc)
             (send lat2-encoder β pkey rkey)
           (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
         )))
   ))
           
(deflex cnx-to-client-packet-maker
  (create
   (lambda (cust client-id)
     (β (pkey)
         (send lattice-pkey β client-id)
       (let ((rkey (random-key)))
         (β (lat-enc)
             (send lat2-encoder β pkey rkey)
           (send cust rkey lat-enc))
         )))
   ))

#|
(defun lat2-gen-all (&optional (sys (lattice::get-lattice-system)))
  (with-open-file (f "~/.syzygy"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (loop for node in '("fornax.local"
                        "arroyo.local"
                        "zircon.local"
                        "rincon.local"
                        "umbra.local"
                        "david-pc.local")
        collect
          (let ((sym  (node-to-kw node :prefix "lat2-pkey-")))
            (multiple-value-bind (skey pkey)
                (lattice::lat2-gen-keys sys)
              (with-standard-io-syntax
                (print (list node skey pkey) f))
              (list sym skey pkey)))
          )))

(setf *all-keys* (lat2-gen-all))

(dolist (pars *all-keys*)
  (send kvdb:kvdb println :add (car pars) (third pars)))

(send kvdb:kvdb println :add :lat2-syzygy *my-lat2-skey*)

(setf *my-lat2-skey* #(-154559303 533062068 -40332601 416003980 490156924 418496472 163736156 228159804 -469521680 198803244 -261867626 -281638604 183302014 193299413 330953432 107217995 -362245647 -95508683 -292719011 158481619 -489639452 -206900779 -90407441 103010632 91918524 89443694 -190684907 221233334 213419691 -79470241 135895917 37778662 176744918 300560719 462071881 -57468766 201316314 471085582 52728582 286280878 156126589 -301710317 530681279 215394950 26088723 -489917176 -125027037 -423212554 16204384 11342687 -129234112 -202941400 175840422 31003280 -196712507 -391385221 321674923 -101491968 250892995 -329201210 71988410 168277608 -326490326 -53584975 275456762 3941321 228089071 -21292392 -269722301 -65998838 337487312 -438640530 -294676487 514678867 -10404255 -466749603 4779611 113547721 -271615838 424039175 249076927 -152477568 48044184 126803514 441963361 -248148220 414691370 405254052 -237843414 432384259 266048375 -488307036 333087493 -159731002 114320129 462064990 205862965 -404929347 164324704 -523321635 -431425961 465783067 107091270 386450875 196867707 501646760 -122973446 527118740 -534105385 218856895 323380752 -315044689 31211590 -11905226 -491797104 -494791772 -64614248 451239316 443052781 519040225 -107827436 -236352834 16936616 370848794 -6778888 407762670 -481956488 -198946807 -509156578 -7916250 135897474 433065506 216954791 461649726 135652642 215820721 -276904806 329662939 75355554 -384552280 487022702 -364720296 181590377 488672109 88097317 169930707 -262216090 -181201693 -50163875 -8386411 -241365843 346229201 217804861 330694843 398968498 316561228 -216848575 142283611 101276094 216152712 158184598 237047134 -194608107 365487932 101022781 139669377 -454423176 -326160698 521579837 235579108 266126965 201927883 86833135 460188400 520164364 210727069 319737359 -267394249 502671943 -329186338 -533954676 -428979270 350134118 -488890468 -277187412 -159923731 235667683 -417143181 51427429 301302949 -167700399 91724324 402392772 -304672048 450005914 -265118294 471017299 -520292860 -81985623 -99664176 356986513 -224844087 -150166084 83031397 -447721939 -105977862 514432709 -218418425 500381 -480125286 -304584037 429601466 -398908045 241071419 1219021 504370893 178184111 115060047 -230935494 -57478985 -227034227 424812994 390508185 61805666 105926558 -533908827 101798352 -141211011 476354106 56556922 -448604466 518373838 -355379358 429851868 189721426 14348418 220001197 374593304 -126470611 -273268212 -331494169 58610492 33822003 16415678 528601266 310690360 467912461 -535336367 -115905937 510166887 350340950 38067617 -393824378 124827868 -28737164 -438931060))
(setf *my-lat2-pkey* #(531663300 181801235 533415408 202539973 321524186 383632446 236938734 431408644 -4574292 -498507059 461717538 237670625 21430860 420098654 -492324337 -389207359 -243791500 -415044725 -493542659 11458401 -310515254 -71031156 -359226401 475342638 164945405 -280232253 291383061 122326385 -97684872 251475609 391866034 413974849 -521437717 81333734 155231148 -447072774 -305880137 -387332854 413408517 184570608 386978149 155775617 -152556976 -377838205 243188874 -533941348 -445603433 -117832634 481280648 -431097413 526499147 9985580 -306848818 185163624 202392269 489454674 -85038229 -361017230 -494714525 35116005 420326797 -158953973 -112359989 -257022722 -7091459 -471064265 68254521 32590799 144466326 289857619 -177572236 19548446 -507504255 2196221 337807927 -198066351 478531054 -475252116 489456337 -139628697 390207232 -300282633 207970830 66722571 -177310880 -210859018 138178835 249540592 414662161 -34633628 -286499312 319382123 -31297710 236909411 -341690458 525770316 202250304 431645642 405247473 -282684984 -375025908 167162215 -370359147 -160048971 -524357720 -5844409 -369529802 458241269 50504187 439851066 449377457 -395719454 -399473767 -225730673 -371048857 -281881945 195208475 -354550202 163259514 -25106220 -478129748 356635202 198329850 100501698 321715396 -403282472 -411644523 -440837713 -74676482 140014454 196174241 360906508 -318346289 133643504 461492378 382626136 -87595886 269543606 -521402627 508757843 -283624313 59474567 83138321 425774210 282601969 215618695 303600156 515547254 -291837355 -465304417 470398682 -235744768 -302167669 -389536509 443565874 -12301770 497703003 -234000305 387324984 528493893 -111151793 -107054740 -357623482 128029087 51233601 84198207 -277600557 -426754179 273138271 -415236723 499936818 108862072 396480141 -99288183 -206900078 109721771 -268684951 484875097 -351141889 -401743085 -155378665 155334585 -123618128 305248186 491034954 274651257 -84705572 391318801 301403006 437270589 -535487906 534454562 -202349271 -404711509 123380647 274835541 -215986388 150959968 521057044 -423132933 437193129 417016192 209924510 376685006 -411991349 382193613 -406395012 -66485155 -474693902 15721914 -56035682 -203410161 139773597 23007840 -28597333 325785928 -218421043 290771516 -268390889 -122253713 305789535 -504725541 236111011 296277000 950673 320208551 -171265292 -298142433 281919171 -342876508 116709395 375354914 407208673 525577908 -97333420 -469733700 345640463 76830400 -497389068 225188738 100570648 332790718 -474064211 223578722 209677602 -79720228 458032936 219065247 209038892 16009470 -299240657 201952947 116049612 115407538 231298916 390280793))

(let ((sys (ask kvdb:kvdb :find :lat2-system)))
  (with-open-file (f "~/.lat2-system"
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :direction :output)
    (with-standard-io-syntax
      (print sys f))))

(with-open-file (f "~/.lat2-system"
                   :direction :input)
  (let ((sys (read f)))
    (send kvdb:kvdb println :add :lat2-system sys)))

(with-open-file (f "~/.syzygy"
                   :direction :input)
  (um:nlet iter ()
    (let ((rec (read f t f)))
      (unless (eql rec f)
        (destructuring-bind (name skey pkey) rec
          (send kvdb:kvdb println :add (node-to-kw name :prefix "lat2-pkey-") pkey)
          (when (string-equal name (machine-instance))
            (send kvdb:kvdb println :add :lat2-syzygy skey))
          (go-iter)))
      )))

    


 |#

;; lattice-key-exchange.lisp
;;
;; DM/RAL  2023/03/24 03:26:29
;; ----------------------------------

(defpackage #:com.ral.crypto.lattice-key-exchange
  (:use #:common-lisp #:lattice #:vec-repr #:hash #:ac)
  (:export
   #:lattice-skey
   #:lattice-local-id
   #:lattice-id-for-node
   #:lattice-pkey-for-id
   #:lattice-pkey-for-node
   #:random-key
   #:make-aes-packet
   #:decode-aes-packet
   #:make-connection-to-server-packet
   #:make-connection-to-client-packet
   #:decode-server-connection-packet
   #:decode-client-connection-packet

   #:cnx-to-server-packet-maker
   #:cnx-to-client-packet-maker
   ))

(in-package #:com.ral.crypto.lattice-key-exchange)

;; ----------------------------------
#|
(defvar *dict*
  (loop for node in '("fornax.local"
                      "arroyo.local"
                      "zircon.local"
                      "rincon.local"
                      "umbra.local"
                      "david-pc.local")
        collect
        (let* ((skey (lat-gen-skey))
               (pkey (lat-gen-pkey skey)))
          (list node skey pkey))
          ))

(defvar *nodes*
  (loop for entry in *dict* collect
        (destructuring-bind (name (ms skey) (id mp pkey)) entry
          (assert (eql ms mp)) ;; sanity check
          (assert (string= (str (hex (hash/256 (list ms skey)))) id))
          (list name id))))

(with-standard-io-syntax
  (dolist (node *nodes*)
    (print node))
  (values))

(defvar *pkeys*
  (loop for entry in *dict* collect
        (destructuring-bind (name skey pkey) entry
          pkey)))

(with-open-file (f "~/.lattice-pkeys"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (with-standard-io-syntax
    (print *pkeys* f))
  (values))

(defvar *skeys*
  (loop for entry in *dict* collect
        (destructuring-bind (name skey (id mp pkey)) entry
          (list name id skey))))
          
(with-standard-io-syntax
  (print *skeys*)
  (values))
|#

(defvar *nodes-dict*
  (let* ((dict '(("fornax.local"   "F64C29AC38A5D42D66FEB97CE72FE75178A62216DBF018D4AA8BCCF6723DAE8C") 
                 ("arroyo.local"   "799EAE208DC03EE713D9B93259A36BF7306A1ADF501DE4C0FDB5A5AF6D002C44") 
                 ("zircon.local"   "A7AFB5BB0F0BAB70108142382BF47282AD304E2239A88D992CD2BC2A2E8207AC") 
                 ("rincon.local"   "522CDF4F3EFF13C960B120EAD7B6D94CE90F755C0B0B0C619F09652D07759F05") 
                 ("umbra.local"    "97E927F2A215632279DC629869D3B5A32172D080ED2402324896BA1781891050") 
                 ("david-pc.local" "CB81A6A56C5420F9E0B0C20090176009112D368131DBFE0129FAF4DBB1FCDD46")
                 ))
         (mine (find (machine-instance) dict
                     :key  #'car
                     :test #'string-equal)))
    (if mine
        (cons (list "localhost" (cadr mine)) dict)
      dict)))

(defvar *pkeys-dict*
  (with-open-file (f "~/.lattice-pkeys"
                                 :direction :input)
                (read f)))

(defun lattice-skey ()
  (with-open-file (f "~/.lattice-skey"
                     :direction :input)
    (third (read f))))

(defun lattice-local-id ()
  (str (hex (hash/256 (lattice-skey)))))

(defun lattice-id-for-node (node)
  (cadr (find node *nodes-dict*
              :key #'car
              :test #'string-equal)))

(defun lattice-pkey-for-id (id)
  (find id *pkeys-dict*
        :key #'car
        :test #'string-equal))
  
(defun lattice-pkey-for-node (node)
  (let ((id (lattice-id-for-node node)))
    (lattice-pkey-for-id id)))

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

(defun make-connection-to-server-packet (srv-node)
  ;; Clients send client-side random key to server using Lattice
  ;; encryption, along with their lattice-id in an AES encrypted
  ;; packet.
  (let* ((pkey   (lattice-pkey-for-node srv-node))
         (rkey   (random-key))
         (my-id  (lattice-local-id)))
    (values rkey
            (list
             (with-pkey (pkey pkey)
               (lat-encode pkey rkey))
             (make-aes-packet rkey my-id)))
    ))

(defun make-connection-to-client-packet (client-id)
  ;; Server replies to client with server-side random key
  ;; using Lattice encrypted packet.
  (let* ((pkey (lattice-pkey-for-id client-id))
         (rkey (random-key)))
    (values rkey
            (with-pkey (pkey pkey)
              (lat-encode pkey rkey)))
    ))

(defun decode-server-connection-packet (packet)
  ;; At the server, we decode the random client key and their Lattice
  ;; ID.
  (destructuring-bind (latcrypt aescrypt) packet
    (let* ((skey (lattice-skey))
           (rkey (with-skey (skey skey)
                   (lat-decode skey latcrypt)))
           (id   (decode-aes-packet rkey aescrypt)))
      (values rkey
              id))))

(defun decode-client-connection-packet (latcrypt)
  ;; At the client, we decode the random server key.
  (let* ((skey (lattice-skey))
         (rkey (with-skey (skey skey)
                 (lat-decode skey latcrypt))))
    rkey))

;; ----------------------------------------------------
;; For Actors-based code, using parallel Lattice encryption

(deflex cnx-to-server-packet-maker
  (create
   (lambda (cust srv-node)
     (let ((pkey  (lattice-pkey-for-node srv-node))
           (rkey  (random-key))
           (my-id (lattice-local-id)))
       (β (lat-enc)
           (send plat-encoder β pkey rkey)
         (send cust rkey (list lat-enc (make-aes-packet rkey my-id))))
       ))))
           
(deflex cnx-to-client-packet-maker
  (create
   (lambda (cust client-id)
     (let ((pkey (lattice-pkey-for-id client-id))
           (rkey (random-key)))
       (β (lat-enc)
           (send plat-encoder β pkey rkey)
         (send cust rkey lat-enc))
       ))))


#|
(
 ("umbra.local" "97E927F2A215632279DC629869D3B5A32172D080ED2402324896BA1781891050" (1073741789 #(1 -161937638 -492658049 -237313254 488207506 394183558 236851323 185638625 -20306808 -92885296 -377467018 365371140 -109835514 -57687738 -450152363 286832279 336535996 118811934 282331997 11189957 -41786718 -529658610 -95323628 369317637 368170790 372415684 51781074 -318170073 -520733060 531284820 442324138 -398504494 363709980 483411220 -339357707 44927766 525771252 -340384498 -480650706 427299837 218800620 -443585957 -176551613 352328055 -440233734 -392522283 123187243 131408716 -48720338 474653562 -93932759 400303493 -9754467 392583730 160664880 -431437247 -199150560 313217332 -492961821 -145026449 -335184864 -351242902 65048279 -58854732 68469548 -289309932 213046811 386921404 461219009 -310326131 47445783 284267062 186792055 373816412 -229653684 303733700 -72605945 -112316282 348050741 -268323448 15799643 -217746148 -266881796 -136385993 -381597270 264521757 17797210 122671263 510643708 -533327037 498434836 -412586376 -254423325 -7378681 134414460 -115430947 369306278 12414831 20305188 -253821539 -76816698 -361949705 -416864716 -220729339 -10973501 61491813 257074774 -164354553 360206741 -216070104 -303257746 496080455 -69274963 480668059 -286606188 -26867418 -62591214 528566276 209384844 -392964149 152185424 -150925228 75136756 159023053 222645419 103910649 152523034 512167944 359842109 335261979 385946095 480479515 -81271986 -157374110 -431915902 -271276335 -45022530 -482805061 -522384605 270133681 -488004363 136846710 -123037044 23489442 -472479335 103836720 -92131046 440066795 269393580 1687944 32085931 -405556906 -209456298 -22480011 519114827 389277618 471254323 493670438 -480042190 519084736 156154233 -471176914 -143395473 270832939 267184722 -324940704 -361310485 528276492 378930968 -438755208 -507450481 -249947003 -109668989 281823807 405508935 159331570 -125801733 163608307 -290414091 62998752 313258384 138221182 -199304421 406793797 -76406583 -284702315 354867011 488849816 -172057785 501372640 -263731907 142492184 -22773401 23672846 -5700133 313424200 77804580 86632074 -340170552 -101310038 -270097154 341565754 531210807 361788970 -438239644 -196413548 -78242545 -347058820 175245003 477441951 -353874858 -502903716 190989397 289371319 281211456 207035377 -370783318 487024134 206689334 137327991 -456870065 -190980042 355272699 -191283639 196267619 397001318 97882498 422108713 -397422617 -241363299 240748573 376334618 -413348737 199399657 86393711 423069065 113943336 -89739544 159226325 -425043711 32433209 -249124982 516439893 -37150911 -447402857 -218146107 -154203783 -329349422 488984579 128732093 277592695 346881762 108808264 -80562571 -504696654 -372847642 198469639)))
 ("david-pc.local" "CB81A6A56C5420F9E0B0C20090176009112D368131DBFE0129FAF4DBB1FCDD46" (1073741789 #(1 266404433 198502941 448415021 -428289865 -10476654 520004440 -441251886 464316383 -219473156 -283491227 359204486 -294934177 -497972519 -17057719 402053953 -295175465 -375111692 -3096861 223572790 -216029879 276735379 70109775 -173267958 -4919526 210304231 35050742 297193407 -26017397 84187482 385537620 -300037149 331288650 464399979 349378313 -306735272 -345484584 354509709 366450433 43722660 485878886 243717589 202694667 28075363 498698268 403985740 -51442244 -348704967 24297773 258315624 513163497 -413170933 57018578 318811342 321891654 468103204 166805377 186611740 -74372109 506768014 225951050 111388612 -495267650 -394665132 -266056206 121608934 520815099 -179740931 -164333407 379937997 -529588004 -252646258 -59930339 -12913348 256023191 322555738 -246694081 442612749 -304214406 -305943021 -166062363 -353606916 -252283637 174915895 -358249490 224466583 212463148 508371666 -509813090 155444501 451654965 171613287 519775726 -337360721 77774657 -131547097 -527948041 203013243 -124966937 210454168 -394838581 -152026687 -523983197 -242341788 40880717 -386428261 301558099 -191932047 114485420 470350103 -378784576 -233425014 416223093 219324614 -355968468 60499652 -529286626 341437214 -316525779 -492085885 -296473693 -484066670 -502595655 -414552899 -358633151 -135586156 402649291 32481783 153451194 530360268 44440482 -378443240 323278899 -259028235 407867278 -432352016 -43443363 34749707 328298460 174790017 16627024 401559125 -431805693 84932254 208588970 -92413303 490090393 204523456 215445186 -410082485 405649777 474372487 -413872418 262053080 256960104 -28157520 -73642807 376139974 305617932 -230204538 -12796254 238260490 242813747 -173899335 213382844 309001803 429228801 443035965 -363678303 282914443 -66271983 443654581 -149462876 -388518120 -453336154 -97101942 -284465753 -100010415 252635813 -166617168 168128182 -448586726 -136723852 -202874918 -109037669 -39495440 139349209 -228022248 -199856208 261958633 48804442 381640355 -23093992 -69313118 -139653689 360872719 188305684 -465183411 515537731 99814241 -170636452 496662304 -30230293 -536633842 -275194362 -95776049 474194458 -82082140 -11790600 -476083449 183964561 204255262 406426537 56802266 405485193 -360667222 472721012 194909572 399327722 -405983595 314837442 -519440755 -298118889 -128037379 187841880 421055116 -411776908 459254493 -419903324 502733870 -366793759 27529256 294773089 59054488 -294061468 -99461650 216828935 517545548 191755380 -486909719 -156879764 -250485002 473947063 -486364284 -161682169 -388171846 134376904 380536504 327939297 140832186 96227534 524572032 339695549 -126582719 -433848192 525842172 -296646788))))
|#


(in-package :edec)

(defmacro gen-chk-sig (place)
  `(list* ',place (cdr (ssig-sign ,place))))

(defun verify-chk-sig (chk-sig)
  (with-simple-restart (continue "Check remaining signatures")
    (assert (apply #'ssig-verify
                   (eval (car chk-sig))
                   (cdr chk-sig))
        ()
      "Data does not match signature: ~A" (car chk-sig)
      )))

;; -------------------------------------------------
(with-ed-curve :curve1174
  (unless (boundp '*my-pkey*)
    (multiple-value-bind (pkey shares)
        (gen-key-pair)
      (distribute-shares shares)
      (setf *my-pkey* pkey)))
  (assert (ed-pt= *my-pkey*
                  (ed-mul *ed-gen* *my-skey*))
      ))
;; ---------------------------------------------------
#|
(with-ed-curve :curve1174
  (let ((*print-readably* t))
    (pprint
     (loop for sym in '(*my-pkey*
                        *my-skey*
                        *curve1174*
                        *curve-E382*
                        *curve41417*
                        *curve-Ed448*
                        *curve-e521*
                        *curve-Ed3363*
                        core-crypto::*wordlist*)
           collect
           (eval `(gen-chk-sig ,sym))
           ))))

#-:WINDOWS
(with-ed-curve :curve1174
  (let ((*print-readably* t))
    (pprint
     (loop for sym in '(pbc::*pairing-fr449-params*
                        pbc::*pairing-fr256-params*
                        pbc::*pairing-fr255-params*
                        pbc::*pairing-fr250-params*
                        pbc::*pairing-fr248-params*
                        pbc::*pairing-fr247-params*
                        pbc::*pairing-fr256-params-old*
                        pbc::*pairing-default-ar160-params*)
           collect
           (eval `(gen-chk-sig ,sym))
           ))))
|#
#||#
(with-ed-curve :curve1174
  (dolist (sig 
           '((*MY-PKEY*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              758543681782577827240290608192258872005020219052136774771519051277112697377
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "CB051DB36D2D09E3DBD5D806ED2399807640CDE86E0CB562BEA98AB352CF2280"))))
             (*MY-SKEY*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              869418194818775491129977200000737623891947502285235121157391230834895276949
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "21DDB0B2A4C8C5DFF3F52F7571F20DED38B66D34CD377CA520747B183EE13B24"))))
             (*CURVE1174*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              468599109557669461177927853396187471563341194564598817451035153185136100733
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "BC306369BDD2BE84AF51093DB5D4420658E1B5A7BA4DE991AA0CE8D91AE05BFD"))))
             (*CURVE-E382*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              484397516741198798699034007121686190688097342728532682577653378554046857150
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "D6E41A507691A101B094CAC7DCDCF63581DA19E84A72BFE934DF6915337CFC87"))))
             (*CURVE41417*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              46532636947924243763542671124561595251569190509431885120512854189401393504
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "93E64CAEB9C394C649CC4E48DD08DA8140003DCBA37045F2214A2B7D17F4BE71"))))
             (*CURVE-ED448*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              376543773043012490977968931615838724689460157991351732772025298972641638867
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "59B9F126FFEA5A388C47507A29FFB3B2C2E56E0AF5625ECA5DD2861315022A33"))))
             (*CURVE-E521*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              454585345109319697531840261708269508841072591287453841421045100782061777793
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "76CC90F47EC5E6F8DDAB603875FC55F6266007779473E475CFFD31C029C639DF"))))
             (*CURVE-ED3363*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              538058941590651297239734824341845095674474840641530786256133075384802388630
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "71B4233ABD26A9293AD236BFC79176C1C72A92A81E12FD8B614152DC27177EE1"))))
             (CORE-CRYPTO::*WORDLIST*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              462372964569375739953485693380679343956101326818656232316094150805965795590
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "9AB9347D7606B32C637592B4CC3F14FFC2A29EC443083B0D42EFF6AEB9DE06DB"))))))
    (verify-chk-sig sig)))

#-:WINDOWS
(with-ed-curve :curve1174
  (dolist (sig
           '((PBC-INTERFACE::*PAIRING-FR449-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              594316710033757505209679744673507617898578861086788350620884289530630554616
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "1D3FFEBDADE04BEC0807A671FFF81FFF490B5972E88A44C36749904C10BF51CF"))))
             (PBC-INTERFACE::*PAIRING-FR256-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              126805318437049989062670923854239281363346355148875080677894944729444476204
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "AFB11E4D6514478888A65860B3FEACAFB362D347777D9051406E3DA1B97112D2"))))
             (PBC-INTERFACE::*PAIRING-FR255-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              203823612183307587038651737930833562436822902625953311036843194014559078230
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "C8656FEC05CFD36D2609D502D658ECBC9C1E0818002AD0B472BC7AB89E820891"))))
             (PBC-INTERFACE::*PAIRING-FR250-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              206641328755038737305714365198077743557214666033629303316551004059142883835
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "72DBAFFDAC23E4A50D018E3224AD5BF1EFD5EB123E2DD1A697AE8A0A4052FF73"))))
             (PBC-INTERFACE::*PAIRING-FR248-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              454524217570687133391475538825160942612774432250488401872409728758556467777
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "939768194AE08D7FEDF984E0F10E1DF41A39939C2BC730C10F799C7F3F81CD4B"))))
             (PBC-INTERFACE::*PAIRING-FR247-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              752463187022338341416357075120619337933085545968671996330655102048716752948
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "7A7B913A2F79ECB79A38B794E30A7A23E397D8B5F9BD5F6D73997726685C6560"))))
             (PBC-INTERFACE::*PAIRING-FR256-PARAMS-OLD*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              703112376618832151772681645860716319635521615121226254876328420258564418068
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "E092044DE73C5AD24A09021006199572F35A331CBF88233652512D6DD6FA80C4"))))
             (PBC-INTERFACE::*PAIRING-DEFAULT-AR160-PARAMS*
              #S(ECC-CMPR-PT :CX 3238706287253458695043697889970663825594649144648473593115026994751520435692)
              857337911544314451398253156481583132782118407098185883127743917105188582817
              #.(make-instance 'HASH:HASH/256
                               :val (bev-vec
                                     (hex "522E5911A975921BDCA61CD72875112FA34BF54B5CE89EDDDEAD35D97ADEE710"))))
             ))
    (verify-chk-sig sig)))


#||#

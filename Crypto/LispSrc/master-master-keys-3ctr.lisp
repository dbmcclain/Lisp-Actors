(in-package :ecc-crypto-b571)
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

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

(defun gen-x ()
  (convert-bytes-to-int (ctr-drbg 571)))

(defun gen-share ()
  (let* ((x  (gen-x))
         (y  (ecc-mul *ecc-gen* (gen-x))))
    (make-crypto-share
     :x x
     :y y)))

(defun format-share (share)
  (with-output-to-string (s)
    (format s "(make-crypto-share~%:x~%")
    (format-fragments s (crypto-share-x share))
    (format s "~%:y~%(make-ecc-pt~%:x~%")
    (format-fragments s (ecc-pt-x (crypto-share-y share)))
    (format s "~%:y~%")
    (format-fragments s (ecc-pt-y (crypto-share-y share)))
    (format s "))~%")))

(defun encoded-url-query-string (str)
  (with-output-to-string (outp)
    (with-input-from-string (inp str)
      (do ((ch (read-char inp nil inp)
               (read-char inp nil inp)))
          ((eq ch inp))
        (cond
         ((alphanumericp ch)
          (princ ch outp))
         (t
          (format outp "%~2,'0x" (char-code ch)))
         )) )))

(defun make-qr-code (str &optional (size 200))
  (sys:open-url
   (concatenate 'string
                (format nil "https://chart.googleapis.com/chart?cht=qr&chs=~Ax~A&chl=" size size)
                (encoded-url-query-string str))))

;; ----------------------------------------------------------

(defvar *master-share*
  (make-crypto-share
   :x
   (big32 #x011E7FD6 #x1446246E #x92BA25AC #x58624A3F
          #x8B299364 #xC5A19187 #x28EAE583 #x7FD6CC2B
          #xAE00C528 #xE6835BB7 #x6B54677B #x0DB72F6F
          #xC744BE46 #x8A90FDA7 #x1834CE4B #x3E651C56
          #x6DA9952C #x17E0E4C4 )
   
   :y
   (make-ecc-pt
    :x
    (big32 #x03FE4DA7 #x77A4ABCD #x80FA3830 #x448DCA75
           #x326B08E8 #x02FE5610 #x4F245347 #xC6F09E97
           #xAC9AC6F9 #xC18C561F #xF5CD0D07 #x1E2D5FED
           #x4EEC41B2 #xA24BE7BB #xB35F3926 #xB149E794
           #xA49C4ABC #x68277B8D )
    :y
    (big32 #x0111366A #xD00EB0D0 #xF33975FB #xB1E8E895
           #xBDC48ADA #xCD0C2168 #x2E5DE2CF #xA75E55F9
           #xCFDFF615 #x25F04865 #xDD690B5F #x18EBB1F5
           #x97FD1829 #x2F821B11 #x5D567AF1 #x615E2AAB
           #x11D9BD21 #x291ECD58 ))
   
   ))
   
(defun encode-master-message (text)
  (let ((key  (encode-b64 (crypto-share-y *master-share*)))
        (len  (length text)))
    (encode-b64
     (ubstream:with-output-to-ubyte-stream (sout)
       (with-input-from-string (sin text)
         (3ctr-hmac-encrypt-stream sin len sout key))) )))

(defvar *master-curve-share-1*
  (make-crypto-share
   :x
   (big32 #x02708C46 #x60482D01 #x40122E78 #xCDEC95A2
          #xD7A61C22 #x591E41CC #x10D48049 #x0F4BEA63
          #x7957815C #x25897DB7 #xA2B5D2B1 #xCE296134
          #xA23D0FBC #x204F16FF #xDB214AE0 #x0350E887
          #x9BD286F5 #xDEAD0D20 )
   :y
   (make-ecc-pt
    :x
    (big32 #x0404F13F #x602D9493 #x333FCB8E #x3D92BDF8
           #xE5AC65EB #x71FD5523 #x37137319 #xEEF3457B
           #x5D536CD8 #xCFF1BF67 #x8D8554AC #xBEA21CBD
           #xA2ACF66F #x885F9D14 #xE884F2B1 #xB956ABE8
           #xA7B26B3A #xE4981C49 )
    :y
    (big32 #x06FF0D0D #xA66C6E8B #xCA72F390 #xFF50172F
           #x920DD9A1 #x538C9DA6 #xD61DF1F2 #x1191A20A
           #x38ABDD6D #xF5BC9A80 #xCD59338F #xE56D13B5
           #x75153DBE #x4D52EA02 #x421B37C4 #x743E28D7
           #xD9A1FE27 #x5A54497A ))))

(defvar *master-curve-share-2*
  (make-crypto-share
   :x
   (big32 #x076B9A0B #x678E44A6 #x2BCA7D8F #xD00DEFA2
          #x097B3925 #xB32D2F86 #xEA9F0176 #x7496CBBF
          #xF216B9BA #x268ADB87 #x3864BCA7 #x261913E4
          #x2243BB62 #x9687F950 #x8BD02C7A #x5F3BB68B
          #x9EDA7377 #x842B718F )
   :y
   (make-ecc-pt
    :x
    (big32 #x00255846 #xE3CB4528 #xC37E10C6 #xBAC6C8AB
           #xBB1EB557 #x900C55FE #xB9ADD432 #x8094FD3B
           #x6F231778 #xC9ECFA91 #x5EB9E73B #xC11A888C
           #x97DE0F24 #x92346547 #x0ACC38B9 #xCB6CE586
           #x6A441FCA #xB82AFA3B )
    :y
    (big32 #x0710F754 #x617CA79E #xFC2DB76D #x9189382A
           #x1DB1BD35 #x65903285 #xA1B2BF93 #xBC248A13
           #x00AE19D3 #x4C7FAABF #xEE05EC0D #x5AD46148
           #xE5C125CE #xD8137FC2 #x019131FC #xFD98B6C9
           #x6FD774C6 #x819F1816 ))))

(defvar *master-curve*
  (list *master-share*
        *master-curve-share-1*
        *master-curve-share-2*))

(defun solve-master-curve (x)
  (solve-lagrange *master-curve* x))

;; ---------------------------------------------------------

(defun gen-master-share ()
  (let* ((x (gen-x))
         (y (solve-master-curve x)))
    (make-crypto-share
     :x x
     :y y)))

  #|
    ;; board members:
    ;;  Me
    ;;  Quan
    ;;  David C.
    ;;  Arthur H.
    ;;  Barry Baker
    ;;  Paul E.
    ;; -- aux --
    ;; Helene
    ;; William
    |#
  
(defvar *board-share-pe* ;; Paul Eynott
    (make-crypto-share
     :x
     (big32 #x0710E7F7 #x4B07B856 #x3449FFFA #x7B8816D5
            #xC7FEA71B #xAA16EBA0 #xA4D6C7E4 #x3ADEF57F
            #x779E39A0 #xBBA048C5 #x1C1CB272 #x9DEFC122
            #x2C826C9C #x485D2E2A #x65FA649D #x83911B5D
            #x58C5EC11 #xF08B175A )
      
     :y
     (make-ecc-pt
      :x
      (big32 #x01E054F5 #x3AE00310 #x30B4EA01 #x84EDB418
             #xBD105E43 #x731C9394 #xA65B067E #x2A10BF1C
             #x6827AB35 #xDED7D12E #x1E95038A #x9A77B101
             #xF1CAFC05 #x3CC3FB97 #x3606DD1B #xFAC6AA1B
             #xDE40F645 #x5D15D275 )
      :y
      (big32 #x02A6ABFB #x42B3F0AE #x109600B2 #x6665E22F
             #xE9D186AE #xBCB469D2 #xFE0AD1AC #x689D9161
             #x4D236D56 #x45C194F4 #x0E26D59A #xC3A74159
             #xC560264F #xFDD58124 #xE0C1AE85 #x6B6F736A
             #x91497257 #x9CF95189 ))
     ))
   
(defvar *board-share-qn* ;; Quan Nguyen
    (make-crypto-share
     :x
     (big32 #x00E857C9 #x641EB015 #x693DCBCC #xCBE2C4A9
            #xD6261DE2 #xE17CFCEB #x0D32E126 #xDBA242F4
            #x55E661A9 #xFFD3637B #xCE4C8580 #x04A7AC29
            #x4D0E5456 #xD0A06F7A #x47E9AD6D #x9014CF46
            #xC356F0F6 #x89B32A63 )
      
     :y
     (make-ecc-pt
      :x
      (big32 #x05E2BF54 #x849B0762 #x77415AB5 #x2E30185A
             #x44B16B54 #xF17845EC #xCCE337A9 #xC35DCDB3
             #x3DD2BF80 #x10FF0582 #xBFDA3A73 #x733AB35A
             #x2443D939 #x3A73E200 #xC8A6E435 #x627EDA64
             #x2271FE26 #x297991B8 )
      :y
      (big32 #x07AA5F53 #x9C9BEB69 #xA6D8491F #x3E3839EA
             #xC0AE339D #xDF52D10A #xBDF1A3FD #x82770A9D
             #x921F3AFD #x7BDB3629 #x2F327DF3 #xCA1D9919
             #x8490B33E #x5D39E1E9 #x4B66029C #x3D25FF77
             #xA3E0F37B #xE592686E ))
     ))
   
(defvar *board-share-dc* ;; David Cohen
    (make-crypto-share
     :x
     (big32 #x0731C1D1 #x213FEAEE #x7C4E7BA5 #x65938609
            #xCA62979B #x2E52CB33 #xDA527ECB #x4830ACBD
            #x9A389810 #x42275051 #x281A53F4 #x347626AC
            #x6AEF4FF4 #xCB5E0DE9 #x7F8A947F #x750C65FA
            #x3E06C323 #xD2DC6374 )
     :y
     (make-ecc-pt
      :x
      (big32 #x05C57DF6 #x814CF70D #x51358BDD #x57C06A1A
             #x8093E554 #xEB30E564 #xF1C5E504 #xC8FEEA9F
             #xF6072518 #x383F8479 #x933F3B1B #x8E3CDB64
             #x557F719F #xCD8B3DC1 #x372894F9 #xD881BF1A
             #xBD1089C8 #x1D53EE37 )
      :y
      (big32 #x0257D0FA #x60B2F3F0 #xEDDF7138 #xA23FD705
             #xFC694410 #x8CDC2025 #x06EAF221 #x5E740D61
             #x0C6DD4CB #x568A2515 #xAA3B3191 #x97D6F4FC
             #x223782E1 #xACAEEEBE #x9B6589E0 #x724C07E1
             #xE9EDFA60 #xEC5DB17F ))
     ))
   
(defvar *board-share-hb* ;; Helene Barab
    (make-crypto-share
     :x
     (big32 #x0108FE13 #x6646543E #xBC7A97C1 #x9E155D8B
            #xDAA57B98 #x529B6762 #xB201DDA6 #x8370EA3D
            #xCF9C7533 #xDA92EB3B #x5A468B1C #x9DA74238
            #xAD92913D #xC999A957 #x769DE1F3 #xE3B86E33
            #x6BD062F7 #xAE7F2668 )
      
     :y
     (make-ecc-pt
      :x
      (big32 #x014ED284 #xAB0A6F27 #x8E5237D0 #x83A3AC43
             #x3AB1FC68 #x6EA03A28 #x88C86F59 #xBFB4530E
             #x126D12B1 #xB1E12F41 #x9D9B4BFB #x35DE8603
             #xDF84AB87 #xED798958 #x90234850 #x2F55B344
             #x698C6349 #x384E6216 )
      :y
      (big32 #x06BAFFE6 #x5EC085E2 #x308541A0 #x4B4F5ED9
             #xA3012347 #xBDFB0A82 #xD3C41645 #x347AC8D5
             #x80BA3A43 #xE14D0C6B #xB786E3B3 #x892B6AC2
             #x5D30F90B #x13A83902 #x7D30A992 #x6AB1381B
             #x5053C9F0 #x6E9A4716 ))
     ))

(defvar *board-share-bb* ;; Barry Baker
  (make-crypto-share
   :x
   (big32 #x00694313 #x135FE5AE #x9AA8368C #x5BB64E85
          #x803DD717 #xBF646A67 #x52A27D14 #xFDFC61ED
          #x76ED2D1C #x3950D300 #xA0D94F2E #x0D607C6A
          #xF06D9122 #x7377E52B #xFD01F02A #x7E76C3EB
          #x57ED07BF #x1C9A19A7 )
   :y
   (make-ecc-pt
    :x
    (big32 #x03A83979 #xCDBD4151 #x0B0E6D17 #xFF7650D6
           #xF04B076F #x99A6BC42 #xB6F0EBBD #xA51B3BE0
           #x9CAE66DC #xDCD90C37 #x1171361D #x9CB9FF68
           #xFE17404B #x93CCF0ED #x11DFC14B #xA7AF5C32
           #xA9BF7B4F #x35F192D0 )
    :y
    (big32 #x07116B77 #xDEDE53A6 #x115CE1CB #x64F5D139
           #xFFD38345 #x077EC45F #xA983FC61 #x676E680D
           #x416DDB04 #x51A54D4A #x2D6B5CFB #x62F0A817
           #x701DB87C #x0E3C19A9 #xCCA707B4 #x4318E896
           #x47D35320 #x4FD2E5AA ))))
    
(defvar *board-share-ah* ;; Arthur Herbst
  (make-crypto-share
   :x
   (big32 #x03BECFE2 #xED0FCABA #xC74231A3 #xE3648308
          #x333C5AE7 #x38103C38 #x33B4B100 #xE626E8B8
          #xF2D3FE43 #x092B98A0 #xFAA9A649 #x9CA3A5C4
          #x439D3B06 #x2B02B407 #xE28EDA96 #xAC7B4E2D
          #xB0A38444 #x15AD9C39 )
   :y
   (make-ecc-pt
    :x
    (big32 #x03B8BF64 #x138EBC83 #x80EF0EA2 #xAFAC4E6F
           #xCF7F3815 #x3BFFFB3C #x69F24431 #x4C559DA9
           #xAFD034B9 #xB35162D8 #x394BC986 #x582FA3A7
           #x9810CD03 #x3CDB05CD #x7FB1B327 #xDAB9F16A
           #x0FE5AE7C #x5849D7AE )
    :y
    (big32 #x0004BFBA #x2584AB6F #x34792D12 #x458C7DF2
           #x9DD3405D #xC455C96A #x2BE77803 #x29180B15
           #x4A068E68 #xCE98A197 #xD1811CD6 #xCDC727E1
           #x86CBA3AD #xDDC86F47 #x6108B246 #xE8038010
           #x295E9154 #x9F090B8F ))))

(defvar *board-share-wm* ;; William Masson
  (make-crypto-share
   :x
   (big32 #x0093EE8F #x532D58C0 #x2D945A1E #x56DC0437
          #x1199D933 #x93E19205 #x10252FC2 #x7E633F74
          #x76759987 #x3D569BAD #x6539323D #x383647F0
          #x6CD47221 #x6D04F68F #xED256592 #xC0DEB47D
          #x0AB61A3D #x8C02DFA6 )
   :y
   (make-ecc-pt
    :x
    (big32 #x0352A114 #xFF7BD7B2 #xE0352138 #x85B56C47
           #x96A6BDD7 #xF82F7D55 #x579B7948 #xABBD4BEC
           #xB658126C #xB01B35BE #xED34384C #xDAA397AC
           #xC6F1ABB7 #x066615BB #x72F3F4E4 #x00BDC60C
           #x5B504D16 #xFADAEA09 )
    :y
    (big32 #x040A759D #x8AFF3B25 #x9BB1D747 #x448DF450
           #xB17BFEF0 #xDB6FC92D #x48817CF0 #x988B57FB
           #x7944CA29 #x1A6411ED #xFB2AB8E9 #xA4E886F4
           #x32F5E922 #x55DD0E3D #xCC729743 #x65725BB5
           #x8DE5AABC #xCB886CEB ))))
     
;; ------------------------------------------------------------------------------

(defun gen-board-shares (board-share)
  (let* ((share-2  (gen-share))
         (shares   (list board-share
                         share-2)))
    (loop repeat 10 collect
          (let* ((x2  (gen-x))
                 (y2  (solve-lagrange shares x2)))
            (encode-object-to-base64
             (make-crypto-share
              :x x2
              :y y2))) )))

;; ----------------------------------------------------------------------

(defvar *bb-shares* ;; Barry Baker
    '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSG8PLJt5/50oiJ
5pGtr+6kyeOVnO3RyaKYh52Suc/i5t6Audy3oNjYvsXFzNLc6L70yLKJl+Wqp5ai1NSzjq+4+syA
192lvN/nxenZ8IqOgflREQsBAyAGRUNDLVBUAgEFAQcEgsT8z8CGmY7Hk6jt/YjembPurLvD//fK
u9W27NSq9oeCsfLc97r9osiq1pqQmZu7ovzkgOmuvZKSjcvG0pat8JbG9Z3vqOD9qZ7qjNbO3cLO
VQSKyf7W47bgz7jjo/fF4Lqp05rZ3s+Vn8n56fC7ktvIt9/b+5GAh+/a7uuU3Mmh1aLWoNCvo7mK
tafX6fGDgfzCs8uXmrzYht/l2bTs/azSs5lb"
      
 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSN77flltyPleCQ
hffF2++ospO5z9y4+7ji5PzwtKrv7vHIiP7W57zP3peEnMLx6oD3/KbskNjR/dr3y4n/xrSkr53K
4I2B0avQiKeipfH7nY1IEQsBAyAGRUNDLVBUAgEFAQcEiYKLi+HXyu+N9dqvz9aRpca47feGnamF
n6/m1cLT2JGT/JLP9Iv/0dWsqMbrk8f0gsnc6Mi0qbDgwLfq05DrrM21sPzAk/fD+aGe7valwazf
cgSP+JvXqOOrzLa4q663lLnX2N7jk676mqXtwJSz1f+NgI/errrlibWw4PqW0dyfhLXmxujI4JD8
qqKkp/yBp8/fhqqri/eclJ3srMGj+JSj8ONb"
 
 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSBzvzQvL733+CX
oNTGp5z01a76ytSrkbithNzrx+iu94nuwpSMh8G4yd6J4KmCxNf0+a2Kkqm0pKHY8fjxhe/+z/Xi
4Nb1w8HkleaI9qmPqZJeEQsBAyAGRUNDLVBUAgEFAQcEmJ6I1/7/9ITz9L790v/4wte8nfvCzYmd
85jVvJ+m3qytyM7/7Jy6iJvK2dTH5YSkjOTN2eaz77CljMnxwI+CxYL+1vmyu6m0lLT0pPaa2dZN
BIPEt5uarby9q/SF2LKM7YOhs8/Ut/PA7MP2o8Tbw7KC9u/6lr2JkKfA6sSWm9G6sva41or7j6G9
kZqYt4X+4cnKsIHDqaH64rmiqdjBpcHo5QE="
 
 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSH3PWS/83fua+9
/ubyjd+Qlp+4sKSntpHBtKynrvm6tsyn68qC6azb4r/U7v7Rlcy9vM/L36vl4argl6TiifuDmZHw
u6C09N7a8JaFx7G59rdaEQsBAyAGRUNDLVBUAgEFAQcEgcrc2NX72ICC16C05ebHqPLp3pqjopSr
yK6J8JDe1YHVvsqvheSDw4aZ8eDjioTM1YON3Pr/s+LblufHhKmsto7s1qnAmv+FiJ/lg43LvIa3
agSO9ofGtsaP87v1uYriycbnz+6e6ejsv8v6mrb/lIjV292u942+sYOItN/u3sTdnOq4yt/J0K+p
tqjk/djQ1pr6xJjP0pWOlpnU86qUpeSlnP1W"
 
 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSBo4PfnJXR+7ei
/uWZwtSuwN27ubWSpM6d6dKxg8zovNKQkemZyYmjqsLQ2ceCjJPtn46318STv7HyjL6+y7+67+SE
/c2B+sP12aXjjo3Vg9MqEQsBAyAGRUNDLVBUAgEFAQcEiJrVl6C9+buJzfah+Ya7xOCEpu3x997K
85az1/24xprbytie7Kqnj8Kqr7a2v9fCi8vQt92Lld6Ywuv4pbXjzdSoyY3ZgbaMh8TC1aS61ei5
FgSMu46dw43r0erVtJWAs6iM1sni+vCH9+q3x4Sk+5ek06WgjduwseLS4OzOjfalquOlgPymh/6D
vZDe2qK51LLc4rGwi+eLpovD8Mi69e6jw/sP"
 
 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSI4qmYu+/Zs+Ta
4uyj0urHnd7vx6K3nZbLlbymrqCn77+yh57issHP84vdouXvg7Pd+ILD77XauLeRqPe00+q6yq/6
l7WjotfY2qawx+PzqYFlEQsBAyAGRUNDLVBUAgEFAQcEhJuNuumLg8zsxuTq/dav6rDqwJ/gyNnE
wMPSzufJ1OCl7pKFrqvawYn02LqUmazj4PjKxvWw05Pnm6TtldrMpqTP/POBhabtgqW0rPj7l+Wu
NQSI2MHVh6Drm8G/66vh6bmAoZ/C4p6ymJvVp4e9ran91bHnhKPunY/DyN3qvozAlcCpjJv4i6eB
q/id0JfxoNes6qLA1Pmfqo3Ph47VntD/qOQP"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSE353DgYK07M+Q
2pqcvvvUlIKf+7362ZzT+5v/zL6m8dzi9b/WqLHUvIDz2a3F6O+66t6c3Ibkz7ipoOHE5uWl4a6p
wZCsyc2p6YGh7uiB5f82EQsBAyAGRUNDLVBUAgEFAQcEi5HO8rntr6TLkL/Ex8rc7/Ow2+Sk2YSI
uYT7m+T35P/o9euUpL3pwJjTt46UioXW3vezvMe+67ub4JSqoPTev6b0tJbkyu3ywpeF19qItu6z
IQSF2vzErIb9zPvwxo+e+s3Lxtnqz8TrpY7fv4ywmabB9rfTrZ2Toayiz7aV7pCOyKjZ14XJs+Sf
0/qi0Yuf+u+/4L7K8Kyi8Kn815udstWa58E+"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSGup2/jffp9oCO
ibzW9LTsms2RmK2Gy5/+oZDY6uuW0dW/qIzmsN3Po9inlJHnrdjEoPqVsNiohMCFgIOMpb7opMbD
vK3m9Obclqfi3uKutdpxEQsBAyAGRUNDLVBUAgEFAQcEjYK3ppiyjaKCjtqX8KufmKL7+d/O9sbP
ztnMvPbPq46Wtu7DxfOcp4DjqIuJlqLviO6u2ZOFwuylidHBherF49i/x6iW/uWUr8aO5qffiJL5
IQTqxfDZz6y61a6Xp+380Nij0fW3ttOs2Mvm/uiA9p3J77WK7bqvkL3wnfiZ69+gmb+zpNPnlN3X
3YiXk+v3ss2ppaSBl4zyyvDcmNGim8+373I="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSI3YTE95+I0amd
2tXtyJyahbjVof3Kp6v969L4lMrt+faVifHE/onRgOXglamsjP3q85rc64O5x5XH0/GVl/bEoNu0
goaJqfyc24K+0pib7L5JEQsBAyAGRUNDLVBUAgEFAQcEiobW3KXY2OS53uW726+IkZCNr6qr5Njo
uoPV7/iIqeGKn7OJ0tSenrWFzMDqzqm9hvXo9LDqhJLYoNuJzufo4fPGyKaqu5r5saTFgKiBkODr
EgSLypDUxe7ziaDalIbllquJi9mo6arB7tHwv5DzkaXQz/qGy/yDg42hjqnht8yJzYqS+5mMmaOL
6tOYoOa1ktePz9j2lN3ZsJukzZG4h8jI2ule"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSBzPKEscyY6+X5
+t6Ot4me6/7YzOLK2LzarsLs9NKH5Nbd6KKXxvSQ6baZoMKWq4Gm5ceJtvPIm/vZiOCc3b/Nw9Pb
noKapZnzqNTwypijq7ExEQsBAyAGRUNDLVBUAgEFAQcEi8bp4/fB4IejjLeUl6KywsGPntio64zx
tKG5tvCh7an/lpfx5ovjmdX6mab+qcXfu77f8Lu38r7ludOYhcug+OrLocGvr7vylu+PvLK0j4Do
cASEs4mSvPivhb+omr/J6crdq+KJxc+7hdXG8szI17aIzLnYiOPBjc7bmuam95CL26HcxtuJ9bXD
0Kvomsifpt+H/um625yxsKyjxtHmuZaAxPVY"))
  
;; ----------------------------------------------------------------------

(defvar *ah-shares* ;; Arthur Herbst
    '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSKxLDdmr+9h/fu
65a0yezHjqb7z5PG9pqcqY2C6fTflbbN9KXvxIP78pLMgIHrzrXc36eC3rj80uTeyJv9zZCKptv1
yYq18LaK3Iztob/Mw+42EQsBAyAGRUNDLVBUAgEFAQcEg/e4+4/qx9iKlemh2tGH98fHn+mIhsfq
ydSShv2qjOKaxM6Gz43Pqt/Hvvur8/yOp9Tt4PiV2rqurP/N5+ib1NXThYGSlvWru5Sw+Z3LtprB
BATj6s+g6/3mqe+HuI7Y+bCbm5GCpcuu+tOH6ZXB7OXph7+myfutseXx0aL/2tKRxPvC87itv425
3bfK0NGqicOlr9+j/LykoZDI+8GftfGDtXI="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSMwbbCwp3YrrOx
+8K6iI+k/p7nitOWgtfqgPWQp5S8vbPG1//427inw7joxbeU0reHw7ngw63ZnOWs1crP4++siYbz
kMbE1Mjdy/6A7Nvrn8xNEQsBAyAGRUNDLVBUAgEFAQcEjsLc3dLN0Irn+pDJuNmjyvmv95/bxdvF
u56SjruXo9S9vZONuLXX6oyD/Yi2oa2m3fSwj9Odr7H4vfud8Jek2qHww7f5it+xoI7536Kd352/
fQSI06Gc57/Z65CDlvW4waHh29nh85aBp/Xqr+XGvdHSntCutMfp5e6TnYvBxZ3NzvDsgv+8zpuj
jubo+depjY3J4b6wut/L6LXAkuei3MfK6/I6"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSFzqzYvarY9dvO
9sCRptK2++zeho3V8N/c+Kekmvqk8KSHnYWxxMOv2seg/YnKgdrvrqKLp9Xd6eLKt4vMzruF1LWV
se6m2aa0jNOig7TejhoRCwEDIAZFQ0MtUFQCAQUBBwSE+vvavKi4lsOj4ufZl9btnuax+8bfusWl
04+K0Jae09GQzOiylefess2Rv7rXs4aAwIPE2aiY1tuElaiuh4PqodDak/OLi8zUy5Teya2YiZZF
BI+Bo+GK/pbAo6WZ5f6Q5K2S1M++65TYrLKMoIvziM+Bnez2hfq2jPrBjcD8t9DSlozhjO60uPm5
9t2Tvc7o2YW9546n2O2K3du+9qLklLnCgB0="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSC9eXkroDWmobm
g5iyp6Lan7Hk5+vctN2BhPDD/tL1sJybmZa81IDA1rKhzJqAkYLl9Ojz9p+LzP3g842s0/mlpfT8
ianK3v6T8Iuwqr2w4N8eEQsBAyAGRUNDLVBUAgEFAQcEjcL8zNmWnv2jtOXlrN7J99XH4tzjvaTG
lsWv1Ljf6rav8LTXkrPIxIXWv/fMh539yt/D7I7rlMLBh9bMwqmIoo+nrMemydmxqYaxmoPCydqQ
dwSK0dmhp5TvspyLpI3WoPDqmYTP6tTH3bit8LmX/6i57Y3JqYCSxPX/sZ/0/O7auumV5IWmsJ/l
8MGHn5Svi+PnvZ/VhfeY2uHN56itkMfNorl4"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSI5sD7rJ6qxtLp
3Ovm74uhrOvs2p6E1cGBxMXx3szk58G6z/uw4PuZ45jRlYq4yKuX6Lr07enUvs+P9t/YyZm/4fyo
kI2q1On5osSKhprZyN9YEQsBAyAGRUNDLVBUAgEFAQcEi/7Pp73Fz4yFtc6EmqKZgqWJr/Hc3/nH
qYOX08DjuMeD5ZrUy4eE69bK642Vjcq2w+WgwaWNmP+4rfuGgYH1q877nYii8Lad9N7vlcD8g/Oz
JgSCgfjP4arnn4/ZjYeut4SD7/Xa0IiF6qeP2/zoh5ueioSR7eny8rSGtMrk4Kib6sTw3sfjkdPy
+ama0fq0k7m0pKKXl56+8r+09NWky6rq1ZFG"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSMteCjo5LWp5uk
y+CtofiDs+mD1vGNua6fman4tpWI5Jjmg4WM9bOJgruI6eafy8SHvOfVu8rUz/rtg4LutZm+rqrN
2+HNm+7ln8a8wOOFrb8jEQsBAyAGRUNDLVBUAgEFAQcEiN+jocm04ZrP3fKx8sSUjMbC5pjUqZSG
xtXtrsygwoqHz9Kkkr3uo6igu9Oon4ij4oDAtYb4xbXvrs263IeZpJfokfnD1dX90fLGzeHjgfKS
DgSEmLrk1sfN9/jD+ZqBrvGozJG3yrya6oy8pL2g99mz0dfj1Mfj1qC9pbWchN+/uIyOjZm72K2W
pfjgy7/r1anpzt3kxJ3619qtmdXi5dGdwIgR"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSM9q7K0YqXuKT4
p8ejh87Mo5C5labVpP+y8tj+hvOJpOD28sK8m/6CsIrt+PDDy5T0qoqfwInV/NeW7qW4+8mauO/H
6Kv9i9nq+MiK4YXwwYNkEQsBAyAGRUNDLVBUAgEFAQcEgofKw5q71IDC+rvulZKViOHL5aHLjar5
2uHG79Lko8mgjeT3uOuGsdbCl5PV6p3yst7Nldee3/veg8uq5M2xj4evp7D53bHO6bH+6P/7hOXe
fASEj/Sj6YuCt+WOn87/l+6luc2uzurYurPz9p6S97TQ4bmHgu3ko8/q0pmPn/2G06r5ye/K0euc
vca6+tety/rk5enV8vCKwZHn76Tsm4q0mexg"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSIy4qr0MbzhL32
j5HSjfGQ2ZSwhuq60Z+99d/R9Y309vud1Z+qhLvs5smyjeeM0svhrPCoi86oq8uiuP+7zZ6sovjW
7oCj39f178u0/8/X5YkHEQsBAyAGRUNDLVBUAgEFAQcEj7LB8p+D3sbM6uPR8u77jPWmlJWkg4q6
2oarmeHg+p2E7o3y8tHJn/vVpZXD08rD8Miig6TQ6ceY1Nuhz/ixo/r/tOm5uKfWkYPa/8qB+IrQ
AQSq77fGnMDEn+/G4OD02JCvoszCqa330aSR5f6Y7Jje67/g6uGep6bgjIXngIr82K+17tb+qfLL
3ej37sS6isbgmP6tkNO8/tHltejG9JDDpwA="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSPtsrw8LmEroO9
79uF+42+zfPu6qe7+q2p9uH6y8eZ6riy27as1/bi+fC/37TqyNz5lt7rvYLx0N+C24HNxPO1kuTb
mM6r0KGPrrbY2eDSvqJhEQsBAyAGRUNDLVBUAgEFAQcEgs7+6cuSo+nWmpby76D3mfKgmZa7h+PD
rYqJ8ujElpzDm+3WgJ+j4ofbyJfN+ODO0aurq7C/6rSe7+fKq/Gau8jPwI7WyObMzNuAssj/y9qM
WASKj+LPxbaQ94KJ/JGc/pDUmvjHy+Oit6jC3uOpzKHyxYXzkv35kJvv+9a51PvN+N2TnZqZh+6x
v+DL0KC7j/z57OiHpduXn5G72quLsbuQqsku"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSH/6eTiZqTw628
yMin9PnFtcGZiM3i8bG4/pqcss/uwpfKl66MgtLo9ZbB3vPcpcDruL/etYrkx7vYrKyP5sSmzqvr
nI7i6vuNwMHRm838tfUUEQsBAyAGRUNDLVBUAgEFAQcEiaWpmoDLqJqXhpLF1uzLm8iGnNWFneXZ
jdu08PfF7dfFgev/qsax44fcz/a3gvWMiIPBlK+/0cSq45WilbSG/ZbArebv+IS12KuuhvWxtYWw
DwSctq+JmYzD4f3ytMHW3LHZ/5DuieqSkbybuuCX37+f0L24+42b052g8pnXuJH2s8fxotaGne+h
l7D55PG3vf/468DQwdac9Omr7dW9jve+4gY="))
  
;; ----------------------------------------------------------------------

(defvar *wm-shares* ;; William Masson
    '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSLo43I0ur4o8S+
wc7r1LDDpaKY/6OYnK/I3q+J0qCkhM/ErY/zp9DIj76904mysoj3ofum36uaq6TD7ebFk4HKh5GT
1YiG3rCz5aXjiNO55vBNEQsBAyAGRUNDLVBUAgEFAQcEioOvk9e8n9W/5Ja15Zean5u5y+mVxpHR
yOeAztj9kreM9o72sNPSvvGu5PG8qabI1v3rqMH329zYu6XVlbzIiq2bvYuXvqnA2f3lq4mHl8r/
VASC9Lyqmo6hl6/9j52Xyfq25bOs9aPqqpDO+fDo2bDYmeX5gd+v9Y+o1eaVkLXCoJiY4sj6xKL7
9rz64NTj74KIrr/K1e/AyMOfmYOv64281Otq"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQT9tK64ocO5/5Pz
lfSnlLOX+7GzgbaD2syRwZyvvKSM44jYxdKSvea07e/jxKSWwoWIoYe1wIzf5uLczOWkuoqw+5Ln
6KDBt5ar7PmH8dDElUIRCwEDIAZFQ0MtUFQCAQUBBwSPsMKlmrns7dbg6PTm++maksHV1KSs4aXo
2KeO/ZO1nYeRmK3apduRnqfr86bGn/emxImq9ZH+ofTl+bHBzszfu4HGsLGYi+/bko/vmfWqmN5C
BIzRuYK1urnFrqK8ndqgq73W6PSlmfKTiZbntcbinN+75s7ey/LLv7eo6MqwyvXU0Zmu+7GdquCa
w//ivY24hfCdtdDRi4Lx/+CowMfkmev/3iE="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSN+dqZyuj0xc6E
/cKkj6LHsI6G9cHQxo/AivyC+Z7zu5/FiP/u8K7xuoX3q+rgjv/m0Mzr7uunpM/gqYXp7uW9zvni
pNnT5eX+8Pqj2fGL86w2EQsBAyAGRUNDLVBUAgEFAQcEiJ2F2+2s7N7xhKjOmbTQz8jIn6zL46rT
m8f+/eLqlpPKtrHiheTHsOby28/pjqjv6MzGtu+LvYjH9bettNTVhpTIvqjXqYaJh/vtv4aWwLWI
dQSuivf3tqGjkr3Q/Mvr7Zqc3o6/rsHr5OabioOu/pO89YKL/K3/h5KRrZyHqeTrtI6h59Hut+Dl
6Zmn+dOdrM74odCtmL+91+a2pNHvhuyLmXE="

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSLwdDjpqi0y++F
15aU9NXvlqnkj7rR9cWFhpfQ2PCr6uettMq6pPXj3t/HrfrLosX24++m0bykpczghKml5pHHpanR
rYuX8Y2X37yj6dD6k7AiEQsBAyAGRUNDLVBUAgEFAQcEgp2o496G4rPBoYbIo/CRyoTAsqf4kY7b
u9airZjhvPaNn9nK9vKE9p6I4Jjena2CzbHTi8K/kZPkieiMnKalhYv10bmN873TtdSxi9LAjue+
UwSMt7LShfTTq9v++orIuNqogJyHlr7h/Y2rneiCxLni486L7IHdoIifvJHYx/Ljzq3fwePW0OTP
5+mqrION4snVtfG+l9f/yKnxr9vn0/bUtoRp"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSJ6oigxv/d7aus
/4aUkYGNs7/Ltsmji8Sf6pDy0vSx2J7548at9uSg19Lqsp2wgJuHgv+gzP/sjMLOw6XL3amD+dOh
69ux3sS994ahw7bIrIhtEQsBAyAGRUNDLVBUAgEFAQcEhN+wofj9jc25nr2Nz/62ibuA3Y6e5q+9
k8Kg0PzvhK7Iu6rBwuSjoIGsyJfG9a6UgOrJkofEl9iP+dy26fLkhu2U0Oz485WctL7bqqrNhZ7V
cwSG4JeXoN2K17vT84mjtpXo9PLWnqKKif/+ipL0sK3j2sOtisjHur/oxcT/q4a+69uO2cf7y9XW
3YDR2pm8gY2QoNDl0YaquOeN4uHar7uOxs1v"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSG6/OGj8GT84CY
uraX0aGmmaCflc6wvuXolIDmluafvoaz+6L0s7y8j/fLuNTXu9/kqKTl6trDuIypj6SQrOuKzbjC
qLHbjM7ikq/9mLLA9qdREQsBAyAGRUNDLVBUAgEFAQcEhdi60pnD/tbp78LSrJmg0OfHpZrXwO/P
mPX525OKo+i8s+mNuJeX7vS/ify8p/H4m+Cft++mqdWqmcfWhdWeiMv6tub+p4iJ6qWg7drH5L/s
NASJ0vTRzJ/nzb63sMGbv7bIo/m1sKHyyKvlrZz0opDRnLeW2P+zpsuD5IOi0sv/zo/374Pjm9zm
+/a+2IGmrtDZiNDq7r//gPyyp6z+49K9x4Zj"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSC4OCdtI3xvbm8
upmmlKWpqfX+gpSnpaOuuP2J04e5uqft+tDurvzRusTxp+HJuoOs5eX9oqTzvsS40prY39jtudmE
68+ditbbuNSH0f+M4JlMEQsBAyAGRUNDLVBUAgEFAQcEieTSprbP65/XnZmJ6s/z1O/jrsG+48LV
t7jS0Om0/Iv2p/Th0c2hhsTvwZrB9OP8qf2uw5mMoIGVoua0iLDOlrfD5aHj1Puk/5Dvq+y+oOfi
JwSEjbilkYb3m/L/8IDLp6aPz/vJ+pyB5I/7gsuB+J3s8ae8kLOuqeX+hdzipMm1y/m++7POgLLL
zN/m/LT167306bH42fWV9c/kv+zdhvqM/5t6"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSN8KndiLnLjcOj
+8HY9PPBzbDy54Cq7bykw9CT2prOlcWMs5qb3cz2hdTmze6Wp8j10t6o8LOmiYHb+vjFhbT3yu2a
q+i6kMKbmvuIrY/r3NYSEQsBAyAGRUNDLVBUAgEFAQcEgefW15DgjovL8MH79LG2zfq30quzgdTz
k96RqJ3rw/v7q8fAzrnDsfuSlMOHn6+C7ZPk2fPT0qeh3+ey64iwnb+TsOqg7tny47ih+Z/e4pvd
MgSP2JnE3Py/p6+W8IiG+ajj69KeuYWZp77X9eP0+bDo2PHlgajQsvzG0OfmiO/YhZPLnYyXqoTJ
1t+D19fN07fug+uAvK240M/g08nUprry6tR2"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSJ7snBuN/87rDt
y4zU6Nm6/6m85YuHuJ+8i8Lpq93Y7LO287TpoLaFyNWhlqPPr/b22q2d9+rw5fiJ0NW32OiCosjB
g9vuqryG6syqiM3smux3EQsBAyAGRUNDLVBUAgEFAQcEhqOj3qzG+IyHgNv+3r70lLudsvnb0O/V
pJranP6s2cTUxqSNtbn2pO+I+5eB44+buJCzn+Xiz+ny2cCbnInWhMWd7L+I7MnP+6qU9KiN57q1
KwSMi7CspOSkj4fCgsDLhI66wtqBw76ayNOuufKC3anBmaWK67/9sK2Mu+7Urbii/J7P/ui/0Kf7
xq6ho5G6zOzYpJaz7ODc3p3l6syhtfrGqKN8"

 "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSIuPm/2Z7WlcyA
r4++upG45cfU7LHI9cmrq5SItrW8g96r//eSua7fkbKXkun48LyIqrab55Cqz5aS2PWwjNn+66nv
3cLbh82Nja/EmJWj8qAGEQsBAyAGRUNDLVBUAgEFAQcEg63w2+aFjduX6f3d7ZzbiIL9mJPNtZn8
xLqRhreK5oGUtO7OiYKx15nA+bKD3Oi/i8T1gPObqL6emsyY67uM2c6Wm9vw78KSgsPh5vyHosDu
awTmoauUo/XrruPepLSHorXh47P/zLj8u5yu47zCubrrsN3MgLmGv6ncxdnCn9nyw8eIprvjvo3j
jeiF/4fD7Y6mjO7gr9j2mYaij77b6NPOrXk="))
  
;; ----------------------------------------------------------------------

(defvar *pe-shares* ;; Paul Eynott
  '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSmlOWsoZ6Wo/rF
59qX59LG9+KBhpXprayt4M683d2vzciQ2qWKy9H819Cmq9f9jciM3/vRy5jniILF/cvCtofSiPqj
5fG6sNXFzOup98O14DARCwEDIAZFQ0MtUFQCAQUBBwSOitPYuoqnsNvgrZjn6euu5LW2irGT6sPR
o9GiqaTcitC0iYbboq2J87G8jdSX+b2z3dmggKn81IOXkJWippCb0vW8iLbx2+X20Y3H4Or2kq0X
BImqjNyx05zBs4/AnP2+qvaw/J3gtNSB0aPx6P/sg9zSoMyjw9fipr3I2dmBldDs8fG7uoqfre2G
vMiKytDktpGT4JCDgLqxsqHXxcTy19rLhBw="

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQTcwtnZlNulxsL+
0cfVi/TQiPylvJS9uePZkt6Qv7Tg/ojzjY69+qvD+qWTg5/U5rao3eiSlsD2v/6u5O+Yu63hzey5
g86Wg4y4woOB1aLc8g0RCwEDIAZFQ0MtUFQCAQUBBwSJgMbejdXQ1+rfp4mSxo+djKiCqqO7++eZ
wLrd2Kf8g9j1upjOg/bXraCx7JPMtZ3Cz6D2pa3V1fTT3KnMl6/n4rSjwovG/7nf6MqYrdS2u4Vr
BIuzksuPzqeO6bbEq9/Fnf+527Dq8fzLgd6l9N6Rv4nJiurqoZSFhsbSlJ7c8M/xor7TyOiUj/zW
gI2qsKj67vidqvGXysyqiffT0cHQ5Jro6WQ="

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSP2cTRj66l+97B
74qtnPvE9b/txNvYoYGDmdCG4oqTvJK1rLymsLPrg/K9v+meyOjI1933hfOQ4+rrs8a28Pjdz4rm
jevl2O7H94+hi/qJ3MglEQsBAyAGRUNDLVBUAgEFAQcEi+L74qPluYDTxbeV+Yz7yfmo2fW7x8rV
npi32pPnvfmh9cPs/ri+/s3E2ZeRzLGWz4OhtMKe3ZHN3+npsZaguvioo6+gs++il7eM6u34i+nT
KgSCzNfey/Pipe3t3K3BvKyv+8es1PDM39fV6OTHgo6cyKKVlMGv8IKTudby/Pys+6aEzp+71cXB
hOTdz4WH09OO0bH+zIjX9+eL0LzNpsP6444h"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSFj7mE7suBg/m/
8faF89mhibag0e7ku/H3+qqV7LfV5/z6kPu3xuTHs97tz+/cj+DI1rGjl+rn7Nn68s+yqcmq893r
5ajUx9vUiPyy88SM7OQtEQsBAyAGRUNDLVBUAgEFAQcEjsuV0tr7xozg79/Otf+ewuTx3/rstJCl
v+fa/b/cv53qnYbA2MexhrK+37Dlo6ODh77a67Te7NmSi4bF7bq0g5K409rwr7ujkozG0o24i4Gz
SQSLneOSyYiK/dSyvYLzo/rJ+7HEvq/I66Pgmo6ovYiCgPDajKqKkpKum5S9loag1Z3U2rbnjbbH
4+Cj1abr5qTiyMKT+NK0uYDGycPN9eHomYEB"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSGh6ml6tfW/be0
lteVhOaGkqfpzpejgePJy/afzLj5j/vfuLrXoKKTlIiUpdaFh/zFvcbU7K2qm/a2ufGyy6/Ooonq
8vq9/d784pTwvqSZm4s7EQsBAyAGRUNDLVBUAgEFAQcEgr3fmaydmYipuuHbwcOxybq79MeropHE
8PuNlo25hYTo/r//n93FjbudkK6xzv/A2uzF1crtpYyS69DtzaDaqN2uwqCsmYG0+pjQv5qps6Xk
bQSK8sDFvsuR1bnq48rZz4WaisK6j++mzbue/vzo66iQjKnzl/nl+Oqvj6SV6bLM3cfm2uvu+PXR
3OPTgu284vHh4emBtJ6uutq5sb2orKDu97M3"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSN153S1OaE9KDt
gfvB69To8IHk0t+kz8Cp6fLTjuPYm+fM8P23nu7Ryoqz9Yy6lOTurOzW1+yEo5HQuP+jwtGu5pfz
//fe64ihot3FkNmRgts4EQsBAyAGRUNDLVBUAgEFAQcEgbrildDozvbIuKuh6pyd+6qO0ZLckazt
oeGilbbP7bu9nNaA9bbPzZyIyKXw3Nz/pcGE6OftvK2o7/Cmsbargez73veDlbL9pYjBlNz6i4z3
ZgSBhKSkxq7JjYeDjOfAp7KcivDY54KBjrKExO6qqLbl8cT0iObOlt6QnbjwqobQhsHmgYW984aW
peKRr+OH8qyw4M7ku6GEtpnNv7D+2bPU7bR3"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSMnciEmrzu/YWB
5dzno4u7mtX++O/s3Pnl7p/Ar8eT3sLGkP7Qr4fFvqXL3qjxhJ30xfq//P2a66j23tGwisfT+IPi
5NLAk7mI4fP0nKGl7KJ3EQsBAyAGRUNDLVBUAgEFAQcEjrLHjMu3mI2T2Meq577Q/uXO1KPOxZXh
uauGx+OP0ced19mh+6f7yIrNxPjG54mwo9Hcioj3no3pv/Ho2aiGs8+/kfnb056Tr87f6qe4vPav
RwSJv+eByJnNzYf91+CC1ob63qDVwe63n7b019a25u3/1oLD3tOQnLHn4Z7l1tfg6ZXz/7blr5bU
+e6ejMyw8qOShLPi/d/N6fi9hO7VjZvIzZoM"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSEku+4h96p9PPx
l9b6lI6CuP+mieLdp+GnsYW3zJSru5zPxofA+8vMwunamZemw4qY8cTh1sCKtNLFlNCYn5T9297S
5Pj+ydqi1qO5t+bAvs5sEQsBAyAGRUNDLVBUAgEFAQcEid3d0eDN+bWw5JviqtTXl8eKk+Ti/4CH
8Z+ch4iEif29g/Dp4JW7z7e6u4KgxprM0PeDmIKhutCqsLL/kv3vur3gksq9zZW12p2Cqo3h87rx
fwTQuKmlvbHDvt2Ozqnevpj5tv75nNablMaBqbyJlNW2qLWEhOaL29S0spni4PPlq82ascHXq+/4
6qHewcqx8LGw2vz2w6uWidqoq7fcqvPUynk="

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSMp5TJ3rqzo6e5
hJWaxufs/v/mqNPA3+3Fqb3Z6pHrorzIupXGg6ydmoDw8dTRlZaBx8ST/bKi3uPmnuGiqNiK2bD8
5tTn6/eY0JDMo4amxMdqEQsBAyAGRUNDLVBUAgEFAQcEgZDPjdHUsqjym/OQi4SDopiE7ZuL9s/v
zfPtur+0rbaZ6IWC9On/oJGs7ZfU6IT1nJXur5aS8NW7sr/fhffpyJHs6Nius9LpopGL+piczNPU
SwSIs6fJyuzsjMHMj+SQ15Xn5Jj99uWIj7jO9seMnujDhNThxMv5vKnpneSU8I7nl7mvn5fi4qSw
x+Sr3IeO29nZq7mu05rAr4Gfutf12eaJpY4y"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSCtvT4ksngr//H
ve7rztvckrrAjLDm7+PYnY7vjpmbz4DkqMuPwu2U35Dn5430/JOr7+jn2qvqk+Ci6PWbjf6kzpim
4fem+oWUzozantDZnnIRCwEDIAZFQ0MtUFQCAQUBBwSHwNCFsv2SzMGmibXco7LMjJ7Bv5b9rJSQ
muK27P30hN6XnM/j/YuvxLuEoNjqgouB0IvxnPf52sOmsan2t7Cvyp2k4479yMvLsrehjtmH+fZg
BIXi+5vsjrjN6qea/YTlqa6+zN+F6vbC9Jze8KiDqbGctOCK+Perx+Gbg961/8C55aiY8OK74/vf
+r3UmoLf8c3Zz4n7/azG1IC7ppem4o7fhCM="))

;; ----------------------------------------------------------------------

(defvar *qn-shares* ;; Quan Nguyen
  '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSFyLrbzPCziPOy
3qeW4r2U3tuX76Tcne+fq4a/tJ2++e3OuuPeu9ea4oS/jpLTwdSliaadyuOkwI/vsqrr9ciupbKO
w77BlbmTrq3ex+yXycNEEQsBAyAGRUNDLVBUAgEFAQcEi6/GnMD6y/eNn+qhjN/ChufLha72+7SH
7eOztZKkoISrz8fg6bDvzpX5iYnS97bFpe+2u6yH1aPJ2KnMm77Lj/rFiu+4kIjl9J/ttLKh7Jep
RgSL1+TcnOTr9/LE2tDMuNLV5ti63ISP3szWsMKep+SMs/K0kOuyiNSQwdH7yqaU4cqK0t6Qnreb
nJ+f4LXEsuKr2dbc6uKKsKnS5f6HxbO375sj"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSE/JfTgdi91KO5
iOOP5fn1lr3fh4O5wem++v2wvda87s7q57u1yuqAqden5bnUkqjRlqWCvZ2u2rXWx8qMuLfxkNXY
rKPem/fywInp/rHa1v8mEQsBAyAGRUNDLVBUAgEFAQcEiqTxzqri3ubpsarqsf2yt7C+15WWqdzM
m5Grvsn8rq6cm4Dp8eak7LLm4+CgpsyXrIPC+6iJ+6yl4Y3Cn5KmpJyomdeD8PHwm5/xot+I0Iz4
NQSL9pj4vdfA7s2WjYewj5Sgg7i8qcywkfWF0puXh7+j+bCJ79+MrOjsiLH8loersK+1u4aHj/Kg
0e7ou6Hiq/G/9brPiOzeuY6jkNPC6uyzwcN/"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSDz4//sfn7g4Tn
g8imva3Tm/nEjYy7msLNis+y4ZqJrNX2suDJ2ZHgxIP82JvCrPO0maXXgI+olKvE1OSHw6q2mYic
0sTWpZqsvKeui62n2vQwEQsBAyAGRUNDLVBUAgEFAQcEgeiO3euj3aDC+/7sobCW+/Sz6rLV7P3V
zs6a4cTEs8GKvYT4yZPR+qb3uIfitPjExvyT8M320+b4vInAq7D2qf2otqPh6Pie+YCplaOm46KY
fQSF68bv5cjy37jM4ZTbpOn6mrGv4930hZatloC4pM3r99Pt9rL29pShlaD896Lhk7Xl09KB0rOA
9MSMvNrD8ZHCtq+PsJv9t5Cz0vrqjJfC3Ltm"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSI3++c2q/Ylvmg
+tfV+4Kp44bx3bjtl5zR/tfjyfaSt6zd8Ibn8Ja/9dW4rb6cn6CasbjLzq6zrc3ry/fyiYKLrrX6
vYuRiPmI/4yMhcGl+LFAEQsBAyAGRUNDLVBUAgEFAQcEi+TG4Pali9DhhfGn0+P+vNGgg//H5Nfj
rb/5l6aByfK764+tq6iQ7qiHvpDK67We26m11sqPqvn4wtK8sqXj7uShq8CdpYPQqMLr/t6TuZiv
EwSMnuGup8Ti28Hl0Ims4vjJr8Sgv8iE2+Tzz8CK3vSV/7uW9IG03YyPjYC5s/yV3/6f263N64zl
3oDvl4ewmJSvgITbs/WZt9b8jJrJ8pvn4Ow1"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSB0rq9h9TF+q6V
n4W0nNOsy7Wumpmd0PH91qXNtNiDvYiytInng7Sf9MO4qrzo+/e8l6KRyvDxhJvG5f7umpeq3sa3
vL6/p+OQrJHi8OfR3ux1EQsBAyAGRUNDLVBUAgEFAQcEiJyh9eeb1ryzqtyOjJzo9oaa1Y3aqu/a
ya/sxJrWmqGnuuqn4KvVvvSqsOq+7oCs78r6juGytPe64vnBhYm9ioOx3PD7ipXjq8qbosfY69HM
MQSEjOjr08fExPiEuqyy+ZnXupasvte/kpKBsarHvMOWv/rPmcDur8PW19qO+eXN+K612Nud5KTU
iYvvxpL9w9PLqpK5lNblzN733rO5vKX3rpNh"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSL8+2Y3+n57Iei
4c/4rMTpsdDxorDQ6I7hs+qYg9mq07qhyOj0/eCilMvL8MTVqNa66uWFuOi86eWQy4DB1POp2NXB
4qHH6vXf5sqnp5mAjZwaEQsBAyAGRUNDLVBUAgEFAQcEi+W35MDRs6OiuNGpy8WLr7yD8o3TysPw
5dGi7qTlscuW96nNlYfCnfTf96GZjKLr36Ogr93ay8P01I6Ux9bVr4vwktzMpcLKxNOr/ebD07CJ
OQSE/oiR4svf9a6PxYKCwNXU+saX0tX7hLHG+JCA07j0ve2k8/PJ8KbsloK3orKng8v3n+2G/IKr
5vfb37Se/L+q5IuN/ta2pbW0z7riyorOsccZ"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSK0e/i06PPzIPt
4r2rmOvslOCKmbCx6pDL1LL2nMWDo/Gp+J7vgZXW1uOMsKL+t6nRg66O1Z71mZzYvqbjtcLsjdaW
1p+1943hw+vxufeox5pXEQsBAyAGRUNDLVBUAgEFAQcEhbXHrqX7mfCSp8X10YPs1+fiip+EoKCW
2t7LrZy9sa6jkryUub2pkqjZqrXilYvXk9y0kaOjz62Vr6vf1La8j66QvZLtronz1Mufw/vfz7Hw
MASPpva21bbsxdHgr/urisibq8rSvsWPg7yepYP1mNHK0Nqk3/KljYys4Jnz58CA8M3Xrty48OLR
2vrI8YeW1Lb3r6i/nMygosn6rNT+xb6pjtE3"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSOv+33g7qI4eCS
5riplrW2prXM0tqWxKOdncvj+LmYqIT/jv3gyPGk1ND169uu4v+KtZXO8Mnd7ungroWOla6Otv7z
ivHyhf3qu/nr9KCP47lUEQsBAyAGRUNDLVBUAgEFAQcEj6D6v7Tf2e/q8ZvchuL9/fPHgdO2zY7P
spXnopHQ76vTx8WXtPa1uoinkuuq09G+vfGJw8DFstGGvMrtsuK+3tOm14Sc2ILB7/mZkvjm3NH9
HASLnPOlhPip8PrY+8WgvJDQjbqY6P/9+4y6gNbL1dDptZrX29qjpYi0sOSAgO7wg6SU8Naztoyv
5tz90Iui2ZvWsP3FrMa12I2f2uafmuHOwqxj"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSK94Hd97TYh46v
oOasjOmDk76cjfTH3J+3/OSO4/mggcqg0Nq11/KP1o/2xZbV1IL/xZLQoeqDu/ei5e/ImLafzbDA
xI2/s83gnKPP67O8mfs/EQsBAyAGRUNDLVBUAgEFAQcEg+XX2LHO3/Ccyp6068vE68qzr6a8sZbV
l6v3k+eVg+uVorTL9cPZjv3iz4mumaWtwOPk68HRlaqw1crR+5vx4Pjp6LyQprifhpqwpcuYiNbA
YwSD/6yzuvvDmIe86q3j67/27qidib3nz9WynuuFo73F2NGk3qKXttmGpOXB7Nij4o+CjKWgsM++
gtLs3ZrojL6cxfaLk4mxo47D58qKxO+pwYgZ"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSLmdzhtM7B9pSZ
l/uCq8Su+qrtt82r7ujIks/rlpnVo/7C0oWPm+mL3Nis65y1sIjwmOu5/u+r8OaPjOSo2OKanNyO
sP+Aqcy8o5OU0siSz8U4EQsBAyAGRUNDLVBUAgEFAQcEirHGzMzW2tn6rsjYo9CG4PmS3unD14qX
7vOlsf/rseGWiei9na7s+YLU2s7I4snX1eTdzpzV/u2JzOOa+9banYbf1LCAjL3eh46bxJ+crYbp
cQTLq+vLpf6etJDx7c7ouKuLh46P98yK553W/d3Jm+LRtoPFrLbbht6mvvay1dPG1ZfHgai9xPjT
/NOykYCnv6O+4NXX5b2GypzLx4XxwuHJxzU="))

;; ----------------------------------------------------------------------

(defvar *dc-shares* ;; David Cohen
  '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSB+te888WF2YaA
koCf8cXs98mE67SIqJfvtKv35M/jm4fAvOC7hMKb4v7VkdXQ0qfgsvfgyMClucuQt5HpgojJro2V
xYKdzNKQwfSJp4SdqtIHEQsBAyAGRUNDLVBUAgEFAQcEiafxlsXQ3IbsksbTxIeHgre71M2M56vT
9t653rvt78+ApPLLnbqf4rTporuatpPz6ea0w6CFwdu2roWgipij7vmh1qysnIL5q72M95DtjdDT
SASM+Z3ApJaZudP33MuM+5aW9ojyjPzIp7XtuYSf0Zn++8Os9YCE27/XkeqlkOObzOXS+dOYndfG
tNOIwOztqNWKna2Su/DThf2U//La58K1q5x7"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSPpvrBsN+86t+H
5YTRzvLPpNu/jouz3e/o6vK5w+eH0M7xtZWTreD94rzAqcWk3be5j+m41f263eSBkaKQ35fhiuqH
+9GEse7V95iotJyw49IwEQsBAyAGRUNDLVBUAgEFAQcEguiKsb6PvNXd55Dhh+Tw3ZXEpIK8m7mI
1dT9nIG/qpibo+fCvJ2L2Z/ckOLs0sPvxtOkrtrv34PzzsCzpoHQ06XzlNq2sN685NLs94fD9KLP
SQSLu8Gah7GJqd3Eo/3CsIaU97XnyaHsuazvu8uQtce2556Iyo6Lr+reibHZuOL+2pDN4Iup0p+u
pNiT69jv55XKjurfx8aDsfvP9tLmlvjsgO4J"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSExrilrrvWjOub
kO6cmunzpOupttTv/6To4IqIxZnqzvzmm5mP+viEsv+Js/vRqdzCqIGRyemG0onb16Tr7oLWz+34
nY/R3ZHEpbr9ifOfjf0dEQsBAyAGRUNDLVBUAgEFAQcEgpndpo7jtYSduMj5+vzpiYuCkd6bybHi
x5XYuaWM4dn24rydyMWOj7LCh/nt6YWlgKDCqqmyxpDZsqeHoMmv6tLp9bfRgpvB6J2VyLOzo5y0
IwSGtPHO1+CLma/ShZ+p6bfljNSN65m3novppdWKqcHb89f25fz6r4624sfiofveqfzUgLjsp/aW
h9ef3J/Pi5KkvcTgivPYxYfQjrDipPiK0/wS"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSF/pjxjMHnjqD8
gqDEm6KOjOT/87SyoLmRr7uC0fflwpbQzoHSuYmyooDj69aU3qXW15O3jfiu/8umn/ONid7w8oC1
jN3e15y/+ub/jojZnP4DEQsBAyAGRUNDLVBUAgEFAQcEiviHzLSd8K6ExLe/m4Lz1cba9ZXU4O+y
tYWdytWtoIz03Le28MndkvfR/YPTzNnamLLFgK7U0JOS2fTUydnt2MjE2vaJj9vpu5Wh7N2nr924
MQSHsrmm2qWZ/+bg1cbWhYaKnfLhi7SjoMfJ2pbY4qi5j+yKqJGwkIv6x7/vsZzEwLXnj4nUt5b7
ne6XwMC8hqKo75vy5qX6gY+NiIPRweOGqd4w"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSF67e2k9ys38uF
mMDv5679i7jxlrCyi4uOs+POmZCZ74Ptisf0nKnv6J2dwP/H89vfzO3t2PD6pJmdhZebgpfaje2e
o6nzxNmAsK7TzcmjwsgMEQsBAyAGRUNDLVBUAgEFAQcEiM6o5eHEo6/Xw4z4ovib2KGilI7slrqP
hur3l9zEoJX4wKeXi9mT2JTpqZyqq9f99o2VldS+o52RkOLSoenioLGUirvQ3+C57Jum0J2guYnd
cgSJ0b+74rq/2NrruLLiwo3muPr2nLvx/KHMhuGetKH65uDdnuKp4J7boOObxevej5LZ1O7KvJy3
stChycyE+bT8n/OCiumj8r2a58aKubGn2b0V"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSJle/mu/Gbn/KE
ypTo95KU/+Wt9Ii75dDWz+LNz8Cd892h1sTXoIDU34eY97frwKWl74yGhOm829T4gJ7A78feq9ij
rZHvk7+827mVrP7cidZbEQsBAyAGRUNDLVBUAgEFAQcEhfzZzv721pfHk7yt/6bg4dDIjvfF7OaL
oPzSwou3xP6f5ceiw6ac+Ou4ms6kpc7c3afOo6+7yI29hcSZja7u5qXC/vHN7K69wfqd4Mne8b+N
SgSMhc77je/noeuDuqDH0tjmvpuytODEzObT5+yu9bm8rbq88My+6d3irsDBp//cvbHXjviRi/Gx
rb6Z66i8pvS7uu7R69+Mv7786YT7tICahpki"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSFkI2usdL++szq
wuC1n/W165fMs/KHyov2l8GOuqjc56j3yp2T3cC6n+z196WJwMXwrPK2zvHo//3zr8nD9KycmeXP
08DCqviM3rzrnZCS4d0CEQsBAyAGRUNDLVBUAgEFAQcEidHl076Ky/yqjqWNja/y/Zr6/ZuL5/yq
8LSRpKGskPX7pf3O89qcrffOuvmP/6qUiO/q1bm09O+dkaL7xcSE5Ie8rKfmoqmLx4zTzY+29IOQ
NwSE4+v0rKLel9/xvPaY94Ha8Pq/6v792NmMgrL87Me01Ya+8ZS5k5Gn376a/t7v7fWXjY7etuHL
1f+JxY3wxvmelo+Q0oPd4MXWpc/78NvuiZgF"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSGgsmj4YHDnbvI
xKqMieDL0cfG84Xc38GCw+fy6smzgdGPvsjj28O16p2157jUk9HlhNuIobf03eb7kdOU97Pvsfrn
w8momqTI6K/w7ri1x4dJEQsBAyAGRUNDLVBUAgEFAQcEjsn0u7LO64vFgafhy8LTmsG0wdngrdC7
9q3v8L2nkZiYkc3uuuGlm4/Km/Wr7qbxoJzY8PiWo93Y0LGRs7e8343PwMTD9ZmxsqfWwIjIivXu
cgSPiNWBorvvy/a/25S9zKnjkZ/2/5yCluutlL/Q9933yrnx6eyV8+n14ejkhNeF58Ld9vf+78Ck
m8C4ku3RofOekuit4/qVyc2b5tGT6cTF24Zg"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSBo6ux+4b+1vyu
3Pfb4oHb9/PB/uXJpsWY7qu277Cv1MqUpPek0oS30YK3gdPQq/2DkPLTtIbR3vrG9rOu5buvoLXQ
ofzNh+PwxoadnrykwvkwEQsBAyAGRUNDLVBUAgEFAQcEi/y1/4OFkryhupWn8p+u3POb3KaEnvnq
/rqaq4W+kJ6P/azkopPq+Pmo0MCFw8zs1q7q3bOB08r4uqWg4uS9xrzz9pverKrxwbSwhZ+4tICi
bQSEnJ/El9XK9uDdyf7k8J+OkbuZlojWkbr4/MOZ25z6lvbYu7SKpZCShLiLtb/BhMX7gp2kvcrz
sLTG3cDNqMOFp8DJwpK+nsee6/ybw6GFk6Au"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSGxvuq04+03bOv
tK/0q4GX48Gx3pT8wtje05fH9rqWgcGShIqE/uCWmM/WndWgjZXhycbb65jN7Mu+gfPF4c3i55e1
h8fpzcOnwY/hkovKmp5hEQsBAyAGRUNDLVBUAgEFAQcEhKH6zunvwZ/n4vuGl4/s8ajr0trEmM22
x/DnoKDE+eWpx8mS2ZbYuZrFj7eQtKnOzJqT2vy12KaFqujpw8iV766DpLX+y+q2xreXtZHE8+Lk
DASB3OrQm4a36vqrzZyF8Oyzx5G3wLykuO+h4IzPm6W2oeDZzNTr7eXIpa/ejarG2d/xrfbB04/d
6KCQ2tObk73PnY+igKqmhte1r/+68KaErvES"))

;; ----------------------------------------------------------------------

(defvar *hb-shares* ;; Helene Barab
  '("EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSM7+jSx5GWrZS3
k5LFhcDZ4PyLkdaC9Ov0zfP6utqdubK7rpHP78CyspeZkaiwt6D7o7qZotOLiK/4/qfJz/a9pZXq
zfvDzvzTou3Zh56Em9FJEQsBAyAGRUNDLVBUAgEFAQcEhqPcgvqA2Me32JOoyrW2rZ++k6uL9o+I
gq7IuJ716tONrpfYzu/d7sWz9s/2w8iWs7Kp9Pi9k6KT16+nvOXYycCjtKyTjMSdncn71aycwrXw
CwSO8ryV4evLl4bMr9Ky+9blvojZkufVvPmjlvzrjveFz+W09KD5rISXqvyiiZiJvMfmptK/+KjV
7eeW26X5x5nT6N27hZ+/g+jqgMbTvbXZuNpN"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSD8t7gy5u02ZjO
r7C5jrur8ZTNgeq788ylqefFte/P3tmClfCm9ZXj1ITgp4Gort+68ZvtyJvMiLKxn8vUiZOykc6m
t7rzkrW1mJ6Vn8G8uLogEQsBAyAGRUNDLVBUAgEFAQcEiOfmzZn1yMPv1cb43qWXvOjH65jbseb3
r52vj/n18OzG5M/B07vS0umuoZmBicKj69DRxanFgIiom+nE0vu1xMz/vd+xp+yP8u2Cufi77vC7
EASSsZ324oeQq7ze2ryvrbmFhPyz9oCQ3oLN/sW49PLP1J/nnKjSqcbSpsDRv77kwoTYs6qM46f4
monfgqaWxeTus8v4q7Gu56qo4fWVv+Sh3gM="

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQT2o/ib76Sa6eXd
/ejGpa3387HeiP6dtf/66tKb366Y6PKQ5Jqf5Ib7jr3K3p+ehsqzv9zq5dyHpb2DyaDE4LrK+YWN
tO+3lZPH35Dp6qbDmS0RCwEDIAZFQ0MtUFQCAQUBBwSLj/vxu+m1sYeXx6q66bemxr+r7cjh5p3S
uP/926DfudfU8+eXw/rQ/raAjti4keWa17yDqaLP8O3AkKKgjZnDz5WupubslJzTq8Kphb/Op/Ji
BI7cxo3N0LqC1P6t3v7Tm671zYnIsJf6hp2XjfSrv6i46YeLt+X9qcKs396JhKuQ3fC8hoO1+fSj
iqH63bzE55ibsdX/xoG93cnttO/41Z+Ltlo="

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSByKnh3JbD8PaL
mOmDvuOQ8KSDzbjp5Ybgybym4sq88ZH4kaOBrt2a7ODS9/77kIGWudDqqL+im7n10+SdvaOkkMuJ
meDn4IvK9bmKvcuHhZJdEQsBAyAGRUNDLVBUAgEFAQcEhPLNw+yL4tnWhZjku+rq86ma4qusu/uA
zJWT4oau2o+UifDSwq/tts2l3u+V0tziiuKDjvqh15fjiMmOupGBp5Os7pLN67Dx07G0iZfJtK6e
EwSN1a2XlpCRycq5uaOIurK9xMqQsI7Cp+7ynM/3qtiR35icmJnKvI/BwsPQwL2q3rSe1pT17MrE
lLWa26ukqfDbisiBtvCj2ejWrbGPtIqJ57E7"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSOttqz8Oq0+fiZ
+JHfnL39gdagp+rVqNeS+fbMybPby7u5p4mq0an43qvsxeajyPiNpP305eOhpfT5xunY/KvOmejj
+bnJsLq8sO2I8qLz6MpMEQsBAyAGRUNDLVBUAgEFAQcEg/Kjhpytj6e//caD74vz7PWgmOSr7sba
qOaAv4Pyoqjg6d3f1L/Ahrnm3Z3v7s+ptobL+oC1886VjMjM2N6NlMqWk5Xq0fqY7bXtxPjMu6j8
fASP7Zmcx53KuMvTn7XyxLTglc723ayih/vE9ezdjZ6Rv5Xf1bLnwsS2xKrNob/E7MDnnoDAj9e7
8ZH788C058alpsKA0reZmaLhsa+Ulc7jt9Nd"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSE2J2ejrSq3Nmk
is+1rajxx7W3197bn/Tinub3+KP12J/3jf7l1KfEmfbrlbDYoPeQ9qOu8Mvj4oy0+aT9rZj+i6T9
v+3b1MyM68+b2/6A3cRREQsBAyAGRUNDLVBUAgEFAQcEj4+826OK9bTi5a+5oLqCzOL1rIGN0re5
i42gwMrXhry4j8WurqT/k9n4gfGiweHuqO606sHcnqGVqc/Pp6fs3NWj8JyKlYTqvdmi572Ft42b
HwSFz9Wk2Nfyss3Wy5/969Tbu5qo9qTaurCP17LYwrDcqLa0m5X/sqnP4KDnpLStp9LRoIOaq5yU
jIbDg5ahhIaqhuy3k4Gk9tGRts/QpNTv4IBv"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSDz8CChcjOxLqM
+b+Nn8uusq64/NXHkqGS95TQ2oGB5det+dO62t/e17jo36//vI/Fk4DgmLbO9Z+zy8npvv+86Iit
nMvFu7HE+vHg3pC1nPpsEQsBAyAGRUNDLVBUAgEFAQcEhJn9zKyqlaKwuazViNeJ5qnNi57ppo2f
4tu86ajjv5bhh+2FsJLz/8Ky4/W0i5+r6+KN8YjB4tDOi4Dz+cCTzcuWj8CY48mepNv4nqPfkMbr
HgSE7Kb6lpG0n+vNt5GY0q75qsbUsPGxuNz7w5So95vMs7ju2fei86nn2cr+4erhlpnm1cuJiKT+
ucTA5tD7/s3CpYKsicvv0e6at6Sh0ePZr54R"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSNnbnPjtq15vmp
lLnZweChqN3J4Y7To5yMu+bjwtXmyOSKhpOX0J3MpJLindbytrXdrsKe5uOJl5Ce0aDn6LuAjNCA
6vbHlt/crsyXncyRx5IiEQsBAyAGRUNDLVBUAgEFAQcEhoeXiszD2arPnICrpeGh3ZrM8tOd94/u
nLbihrbbmYfN88TupdnD45vcjrzUj5HRoqPJoofn0oLK8OvkpZWklKa90qD5xb6qmKDqmJHnzvbI
RQSI2d+K2POdrNiWgM76nbm87cLq1+SF657fpo7w/IvQ5Zb+ntyUnpPrxeKx78bRxPbmzfvIrIrw
3PKPlY6r5PWRmtr7tYzhmpncqdv8j/Ck/LNG"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSLxO6/wo/y9vHg
soOfsMHQ6tzvqsm10Jjsos7srNi/+obK3rDmk73o7feM1PPY0cbL4uqclrfj8sSK5KGH/oOFiKPD
t5Wc9rrV7LbW3fTs5MZlEQsBAyAGRUNDLVBUAgEFAQcEhvDq9u2r/9nK+arPrZ2plpqX5KzZ4r/F
nsyA+Oysysm2s7ims8GQnLi7gcvxv8yU7+3ZkeGKx7fW5NqVwre9kczgpcG0s+iEoNHAtcmJns2B
IASHyOSz/7esurDoxL7doIXKqKGi3O/26ISe29zQz6nQva63x9jkxqy06JWvzpeJsZ/ng9rs5I/e
v5j/6ojf8qD4romgu9mjsIPY28qn/O+ozpgD"

    "EQsgD0VDQy1DUllQVE8tQjU3MSAMQ1JZUFRPLVNIQVJFAgsBAyABWAsBAyABWQSFoaLFhbXDiMra
iPmFkJLRje7SybGopabwsvqGiNjOqLbT8JXdrI+F4PS0+/LnkcDQyJW6jLS5upqctcOA8Zay2/O3
mtO2urHCyLCF64uSy/1iEQsBAyAGRUNDLVBUAgEFAQcEh+3l+vTP6aDDoduB0pSq0s6t7NP5s8uV
t+O/s82o87LFuIfSzObkpZ73rLeQ1eaTne7m8oeh54iiruycxs+m99XD/8Syh6+OubibgOOs5KW4
eASK0b/c6tu8/ZO0usr1zKH716vaqPTW+JPmzbPolInvxMbBo6KM5qPJ25mNxZnqy8Cg9tfwnoCV
u+rbrf232tGbo/iHscKGpab3/Yncu83EgPUO"))

;; --------------------------------------------------------------------------

(defvar *escrow-key-file*
  "./VTuning/crypto/tools/escrow-key.txt")

(defun get-encrypted-string ()
  (let* ((s (hcl:file-string (concatenate 'string
                                          *escrow-key-file*
                                          ".aes")))
         (x (map 'vector #'char-code s)))
    (encode-bytes-to-base64 x)))
  
(defun encrypt-escrow-key-file (key)
  (let ((key (encode-object-to-base64 key)))
    (3ctr-hmac-encrypt-file *escrow-key-file*
                           (concatenate 'string
                                        *escrow-key-file*
                                        ".aes")
                           key)))

  
(defun decrypt-escrow-key-file (key)
  ;; key as computed from compute-ip-escrow-key
  (3ctr-hmac-decrypt-file (concatenate 'string
                                      *escrow-key-file*
                                      ".aes")
                         *escrow-key-file*
                         key))

;; -------------------------------------------------------

(defun encrypt-share (share key)
  (let* ((key (encode-object-to-base64 key))
         (enc (loenc:encode share)))
    (encode-bytes-to-base64
     (3ctr-hmac-encrypt-sequence enc key))))

(defun recrypt (k1 k2 x)
  ;; k1 & k2 are two shares from participant
  ;; x is his board share abscissa encode-64
  (let* ((share1 (decode-b64 k1))
         (share2 (decode-b64 k2))
         (curve  (list share1 share2))
         (x0     (decode-b64 x))
         (y0     (solve-lagrange curve x0))
         (xk     (convert-bytes-to-int (ctr-drbg 571)))
         (yk     (solve-lagrange curve xk))
         (unl    (make-crypto-share
                  :x x0
                  :y y0))
         (enc    (loenc:encode unl))
         (crypt  (3ctr-hmac-encrypt-sequence enc (encode-b64 yk))))
    (encode-b64
     (list
      :x0    xk
      :crypt crypt)) ))

#|
(recrypt ;; HB
 (first *hb-shares*)
 (second *hb-shares*)
 (encode-b64 (crypto-share-x *board-share-hb*)))
    
(recrypt ;; DC
 (first *dc-shares*)
 (second *dc-shares*)
 (encode-b64 (crypto-share-x *board-share-dc*)))
    
(recrypt ;; QN
 (first *qn-shares*)
 (second *qn-shares*)
 (encode-b64 (crypto-share-x *board-share-qn*)))
    
(recrypt ;; PE
 (first *pe-shares*)
 (second *pe-shares*)
 (encode-b64 (crypto-share-x *board-share-pe*)))
    
(recrypt ;; AH
 (first *ah-shares*)
 (second *ah-shares*)
 (encode-b64 (crypto-share-x *board-share-ah*)))

(recrypt ;; BB
 (first *bb-shares*)
 (second *bb-shares*)
 (encode-b64 (crypto-share-x *board-share-bb*)))

(recrypt ;; WM
 (first *wm-shares*)
 (second *wm-shares*)
 (encode-b64 (crypto-share-x *board-share-wm*)))

;; ------------------------------------------------------------------

(compute-hb-unlock-key
  (first *hb-shares*)
  (second *hb-shares*))
    
(compute-dc-unlock-key
  (first *dc-shares*)
  (second *dc-shares*))
    
(compute-qn-unlock-key
  (first *qn-shares*)
  (second *qn-shares*))
    
(compute-pe-unlock-key
  (first *pe-shares*)
  (second *pe-shares*))
    
(compute-ah-unlock-key
  (first *ah-shares*)
  (second *ah-shares*))
    
(compute-wm-unlock-key
  (first *wm-shares*)
  (second *wm-shares*))
    
(compute-bb-unlock-key
  (first *bb-shares*)
  (second *bb-shares*))
    
(get-escrow-key
 (compute-hb-unlock-key
  (first *hb-shares*)
  (second *hb-shares*))
 (compute-dc-unlock-key
  (first *dc-shares*)
  (second *dc-shares*))
 (compute-qn-unlock-key
  (first *qn-shares*)
  (second *qn-shares*)))
    
(get-escrow-key
 (compute-hb-unlock-key
  (first *hb-shares*)
  (second *hb-shares*))
 (compute-hb-unlock-key
  (third *hb-shares*)
  (fifth *hb-shares*))
 (compute-qn-unlock-key
  (first *qn-shares*)
  (second *qn-shares*)))
|#

#|
(encode-master-message
 "The key for unlocking the Acudora IP USB Stick is:

     *********************************

Enjoy!!
DM/Acudora 12/29/2012
")
  
|#

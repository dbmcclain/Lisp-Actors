
(in-package :ecc-crypto-b571)

(defun gen-x ()
  (convert-bytes-to-int (ctr-drbg 571)))

(defun gen-share ()
  (let* ((x  (gen-x))
         (y  (ecc-mul *ecc-gen* (gen-x))))
    (list x y)))

(defun format-share (share)
  (with-output-to-string (s)
    (format s "(list~%")
    (format-fragments s (first share))
    (format s "(list~%")
    (format-fragments s (ecc-pt-x (second share)))
    (format-fragments s (ecc-pt-y (second share)))))

#|
(defvar *master-share*
  (list
   (big32 #x011E7FD6 #x1446246E #x92BA25AC #x58624A3F
          #x8B299364 #xC5A19187 #x28EAE583 #x7FD6CC2B
          #xAE00C528 #xE6835BB7 #x6B54677B #x0DB72F6F
          #xC744BE46 #x8A90FDA7 #x1834CE4B #x3E651C56
          #x6DA9952C #x17E0E4C4 )
   #|
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
   |#
   ))
|#
#|
(defvar *master-curve-share-1*
  (list
   (big32 #x02708C46 #x60482D01 #x40122E78 #xCDEC95A2
          #xD7A61C22 #x591E41CC #x10D48049 #x0F4BEA63
          #x7957815C #x25897DB7 #xA2B5D2B1 #xCE296134
          #xA23D0FBC #x204F16FF #xDB214AE0 #x0350E887
          #x9BD286F5 #xDEAD0D20 )
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
  (list
   (big32 #x076B9A0B #x678E44A6 #x2BCA7D8F #xD00DEFA2
          #x097B3925 #xB32D2F86 #xEA9F0176 #x7496CBBF
          #xF216B9BA #x268ADB87 #x3864BCA7 #x261913E4
          #x2243BB62 #x9687F950 #x8BD02C7A #x5F3BB68B
          #x9EDA7377 #x842B718F )
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

(defun gen-master-share ()
  (let* ((x (gen-x))
         (y (solve-master-curve x)))
    (list x y)))
|#

#|
(defvar *board-share-pe* ;; Paul Eynott
  (list
   (big32 #x0710E7F7 #x4B07B856 #x3449FFFA #x7B8816D5
          #xC7FEA71B #xAA16EBA0 #xA4D6C7E4 #x3ADEF57F
          #x779E39A0 #xBBA048C5 #x1C1CB272 #x9DEFC122
          #x2C826C9C #x485D2E2A #x65FA649D #x83911B5D
          #x58C5EC11 #xF08B175A )
   #||#
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
   #||#
   ))

(defvar *board-share-qn* ;; Quan Nguyen
  (list
   (big32 #x00E857C9 #x641EB015 #x693DCBCC #xCBE2C4A9
          #xD6261DE2 #xE17CFCEB #x0D32E126 #xDBA242F4
          #x55E661A9 #xFFD3637B #xCE4C8580 #x04A7AC29
          #x4D0E5456 #xD0A06F7A #x47E9AD6D #x9014CF46
          #xC356F0F6 #x89B32A63 )
   #||#
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
    #||#
   ))

(defvar *board-share-dc* ;; David Cohen
  (list
   (big32 #x0315B82E #xE188C4B5 #x5D11664F #x52145BD7
          #x3777D801 #xF5D87C44 #x1BADC9EF #x93685CEE
          #x0B4FB826 #xD029A4C7 #xAEE5403E #xD8F6F2C3
          #x5637C5CC #xA59E7748 #x3FCAF462 #x7D1D7FEF
          #x88FBA2F3 #xEAAA9488 )
   #||#
   (make-ecc-pt
    :x
    (big32 #x00AC670D #x303C2EF7 #x6042EE07 #x592934F1
           #xFE8E48B0 #x0584BD4B #xF818C802 #xE98C743B
           #xBE8721E4 #x5C11D4AD #x559B0411 #xE5D351FA
           #x5750DC73 #x2934D1EC #xCE040FA1 #x6B2C9A7D
           #x4E60F5DE #x6D00F851 )
    :y
    (big32 #x00D9F129 #x27C8F9FF #x5344B81D #x6C00D405
           #xCF465391 #x9F06B55A #x97780FDC #x4BB41A22
           #xB4E2E354 #xE78A0E45 #x17C04E94 #x91CCBBC3
           #xDB171BD8 #xC20138ED #x931F2426 #x9B5A4964
           #xD99979FE #xB80B11BA ))
   #||#
   ))

(defvar *board-share-hb* ;; Helene Barab
  (list
   (big32 #x0108FE13 #x6646543E #xBC7A97C1 #x9E155D8B
          #xDAA57B98 #x529B6762 #xB201DDA6 #x8370EA3D
          #xCF9C7533 #xDA92EB3B #x5A468B1C #x9DA74238
          #xAD92913D #xC999A957 #x769DE1F3 #xE3B86E33
          #x6BD062F7 #xAE7F2668 )
   #||#
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
   #||#
   ))
|#

#|
(defun gen-board-shares (board-share)
  (let* ((share-2  (gen-share))
         (shares   (list board-share
                         share-2)))
    (loop repeat 10 collect
          (let* ((x2  (gen-x))
                 (y2  (solve-lagrange shares x2)))
            (list x2 y2))) ))
|#

(defun encode-b64 (share)
  (encode-object-to-base64 share))

(defun decode-b64 (b64-text)
  (decode-object-from-base64 b64-text))

#|
(defvar *b1-shares* ;; Paul Eynott
  (list
   "F4F9JwIEjZGtif+5m+bf46TZ8MCU9NfGuIi7uZLbz6SB1uH38/nc2eCGxsa+7uCQ69epn+WD7a+g
x9+Wi53e1oCeqvDrjeyxmsT4heyI9qvXwObElJH6SicCBIfQqPbxrJ7ByYCXkbb8l7Tct9yDqo67
2c/I0/imuPLOsJyvtvSG37rBps+Uks+Vm/K74tiSiKeWlJOX7sOH8KKGievqkq+Gkou3v62Qj/mO
zlwEhdn7jv+Dkv+3o6Hl8fuB9Y/K6Y7994rX6cvh7pX+obzmhJKsk9zXktWivYKQn6Lfr9Gou8SQ
15Cn1K2E3t2J99LIpZWI4v2kjYDH8PCjuLmEWg=="
   
   "F4F8JwIEkJKIyvDnzsfIvbW9x7GT1qeaxK272oO2666hivKR59S/s9uftajAmuCNsPu1hOaOy5G4
pMmIuLOxpdaX9sSetMnOrKmM08y67ZOgsLrNo51fJwIEi+WOv5XR8tml4KGniqfl2Oi/7oX/gofh
0Kum+dqhvP2QiODh2dPR1vaEo8TK/fa208ukir7uuLzIz6Lhkcy01OOt2N7x5tWl7JuSnu+6hvmT
GQSB7Yzgq7HCsoGwpduDm+edtt2n8v/wsdXG8dyxuZeT3siiieKqn4XxgrCokuDTjLP10s7czp65
j5muh7/23cyj5r+Qt+2KmanG8Mva4sCBjbwU"
   
   "F4F8JwIEjJ+nh/3488uMqI29sYziytSn4PuzsMr2l6iAuJCehtC+uqz4ppHmjPvckcX9isWE0vnd
y6msoqOW1Zvit6vCkoWUh/eCx+viv+iq47eTurPyfCcCBIaXotPJsu3S88DtpKfz9taRxKqdsve7
l56kjYS5u7maoKDU9syr4OeXo9jmmbTPvquRqYjTv9STlpbPk/TpqYPg+eHoq+PziYrI9r+Ps+Km
SgSMqI7T3uzLtpTIk5rP0cWn9+D++NvF3L7e6eXwwO+hvKnd+MTZ/+fU6o68k4HNoZvUitPm4qe5
rsTcyJK/g+Dz9c/55uXir6namLWhsMSB+YVP"
   
   "F4F9JwIEj+nG57LapsWUp5Hnipb0jKvP1IPM2bS65Yy+luDq/avZ86q/kpGCtq6ZxtrmleWnqsm+
iYbt35mAi4LLvPfS3PHkpvuUkITY5Nrn/riV8MnGWCcCBI2c66fBnLyFyO3HyYin9oyM8Jza54zO
4OD0m5KjqeWkoYXx88Lttd7Lx5mOh4/7hZbousWxz9CGw8zf1fSg9tH+kLimoemK6LP8+urV2K70
xVoEhozDpNHuo4CRk9vfxfPyksDzrPuAttCogPWkv5mO4cv1nPbZ+srT6uORuKbZiIndvIDor+qL
9LqCmsfdzNyo8Ieel9Sri/zQxdmq9qbeibSgMw=="
   
   "F4F9JwIEh8q2x9HinuHqroHYiveS3f+CiJDm1O/sz5u0g6zkxoyWxae27qKLkYXbo63Zqtzs7fmM
zf/Wl4WHjsCH2o7817uJwdeflP2VjfOTkP6y1/LjGycCBIrGnqL4wf3Sq6WwjbaDtdf4w9i5x47h
l7qApeKS2a+EoffSgfiSnN7986mT5ZrUxYGAm+WagK602Krcj5ySoP/RqZ3F+sKGqKGMveri7tSp
1jAEiaWGlayZo4+rhoDBiPGk3t6Pg4ri/YSxn4exgOediOnPper676DDmKS1jrSA3+3PjaqgiILn
xMX0m6i0+8Hx/qCBy6nwhuqCve2DrNzQg5a+VA=="
   
   "F4F9JwIEj4P2zrK05pjTjabpzMz4hNjlhMe8xon6zJTXh8zVobmxyq/18KGXxvTVuvOii96YmIfT
l5rCvOeu8rq65LWH0O2op+ajif/gjdCJ7c/xydrLbycCBIOn1K/f/5Pos+Pygqze6fW/6aqvmKv0
j6LdyMaLoMDSq86RyNWN0qrcwv/ghZPlvKW/orbc4b31hvvYiM+Cs/HRhJG8h7uVkryuzfmc+oi1
0CIEipajq+KixaW08Y3G5Yqaidqvk+Lsh7f2zc7fivPZlNCZlLHVkN7dpqWxhcSisdafudXos9rI
lf2v+42Umuqwy+v69sfC7+Xkj4HinJbhldqzIQ=="
   
   "F4F8JwIEhZG1y76X0crZg9KL9YCJ2Zzn6vrG1sunpODVx4nfwdPRy5bs29LGneDX4YeAgpHw65yX
xe2G4ozop6K+96nI+OLMxOy6h8r4rNC1oarx8ffaOScCBN2Txp3Uvfz2jebWpoHq27icuYq7w/TP
soDHtNKOmNmOw+Ou4qf/nLnv8OiI4vL22orOv+aaxteI0oTG34KShcCK0P2dmMmK36PesvTF39/u
DQSF0Jjcopjrv7ybjoi0maaEm4/Z5MvY+c6g4ciw2qna6/PP8Mml3OCvt5Xa9enDs46089aXw9jo
xt38iqb4tPjrpNj7yLmn4YWZl6f6zIik9dp0"
   
   "F4F8JwIEz4fZqdOBhq3Sx+zz19nz+sGT7Iet5fHMoIOR2dK+15bCqp3+w/LE39zTirL0zJKpoMq4
rbDChvD8686L+ee+8vKZi4bC0sXww7PhhuWln4cPJwIEhKqCgqiM44uSha3yyI6zvM2+97HriNbQ
+NGM44K2o9y12ImW4be42+Wk/5X8l5fojvT4vfG4sfDB/J+BlvqtjfuxtKPWkuSE17Kno6Duq4KR
JQSK0r2ygPbPztu8qNvV5snNptnm5cbAkLGB04W+zpfW5r3YsK2T1dqgv/zOq47lju723dWO0LnN
1vv7zt71y5GGn9uk8aenu6mrrv2EiaWymIdl"
   
   "F4F9JwIEjdrVgeulxJLEiNWmjpuxjI3vuvvki6PzhZrwj5Ougu/frNyB0u236fSfycvB5JHa6Kap
y7jVl6n7nfzy+KTH3erLwOHIp56po7L+09OHp/ytPCcCBIvRr7ihzbbGs5vI/eqjidSvi63eueXG
w6yVr9vNrfyRsejpqP6P3r+lj8G9q6vd9IKN+uyE6uaLpvCysdf9pLPS7MnWxZvs06KR2uPOw4ep
9xEEgrGb6PvQrIq3y7i36di9i+/G6vuy66ry7dCYy5Dy/e3yrqzQvari4bzw++C5mPjt/Pyoqe+u
rtiM6v7CzYbkzpSw/eO8i4npz4PEpMv89/6wZw=="
   
   "F4F8JwIEi9i34ZHSx8D0npTS8MTunKqfjvjF39Lxh/vdmYqG14WPnpKD0f+ghpztvd+rs7aPw/G2
y/3VpLjurMy88OTquKHJ5evXvOeVgO/w7LnZ+v6NPCcCBMqKsr3675j4+4fusKeLk4SYorTU/onn
38bDwOukxvihmYbK6bva3JDB4oywlcCxx8HOkc/C07iOl8TGu/Gagf+HmMPA8bKRtpD+uLv+8IiN
CASBucLc/syLq8aU3sSi9pXlyt79pIzUntS07ITEorr/k+mQnYql4pK3p7WE+ePau/SIxPSW7Zi+
u6/k8pW7urvh8o3jlKbT/feOwOfY7brK/Jls"))

(defvar *b2-shares* ;; Quan Nguyen
  (list
   "F4F9JwIEgYrRxJTD5P3x/ong7sPj7Im4/aWCmsCHrYbZ3Pao1vmexqm6wYrG5LvIj4Wi26PE0uDG
nfSNso7li+3V4sa50Ojx9YzO6Ibz7LGs6Ozh//b4UScCBILWmY/4pdTh+5PJ4PeyndO0xoTwg/21
mOWz/tzi7LXosf3KpoC1ydvg1PP4xJat9aL8l8fCkZf7+t7wtYH20Mv4vc+y2c2y7OHvsavYu8yg
vkAEjOP7loXMnOjv5aDCzuW3zdz18YWk6biXjIrwyJri/f2M2vrz2OG6qLKK9MDCg96Ciej2iei/
jJmK4u+8lr2DhZGk1dHggcSrm82tnMbmz62aHg=="

   "F4F9JwIEiNz7yeeslIKez6Lgo/zbmb3UlNeA8suzic7Mjc+Yo7WIp/vQ7f28/uWOl76n1L6H9MXq
4P7C6LKc1vWC64/I/OG6hqLP1rTv1cn856qPocCYFScCBI3+n8zi0obFgbGZ4ey0m8X92NroiITU
mNbvnuLMqOKtgsXMjvvykKzqjpaJv/aCha/0zubGu+q0u7nc86iTvODG0fr/uoz0gP/Ths/09L7B
5wUEh6r53d/T1ZPNk8jQp5vJ9+eHk9z/tO3nkoy90si+7NeX+KX57+7Bq72H467Qxrj2wsXV74O2
uvTW0o+D5rjCpJPE39qb5Mrt3N2Iicrm8Ze1YQ=="

   "F4F9JwIEgpL6+f/1o4yoybrvssqL3L3E//q94qbuvObu5ebj+Y3g7onDv4TbwLDR99Ca1fDuyPf1
mIO237XK0tLC8a2vkMCPvoTArLuCt/Hll7bPocy2dicCBInI0uD+9++SoavMztu5uY2Bos6fvPCB
6tTbyubfkeS5u6z7tJy1kuiyp5797r/C0tPfqr3gg56SpKnog8/559DMosLOlo21xqvD08X2wrOB
8RMEgpneoq6ugt7g7K22kq/Vuc779/f8lImc7ezX+/KM2Iuu3cjY9Lr1rOzptMLU1pn7rKHan8GT
jLy5nrXr0d/5qframpre3KrMmMfI67OTvfXMAw=="

   "F4F8JwIE/qqul/T35qP6lvGHiMmqu4fdppabx7GK6IDP1Jjdupi6gaTr+I728ZOzy6+d3NOHr/md
scuI2v7rkJWjob/s96XRzOGb1LDB1abQh++uudQ3JwIEiuSc2LTx+qLO3+SZodGl74XN9LS57Ynk
r8rUwv+u/MjOv+HEspjztobkt/i3x+fbsKDb8Nieofr4nNSOlauewKSe7pzX7/C9rLqo64rM2byH
YwSKr9noq5LI25XVz7fLtPGw5JbXvMT0/9LE//uCuInF2e+qv5Tk34G0lJrwkoulmp6rp83Vzu6V
jJDjt+iys+XP8YCGpJOFgJ7H2/3D7/zB7Ooo"

   "F4F9JwIEjvrt4bPJ4MSTpNuFxaWOwqekq6WakqHev7ip/OfpmpTr8JLC0JGvtpmf0IyB3YzxrsfV
2Z7Tsa3BhLC57em5lMy1rf7J27uQnfPr16qg5cfqXycCBIXf6emyov+a0aX58N3TkKWVsrnctrK0
kfr3q6Srr6XJ0OzgwejyjpuX/+W8uN3S1b6+5LaMq7P1/KeLqdT72KipwuX4+/Suq7yJ/+Wwu52z
9R8EhovHt/m+gMiLyca+1ZKQjOWW3/zI05ulqITn2+OtldT1yoL8haWozf7W64G8nN3Er+zRnL+2
gZ6f4/DBr/2/rZaws8eQje3Toty8w6LfuInxNQ=="

   "F4F9JwIEjazhvI2+ypWfwZ+xqp2p49nAp4Tbvsf46NLPoOrq/+umj9XkkImNo62BuIish46Ej66S
uJqShqCytMXJ/set3NWjuq++xPnK/9T8u/X6lfTFUScCBIKroffa1+K6/8uX7ffti4yQ8OLd9bjX
7NWJmKfF6t7E5eqz3KH+68qznPnnz8nDvdnSqYr1vLm5kqmAjt60lMvIif3T+IjQjt7Sn8+/jM2n
tmAEh8yeh7HRg7rz6aWMnb3upaOuzMe+4eiu/6nK0rb6xqrOwfadrvrKr7OQkKKcuLnulPeooeu6
q8v/w7aKkJSsyarYs+3g4bTX1/2Y/+2CpNmwOw=="

   "F4F8JwIEj7PRueXr7LvFxYz8tJ+Rz9qHo4+W3smjvpvsmPetrOTVj5CAr9ySzJ3UmfubpsO++LCz
+Z7zt/W9h57SuIOtve+67uyM9fS4ydyf4KHLvabVdicCBIe5tJSk9auGvIK/vL6csL3iwrun5c6J
7J/OpfyyysHY+Ya214+inqiwlsblwcu5zsDxpfHO25bhioCVnKKbhMLruM2ZvPjUgKTXk4GZy+Hf
0WoEq6Sqpfb+tuy47MGxjf2Ysvmm4OD4nfmDidippfW5jtm2jIzo0JD3+MLJp67Ggt7DhpXgsvq9
/6GA/K+pwZjK8o62kKm0qqmTpbrEiNa4g7Mf"

   "F4F7JwIEwrWBsuWW+I7f3e3Vpf+zpv/o6civo4/y9NDmxrL2wNOuj9qGrJHkx6OAx6GK38u4nuGY
mqHYrPiWrOPnm+ql7amz6sD1o6vZtbDl9pzjxoc4JwIEhOSI9aCj5YukkfCxl+KQl/Og1Ln7oLyI
/fTOjrHt+pyi+pjH47TavNiRgtqXwqfB67XJj9uZ6/3OqNKi+rTcttfd4a6hr7P1/KuU0I/0mIS5
JAS/6N7q2PTOy63NrYWo0bqgy4Tf/YGZlP+/3pfB07vriPDAmMeRqcOu5vOXgKrjrvGX1J6O6KOf
8Mqp3eyCjtWLkeb46PWDociDmbevqL3s5RY="

   "F4F8JwIEiZ+78Njyu/rjivDOlMyE//q454Pn9eDG3evDzLKF3oPoysX8kMTYoY/rt6nBwcTs3Oij
r+/hm4PNuJeZu9ni7uiw6ej38oSl/5v5+9yShbr8PicCBIXQhKeDxoTgmLfH5r3107uQjoq5le//
ov7I5YP04Iydr/C97pPNkZeKmrWkjrnnyvyXse+FkJeSmqWyt87/reHi5faX2N289Ma//rzlnfqR
6CoE/MWpoeuvvrXkto2d5+z+tszu1PCY+OCx6aqJk9aNqKq7qfTbhOmNzcDl6/LPw/Tf6qKG8tKH
//W89cHggOPB5cKHkbqSuN/3te3Nsejrud0/"

   "F4F9JwIEjYvlta/Jj/6QiKPEqP/72OLi7LHtrZWF7Y/i1ZOzoemi98Wj4ZGIqtfJofPfn7HJrviU
je6divS9/I778Oq+15Wom6HkneSsntqE652c35OuJycCBIPlter29Jyf2qHNxur5yobX8rKducSW
8sX16LCGsK73uMLRm/7x+u7EroKhqaKH34enoMqqg9j468LWkov0/6qQ1erGs8TeqYyEoO2YyKSP
8XkEhZjRhZDmhcuAko/tkZL54JGS8e7b6crjg8a209D9wOLatI7X9bj5loOFlYKw/oSI+Ia8z+rz
4tCv16Hd3q6l15TiiZ+nmIeo2qbByuyos4OiLw=="))

(defvar *b3-shares* ;; David Cohen
  (list
   "F4F9JwIEh6P+gd/9tabKnd2g0tLs0Y/W/bXBnpSc8O/F47mxlprr1/a49J+UrueHhc/fz+qGhPKz
ivfsiqzkqoHhnp2EpZKOzPXTkI2FlKn28KPEx8ufHScCBIbDq53l//WRseaCi8Kv8unlka/SrpSq
6Zb4hLOB1MXzvbLvjeny/46Fs8/XuaC16c/9kePtjefg4de6vJvDipSA16Dn8pCqj7/W7cG9///O
q2cEip7xu87t+5eAzI25uMy9i7eb3Kf83a/kspzLi5vWuN+bj879jqHF3Map/qLMssCRnPLTqPjP
w7LdsY/Sncrw4OGNuP/sg7G84pGE2ITn297KXw=="

   "F4F9JwIEg6eKl4zHrpbEyM3LjPCs9Lau3uGG+u3jlq6ox+DDupC36YvZxIHtt6TNxYiak+zvgOTY
j5G2zL+2ooGDm8vM0Ze/1ZfLtqydp9L63+qGlIXUIicCBI+z+PLBqui16pL+qN/c4+Tr9vuAhKSf
gu3DgqOg1uWM/oTB/M/flJaXneOZ8PmaiLGP67yJv+HRm+L6vebvpOXbuN6Im8SJyMKmsK6Ejs6f
u20Ei8z4qsSg3OW//+ndpdHb3oO69pKj197lhvWW6/Pm7tGToMis5N+b6+35u9zxrvH/9LrigcbE
wYvZnuqU7rXQnYnVwbD0t+TRtPia8JXzsIW+Mg=="

   "F4F9JwIEjZakkvKGtb+hzYroiPnDveThgOLz5ovz/O/a8orE5LqNu+7X4fHY15i6pbTJwp/Djcfp
5JKik7n/6o/o/Yvdi/6lvYm9iY+2tdfxjMzC+966KicCBIP6ms+N2bqNzMTAs57K+e6MqvPyt4uM
0YfptKXB8J/o/4ia9dKNsfnH3sTX8duv2vnU84nJzI2l6ODJgPHp/sOQnLas4tiJwILXmfXvzM6S
4nIEhLTwi4bL1vvZirX16q2XxYzT2pP876jVgtbeyNP606TM1cGa377cud2n46yNk/uDy4CXsJzN
v6WxwavA3ZWl/OuJ+/OfmsuozuLz57CuhKPJaw=="

   "F4F8JwIEgfGVzcK2vsi23rzRqcTyjtTVgubzgteJ7ICV2YP+3Z6g1YD1sPrf9MOrxcyFkPTlgIOX
uLuty5OIuOvT5u291YGG9vOA8LfA7MHhhbvr1KCmCycCBIHo7fLJ5/3n+9nxzZiqq9HVurHOrICq
q4G/u7mS+vX+/4nKrtevuL3OvsyZ/9T6zrWWwfXa7d6w1PP47Yzy3b+qt5euldfs7ebu6vf91uDL
pRcEzPqb1Y3v46fS/e/nj6OGkaqq8Zi+xui629WnqqnFsN2/wMDvydeF6fTy79G4tMC9hZfKxfWG
qLDT4eHr8cnRvYPUhJCo7fKmq8SkuMTogcka"

   "F4F9JwIEgueU99v98bWomPjQ9P3txMT1kKmel8j09/yHm72x/6byy7mOsPz8/sTHjKm5hs+Hpb34
wJ75y8OW2/jI+bfzn8X617LC0O/7w8X8n/O528joTCcCBIP827rL5dfMpsuZq9P96LbWo4eM+YSD
+LWrsYrqiv/6gN6MlqnJjqOQkYrLhprH3Y/+nYb25Ye8oNqXuK/AvcrXyaSkvfXfqrjw7KbovtH2
1XUEh9CUwZ/Z2om5ipuy5NXx89C8ntC0g7zH6J3JtcyNj83miLuOz6ugnaLH2Jak3N6M74mm86bf
vs3ly6iZ74CTt/XDn/HgpIqOkuSylLTZ6ZTEDQ=="

   "F4F9JwIEj+L4qJ7R4Z/tuf/bu7SKserxzqGTydnC7Ia6goPl4OLk1b+b1rvatsyR3+ulsM2UovPL
v5TRko/5r+Sz8oPKp8rBsLHqkI3t3Lq0qpqs4Ji+DScCBIekjOvL+uG+tJvr1cjTr+6EiPbrnLDn
4Ne0+Lj8rKuO8KvygsP5gdO1l4P2gfPwhtml7dDbxbmNnPbl4L3l5emE4PSa7b+EjcqMy8Omg9Tl
3mQEgc3Hp6WftIeA6LHN95eI0fbQpbCf2pyF7JjCvKnK/vaehZ6ar8SGwdetnPCAtKC7i5L9w6D7
5PSg9c2nte6K0o2FqL6Z+Y2D4fyJkPOGoNHsdA=="

   "F4F9JwIEjrm9pYjbnL6Z+oak4bHFqYr75Pazs6mCytaq++Lo4sXswL+X0PvTk6Cthf/dkaKsy42k
oq/AndLI96OUh6PW9bjdupOUxOjN1O+r78aquYG7cScCBIuI9fPIgqqtk5PFnLvG59zj/6e/wYSs
1obo1YqF/vDbrf6ZoMuc7rScw9Cc0OTdvPuJrqSb0p/RtYGQw9aR7t24/ai2jNfrvMPsmZ26sLe0
ikUEiNLLl9jH05zW+J6poMe4grfkx46Gx5LUgvC/0/fIwLrGg6Kn0OysnO22ndKWq6Kz0u+z+/Ly
++aqo9Kq0/Dj79qK6YiSvrmC/Z+yuujXzsP4Cg=="

   "F4F9JwIEhPnKsaazjqXYhIuyi83B1LOJ2bD44+OqxN250Mze98unqrf0r5qogOnqh7fIiMqFt9Wa
1aWho+701t7IwMbY+cCXiLzdlrSXu+Wkq4G5woOXcScCBIOTwMC/p+PQ5aPf45X249CulqPts4X9
7Pyzw9vpgfqk65z628zPlrv2m+ma1aanz87Rn9C+mPWpkobh+NWOmbad5eaBu/+xtviDnfv3tfPb
k2oEiYiXtIeBqNGAn4/3l+bbp52l+Nv40Lr947fli9v9nNPCoq36nsv+/97J+aG08/zRpLvRvcCd
kL+ep6KBpt7knYeD8Y6a8Im9lrrovZ3snIHXcQ=="

   "F4F9JwIEh9fH+9e0ibquy/jNmZDj8aSe8YCmtYefxc7W1orEs+q/n5uBmt6MpryanKH679CQ5ZKm
vNWE2Jmfir6v6YW2wKTOlqLO5+zmjNnW1IXetZmsQicCBI3Wz/Lwh+vr6KHfxvWOvof3++btgsij
htnD9q+zsuaayo3g5oO3vuyv08fs8M6n/sXyx6SinOiA1+3E2pWkn4vgg6eKhdfq7++5xcb/tuGK
jS0Ej8rS1M7LguOxuqzV2cLHvezS4tr7wJm995OivLPvu5jvlLa6kre5lOCa1fTovLKdkNOZsqre
peHZ6rqduNLo1uyng93AzMO6usXp1bb89fHOTA=="

   "F4F9JwIEg4Psz7bky5y6gKGspNX6zbHCvruq6ZyfsZrkn/bzs+WsrcWj+/DAiYnPq5qz+KPK2/nR
07jxkrb/7OiF3rqb76CgptKB9tT/0d6V64T064CvGycCBI25iuX4mqSbutL6gZyZysOZkeHWv9zl
kpXug8WOtOX7ktqg6tbYgtyCwb+u5/iSpuTD48en2NDUwrDL55yCvPXGppi1uq317Ii7p5KlzMeo
/TwEhaS3uK29lNLWl+qJhIOn2+7G3e7dt+TSm5qP/vT54pPkiqjkmuOL5f+ix++mmLu+nNyFpbvU
qoOYx92hjdC8nZmB8dq+n4b7paz8s7L/9tbEEg=="))

(defvar *b4-shares* ;; Helene Barab
  (list
   "F4F9JwIEg9Lx3J2Us87IoLrv65uVn6KtgNqU3vufkvzw79yi8I+AkuKbkezijJbO9tSU0bOn/MbJ
69SM/+jt67Lj1ODYpbmYnuvptvqd1NyZsqWjkeyJNycCBI6+pLGXnqXS3Ymen9n97JOAvdrPs6fg
xPatyPzYjN+/153fg7f1qMnb24HY1fOtqtjr9uy49bnwm7jL+sb6+4uG5/mIn4mox9r09vzZ49H9
8VEEj/2LvKeT45Gqr7/IlL+z14eOoeXS8r2hq7uJxu+4wMeS7NaamKi9wqitjfvQm9HNsoWxtNLJ
ltGXt56P79PXpLe1oKmowZnogZnJ1trVv7mdYA=="

   "F4F9JwIEh4nahemX85+vnoGf7M7UnIytlILYsvuV96H32Mzc1vmPm43ZmoSjhZDC4MzbxPH1kvzZ
jrXSr+uGx6ba/oyi0Jbt4feUisWNsbbHz+ODze2fVScCBIWbkJPt29+qx8O1hZbZmdqq08W6gPXE
t42qy/jDvuirtI3nqoXu0Irc64Snld3G8Pr57YCb/unH5NnY5LuStJSwpcixgI/Fpsaf8Ij1k+r/
nF8EjZqvnZ+/q5e3gJ3Ln6WRu//d8MKD5K6O5oDotaLV+b6G4ayGheGU9vCX4KGPpoCZ09OIxeKg
p8On/8+VysuqjOa+vsHg94uVrZPUxpmh9pu0LA=="

   "F4F9JwIEgajYkpziocXXw6nc1oO5r/3VsbaCiqih1p+qkuGo682R6vmnx8XBzJ+SspXnsZm3t8CY
5N2svaODgND4hMOxlo6U296isdnMnrTWpLS01e7lKicCBIXH6J/YotS8/q65iL2z0oKRw9mv3siN
6sDGiNianMXn997txa3M4ZH527ftjOaSvaWBmd7o7euBoqrbyo/Aw/yJmonGsuiJ9OWezfSQq832
sxUEjsCW+LqjrPSl9pz2pMCgjNb7nPzs9a/IpeK3m8rXsrXrm5nos+79rOOMyfDykOW4oe/KsbeS
/aipru2DvrSOt47WrN79qq/kg4zyjfCvr6qDXw=="

   "F4F9JwIEg+OJyIWW45+6udLOw/+2wZfz+IXZnI6F3YyTq4Od7a2T/6jj6KbwtcXcwcXfvs7y3MH2
9JKwsqS+nqCMyOOe68m63syAvu6Jx7mipJq/iNzidCcCBIulz5OFwMLX2biNiqrFgqalvKvx/Z+t
kNOajt/QyIfPm93trdi4rKLR0sWSsa7M7szH+fyQ68vhvb3dqsOc0Y7j8raO7L3Bnd36tq2cg9z1
xX8EhKD0xqeCve72iqipuszzmqOgl6L3jMacpLfAqZeF1feJoZKu/c2lr/Gp/sX1kbWW2MWFr/rS
3OfNuqnHsNyN5Je78825uMD69dH7+O/zp+2DYw=="

   "F4F8JwIEs8SJjLra0avb3s7Fj7zBxve4yMmG19KerZup3qLvy4Sn37qsjZyu4Yu40Mus9evzzKqi
leikobfohPe+0qisuuntxMSBk8/Dz7TRs+ejqtctJwIEheyHgsvjq827w7fs0fPD3sHW4ozYpaP9
ptCe8+DynKWmpb3e7/Ds3+rg44j5jIDU/5qEgsjNh/jl6fyE+amUgL6B8vPK+tqb75r9iamAj6/T
PgSI0IGR74iihPnXk9uqgqH+84L83obdnuDr3aTi4cH20/H9/7uXhLa89ISd2pSQmI/VoOPw6v+b
3qTdk5fu06qE8KPqmYHv6s3in5SFuLr1hP0Q"

   "F4F9JwIEhobPuo+u0qLyocbb/PbGpu2R8afS3uyX6rzO/vaHyZnirbC/4tPe4YSo2I+ZktH42uKa
5vna356J0ZvJoqaw5PHF/JmWwJSQo8fqttvPlqSFSycCBI62lbn/n+y839fZ3q3wkZOR6KichNfG
zvnii8Hwr/LWnLv+xOmG5dPdwrzU3tbZqazj+ZSTqPXOn+alh+Ckt9KSqLXbq4+SjMOsmtrI1PGY
+GcEh6vGxNWT5dqC2cOivufz5aCP8/CXh6TJzZWEz4XZ7qiI3OmA34Sy1KLh5MyGoq6s4MHc3rGA
v8e9zLfp3qCt19HGv+274rGyytL0vcWop/DUYw=="

   "F4F9JwIEi/moqcOkxuLYnLnV3cDtjryvo/CB6tGGvIyQ+LzIqZLbotnlnfK/84XF98ac6ZXyqvTb
w8am8O/+8YDh79Wf2o/Kn4iLydHH3uCch4nIzLKJLycCBIvvyv7B6oGN/9T02//K5/ay/dfkjZHd
1YGKr9eyreqrvZuVop/QgqurgubIoqi985K6u+i7iOfek+7S7byB/MiS1s7fhO2R4PuQ44/mnb/Q
sjgEjvnYyt6Kht6Q4duMx9PMm/SouKH7rvnO2bup6dSzlMWGoIuM+9qfrbCv8dzp8v+r7ZHr9tmN
qOPOmreAotSRxtz744y1g5OWsvr14fDx8vPVUA=="

   "F4F8JwIEtuLSuumc9q3ikua0j73UuZPHhrmt7qupmtGjptiA9/P+tMXbg9az1o3bmPvEyO+81L3d
tpuoxKLYw8/0yYGEwYPN3r3D/9X606y4scbpzr4YJwIEiqLTsYPWtOOx1bfcyK/5wOG8pdXQm7aX
t6LuoJOZhqn1j56d3qf+m+2Oyo7G1q/p7LCg7sHsla+ylfHA8qjDhsGEyuGDiu+hot2U95G9tZ/Y
dgSI5ObW3//Lv5m61rGIvrXltpPP9feB67Oc3aOjxrbS4+eAhoiVipT43/f/57nn6e+kkvectZL1
5e62gN2zvfngxYmY2t/Nr8vZifr05YKdu8Vh"

   "F4F9JwIEh43u1PGdrICDu8Sj9o+5tY+K1I7A/8GU3I74y4aS74uWv9Wet53KtOS7++jX0LOph6nt
1Y+g3dfMk9yOn6vtsNfQv/Pxyob+vMuD5dzow/aoOCcCBIS/yqnvhJrhm9Sc0u6j7pmRw629hrK5
7+3wt+K4h9HU6aP/vcXm4eTvu6uS35GA3//v35a+o47p8sPcm9i1iMbo9oK/7ZDhkY/H58nt/Peq
9RcEitKV+cvX556YtN/Xt6n63eqtjoz6hf2LiLuf6rOd2O/A4O+3tfDNhuH6z/6ksp3kusiHwLKF
996I0ffKv6Omv87inYidm93blqvBvOWku/2BQA=="

   "F4F9JwIEhZGe7JX22sLW49q4tuanmqu6ouTk1Y7D9Zj59diiwuPHjNKS//SB4Z+g6/Hy6Pmwgu6m
97T98rX35pzQn9advKeHrM2nopnQlLWY7IrWlqKAJicCBIT6v4Whk+nay7DH/JOM1OfAke28nobb
/Peorbqt/9iYk4G1m8qfj5KAyOu5xKT+xt+Us83+kuzkxOKo6NmXp7ut2Pm6vpv+pdWw1YyElNit
pEcEgeCLnrbPr/zw0ZTfzLfbxMDt77idycepoZKN2N3ejZzNmorjm/SS2IH+rcjUzriEg9/V2Ljn
rJbbxuLa8IHW24SE782zjrndhsqP1unpgcaCVA=="))
|#

#|
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
    (ctr-hmac-encrypt-file *escrow-key-file*
                           (concatenate 'string
                                        *escrow-key-file*
                                        ".aes")
                           key)))

  
(defun decrypt-escrow-key-file (key)
  ;; key as computed from compute-ip-escrow-key
  (ctr-hmac-decrypt-file (concatenate 'string
                                      *escrow-key-file*
                                      ".aes")
                         *escrow-key-file*
                         key))

(defun encrypt-share (share key)
  (let* ((key (encode-object-to-base64 key))
         (enc (loenc:encode share))
         (len (length enc)))
    (encode-bytes-to-base64
     (ubstream:with-output-to-ubyte-stream (out)
       (ubstream:with-input-from-ubyte-stream (in enc)
         (ctr-hmac-encrypt-stream in len out key)))) ))

(defun recrypt (k1 k2 x)
  (let* ((share1 (decode-b64 k1))
         (share2 (decode-b64 k2))
         (curve  (list share1 share2))
         (x0     (decode-b64 x))
         (y0     (solve-lagrange curve x0))
         (xk     (convert-bytes-to-int (ctr-drbg 571)))
         (yk     (solve-lagrange curve xk))
         (unl    (list x0 y0))
         (enc    (loenc:encode unl))
         (len    (length enc))
         (crypt  (ubstream:with-output-to-ubyte-stream (out)
                   (ubstream:with-input-from-ubyte-stream (in enc)
                     (ctr-hmac-encrypt-stream in len out (encode-object-to-base64 yk))))))
    (list
     :x0    (encode-b64 xk)
     :crypt (encode-bytes-to-base64 crypt))))
    
|#

(defun decrypt-escrow-key (key)
  (let* ((enc    (decode-bytes-from-base64
                  "LIYxpFycEeGD0sgqFERupz0gW7fcYGGo+6Acvwqf0/33dHeIeW4R4YimyCoURG6nUhDEQaKNh/Nr
2+4Z2Y3dwO0EpjM4d8XYk3m6BhueB6j2TBA2eRQ8nDLMLjgarWF2aLoTUoOWcIDEnAjIjeuW8EFH
/IH+680do9zb9iESvNL2oWfGXN/3LaO75n1jo4lWhC2sjAPiM7EsFNfnmkWlqMnyPVlUGqxg8rNM
uzuf5e1bYUacWjdlpe1yMz3C+tGty79rnGTdn9LvAga/VYRSxs7z/BHLL9+rmUdyHnMHY6s/OlvZ
IHNIfPkcN6nswGEKDJ3jC/Q+fFZOQFHxO0Ayi3oL/5a7i8RtyQ==")))
    (with-output-to-string (sout)
      (ubstream:with-input-from-ubyte-stream (sin enc)
        (ctr-hmac-decrypt-stream sin (length enc) sout key)))))

;; ------------------------------------------------------------
;; This is the main unlocking routine, once we have 3 unlocking keys

(defun get-escrow-key (unlock-key1 unlock-key2 unlock-key3)
  ;; takes 3 unlocking keys in base-64 encoding. Unlocking keys come
  ;; from computing them from pairs of board member keys.
  ;;
  ;; Returns the text describing how to access the encrypted IP.
  ;;
  (decrypt-escrow-key
   (compute-ip-escrow-key unlock-key1
                          unlock-key2
                          unlock-key3)))

;; -------------------------------------------------------------

(defun compute-ip-escrow-key (unlock-key1 unlock-key2 unlock-key3)
  ;; takes 3 unlocking keys in base-64 encoding. Unlocking keys come
  ;; from computing them from pairs of board member keys
  (encode-object-to-base64
   (solve-lagrange (list (decode-object-from-base64 unlock-key1)
                         (decode-object-from-base64 unlock-key2)
                         (decode-object-from-base64 unlock-key3))
                   (decode-object-from-base64
                    "BIKev/XCxLGR3ZLdibXFw4mUv8XKsramlsORw8qdrqyN/9bmivXghpTR5sHW9vba0c77hu3l9v6d
ib6jotKP7ZywtOeS5+ao8azt1OWlwb+DyUQ=")) ))

(defun compute-unlock-key (key1 key2 x enc)
  ;; Takes two board-member keys, an abscissa specific to each baord
  ;; member, and an encrypted quadratic share, all in base-64 encoding
  ;;
  ;; Returns a base-64 encoding of the quadratic share if successful in
  ;; decryption
  ;;
  ;; By encrypting the quadratic share, we can discern an error in the
  ;; key pair being used, as it will produce a decryption error on the
  ;; HMAC.
  ;;
  (let* ((k1  (decode-b64 key1))
         (k2  (decode-b64 key2))
         (x   (decode-b64 x))
         (y   (solve-lagrange (list k1 k2) x))
         (enc (decode-bytes-from-base64 enc))
         (len (length enc)))
    (encode-bytes-to-base64
     (ubstream:with-output-to-ubyte-stream (out)
       (ubstream:with-input-from-ubyte-stream (in enc)
         (ctr-hmac-decrypt-stream in len out (encode-object-to-base64 y))))) ))
     
;; -----------------------------------------------------------------
;; These are the routines for computing unlocking keys for each of the
;; board members. Each routine requires a pair of keys.

(defun compute-hb-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
                      "F1MEg8Cyw5av/9ri38SGwN/QpJ3CxNbuo5Sx9LbU7NCrmbDi2KmGl636mtG5rZ7er9OXz8m8merk
js7t8vaD87XQs7u5u7iq5amlw7DwqveTrIXNMA=="
                      "LIYxpFycEeGD0sgqFERup3nazHlWdtU+6IrnU+KfBi/U8pNceiIR4YwsyCoURG6nf8J2jzZnFDHO
BBtXpKrFgc/2RHh9GawZqBTb490gvFkmZ7vFBDekBBthtgjhQ7crbHTawQ7RZZ41i7vbvaKdADGL
Zx8glSVK0ylBF98lWD3bSVqVsl54M5hb1t9ohRHOoVH6I3YI+l/hzz62v0hDwa1psq0Oi8lEpK0e
xMcNUbd94VGLQPsDa3ilSYYepP2nE9V5zhweSPu0RLltrbO1hHf8WWzqAKyQuJXGz/ZgqXN2KeTG
YLoqzVTu65ErGD8U1jx2ByvVWHblat3vcoGFsAjJx5E+IWiUzFHrdmAtU8Kk9KYHLAjC42N5xc11
9ufoSDB/8t8FmeWuEhwgA7rtDtJgRMsOCNkhtBM5Q8/cimDunmwn+XYlFRMuvJyJ"))

(defun compute-dc-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
                      "F1MEhMft6IHelbb3r6bGhJ+lvYTJ6oT2x43hztOyg5LC2bfZxq3cipDXmbnmwZDGoZC408P/paWv
xP/AiJTrz4K6hJvSuIrh/NPw6IfynuS/jZWDCA=="
                      "LIYxpFycEeGD0sgqFERup2TTh5yJjXjIeptqdTE00vVb4xF2eicR4YwsyCoURG6nJZJtI3fAq0j6
dYbPBeZ0nFHrQIpPk4qFPYVqU74m1h5PU4LWPXGcCAQcx9RlXKMS5LnGG8YK6mkqw2m0JvsFYUep
2LMZapS7xxSMS0Ls+o/Ymza0vxIJMoI2WL1MsizpPIR975rZ+1JQM5MPjBvHTDrnfUiNEWBCJUM+
EEoDjMRL7bP0FRJMbXxUeolbx1feCrFTRnctivlMB+Am/KJwkF0VBgO+1NN0rI+Mh6PLtrRowkOD
KQ6FM88rs7LPN/Gw9zCVMhbvtUuO5clIbsQQz98Q//qxtYRgWigpqsN1EAtrObgqhHZsUBW3bJkI
w5kB3SaRWpac0NjPo/cYou7nQqBPqUWTES3kOHyvhx89+lsknSJX8qmXRyhokyPE"))

(defun compute-qn-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
                      "F1MEg7CHr6aT2uqzsfbUjMjx9q/Cs8CC4NrA6ILmw5L9jZ2N0LHGxqK6qbTOt/Tb482iq/b4sbPw
rZvfwYuo3czambHVmt7z3emT44i+2begjv+CRQ=="
                      "LIYxpFycEeGD0sgqFERup2UvQsfFUhomhVOhLmXxI4sUvTOyeicR4YwsyCoURG6nB4IIjJwbdJsu
fbJG6y0p0wr2GZ8pWBO+mHOABuaFqQu8h1zZ2wZnBK6CGLnbi346nbW1+5i8j5PuW6BkWDfg2tTk
/HnlNvVSvBv7vALB7I2OrZwYU8cxqkJRD3iJ53rrc9uJ0KTt5NhXv27mqLOfC5Sb7kafXc591ds9
AewPqs6DWGailKh4sH4k5gWx/M0Y+NaiYYX/u89XaFkbpDMbN3pJ12DJjWlrD1RoIuAGr4GMCgEc
wWJUoNS91nCrCLtSzb0l1pnMoguPopJIz06B8yAwhKj1lO1wVju/fV01tM7+zkRzBIb0cxhfUElt
gsoQihFpV/xpQP7S2YRWSbfO5t+8X8rRPSdbcBks0bBZ0s0zCHfqLeKUo0Zu3pmT"))

(defun compute-pe-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
                      "F1MEh6Okq/Cy88/a77+Z3PzU/onBgoT05t6v+pbUrPnatLaVnJ7bvJmQsuuRsu/04Zmhsvyh+pju
k7yV65TSyJ7I3qKNgfCM2qWqrrqo2qvE78m7Sw=="
                      "LIYxpFycEeGD0sgqFERup/i6vndNJ3ZY3shlmCYOI8XKsbBoeiYR4YwsyCoURG6nVx96MPs49igP
tVnpTHrp543yRzEWgXYjhhjkgc6qN1ClVfsglFw3LO+aCPjMHNvNNpCtQjLZ4NH8HF+sDp4jx3hV
PgV0HkiO4wTJXs21aqMOU0q7QaIu3+vXc3NF8oA97dHpe8Ogdnj7KWQzPhzSviQmLSVwTC9cDd8v
QhAKqhJF/vzSJkhp3DgKpKAcwg4wNxJe9w1JyDkTKblnkXmSgD7WSrWvdD/kyVcbwMnaKrABjGwb
2z2cRnZd7eEutfX/xhdBJ9bXhAF1b3V3A0CzeqZ1ly3OyA2t/rFq8XpRaU+Lvqwe9HOepbgOEeV6
6QTKxqxiyxr+sPm9hJkI7RZl+JIPx6311eeq0Y+RO8jH5GATVjJCjoJMul2/yJDI"))



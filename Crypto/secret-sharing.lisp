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


(in-package :ecc-crypto-b571)

;; ---------------------------------------------------------------
#|
(defvar *acudora-trade-secrets-key* ;; private
  ;; (ecc-mul *ecc-acudora-public-key* (ecc-random-key))
  (LIST (big32 #x0534CE2C #x070FAA14 #x120101D4 #x3ECD01E1
               #x2335C16A #xF9BA70B7 #xBD55E164 #xC7C275E6
               #x5E424B50 #xD40EFA84 #xE7511561 #x9A9607E8
               #x23E0AB24 #xEDA751CC #x9101AE19 #x5138BCA4
               #xDA132C08 #x6022BCC3 )
        (big32 #x06D18042 #x12019D97 #x4521645F #x2892ED43
               #x69CBF287 #x7E38E501 #xADFA9C35 #x8EBF118C
               #x78D3ADBB #x056F377B #x4622A629 #xDB93CEF7
               #x948D17B5 #xD9A82777 #xE2C9C101 #xFC3C33F4
               #x2BD096A8 #x98E6131F )))
(validate-public-key *acudora-trade-secrets-key*)
|#

(defun format-pt (key)
  `(list
    ,(format-fragments nil (ecc-pt-x key))
    ,(format-fragments nil (ecc-pt-y key))))

(defun generate-share (msg coffs)
  (let ((x (ctr-drbg-int *nbits*)))
    (list x
          (ecc-add msg
                   (ecc-mul *ecc-acudora-public-key*
                            (let ((sum 0)
                                  (z   x))
                              (loop for coff in coffs do
                                    (setf sum (add-mod *ecc-r* sum
                                                       (mult-mod *ecc-r* coff z))
                                          z   (mult-mod *ecc-r* x z)))
                              sum)))) ))

(defun generate-shares (n msg order)
  (let* ((coffs (loop repeat order collect
                      (ctr-drbg-int *nbits*))))
    (loop repeat n collect
          (generate-share msg coffs))))

(defun force-outcome (m shares)
  ;; generate offset point to ensure outcome m
  (let* ((ans (solve-lagrange shares)))
    (ecc-sub m ans)))

#|
;; As of 11/24/11 DM
(defvar *hb-shares*
  #|
  (encode-shares
   (generate-shares 10
                    (ecc-mul *acudora-trade-secrets-key* (ecc-random-key))
                    1))
  |#
  '("JwIEgf+P2q3JmJD72PaW7bSl8rTpj9qOotLwxPnCn5uI+8X7u9mjscKyvMajopv3xM/pqdPI7uSg
s/bwwdHTn9Gn+4T96e3CstCi1KewqI/emLnaWicCBIuHrp/+oJ/8rLfcgsOG0szFnfKr7cLtg6ud
hZq2hpaTvf7Gmc20ot3mlPfErIWi1+r5keHiypzQhuTHzuSNjOf06p7pvJiKpJmj0dHL26/qqS4E
iMGPgbCz5tfU48i4gNClod6Cn6Op8I6Gs8zxpNaFvsX7jeSVm6Seh/6WsYC5ttKXrN7wneGVodzi
4rqnq7vGjabShN6vu+Dr7fyfscuqwcaWNw=="
    "JwIEhciYgvbPzdqwmoXWr+jXgImQgZ3pwYXxmLmVsbDl9o3u6KrKwNruhMqTipnA3JGOns/VgY72
l//XiPWkiPKp7o+J+cWBtZK+gPb2qbGrlsm3WScCBIKktuT5uIqxyOmZka/9w6fenvz09+/Pmonx
tdGmuaCvrJLTnOP2rqfY/f/2+MaL+MTC2O3X3/XTqL2owe/P7Mub39Ce38TaudikzILWweafwxUE
j52z5P+66Mulm4rNy4/KjrDt+4D94onR3PLzrKzPyviWk8qv4dfrr5j9kuaGxeCyk/yczP7w9v+i
0v/4l9GR0uPK1s/HzZCOhYiar+vfuunMcg=="
    "JwIEhrvi1qnok46fhpiG79m4tJ2m5oT3sKmW8ITH2saXkuPLoumjlcqJ+pfFppKX/e7ft4T91J+k
4b7wi4bMotDwkbXSjLaoquDjr7vkq6Xan8SrAicCBIns+KPdoLL1gofZtYCW1Jz0zMCot/XozoPP
hreVhO+/lPznwezEiYWR8JWV5bT2ntHg44zWjujUmbrolp/W3KCTkr+y3Y+b//mnya2KwbCuzikE
jcHrqKzBwt/17rr7zJWDi83A7Jruj9Ou84/GoLLqxbDt1orA68HupM+D3eC8sbDq07uBuLb8m/ft
yaW8662f79vIj/Kl5+bupNDcnKyktsbeDw=="
    "JwIE0uO/j6rJkrPsyNr2076j4/+Cjun6vuqwoeW5goTG9ZvO5KOOrfbVqdPgzNaMks/Mw6rQ3oL2
8ZSek/7AocuEwZCjmfn1qt+G567N79GGhIldJwIEi9T4pujD0cXOy6us1ryf5LOfwP3g75WFucuf
5vzovYKdg8y8k9zWubu3jLqxz4vU0ofx04L7n/udh+/Dkp2LtM/ivNj8jO/2tbeEgcC70r6sIwSH
n/jRsJzv7ISHpuuF7r+k/P+kle+PsYPa1a640afJ/M+ZmqPpzaTo+KuphvHTkq3+74KOsvm4tfPQ
y4mYtcHrqcfr25Ld95XL5LSf74ntl4Zp"
    "JwIEhYy7ve2E7cO0le/LxrH2+aGGivH+ioi90IqS8Lb6wKfusuLw7tHN3+6x4773/K312c7Vp5/X
nui0lazNwcWXrNeVnrHa14XMmfrU2rbw/4XqdCcCBILjpd3osuanvOHQiZ7dztyu5Mfo09Wm35O+
oqXOl5Oj5tmMjezPuZyl8+yZhNaFgKLEraDyv/TG+KL10dHG9q7RssiL2dOSjq+52orxk4fcqBAE
gYn2/o2Z15OUv/zEm4HLiPOyk8n1qqXy1+OOs9Wok9TimL2YmKD6tuaShv6jr8S5squh/rCSpMrz
rKOPrcb8zMOH0ZnNvIaG5u3LssaD8o/nBA=="
    "JwIEg8OkzIuooMT3uYSrpJuhhP+U+LeloIzrj72qqef8rOnNo4WQ3dONicrUydW07P6PxtXQ2bW1
2M7j57nX2O6w4pey5ruLztSAo4mQpODkjYnNNCcCBIWDoMjWhq7I1qSF1pTGlPHyq9W2jtD6yaTR
tZ/Yxq6xpY+0grLPjrK96rjL6eutr8KL5qezs6LljILM4r6Ykoyypb6N74m++sPdybzDkI2HqHUE
jL3S+IixmqWd3vWf5dSulpXA/of55OnDydTgn82EqK+ToKPOr8PJgo+wj72LzoXmo7Xn9pej9oi8
mN26lqCtyraG4u20xO3f4JfcvOf6uMzMbw=="
    "JwIEhpzr+OjJg//hxPPViozB9M/LnN6LqtGAvN2EuZWnn8C/tLeTno3UnYP19q7plJfbkd2vtuz8
yuCmnYyG37GLl+i455mMtKbiq8H71NfW4p2ZACcCBI29n6f5m92h6ISuzb2+wtyowNTjj6qXg56p
4O/pkvL8gam4xvbSuoW/l+S3/8T0x4mlus6q95/nsvy4q97Wocyu0pKDvorGsZXQ3KnA6oHH4TgE
iNylvdD0t5vp3oybs/XclPnVtpOHg4mC3KKT7un3uOnexMHEtfGt55O4pbyo9eyT6o3S1IqDw7XQ
nMHy9sTbkvihpu+3mMfh8Iyk8sPJjIq3NA=="
    "JwIEh52esOy/3YmY3b7A7ZfM3ezwgaHi2oP7zvSj+9j8+byk4auJ6Y/F7/yzxuHN/cHpuZGF19q3
4Ie1re/D3PaQtuCCmZ2NtvzWzNLu2c7rppiYWycCBI/R0NT85riY5/jtnvD9tYqj5IDorKuSgYv5
8ODZy+DJ4ouI45mh3LvdxJKb0t6/ktKsu7OM7ci015yllImWxqf2xvTVhfaX7vuloL+fk/Chj3wE
jOqupf/OhK7es82Ct6Pa4/Gjkveo6cDHqP3co6ij0NCP3sbOiZLTpJv6obffoaPD7dua+8D7psvR
zPWxwc7mrcGbmpqI/4GIrNqOp63PmLKybg=="
    "JwIEg+blxJyG5PDI4ZiR7Jnz6PbgjZjZ8ueTg6a8w8CugPygtoqe8Lad2vHekd77qKSZzc/5yKTu
hcH297DAku3x1tmSyuPVtO3N+JiLga7n2tbGYCcCBIqf8eWT3L2Gw6Ty34Cu59HbkJjarsHXs5u9
1qSJ/eS/+MvEp7vbipDF5PaxqsSmidOR6vCsiquhv6C2k+vv/6+atoGUu/iHxZD8uI+BupOlJASB
tu/YrM7BpIvKkuSZme7L0syKvqndjZGLlbvs5c6m+9DqgO3c6/GfgL+L+6jH8pr6rJTavJfpiITD
nsGnrILK7IXYtdDb0uu83aKE2tfczaQy"
    "JwIEg8TAveqtncu0rsvg197C6bO73rCdwLKy5PPKuNjM0d362re1g7/m96qZuKO45oudufiL6frO
o+bQjLiO6ZCOt47By/r3gOTBqtnkoJ/m7qG5aycCBIrOusithuGz86rZ9++Jvuvdj7mdvviUuYmW
kNqEh52e38i4k4iplZ7D7Kys+OWs4rGXueq/g6GbvN7a0eqv8Kq/0OvHrZ7X8vPX5K60voaBtTIE
jL3fycuokLe8y8G8j9rNluKi5Mmxy9PwwJbW25yJ08XGiePyyrCPxqm79OjRt6y21Y6a6Pyi8dK0
nvfVqOGw37qe4Z6Q5MzqxvS3nvyb6uKGWg=="))

  (LIST (LIST (big32 #x00FF1F69 #x6C93043D #xD8EC5B6B #x44BC9A69 ;; share X-coord
                     #x1F68722A #x5C227984 #x7CD88F71 #x7DBBB28D
                     #x8C264F23 #x23446FBC #x49FA54D3 #x91BB2206
                     #x7DB841A3 #x4CFD14FE #xC27DD3B6 #x132A08AA
                     #x2760A07D #xE30E6D5A )
              (LIST (big32 #x05875C7F #xF203FF16 #x37B80A18 #x6A53229D ;; share Y-coord (ecc-x)
                           #xE4AF6C2D #xA0D59D0A #x69B062C4 #xDEFE8C66
                           #x6B445773 #x14EF1160 #x5455F579 #x238714A3
                           #x9403648F #x3B20D199 #xFA6A3DA5 #xE181490C
                           #xA3A3465D #xB5FA94AE )
                    (big32 #x04411E05 #x833CD5EA #x6390E005 #x04A86F02 ;; share Y-coord (ecc-y)
                           #x3E8D4F01 #xC199CCE2 #x92B057D1 #x7D8DC854
                           #xDA43C1FF #x166201CB #x6A45D65E #xE0770954
                           #x37316274 #x9D5BB8C3 #x53520979 #x7BBC1AF6
                           #xFC3EC65A #xA8318B37 )))
        (LIST (big32 #x02C8300B #xB4F9B698 #x1A0B597E #x8AE00490
                     #x02774C10 #xBC4C392A #xC5865EC3 #x7768552A
                     #x05ADC125 #x13146605 #xC2238F4F #xAA047762
                     #xFFEB88EA #x9047253B #x8789F314 #x0B524F80
                     #x76ECA58A #xB2D25BD9 )
              (LIST (big32 #x01246D93 #xCB814C64 #x6932457F #xD869EF1E
                           #xF9D3BEF9 #xE684F16B #x4533940B #xD612A673
                           #x1F65C9EC #x7DFFDBC4 #x617E2242 #xB1B6BDFE
                           #xB4D43D51 #x077CFD92 #xCDDFA07A #xFC4B4E6C
                           #x24980AB4 #x1CC7E195 )
                    (big32 #x079D6793 #xFBAD12D2 #x9B153658 #xF943986D
                           #xF603EE21 #x346E72E6 #xB164F95E #x0B1394BF
                           #x0D7D6BCC #x7D259834 #x5C0C89FC #x3933F70E
                           #xDFD152FF #xE0BD1234 #xB1CAAD3E #x3CD20382
                           #x8834BF5D #xF75A6672 )))
        (LIST (big32 #x033BC559 #x4E82638F #x86301B7D #x970D0EA6
                     #xCC13BB05 #x25B8048F #x6A317258 #xE5A2D28C
                     #xACA13E8B #xC54C48BF #xDDD7DB84 #xFB50FA4C
                     #x2FB80B0D #x31150E04 #x5AD218D9 #x42AC18D7
                     #xBBC8AD2D #xA3F11582 )
              (LIST (big32 #x04ECF08E #xEA065D41 #x07B2D401 #x6A873A4C
                           #x80A1BF5D #x1381CF0C #xDCA84DEF #xCA7CCF07
                           #x64412148 #xF02A572B #x4EC7A8E0 #xC632B0ED
                           #x150CBAD0 #x58FD6B88 #x09927ECA #xE8F37FFC
                           #xA792B454 #x160BA729 )
                    (big32 #x06C1D6A1 #x641857FA #xEE75EE61 #x5062E6C0
                           #xD86B70FA #x6BB98F8C #x8196A8AC #x36D61503
                           #x5C1DC927 #x83BB81E3 #x161AA9BB #x02E1B7C3
                           #x7DF6C94A #xF35AD3FB #xEDC81FC9 #x2E7CDB92
                           #x50B87162 #x46D1AF0F )))
        (LIST (big32 #x0052C6FC #x7AA92499 #xEC916BB5 #x37C8F1FF
                     #x043B4FA7 #xDA9821CA #xE41048DD #x4DCEC88C
                     #x72DED554 #xD3C132B0 #xC253E643 #x5542F02E
                     #xDC4A1E27 #xFA021961 #x20904667 #xCF5557C3
                     #x675D377D #x10C104DD )
              (LIST (big32 #x05D4F09B #x443A3167 #x4B56B2B3 #xC3F9199F
                           #x81F706F2 #xA15CCB3F #x9BE687A0 #x8E8398F0
                           #x9DCACE5D #xB718E98C #xF1752907 #xE34C17B3
                           #xFECE87DF #x0C91D16D #x27E27963 #xE0CDFD9A
                           #xB7080603 #xBA4F9623 )
                    (big32 #x039FF145 #x81CDFB02 #x074DAC2E #xE7E93E7F
                           #x485778F6 #x20ED555C #xE28A793F #x2799348F
                           #x4CD49A3C #x2B521B8D #x324B7F6F #x04399797
                           #x0D79D096 #x24C3583A #xD4C7D76C #x95DEE565
                           #xE4687F78 #x9DA5C369 )))
        (LIST (big32 #x028C76F7 #x684DB0DA #x15DF2E33 #x1EDE5086
                     #x15C7F0A1 #x0F680A25 #xC1B7A809 #xF732C5C3
                     #x7519B7F7 #x31C6FBBF #xC5BD6CCE #xAA9CFD73
                     #xDA1A1559 #x360C52EB #x2B953CC6 #xD570B30C
                     #xFAA969B7 #x0FE17574 )
              (LIST (big32 #x01634B77 #x432CC9DE #x61A024F5 #xD9D71764
                           #x8FA29D54 #xD7C9BE44 #x96717268 #xF3591837
                           #x64F72712 #xF3D86425 #x60A01144 #x5A8393FE
                           #x91BC22EB #x468C6ECB #xA8B2902E #xCD324397
                           #xB9B42B89 #x30F71410 )
                    (big32 #x0089EDF8 #x699AE4CA #x3FF910D8 #x196239B2
                           #x2727AAA4 #xBCABE31C #xCEAA8275 #x31187A60
                           #xC20F4DB3 #x120DF91A #xF88E592B #x43F98124
                           #x92B9AC46 #x3D6C6F93 #x2187A266 #x6BC0C1B3
                           #x6D96CA30 #x3E43F384 )))
        (LIST (big32 #x01C34930 #x5A84113B #xB908AD21 #xB4213F94
                     #xF0DD2A01 #x9AC7BD54 #xA73FC59A #x66A30A42
                     #xED31A265 #x549355A6 #xCFC3E355 #xA165AB5B
                     #x13B1E773 #x5EC6E618 #x8BB2CCEC #x5CEA8011
                     #x89209306 #x41A266B4 )
              (LIST (big32 #x02834122 #xB065D22B #x240B58A4 #x629C792B
                           #xAAD8750F #x5252516A #x7EC465CC #x528F6809
                           #x94F1CC9E #xEA712F4E #xB5ABE10B #xCC9D9B34
                           #x59460299 #x89F18243 #x19257C37 #x7897DEA1
                           #xDD92F219 #x01A1D475 )
                    (big32 #x063DA5E0 #x4313494E #xDEEA7F2D #x45C58AC0
                           #xFC1FCE4D #x30E4D4C0 #x7E68450B #xC9A04739
                           #x7C392087 #xB01EF45C #xE0B991B5 #xCFD8BA3E
                           #xC21E18BA #xE8B205B2 #x9B06C5B5 #xA44DB7F0
                           #x17B8F33F #xA713266F )))
        (LIST (big32 #x031CD7E3 #x44907FF0 #xC4E75450 #xC83D27CB
                     #x39785AAA #x201E5D08 #xE4AA73F0 #x1FB46E4C
                     #xF0DA8741 #xF5ECBB49 #x42F6C8DD #x5EDB67C9
                     #x58131D18 #x1AFB1165 #xF438CE64 #x6344D895
                     #xC1F752BD #x6C474C80 )
              (LIST (big32 #x06BD3E9F #xC9BBA874 #x045D35EB #xE8571440
                           #xA98C7AA2 #xE0CF29C1 #xBF492E5F #x00A9711B
                           #xB527415F #x97C8DFFC #x4E91C4A5 #x75395773
                           #xF9D97C70 #xAEF56433 #x1752240D #xF0A8CC4A
                           #xD0B8A606 #xA031F0B8 )
                    (big32 #x045C4AF6 #x8746E6F4 #xDE186D9F #x5B853CD5
                           #x6C4C3831 #x20AE2227 #xBB4F771A #x6F448311
                           #xAF15B9C9 #xB84AF147 #x5D84F50D #xA5505038
                           #x6D681C83 #xCBB44B64 #xBC214DBD #xB988F878
                           #x0C49CA1C #x91829BB4 )))
        (LIST (big32 #x039D3CC3 #x63FBA24C #x5D7D0369 #x79977670
                     #x028715A0 #x7EE77447 #xEEC7CF2F #x12615627
                     #x48F8BBFE #x338D866F #xD83A5C91 #x0B5ED37C
                     #x01DAADDF #x0EE7620D #xB0023274 #x6B6F95A6
                     #x52DD6676 #xB4C60C5B )
              (LIST (big32 #x07D1A153 #xE6670633 #xF8DA7B87 #xD6A291E4
                           #x01A162B2 #x4045F9E1 #x82CCBC12 #x710B118C
                           #xCA1B8EEE #xC4246E95 #xE7E4A92C #x76CC66D9
                           #x0D2B9C4A #x504968C9 #xFB46E954 #x2F62FBBD
                           #xA540FCF9 #x3E0847FC )
                    (big32 #x066A5C97 #xFCE08BAF #x339A09BA #x3B58F8A3
                           #x25DD4698 #x11D47DB8 #x8D423A14 #x07DE8D38
                           #x492A690D #xFA42DEFA #x1470F6DB #x35EE07B4
                           #xD2E8CCEA #xC60CECCB #x609B3468 #x47F02216
                           #x5A1C9D6C #xF30C996E )))
        (LIST (big32 #x01E6CB10 #xE06C9C24 #x61304761 #x9E7A3B60
                     #x1A62CF2C #xE4C1A679 #x0E02E01F #x1036147B
                     #x8363B6B8 #xDE237BDA #x848666CF #xF32126E0
                     #xB07B7761 #x0096DE35 #xAC92958E #xAB4DB37C
                     #x18160576 #x7B55A360 )
              (LIST (big32 #x000A3FC7 #x293B8F43 #x4349CAF8 #x05D9E8DB
                           #x2062D2E8 #x35D99B7B #x59209FB9 #x1FF89711
                           #x3BBB6288 #x45C9D98A #xA88984D3 #x23AB82C1
                           #x4AD0BF40 #xD89EBDFF #xD79A6C04 #xA3BF01E2
                           #x90F8E078 #x1744D2A4 )
                    (big32 #x00B6DF61 #x64E82905 #xCA2590C9 #x9DD2E94C
                           #x14F94DD1 #xA4459577 #xB32CE4DE #xE86A01B6
                           #xE6BE27C0 #x3F17ED44 #x7E46BD2C #x2969E17D
                           #x2202433D #x053AC052 #xB605B0D6 #x85BA5ADE
                           #x5D4412D5 #x7B935232 )))
        (LIST (big32 #x01C480F7 #x52D3B2DA #x2E9782BD #xE85A59BB
                     #xBCC0EC06 #x4CB27394 #xE2C4CA37 #x7D5A6ED4
                     #x1BFCDDD5 #x19708DC6 #x61675CF8 #x17A7D4E4
                     #x79A80C70 #x3B4901CD #xC74197EB #xB80C9055
                     #x59C880FE #x6DC85CEB )
              (LIST (big32 #x054E7521 #x686C2CF9 #xAAB3DF78 #x97DAEE8F
                           #x7275F782 #x8E449621 #x682073A7 #xAFC8704C
                           #x4292A7A1 #xEC58B3C6 #x55989897 #x73A9F834
                           #x26DE5EB5 #x4752FE0A #x9FD0D71D #x69EAFCB9
                           #xD7C8B9A3 #xE0C05AB2 )
                    (big32 #x063DBF26 #x5A820DDE #x4B82F07D #xA9A5B122
                           #xC9258CBA #x7C2016AD #x6CE09A71 #x6309C7CA
                           #x5301F194 #xBBE9A28B #x758DAA8E #x35A3E22E
                           #x349A1EEF #x55461617 #xDD1EC278 #x86499AA3
                           #x746E7BE1 #xBD58835A )))))
  
(defvar *pe-shares*
  #|
  (encode-shares
   (generate-shares 10
                    (ecc-mul *acudora-trade-secrets-key* (ecc-random-key))
                    1))
  |#
  '("JwIEhPqEuv/LqJ3bipG9lY6S1sbktbPcn4+mt8j2icqgqaPj89+9sOX9v4iSo6rn1veA38HSjJaI
nr7J5tq+jsjMj8nQ6fPbnMH4tvig9Yzs9OSACycCBIjjj9qo9LT92OKjpsXD19aXiJ/rxKH9u+bk
hI/Okq/1z4ycut3kstj+yK2PkM2Cm9Kt5+Dfk8f/37W/xfvXwoLD3KimqKScq93WkYDgoeGwtWQE
jcaeh8fVg8mDn+vrqMP3++PogfrAsIuW+paToMDyuqyC9rzhgajXhKOvxvmi/Mf3lI+QrIOjwtGO
0/u2z7XGvMub/qSkpr6dz9m52ZW+1aPzTg=="
    "JwIEhLma1t7ZruzJ47PpqoeL+aSbi/ypqoWDwJKuu+HSlde4s8Cf6de6l/ur+MKlqsD5zKX/jNm4
wab67MqI3o/ak4fhv+/y+PD0nr2M787Hm/KDficCBIzZyfnBnLyowfbR8JH3uNeXi53fu4jIrZXR
n7/4gtuVg5bx7vzZ4OboyZG4+8+zvIuS7I6Q16LlutiQgOuRwuCBy5yJtd/bzsK+stXGtNeTvHQE
jd3GybqStLKS/e6B0rOh163X4Iuhp4jn2qj53N3Wr+KjjP+kg5Ol6aLM5e/L0viVo/Pwle3C4MTR
76zV1rHdxurOwYvX3PjOxYeElLjQlcuGVw=="
    "JwIEg6iU34HBgoKa2rOL0uzFqYbfrvapp/K4xrD01ITAsaa0sLzZneHGodeksMbmxcPZ7aTK4sOR
xe/B47eH8Jyepfzo5bbv0Om1483H1NKZ7Ja/dycCBITG8Nrlo93U78fwrIu57YWYka6muoLYzZCG
uJ78kfWliveB6O3pk8vm69XAu46Ai5ejsIKs1NTrveChnPCQtr7dot2ihuDpv5/khdSNv8CV8nUE
htjwkpKIr/KUnv3VipKDt8TfrcnXjvaSleacxZHni6erjr7u477Jm+fYmZuS7a/pkfPbrfDnuqHr
1LGsn+fs6OWtjKuLuLXh15a8wufxwtmSQA=="
    "JwIEgpep6se+hJqxrIHjvqK657H6/PrIuaTOvoe+yqi3q9nhsqDt66KS3Lzxvoio6MWPkv3HlamW
/dzWrvezpZD5h+Wb8ID18bnHgvOrwpXp/YmPPCcCBIjtudrx34OL6sPrvsnAq7jt5r/vytf174Ck
w5Cz5Juw66Dn6brx87+po/O3vvzT4anW67yM0Mu/s7Co6LjR0Piw7vmpnN6op9vT56S20YrY0EsE
g7qh49bQ9v7d2t/2zv3ByfyO3fjasv6p4s2JmeC/uJyUrKOGq7qT7Lq5ptDVutaRtMPv1sG2teGV
94ux0tGLg9O2kqOi2aD9lv+3hoqzxOaWfw=="
    "JwIEhKT6lte8v/2en9PLk86fooThlPmiu9W1r6CM1LvXhfW8p+Xk85LmkPSc1IGhuI7erZzkicnB
l4aM2vLM276B07KKuuXyhuSZ7qXsx7HC2Ka9OycCBIWPs8armKuWx87khcWHzp6Bs4ORzbD73uHr
vo7S056OlpuZ462qzq3EqNWAxbfg9/bXlKTy3Kaom82kvK3H+rDp2euk78mH756Ks7nTrtLdnxEE
mcnS1pzWzbTyj+qPxOyev96Rrc3qxILhnqazpNmivq2wkdqPn9Hb4oDikIqWw+3A79K9ieKi/uHE
voG9j8vz3Ou18L/L6NveyNO7zc7e050e"
    "JwIEhvzG5ujWlqqzh9+azruGiJKvqMWkxbznl//UhZ2G5MLCk6LFgu71g46J44Toz5XOxdXEi6mA
teSqx962nZXDpLDfpILX/ZyaoMqRuILd7bezCycCBIPtkpfxxrKq/NrFosirv+fD5MXmv93548e1
qqGR6calzLr84cCCyJ3a2NWgl7nx+Nquo7jSwfW4o8/nxr3ixuvC0LzGhbXN3tXAkeOegfeVhhEE
gbW0i4mNvcj1o8Ct//GNiPCBxKS5197wu7zljMjJubbf7/X/ufbP0NCiz4Li1tqV7LvC4un13tGG
xpe3nNO+nITMp6WHmvzA4J3rxI25wbmFLA=="
    "JwIEgbessrzH0aablqTP7oHEg7bKxfHZ+P2A+/OFiryl+4iD2ZOU3tLZoMb4utGh+dunjLanvuT2
+tvZtMmtjfa6tfeYpoa18sX03+y2xMuKxJGVAScCBIH8/Y/aiuHy7omVvY2Zh4O6iLbX56GB6pSd
/oyd8rSI4oDryZ601szX8rOcwOjstdL7n72VnYrZn9KrkrXSj8GSvvCt+L38yO7axPj8kaexun0E
iKbx+ZXpkZG5/umE+4Oj6pCYj9bylpyV/oSD1t7D5KuplfyokYqg3rSeqani8bbozNz+8PmfitTM
8ZeJ2M6Dwpzx7LqNqZfvqta/scGDxduKfw=="
    "JwIEheGK89fbksDe1/2woqvrrsj665Kk8rHDj9Xm4K6Qh9n4o7iCoK3Xn6yt/cHp+pzYr8Gq6OPP
/eGh/eDm+7n+moXdx63Zwd2d4N+zjOPkgeeddCcCBIeTkJiI6paSz8WGz8HkmeP3yNKh1KmywrO3
z9b21MTTiM6cgJLiiqur3pTEg8HY54u6wP6o6sPPjduls6KMz82+67zPvcPH8P/g0oOS4czz800E
g5LjqPb85I+h1euxwqLJ7rLWhu6xzqjFtMWX5deW1qap5/PYi5/0rJOPgoemuM7pzdK15frXwObZ
+tj9mN6Y+eW785uyxefWpZf9n/7f17HZHw=="
    "JwIEhv+S0ruC1Jmmo/vR36n1oLXz5/e8mp+y6IaQqdS8uMTao9rsgeP25PbX8az19q6Fp/KE1ZbY
2pnVtujN76XQv8y6/J7YrorXv8SO7O6C/6m5RScCBIWmgPeVjIXWl53yqY3is/T2haL3lsWZy+/3
rZPj1uPm1Lf+x9zWtqSgt8nQ9NTWteLK4I3j7tXewOOmw5W82YLaj+G05M2g8s2wmdXL7qm+s0sE
iN//nc24rvG75f6+/6fQlOH8kcDu7JHK25v2nemJpL/bgPeYlYqb8evBzPji7rD51ZjI2Or4ya3B
lK69+YHysbiK3MORs/b/3+Sl0OHLxLy/Xg=="
    "JwIE5M/kuPfAq4ywovi9t4HmjJqplp39seyK3fb6kcz6k/37qLaf5dfqsbmY8tqhmOy9/JrKzs3L
3sP67q6Pttjq8abaqKq7uN/1meKnsNjBs/UnJwIEiqGeu7mh+/6XvfydgsDYsfiencu8gqLXm++R
i6Cbp5rilsKfzb3+8sqwguq3xMG0n+2fidjik+3S2JGZ//65rreA5dW1mbbCn8KVxp6a+eyrQwSD
p8OmzPzXnvanz9TWrv7glcmO5P+/+pKRzd7B3smwiLyq3Muz6+rVleOnm5PPnP695qWLrbXC8p7Q
1P295drVqOfGs9i0jumXz8Gkibqb05sD"))

(LIST (LIST (big32 #x027A08EB #xFCB5076D #x8A22F4A8 #xE255A364
                     #x6ACEE1F1 #xE99BC8EC #x26520528 #xF1F3BEF5
                     #x865FAFC4 #x1246AB3D #x6EE02FC1 #xA430B083
                     #xCFA4E6B4 #xF8748983 #xE4D0D3CE #xD9C83E1B
                     #x7841D466 #xCE99000B )
              (LIST (big32 #x04631F69 #x47469F6C #x62469A2C #x3AF58B88
                           #x3FAE221F #xAEF36408 #x3E7125FD #x678C38EA
                           #xEE46563F #x485A3C84 #xD046E92D #xCF82F938
                           #xFFEFB57F #x17DD7840 #xA1DC5099 #x42438AEE
                           #xD6220302 #x1C2C1AE4 )
                    (big32 #x06C63C1E #x3D507241 #x9FD7AD44 #x3EFEF1E8
                           #x03EA0301 #x65BD1626 #x8207274B #x01767984
                           #x0A8AE111 #xAF8DE517 #xC8FDCA0F #x20B01A38
                           #x544753F6 #xDA7B58CF #x259BFC91 #x2267C767
                           #xD97364AB #xEAA8F9CE )))
        (LIST (big32 #x0239355A #xF595DB24 #xE367A550 #x717E521B
                     #x17F14AA0 #xA0E0125C #xEF0D22B5 #xDC33807F
                     #x4D7745FD #xABF1092A #xA81E6625 #xFE32CB88
                     #x29BD6C94 #x22F0FB44 #xC3E17FBF #x978E1D0F
                     #x3D19BE74 #x737C81FE )
              (LIST (big32 #x065993E6 #x09C78A20 #xF6A3C08F #x7715CB8B
                           #x3B7DD889 #x0B4AD13E #xFFC02B65 #x4196E3BB
                           #xE59C19B4 #x4922E3DC #xF66F0592 #xD8388574
                           #x595D5820 #x03591858 #x00CB3825 #xADFB73A1
                           #x3E655633 #x4AE4DE74 )
                    (big32 #x06DD8D25 #xD1268C89 #x7DDC0693 #x3435D6D7
                           #xC02D0A71 #x19ED28F3 #x72ED65F8 #x918CFE90
                           #x1934BA51 #x4CCBBE5D #x2F0551F3 #xE0576C2C
                           #x1128EF59 #x56B31BB1 #xB54E822E #xBDCF13A2
                           #x870851C5 #x02B2C357 )))
        (LIST (big32 #x01A8297C #x0C10408D #x5A662E96 #xC8AA435F
                     #x5DD94A7E #x4E2330E9 #x50240629 #x9A307964
                     #xEE18C86B #xA4611B34 #x587676A4 #x958A1918
                     #xBBE0E36E #x1F81C3C9 #x7E68CADB #x7D0D2D71
                     #xCD8F5291 #x9D859FF7 )
              (LIST (big32 #x0246E16B #x2A3BB537 #xC7E0B05B #x9DA14C11
                           #x5C99D02B #x13480670 #x7BE11EA9 #x457703A3
                           #x6E9272F3 #x6BAB01D8 #xE002CBA3 #x6009654A
                           #x9ADEE042 #x738106CF #xAEA2BA88 #x360D2FCF
                           #xE40B506B #xF8057975 )
                    (big32 #x0358E048 #x9085FC8A #x1EFB5451 #x206DE25F
                           #x5B26B8EE #xC48AE639 #x148E7169 #xD58E7DBB
                           #x1BE926F3 #xD8326C96 #xD5FA48F3 #xB6B78677
                           #x4875D462 #xB0FE7D9A #x32AD18AC #x5B86B86B
                           #x96790B3F #x18564940 )))
        (LIST (big32 #x011753AA #x3BE08698 #xAC038DF2 #x2759D8FA
                     #xF9EA4394 #x939F077D #x29437576 #x70B241B7
                     #x5A22571E #x717C2146 #x88A3C97D #x8E55496F
                     #xB72B2EEE #xCD290F21 #xF29BE003 #xAF1731C1
                     #x735708AE #x9FA247BC )
              (LIST (big32 #x046D736B #x8DF062F5 #x43D6FA4C #x056E36E6
                           #x7FBE557E #xBBC02486 #x419E436C #x35A0CFA5
                           #xD71E6FD4 #xA3E6DDF7 #xCA7854D6 #xD6F06509
                           #x6FD9B051 #xA1C51A1E #x186EF2A4 #xE5E509ED
                           #xD3CE91B5 #x1156284B )
                    (big32 #x01BA438E #xB50EDFAE #xDABFDA77 #xD8327E0E
                           #xBBE2D32F #xCA714D12 #x6703F707 #x0A2C4619
                           #x5BA27B1D #x394D42AB #xAAC45A43 #xDF5A0B66
                           #xB84AF716 #xC6951160 #xE9B6248D #x15941F4B
                           #x7F6E1853 #x38998B7F )))
        (LIST (big32 #x0224F45A #xBBC7FF4F #x1FA72C9C #xE3E88261
                     #x29E513BA #xAD57A019 #x51DD70BD #x5E27CB93
                     #x992CC43A #x1CA8050B #x81D7969C #xC8264C12
                     #xE1865AE5 #x32DBE034 #xD90A7597 #x906C8677
                     #x25D91D8C #x2B099EBB )
              (LIST (big32 #x028F6719 #x598565A3 #xCEC81628 #x79C780B3
                           #x06466B0F #x77B0EB7C #x3A9533C3 #x8B1B338D
                           #x6AA9CB62 #x28AA022B #x7C1DFB57 #x289395C4
                           #xCA0DCD48 #xF16C7F4C #x34D9D693 #x7C90FBCF
                           #x0A66E69A #xEA574F91 )
                    (big32 #x0019934A #xB1CAD35A #x721FA87C #x4D879FDE
                           #x22B66EA8 #x80B09E4C #xCD25944F #x96B02368
                           #x79FA36F1 #x00C44051 #x687B606F #xA4F44E24
                           #x5FB0C47C #x05E8F97C #xEE6B6BC1 #xFCBD16EF
                           #x48A6EE6C #xEBD4CE9E )))
        (LIST (big32 #x037C8D9B #x4562CA99 #x87BE6A73 #xB0C2092F
                     #x51152457 #x99CBFFA8 #x14E86C90 #xA1134514
                     #x16EEA0C7 #x09C61344 #xF2B3A2D5 #x882D4806
                     #xB91547BC #xD8E95869 #x185F480A #xBFD38690
                     #x4A22E015 #xDDADD98B )
              (LIST (big32 #x01ED245F #x8C664ABE #x5A8A8A42 #xB7F9E1E4
                           #x8B99FDDF #x38E3B554 #x848E98C9 #x663AF986
                           #x0029076D #x58AA80BB #x9E3E2D2E #x46E2941E
                           #xAE11CFCF #x19EE28DA #xE1507918 #x2B59B7AA
                           #xC0238CF0 #x1EE54311 )
                    (big32 #x00B5682C #x48D7B23A #xA380B7FF #x11A23801
                           #x8891CD7B #xDC1DBCCA #x3244972D #xAFEFEBFD
                           #xCF69F428 #x229E0B15 #x6B45763B #x858B4F5B
                           #xD443462E #xDCE537C7 #x024C4E94 #x39AF9030
                           #x1DD7106B #x982E42AC )))
        (LIST (big32 #x00B758C9 #xE47A298D #x96493F70 #x1880DB4A
                     #x8BC6CF8F #xA03DF30A #x29E25F62 #x01D92652
                     #xF52B2823 #x7875450F #x9B69C636 #x4EFB276F
                     #x56ECB492 #xB46F674D #x7B984C19 #xAF28BD2F
                     #xEC6D1258 #xA8844A81 )
              (LIST (big32 #x00FCFA3E #xD0AC3CB7 #x092AF469 #x90E0DD08
                           #x6D5F3A10 #x3A8A1DFC #x30EF2682 #x3100D724
                           #xF34AD32B #xF2667206 #x8D8D697B #x3EF4A9D1
                           #x564FD256 #x49AD21F0 #x493EE0B7 #xC3DF9237
                           #x5A89E3E1 #x14EC5D7D )
                    (big32 #x0426E3E4 #xAE92245C #xFED213D8 #x347A8818
                           #x1F5B9163 #x857F0407 #x5AF43C8A #xD495F8A0
                           #x88A4179A #x1E52A717 #x16DA265C #xFDC3C9F1
                           #x5526712E #x26C4E070 #x8E71D8E8 #x6A92FBD5
                           #x567EC608 #x38B6C57F )))
        (LIST (big32 #x02E115CE #xBDB2502F #x57FAC112 #xBD6BA47A
                     #xD6492726 #x30C7D5CD #x817100F6 #x7C237009
                     #x02DAE7D6 #x2DFB074F #xA39617C1 #x55A31CFF
                     #xB850FDC1 #x9BDB9FC6 #x82DD8EB6 #xCC1BA770
                     #x5F66331E #x4039CEF4 )
              (LIST (big32 #x03932060 #x46A2C4A7 #xC50D3E0E #x4338FBC8
                           #xA486A296 #x5099B79F #x5BB54894 #xC44E3800
                           #x96214AD5 #xDE29101C #x1B19C5BA #x81F946A8
                           #x73C6DB4A #xCD10C9F3 #x5F6B793D #xEC38FC3F
                           #xE0A40C96 #x199CF9CD )
                    (big32 #x0192C6A3 #xB7CC83D0 #xD5D6C612 #x293B9956
                           #x0DB98CE5 #x115A452F #x96B96AC9 #x94E7E760
                           #x59FE8B09 #x8F041D33 #x89DA66D2 #x6B97D578
                           #x19ACFAB1 #xF4C5E31E #x72BBE66D #x945CF592
                           #x97FA7FF5 #xFAEC6C9F )))
        (LIST (big32 #x037F2549 #xD82A8653 #x23F746FA #x9EA81AF3
                     #xCFDDE1A3 #xECB40620 #xA6A3C711 #x2D23B5B0
                     #x0E3ED93B #x57E2B3AF #x65C153F2 #x0954B58B
                     #x466AB6D1 #x377A5A0F #xE63AF87A #xC2E155DF
                     #xC41DB370 #x2FEA5CC5 )
              (LIST (big32 #x02A601DC #xA8C0B58B #x9DE4A46E #x267D3B05
                           #x45DCB453 #x32F7F75A #x4F1D6C79 #xAA37FD1E
                           #xE566C910 #x379343A5 #x4ACD714A #xC0371EEA
                           #xB7A0634D #x0CABCB20 #xAD0FC2D3 #x24D41CA6
                           #xB033565E #xE52F99CB )
                    (big32 #x045FFE76 #x6B85DC5D #xE5FCFBFA #x7A0530FC
                           #x230376C2 #x32AD9BEC #x7748948F #xED80EE60
                           #xA8A37C75 #xC199E316 #xE61E6A98 #x91635789
                           #x2B60945C #xF7C81E4C #x5C0AB90C #x8B3EDFEF
                           #xE44B430C #xB88F1FDE )))
        (LIST (big32 #x00649F91 #xC7780AC6 #x3045E1EB #x7039861A
                     #x5258EFD6 #x3B055DED #xE88CCF44 #xFEFB50D8
                     #xFE5AFA98 #xB931CAD2 #x131B1EFC #x352A74D9
                     #x77A1FADC #xB87B6B1A #xB8A6B4A1 #x53B717FA
                     #x99C49D85 #x882CFAA7 )
              (LIST (big32 #x05213CED #xCA1F7F8B #xBDF87414 #x0B0C7C1E
                           #x3B2DE024 #x55CDEF22 #x2D01B4E6 #xB116847E
                           #x6BDFDCA5 #x3005A9BC #x482D0FED #x3E26C622
                           #x7B695822 #x67FFE72B #x9B80CB55 #xA996D08F
                           #xC22B18F1 #xAF3B15C3 )
                    (big32 #x01A7869A #x67CAE7BB #x279F52B2 #xEFD80AC9
                           #x1D93FBFF #x4488CDBD #x06F49602 #x1E2AB92D
                           #x9EBD554A #xE34E6C9C #xF39F9EE6 #x4A2D6B58
                           #x5C8F50A9 #xF5EE5B55 #x54678CCE #xC341DA4B
                           #xCF82904B #xA374CD83 )))))

(defvar *qn-shares*
  #|
  (encode-shares
   (generate-shares 10
                    (ecc-mul *acudora-trade-secrets-key* (ecc-random-key))
                    1))
  |#
'("JwIEgaTdyM+Xr+WG3aSst6Tk6qTt5uWYhr/olZb7iJyL9qXlvfLPlIONj/yz6bv84+ailP6O++G4
k9bI+7vU1dXbvbCkvurd+LuzpuO94838jKjGMycCBIqaw9WwuI3Vus3WgceituDbh5OQ26KA+pPi
7LH1y4uE0LmjqqaezPGN0J6Z5YSAnoTJyuLh7d6E5cbytqfCu9jk15+H8cuo55Xp2an39eLarlAE
jou6osvY7Kmb3I366YqXxpPI5JjV4Ky7sKL9hLWj6pv2uYTYoKXC9aXY5/6QpPHb/7nqx6nJwtrq
+v/97bv3yaGw28T7opbtscau7ouFlOejUQ=="
 "JwIEgviDmu/XqfnsxaSfpaDd+vD5lN/c2rCpgLHHwrfsquSQ2JTvtJaIue2wsbanzYKxq5jxmu3S
6Mu749bX0JqqhOGZjuiP1vnnzNDp0Yu2yvOuPicCBIrczuiBtNKur6yc8tCLjo+OtPSz9ZO/vOK4
str/6MnS4Jif5qq+/f3t6IKw//GH9e/PjOPdytOHub+4+rvMnNiNjcuq+/rQntySg4fu07LFzn8E
g+TXt7XIz//urPaHvMOypPDh58XxjdH97pqKovabj/Ki+KSjwIjO26eYyJfDxJu27IDx6+SM9fOw
4viTk+n6ltvqgPa16+6r+/zbipqh1LyMDw=="
 "JwIEg+3Ivcb3ofq839euxvHg+5nBxI/qp8Wzvbye4IL+5N3Wu5GM45jc34T98aOw6J7N85vonZvd
7PbJ4ovl8+vv2JHR8Kn777XjqtjBm8Oq3f+OHScCBIbLh4TNlt+Lk+Su6MKVmIOg49DAiNL+rILm
j8yqgO+tjtTbzbzYqfb23dzklPLxkIDKmL641+CT2ta++KOO0erXsIHMja7Bnsng9J+IjNyBujUE
juLprOqnwIXGsuzHtcOr87nQs76mjs6F+s2Fy5fllZXRmZj935Sf/rj696vNnsb82YaRjtXmmtbD
pcyXmseox6DUjJnW1PH6pdnDiOnVg7jKXQ=="
 "JwIEh8CwhquRpbThz+j5tNSfs6WwxrHD3Iqi4cqzrZDsh5TWvaW/kP/kq734mLP8hsDc3d6sjMuZ
ybfSs8Lw3/3t7/jTyK6PnJio04ON/6DAhMC6aScCBI+TooSgrP6A9qLgh/Pt6MyzlPCo5Nas5bHF
rNusj6SVw9qau4nOmbuphMufpNO6w5Hsx/eO8JKiofSCtKjE3vj9sNeL0ZXTwemMoPuBlvX+lk0E
ge+au9PT49Ld94X84a2O/MqTuaT3/LDwo5y79JqAjbbsre+s5pjr6a/MjZL/r+agmr+9pfqU4en/
8dij1eHSkYKv7O6N6YW9zZWk47uywJ+CZA=="
 "JwIEguH5o+aO98qjm+nq2LrMsLuX1KTpt+rb0aDjkMLf38GJs/DLjaDEyJKD0PfArebmg7iGl/Oc
9d7VgISj2OD/t6fczYj7uIjkmc7kjPP/j4yZHCcCBImVwrW1oLO3ysLc+ILoxfTG+dzjoI/4toLD
se2f/73+jPKTs46Jls/7iY+IyMi7g7LQloOe1LyWqcKBrJS8qobNnYul5/aoxoDO+YPvqcG7sB0E
i+er3OOn1O3vq/P3gLC537+Hq/6qqaivhpfzuduflOqX7eOboN+0/q6s84eCoIbQooz529fbyrDd
kve7oZDX85zgv/b0jNfrw9mw7qLOzJe2RA=="
 "JwIEgpuu0YOB6smXiKb0wPy81bjM+saZv43NjfC2u+aD/LvR5oHA28GUr9OEvprQxezIxqrNl/Cd
le3CjuPg04ev4dPfzOGtpsyA08W0+4Piv6ngKycCBImp697V2O3U7sij6ZqFpuHqsIOckd6y5aTR
3aDWkJX/g+Xs69CwuI3F5qr04rvMwNmnxt/pjpLJmL/CurD57snS0vC0mb+Jt4mfqaGa2bbPvEkE
g7e6kOWx7PHz0Iv+pYOv6eD43ZW2ztOtkYfvvtDF+r3l4fO41rLntqyH64O+0O6MyNzA8tn2o8qL
5NvExrmrwNGy/qHh9vemg4rJp9bAo/25eA=="
 "JwIEgvn9x5u9hbjPmPq7v8es6N2IrqDigsTwkrfIxaiiioSpxoD5wcfZrK3qxdL9s6qVo5GlgdSA
wa+p2N6+nZK9u/D0venbkfbHyrWp8/2P+Z+1bScCBIz0mOWC96+oxraX4JrdvofistC9g8Lx8p6c
lP7yi9G06pPfmrnJ5sadl4K8sbXuiay1/ImDyM687sau0YKN/r/x/Mej45iknt257ZKfzezeqG4E
jvmHsdHZ0ezGgPS6/MKj99CNz8qX/rbS/O+YqoSc0pzGmOXB3O/+tbPXhprV1qSap9CR7ueN/Ijk
hM7fsMvA/rqC4czYi632gIDN05ycqvLfTA=="
 "JwIEga+gpNmh5sLgyPWquuSevarBpNX/+IWgjrSk1PPBkZ+zxce1tZfGprSNvK37tLaE8eui4arL
3dGS3pq+nfuvmZzvre++wN+RpIL89bKiteK0UScCBIOzhuuRt4Wo9t2lyMmxqOv9vpr/gNf+paLn
oJXWs8uS2ZPHtuCwv7K25t360q3Qk6m4iNKer/+et7yL7/WJmIuvhOa24pWN+a6QyLKC44jSilgE
hqi6rKL86v2K3O2ajtjUgKyf7p/g8eeLqouL/M+Rxbrd3Ii+2srU0PzggJC4zITmhL+H1paInKaH
+/qOi7iPotvu0Iis3ZTynarKv6ju3+24LA=="
 "JwIEhazkiJ3moImZi6fUp9HU/ta4/PGcuYycwa6lqu75o6+vg6D+j6So9NDgu6upx6yfvNH46tnv
nqPA0d7ru5niv4Oi4siQrfzV1LqI3Iy/2IewcycCBI6nvZPy7syczsHqn6fehMyj6veKk+3tjtXu
5pyIs+G+ss+E+NHp05CFsJCgienwgeiDyrrG57eF5/2DkpDQ4eeX+r2TvOrf8eLRoKrh9c+JmyUE
grGq8+LhrpW0he70rOP+7oOTmpbg3cmQ/9LHw+jI18GIyP32v+iMqOeIj5mtlefv6922oazAkuPF
pa+2tt/ix/2tzbiauKrl6Ij68PWHtKv8BA=="
 "JwIEhdGVtJicquLvld+q2OO46qDCo/qslMj8wruHiPbZnuSH6K63vonXvcqrucO6qd+q6ujwjO+e
xqCMsbH6ubPe/qOft+7UhtOp/vqC+abBsb2JficCBISaid3zi7yfn9H19tf6h/jlspSE89r62f+w
yMnl3IT65JTUpJncs8iy/JvijM/20IfhmMDSx+eR7cmMvobU9sjbif/G+ZD5vZnZqM+ixpjcgD8E
kezypoqKlvO/uvrB9K6xlK3Jnr2ZpPLWroT8zsXAz/T+p9Pov6vYxdqT96nxwKrh6/Cdp8HdyeTm
yv2pxsWLkdKQl7mkmK3R/Yj11tOGne8H"))

(LIST (LIST (big32 #x00A4BB22 #x7975F943 #x5D48B1BA #x4C9A926D
                     #xCD94C067 #xFA0A96F6 #x20E0BEC9 #x72BDE53C
                     #xA031A3FE #x33D2EFE6 #x3CC88A7E #x1DEF0B82
                     #x75A47B77 #x52AD5B6F #x58247DAA #xEF876CD3
                     #x637B8E6F #xC18A2333 )
              (LIST (big32 #x051A8755 #x8381B55D #x4DAC063A #x26D82D87
                           #x2642DA20 #x1E89E2D8 #xC7ACB161 #x283946A9
                           #x31E99C46 #xD03C6728 #x40078249 #x958B0EDB
                           #xC132C6E4 #xD93C2776 #x32573E1F #x8CB519CA
                           #xE9B2A7BF #x5C569750 )
                    (big32 #x070B748A #x5D8D8A4D #xDC1BEB48 #xA2F189C8
                           #xC862AE05 #x8ED822FA #x11AA3D46 #xFB390961
                           #x02585D52 #xD8CFF882 #x4E36FFB9 #xD51D4C98
                           #x56B57AFF #xF76BBEF2 #x50B0B713 #xDA22DB58
                           #xC65DB858 #x5299D1D1 )))
        (LIST (big32 #x0178066B #x7D753E76 #x45487D2A #x0BBEB879
                     #x297EE5A6 #x0A40318F #x09BEC559 #x085829BD
                     #xA1610E76 #xB062D93C #xD04C5598 #xE26B6D2D
                     #x12DDE3AD #x5E81A541 #x30991DA0 #x7D6F39E6
                     #x50D3445B #x695CD73E )
              (LIST (big32 #x055C9DA0 #x0B4A4B97 #xAC39CA80 #xB1C3C734
                           #xE8CFA937 #xEF313865 #x6BFE8934 #xB0183F99
                           #x53EFBF76 #xE804C3FF #x10FD77CF #x198EECAA
                           #x61DCBF71 #xE9DCC396 #x068D96AB #xDFAA07AE
                           #x12061F75 #x3651677F )
                    (big32 #x01E4AEDD #xAC89FFF7 #x2CEC1DE4 #x36493861
                           #xCF1788DA #x3F771A14 #x8BB1B1FC #x9178488E
                           #x0089D6D3 #x98905E1C #x436DB600 #xE3AF20CE
                           #xBCD862F0 #x4C9E9F45 #xADEA01D9 #xAEBDCAFD
                           #xFCB628D2 #x1A8F060F )))
        (LIST (big32 #x01ED90F6 #x37743E9E #x5FAEBA37 #x1C1ECCC1
                     #x883F5278 #xACDEBC3D #x8017EC97 #x6B3B2233
                     #x198B97C2 #x7DE28D86 #x83D3799B #xD074DDDD
                     #x9DA4E217 #x979EBDF6 #x08D1E0A7 #xDEF6B8D5
                     #x58826E1A #xABBFC71D )
              (LIST (big32 #x034B0E12 #x696BE2C9 #xE45DA211 #x5300D063
                           #xA100452F #xCB01661F #x31500DEB #x4754B735
                           #xE5853DBB #x5DB990A7 #x2E24004A #x30F9C57C
                           #x04ED567D #xE118EA3A #xABB00330 #x6AE827A4
                           #xE0E87C40 #xCB805D35 )
                    (big32 #x0762D2B3 #x52780163 #x32D91DAC #x357CDCD0
                           #x66F930E9 #xC17D4D0B #x2CBE52A5 #x689931F6
                           #xF943FF9C #x7AEEAE69 #xE8DF2C86 #x223AAE63
                           #x55A1A598 #x5CD47511 #xD0541866 #xB54E3E92
                           #xD986234D #x506E255D )))
        (LIST (big32 #x03C06019 #x5914AD30 #xCFD1E5A5 #x43ECD2B0
                     #x8CC61DC1 #x48B0CA66 #xB486C0E5 #x2B3D4AFC
                     #x87FC8ADE #xF830CFE0 #x68172EDE #x58325999
                     #x2DE93385 #xC2FFDDBB #xFC5390B8 #x79C30A29
                     #x831BFD04 #x00901D69 )
              (LIST (big32 #x07934411 #x02CFC03B #x22C01F9E #xDD131994
                           #xE0A32565 #x9958C559 #x6D60F485 #x61DA34EC
                           #x4CE32ED4 #x84967D25 #x3750C8EC #x8FDC7702
                           #x4890F404 #xD1444BDE #x3EB0AE2E #x895A7074
                           #x8C41EC09 #x6EBF8B4D )
                    (big32 #x00EF34EE #x9D3C74AE #xF70BF30A #xD1DF2513
                           #x7293BFC6 #x1C119C77 #xD0D001AD #xB62DDEB3
                           #x318D7A57 #xCC1A4BFA #xFCC80D3F #x7A97D14C
                           #x3A7FF1B0 #x8EAE1A44 #x412FD9B8 #x6E90AF66
                           #x95498DDB #x2807C164 )))
        (LIST (big32 #x0161F28F #x30EEF291 #x9BD3AAC3 #xA98C1D97
                     #xA8934B7D #x56E8A0C6 #x4215FBF0 #x44B3E12C
                     #x6A089209 #x03A1DE02 #xDCD981B8 #x0C5F99CE
                     #xB7AA8008 #x8EC60FED #xD3DC9A23 #xDB81190C
                     #xCEC8339F #xF1E30C9C )
              (LIST (big32 #x049584D5 #xAA066DE5 #x42B9E016 #x88BD2379
                           #xB98D00FF #x0D814363 #xB4FFF7BF #x867226CC
                           #x7092D3FD #x891E2244 #x8760D950 #x2C0CF547
                           #x8594C202 #xB0A3C541 #xA69D1697 #x3F651180
                           #x4EF20F7A #x982ED81D )
                    (big32 #x05E75773 #x1A7A9B77 #xABE7DC03 #x0737DF87
                           #x57F95295 #x0BC317E6 #xE6D9F29A #x8BEDC66D
                           #x05F69F97 #x2CE61C12 #x00D4110C #xF36EBDB9
                           #x4C2E92EE #xED090AFC #xCE607FDB #xA0CAFAE1
                           #xD961B914 #xE985DB44 )))
        (LIST (big32 #x011B5D44 #x181D524B #x884DD207 #xC7955C4C
                     #xF518CBF1 #xB346F06C #xEF303F8E #xE8E60302
                     #xDC128BE9 #x847C6A84 #x5D92232A #x9A5F81D2
                     #xBB610EC7 #x829875F8 #x69DF9985 #x6A698029
                     #xC569EC1E #x27EA702B )
              (LIST (big32 #x04A9D77A #xAD8DB537 #x4847A4D0 #x54D87530
                           #x06708DE6 #x595251BA #x82B102BF #xC1E5D9AE
                           #x83070362 #xE655D313 #xB9902CA7 #x8D7F48E2
                           #x524C3F84 #xE9879DD2 #x6952E0D0 #xCBF12DC4
                           #x9F5284D5 #x96D3DE49 )
                    (big32 #x01B77443 #x2B1D9C79 #xD017F928 #x35FA7078
                           #xBA55B4EA #x6B4887DE #xFA845F4F #x72E1E6E2
                           #xB32CED96 #x07D60DF5 #x0DC3245C #x81CACF64
                           #x7285E4B7 #x12339570 #x28B2FC87 #x0F6EE981
                           #x8A929EB4 #x047F5CF8 )))
        (LIST (big32 #x0179FB1C #xDBD0AE27 #x98F4EDFC #x759A2E88
                     #x5C831028 #x9C093791 #x15422141 #x14C601E6
                     #x0C7B2B16 #xEA8B4BEB #x35455191 #x4A06A008
                     #x2BD4D8BC #xF8E927AE #xF8747BA6 #xD91ED1E5
                     #x3553CFE8 #xFF27DAED )
              (LIST (big32 #x06743194 #x1775EA23 #x362F80D5 #xD7C1F132
                           #xA0F41C2E #x3C8F1C29 #xFB90BA2D #x3513BE69
                           #xCC9CD18E #x9704F18B #x5DC25635 #xF8241C89
                           #xCF37465D #x4410DFCF #xF8FC8E8F #x198487AE
                           #xB9DA48FC #xDD97946E )
                    (big32 #x07790EC6 #x8D9A3B23 #x00E8EBE4 #x247DE80D
                           #x9F28BFE6 #xD4BE6F30 #xA821CA47 #x2318CB06
                           #xE6FFCD59 #xD70C6AAD #x648693D0 #x23BB38DF
                           #x8232049D #x7D84B81F #x9D02C332 #xC0B5BD80
                           #x009B4CE1 #xC55CAFCC )))
        (LIST (big32 #x00AF4092 #xCA1CD0B0 #x48EAA9D6 #x43CF5541
                     #x4957FF80 #xA8073449 #x539C1227 #xD9C58ED5
                     #xA978C99A #x0D78B7DB #x46C138EB #x458554BB
                     #xB4495E34 #xF8EFB5E6 #x4E6F5BBD #xF40BE452
                     #x02F9D592 #x26B89A51 )
              (LIST (big32 #x01B30DAC #x8B70AA3B #x5D4B224B #x151AFEBE
                           #x35FC057F #xC9516740 #x56B33964 #xAC938EDB
                           #x0307EC9B #x66BBEA92 #xDA04D4B8 #x1148F2FF
                           #xE79BBC17 #xBFA89302 #xD784CCDB #x1151BE57
                           #x1090C816 #x31148558 )
                    (big32 #x032874B1 #x17CD5F45 #x5CDA6875 #x8A80161F
                           #xDC7F071C #xE2D50B17 #xF27918AE #xAEDC10FA
                           #xD4AA943E #x600041C4 #xC099823F #x0F58B083
                           #x8983FBF4 #x385B81E8 #xADEEA021 #x65D29C8E
                           #xAA94FD46 #xEBFB5C2C )))
        (LIST (big32 #x02ACC820 #xEE64024C #x8B4F513D #x1A9FAB38
                     #xF9C4E391 #x8720AE4A #xAB77946B #xD78341F8
                     #x7A451D28 #x6076AD4C #x7587DE51 #xF1AACEF3
                     #xC8E051BD #xADD99C4F #xC1A2C520 #x82DF956A
                     #x3A117063 #xFB01D873 )
              (LIST (big32 #x07277A4F #x96E98727 #x41D47D3D #xE09311EA
                           #xEE289EDD #xA3AAEECC #x70433C2F #x994F09E2
                           #x8E9A6402 #xB020804E #x9E007403 #x94EA3676
                           #xE173FD06 #x48850C39 #xCBFA7A4D #xE6ABFC71
                           #x5140AB0F #x59E24DA5 )
                    (big32 #x013155CF #x1615C55A #x05DDD166 #x3FDB8193
                           #x345B05D9 #x243FD28F #x0F448AF0 #x4448FBD9
                           #xFE818A33 #x881E6569 #x5CFBF5DD #x6C856402
                           #x58E2A55E #xD9B5FC51 #xFEAD9AE0 #xD3855974
                           #x08F5C3A8 #x768AFE04 )))
        (LIST (big32 #x02D12AD0 #xC1C558B7 #x95BEAAC6 #x371A9042
                     #x47E96149 #x1F213B0E #x23B593D9 #x03E85CDD
                     #xF09AEF65 #x2B730DD2 #x9BEAB568 #xE03379E8
                     #xC8063163 #xE9CB3BDF #x919F6FBA #xA06A6A7F
                     #x7A05E534 #x162F44FE )
              (LIST (big32 #x021A1377 #x98B787CF #xD1EBDABF #xA0FE32B2
                           #x28139DAF #x567FB091 #x272DC09E #xB214A890
                           #xCDC67219 #x7C378864 #xFED403E1 #x3102947C
                           #xE476C918 #xF8354ED2 #x2D89FF1B #xC90F2F4C
                           #xD9513D14 #x6317003F )
                    (big32 #x0011D9C9 #x30A145B9 #xBF75EA0F #x45CC4A2D
                           #x9279E994 #x9CAB2E09 #xF2745813 #xFA7E4F4F
                           #x43F57622 #xDA27DD4F #x180AB0EB #xE0753C1B
                           #xB2726695 #xF54C68A2 #xC8D2205D #xCA430B68
                           #xFD11D6B5 #x30C77787 )))))
|#
;; -----------------------------------------------------------------------
#|
(defvar *offset-point* ;; private
  ;; as of 11/24/11 DM
  (LIST (big32 #x0664104B #x8A0EBB2F #xF8158B5C #x53926009
               #xB168D994 #xD0B12289 #xCC75CD7C #x27DAC5FE
               #x06154E8E #x5E19E1EC #x12A904D3 #xEB4EB5AB
               #x3301AD33 #x27565DF9 #x47AB24CB #xFBC2E1A0
               #x74E47B2C #x5A4E452A )
        (big32 #x068D6880 #x4B7BD5AB #x2217734F #x5A9A1409
               #xF40E8A7C #x1FE467AE #xEDBE4122 #x83A689CD
               #x6C0D1CEB #x1FCAB01A #x26ECA5EB #xCC6D7600
               #x8190B9D0 #x4DB5307D #xC3EF4A85 #x09417177
               #x9B4A39A5 #xD45107E4 )))
(validate-public-key *offset-point*)
|#
#|
(let* ((hb   (solve-lagrange (um:take 2 *hb-shares*)))
       (pe   (solve-lagrange (um:take 2 *pe-shares*)))
       (qn   (solve-lagrange (um:take 2 *qn-shares*)))
       (shares (list
                (list (ecc-pt-x pe) hb)
                (list (ecc-pt-x qn) pe)
                (list (ecc-pt-x hb) qn))))
  (force-outcome *acudora-trade-secrets-key* shares))

(defvar *acudora-trade-secrets-x*
  (big32 #x03252652 #x43C29E8E #x349C65EC #x709A9D8D
         #x655196EF #x43AC766B #xCE98D5A9 #xF3EA7FCF
         #x42B29F18 #xB23F3E77 #x38FDED55 #x244DC289
         #x2A80D243 #x463759A7 #x22D17A28 #x9D1670F1
         #x6D25ADB0 #x6A7DC670 )
  ;; (ctr-drbg-int *nbits*)
  )

(defvar *acudora-trade-secrets-y*
  (LIST (big32 #x05AE486B #xC5F37A7E #x6E987C6F #x49828EC1
               #xF76DB57D #x79DCD487 #x936C45B6 #x9DA0FF36
               #x559C2923 #x516A7A2E #x8AB78E39 #x12161525
               #x07C848AA #xE147C0B4 #x466D3E73 #x7ADBDD65
               #x63C38971 #xC4C03A42 )
        (big32 #x05321663 #x9ADDC020 #x5D550846 #xD641A486
               #x1F86FEAD #x88630F21 #x533C6D82 #x45770A05
               #x826E5C0A #x4239A91B #xDD35CB43 #xE8F12E8F
               #xFFCDF4A1 #xA3C1D718 #x61F0B158 #x9E4FDDE2
               #xB6C1F117 #xF39AAE5F ))
  #|
  (let* ((hb   (solve-lagrange (decode-shares (um:take 2 *hb-shares*))))
         (pe   (solve-lagrange (decode-shares (um:take 2 *pe-shares*))))
         (qn   (solve-lagrange (decode-shares (um:take 2 *qn-shares*))))
         (shares (list
                  (list 0        *acudora-trade-secrets-key*)
                  (list (ecc-pt-x pe) hb)
                  (list (ecc-pt-x qn) pe)
                  (list (ecc-pt-x hb) qn))))
    (solve-lagrange shares *acudora-trade-secrets-x*))
  |#
  )
(validate-public-key *acudora-trade-secrets-y*)
|#

;; --------------------------------------------------------------------

(defvar *acudora-trade-secrets-share*
  "JwIEhqWTlMi8lPqctM6ZvceE6ruNstSy7vqO2Pa189ONrafn6r/z6KuU/LGyn8/O88f32tWSk7io
yaqB0qHRxvXNnMXRvYqT0bPD4u2S67aG0/eMcCcCBIuupJr435vp/O7Mn430zIqdwfvbttfr57nU
w+TtxK3au6D/zcrZ4aTG0bWexejV3py5iYXC0qifkMjVuKj8hdGM7Z+c763e9crj4eKunKaA9EIE
irKLmPOt7oDA3arCiO2yhsmGj+Hf6uyhxo+Q1OfG7ImK94WBsKby8JTCnOqjvenXlsP0vKXo//+b
9NDo+J244cPw2NaT5P73xbbg/KL/nOrcXw=="
  #|
  (LIST (big32 #x03252652 #x43C29E8E #x349C65EC #x709A9D8D
               #x655196EF #x43AC766B #xCE98D5A9 #xF3EA7FCF
               #x42B29F18 #xB23F3E77 #x38FDED55 #x244DC289
               #x2A80D243 #x463759A7 #x22D17A28 #x9D1670F1
               #x6D25ADB0 #x6A7DC670 )
        (LIST (big32 #x05AE486B #xC5F37A7E #x6E987C6F #x49828EC1
                     #xF76DB57D #x79DCD487 #x936C45B6 #x9DA0FF36
                     #x559C2923 #x516A7A2E #x8AB78E39 #x12161525
                     #x07C848AA #xE147C0B4 #x466D3E73 #x7ADBDD65
                     #x63C38971 #xC4C03A42 )
              (big32 #x05321663 #x9ADDC020 #x5D550846 #xD641A486
                     #x1F86FEAD #x88630F21 #x533C6D82 #x45770A05
                     #x826E5C0A #x4239A91B #xDD35CB43 #xE8F12E8F
                     #xFFCDF4A1 #xA3C1D718 #x61F0B158 #x9E4FDDE2
                     #xB6C1F117 #xF39AAE5F )))
  |#
  #|
  (list *acudora-trade-secrets-x*
        *acudora-trade-secrets-y*)
  |#
  )

;; ----------------------------------------------------------

(defun format-share  (share)
  (destructuring-bind (xmul pt) share
    `(list ,(format-fragments nil xmul)
           ,(format-pt pt))))

(defun format-shares (shares)
  (destructuring-bind (mult &rest shs) shares
    `(list ,(format-fragments nil mult)
           ,@(loop for share in shs collect
                   (format-share share)))))

(defun encode-share (share)
  (encode-bytes-to-base64 (loenc:encode share)))

(defun encode-shares (shares)
  (mapcar #'encode-share shares))

(defun decode-share (share64)
  (loenc:decode (decode-bytes-from-base64 share64)))

(defun decode-shares (shares64)
  (mapcar #'decode-share shares64))

#|
(defun unlock-vault (&key hb pe qn)
  (let ((hb (solve-lagrange (decode-shares hb)))
        (pe (solve-lagrange (decode-shares pe)))
        (qn (solve-lagrange (decode-shares qn))))
    (ecc-add *offset-point*
             (solve-lagrange (list
                              (list (ecc-pt-x pe) hb)
                              (list (ecc-pt-x qn) pe)
                              (list (ecc-pt-x hb) qn)))) ))
|#

(defun unlock-vault (&key hb pe qn)
  (let ((hb (solve-lagrange (decode-shares hb)))
        (pe (solve-lagrange (decode-shares pe)))
        (qn (solve-lagrange (decode-shares qn))))
    (solve-lagrange (list
                     (decode-share *acudora-trade-secrets-share*)
                     (list (ecc-pt-x pe) hb)
                     (list (ecc-pt-x qn) pe)
                     (list (ecc-pt-x hb) qn)))) )

#|
(time 
  (labels ((grab-key-pairs (shares)
             (let* ((keys shares)
                    (nk   (length keys))
                    (key1 (elt keys (my-random nk)))
                    (keys (remove key1 keys))
                    (nk   (decf nk))
                    (key2 (elt keys (my-random nk))))
               (list key1 key2))))
    
    (unless (equalp *acudora-trade-secrets-key*
                    (unlock-vault :hb (grab-key-pairs *hb-shares*)
                                  :pe (grab-key-pairs *pe-shares*)
                                  :qn (grab-key-pairs *qn-shares*)))
      (error "Oops!"))
    t))
|#

(defun add-share (shares)
  ;; given a sufficient number of shares for one shared keying
  ;; generate additional share for same keying
  (let* ((fn (make-lagrange-interpolator shares))
         (x0 (ctr-drbg-int *nbits*)))
    (list x0
          (funcall fn x0))))


(defun add-share-with-key (key shares)
  (let* ((fn (make-lagrange-interpolator shares)))
    (list key
          (funcall fn key))))

(defun add-share-with-plaintext-key (key shares)
  (add-share-with-key (make-key-from-plaintext key) shares))



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

;; ----------------------------------------------------------------------

(defun encode-b64 (share)
  (encode-object-to-base64 share))

(defun decode-b64 (b64-text)
  (decode-object-from-base64 b64-text))

;; ----------------------------------------------------------------------


;; ------------------------------------------------------------

(defun decrypt-escrow-key (key)
  (let* ((enc    (decode-b64
                  ;; from encode-master-message
"F4FIgicuCrDBEeGcVcgqFERup5b2ptYWrYl/392pSxRke6IADKtGyv1jYwAAAAAAAAAAts9v5Avc
Z6oRFdvgm9JUK0emb0goWIgMxt3EOZin1bm1DbU9hjwQIIAkIXFJV+3VBd5IGyrOBaqeprQ2ZkRE
IByLXOs948nkaHoeW3oKbbph5XiS1pcPjE+qSfnyELGSHZiuPV4GzhBdHhfqlWUzKq0aFUgJH0gB
BKGVLArbWEyCAPWTV/XLYwrzqWdFqxcT6aTj/fTXrqg="))
         (len    (length enc)))
    (with-output-to-string (sout)
      (ubstream:with-input-from-ubyte-stream (sin enc)
        (3ctr-hmac-decrypt-stream sin len sout key)))))

;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; GET-ESCROW-KEY
;; This is the main unlocking routine, once we have 3 unlocking keys
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------

(defun get-escrow-key (unlock-key1 unlock-key2 unlock-key3)
  ;; takes 3 unlocking keys in base-64 encoding. Unlocking keys come
  ;; from computing them from pairs of board member keys.
  ;;
  ;; Returns the text describing how to access the encrypted IP.
  ;;
  (handler-case
      (decrypt-escrow-key
       (compute-ip-escrow-key unlock-key1
                              unlock-key2
                              unlock-key3))
    (error ())) )

;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; -------------------------------------------------------------

(defun compute-ip-escrow-key (unlock-key1 unlock-key2 unlock-key3)
  ;; takes 3 unlocking keys in base-64 encoding. Unlocking keys come
  ;; from computing them from pairs of board member keys
  (encode-b64
   (solve-lagrange (list unlock-key1
                         unlock-key2
                         unlock-key3)
                   (decode-b64
                    "BIKev/XCxLGR3ZLdibXFw4mUv8XKsramlsORw8qdrqyN/9bmivXghpTR5sHW9vba0c77hu3l9v6d
ib6jotKP7ZywtOeS5+ao8azt1OWlwb+DyUQ=")) ))

(defun compute-unlock-key (key1 key2 enc)
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
         (sh  (decode-b64 enc))
         (x   (getf sh :x0))
         (y   (solve-lagrange (list k1 k2) x)))
    (loenc:decode
     (3ctr-hmac-decrypt-sequence (getf sh :crypt)
                                 (encode-b64 y)))))
     
;; -----------------------------------------------------------------
;; These are the routines for computing unlocking keys for each of the
;; board members. Each routine requires a pair of keys.

(defun compute-hb-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASJ34T5os+U+LnKz5+l4NLqzNu2yb/Umdyew52d+fqOwIO1pO+TvJm+/O2kudjf8dn4
5syu1Oz7vJb5t5ayiuHZvuH5nN3n3bTMsMb7mv6Pu+URGCAFQ1JZUFQXgwiCJy4KsMER4ZxVyCoU
RG6nN7GxZpm7hQUCVhklo7CsSgAMq0a2REcJAAAAAAAAAAAL7ZsKC1BgIvvleuHMetAXsxLJ28Nw
ey5E8v8NXQvQmxdG8EczBoImVHSc9Aqd63r0j6UmrSS+ecLZuxe7pGuNui1SU+/cA6ORpnYCgWns
i3mFh3pDv5clFhjsRb9lnMJkak8tc3VdoyAp5+tCXejnuv+PHloer5qpb/YAx80h9kkxUPOs82hd
qIQLTUkiiF/BYjl/OF92q6gVH98cXJR/TIpf8r/wYx/vj6uu2APUqoTulmQjAvIPAUDIYAToJbQc
z5+ucU1bxeXW0C2dg56b/SwjQKNbwuHeE/IzeXV03g9xyaMQctIXm/BBqsuoCHGkcYiNy9MILLLz
9S73mloJQ5ScMqWTPAuKHQy8Vy9blzCX9XO/SYdSO4yzq5t2uZk1UL2oXuhaH2BU9JYmizJv+Qm4
uJKpL0kJlzvsRnXqTheXela2HjN2OZJq2hZE2kPYHfK6AKQv/Q=="))
  
(defun compute-dc-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASDo9iAhOTfvpuZnfTAwe7Eh+3Vzqqgj5qeovj10rXO75uFneCs9ozcmvz7g+Odx6zA
udv1m/X3tdfR0uzf777Cyejg4fnZ5+ilr73GgNOqqpE8GCAFQ1JZUFQXgwiCJy4KsMER4ZxVyCoU
RG6nSxje+LDUAce9bN4F62H4hwAMq0a4B4a6AAAAAAAAAABj5gfOVq24OkfsI1ql2cFMwqCFRdFd
DEtblgUM5wu62t71Od6uKUsEytf/GU4AO/1cG/wzz/OGYbyDw9Is5SCx54RjkU1zAbyfiy2+2m0k
YMJgs01vw203CPjDutXkYAGzn08HiBClj2Dc3EiqNccJkqYdswMPO7wkY57VU2oRsBTbfS5aLaGS
ULmj++OAtbYfWnvVKUT3egXPue1rlpLZXrFAvOf/h9eq0HtuQc9J8Db1aQh8D4RjQqDSn3s6cxNX
HKZl22dst+7D2C366TbCc2Mf1X3yxmZq1mzfJNYw0rVzD760MsxYwYXBJzlP5qU80aukSpR30qhn
AJ3Zo91pJo5kyDpttvM5oxeMwjSkivpcNSaiiuy7IrDSzmAj+DzqmlRKo4fsxOhNA0RQG7rLsxKf
q3jR5kQkMNqEsVcwXt6ShqccHH7eG5jfu2WhYvCb0mDblmUEXw=="))

(defun compute-qn-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASIzO6Tw8C/95aHhZnn/Mil5Yb+s9idxL+c3pTG15SKpcbVwJ/vzaaPvt6fkpTyg+eE
ivve7PmDhsSl5Mr76YzgptS/79S1xI2YhqX7jOzhqPEcGCAFQ1JZUFQXgwiCJy4KsMER4ZxVyCoU
RG6nARkKrCgN6eR1g7xrksieewAMq0a5xBRtAAAAAAAAAABgZrgHyh8Qs2cbvDUNKsuXBzPtCnFA
8vIH485O+PYI4ymfML5sl/eux4AyJuPQkcD/Nz7kwa0rE94l4Yt9cwvcV+9L30oO1g5Z/fVLxjDs
T2U9Ju2J/NSifzjEBSWONuUKrMszOpUMtYZTFumyi/XthhFnWAyEKfH5SYboxIupmEtMPOJy5dpR
coG/njYD6sYUUNZbLrPbrPxIwwEKPjLVWx9S0a7dofpHeQRzJfEla319QbE+fb8wUf8OHWVDUK0r
MnSIyBaOHTbkRxjzIA5J+bWLJCr51Jl45NXHphnGzuWVFdbFKgU3bWhsMWfoHFgMPLCuw3kypgf0
BGIUiPr+LmJwjHwFZRvbp9M/bIeXusWAlRtJlv1tAJC6g1AIj7n1Bl9xJ9QrUsoYyJ3PwKzbsXYK
6hn0cx24JKPCOzt9TPgc1U2cqk9oiVEpZIhnN+r1n0XoeNHJWg=="))

(defun compute-pe-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASE+46V8tCmi5nOwZu8kuLDze+vxpacyNXq8oW21q7T0/f88JO1xuW8gb399sacv+Pv
4LKFxrnv+Y+OwuPF1unQi66rrIau8tuaipT4+MeD+KpBGCAFQ1JZUFQXgwiCJy4KsMER4ZxVyCoU
RG6nhMQ55WDyHd3WQ07AApNY3QAMq0a7etZ3AAAAAAAAAABKjhnhwzfe64/8ypEdPc5kwZBSWnk0
qp3VTDG70z0p11biBD6xNeHFu7cwmDo7fe5FnFAHrjSea4faKZuGzl0wDM9uLK+NZKfjAK0Fks2L
j/9p9dV8t8TQMMqJ1tQxWQWrzlJ1TBELG+zYt4hqdu8lpzsQ+C6B4RddzKs252dBmpvapENBXnl5
nAPe377HMzkbNyuojRVRgSR5lSOeR4VmAklpkSNw4oOspO7Lm23yVFdcEBnZfYhR8eGCpz8Xwsle
eF8jiuZ9CzkXcPsrtTLUSSp5tFe3V2HsKrL6F+wyTyG/jdUhQYYjNMi1ev14MYNTVneEb/Ipu5zP
o6cc8Fwl4ABDzozkwLnz+D4Irqwhjoae2TPUaneo2M7MOWqnB8/MNqd6ef2Sx8S4a4on062kGwyz
fi4dhwsRxZjmI2B+crPyr/2Yz1Fw9PpQYP1je9+AAoVhYaH6tw=="))

(defun compute-wm-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASO5fb8rtWJmKf12uiM7MalpoH11amnqtjP+OCDv4q5o8rE6NSFp7qa14yU26DB6JXi
7q+T6OL5t5LW9OrSjuP5wv2c+4PUl6b90p2CuPS6yf8EGCAFQ1JZUFQXgwiCJy4KsMER4ZxVyCoU
RG6nmhbqTTSD18hkxiROnYYQzQAMq0a9BLYoAAAAAAAAAADFJUOnoiB/N37ivw2BQb6GluF2+ri2
8ZFmafqoWpv8a/TsfFl15cFgBECtrJlv/Bw86WY4M/+54U/AH8vZk4x0cl1U5zrAsAtUFvjqgxY2
DaiF5Tt0TSpiGDVKKXnYrBjDdKE/PvVztbge/x61asrRJk0W0Q/znidzCcJq/6iJ2nr2IIsSimlX
imwjeKpmm9ARG34giCGsJYm7R65gdfYieaSwxCNStbqSc3DCF1l1IKl7OjeDJRpTbRRJ2Ao+pHJL
6dusMNTmJ51pS7KYc+lw65zhvlgNUtEuVsZBtJd9PsCPK5M/MrkgbjVyMFXS5INjidYeczcEXVL+
giBDyebwgG7powCHYgpar5690J0xqLsWs4Eld2LK12GfIDNMscer9H1GH/KChEctr2a79p+tXbnE
OKoX2gZNq7dQsbIetMhsLygEo/hMbfzhsQIi6qkZK/BGe6HAtg=="))

(defun compute-bb-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASFjOLpkaGR6M2WwPzon83VldHDpP7+xfLn3syatcqG3/e0tpLKw8m9zc/GjJT55tjs
zPiv96iw3Lmu+Kyype6z3/fnp/zW3KOhjcKVi+TF0ppkGCAFQ1JZUFQXgweCJy4KsMER4ZxVyCoU
RG6nX3fuTDmJdR7xFzlEZVjEkgAMq0a+Y1BUAAAAAAAAAABgcs3/f0VtG6Tu0fb80Or0DyJyq2TV
Qr6RJxqihgxgzUDszd9jRf8SH4Al4qCLjHwM8NfHA8Gj0z1jRT3Z1oPktRONDNvEv+7TPS9OEu+c
b0WnkPPjhdEuvYwYbP57Ny0j+I+iDIRYf9XQKzeRFEl4oQ9i7sM19D5ru48NYUF8/pJTy9EOIpS6
010Jc+K8RNAS6Cz/ZV6yiildAfuliC9bJCdr+qc+ACn2hzheeH9oAQ2QrLMSj1FKCCnTN4qnDbvu
PcbAS2/LblPfHPe0QNJaA6bq4wZHUKlQH2yGu9B0BxsnHJLeIiKl8xulKOQmcoLbgdnaInG1HZBT
oRnuVawk2KByFcXfZJCWQ9aaOab+1WYyc5wpw30QlRx9DsPV7K+Lq89KfGLndZ4q9J63mgcfAWNy
LSDMv2A7ouQLoM4XRbhAQVQVoQa15aZlt+tC1B9ecVu9cLGF"))

(defun compute-ah-unlock-key (key1 key2)
  (compute-unlock-key key1 key2
"JwQYIAJYMASNo7Tr1Pi++tmpgfmQzaPQ04/+4qzX3pH66Z7zjueEvdKxn6/gs8CfnfbFob2tl8Ca
l/fj9KefmZjgj82425vzjpWjl4r4yZThyrPFlu33+5snGCAFQ1JZUFQXgweCJy4KsMER4ZxVyCoU
RG6nHrhb73fxblNWChU8m68w0wAMq0a/hhcrAAAAAAAAAAA3abCEypcIgl8WlsWPfgQDvsqh6Wr5
G8ld82nM1Oc/qgD8/FWuzWPO13vNPiopz4g248pT8LcAd+9rd5bz2Ga0Wjs7vE3igzgeXHO9nfdD
/Dim3RVnfyG5BUpja7YIyZVuzJh1KfaKcfmSyrz04SYIuWUhDp7hhlbJNg8cgVpqGD2PHpYnmCMS
Fbyf683L+GIs+gIOvaiPORI1DCfoBvqchVDrBihK+jgqKoCUPLnHTIZyrKXlWdy7iV9Kb50U/XDd
RnQHGS2YsqSap0qbLRI3WXNr1lsLnW3LBzQO/W0tljtRdfnu46yWjLaxXYoLK7Zf/RkLHtoAvakZ
T2U7CLUYUihNVdR13GN71p+mNCTjjwIVRMBgPy/vpUdYl33ZF2Dzylmf5oqt2ZQw7DXLccVytnNZ
LV5sUpYwOASUV+//DP8z+WTkskCQkIUOqJRkeWxPOgrnou+o"))

;; --------------------------------------------------------------------------

(defun try-unlock (fn k1 k2)
  (handler-case
      (funcall fn k1 k2)
    (error ())))

(defun unlock-one (k1 k2)
  (dolist (item '(("Helene Barab"    compute-hb-unlock-key)
                  ("David Cohen"     compute-dc-unlock-key)
                  ("Quan Nguyen"     compute-qn-unlock-key)
                  ("Barry Baker"     compute-bb-unlock-key)
                  ("Arthur Herbst"   compute-ah-unlock-key)
                  ("William Masson"  compute-wm-unlock-key)
                  ("Paul Eynott"     compute-pe-unlock-key)))
    (um:when-let (sh (try-unlock (second item) k1 k2))
      (return-from unlock-one (list (first item) sh))) ))

(defun partition-shares (pred &rest args)
  (let ((yeses nil)
        (nos   nil))
    (dolist (arg args)
      (um:if-let (ans (funcall pred arg))
          (push ans yeses)
        (push arg nos)))
    (values yeses nos)))

(defun find-pair (k1 &rest keys)
  (multiple-value-bind (yeses nos)
      (apply #'partition-shares (um:curry #'unlock-one k1) keys)
    (cond
     (yeses
      (list (first yeses) nos))
     (nos
      (apply #'find-pair (first nos) (rest nos))) )))

#|
(time
 (progn
   (print "Relax... this will take about 1 minute")
   (um:when-let (sh1 (apply #'find-pair
                            (remove-duplicates
                             (list
                              (nth 1 *hb-shares*)
                              (nth 1 *pe-shares*)
                              (nth 2 *qn-shares*)
                              (nth 0 *ah-shares*)
                              (nth 5 *wm-shares*)
                              (nth 6 *bb-shares*)
                              (nth 3 *hb-shares*)
                              (nth 2 *pe-shares*)
                              (nth 9 *qn-shares*)
                              (nth 4 *ah-shares*))
                             :test #'string=)))
     (destructuring-bind ((name1 pt1) lst) sh1
       (um:when-let (sh2 (apply #'find-pair lst))
         (destructuring-bind ((name2 pt2) lst) sh2
           (um:when-let (sh3 (apply #'find-pair lst))
             (destructuring-bind ((name3 pt3) lst) sh3
               (print (list name1 name2 name3))
               (get-escrow-key pt1 pt2 pt3) ))))))))
|#
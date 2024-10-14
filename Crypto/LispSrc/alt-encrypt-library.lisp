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

;; ---------------------------------------------------
;; B-571 ECC Challenge / Response

;; Pkey   = key point at 0 (y-intercept)
;; Pshare = share point at user x-coord
;; Penc   = share point at function of user x-coord

(defvar *licensing-private-key*
  (big32 #x028F408C #x031CA59B #x46B08C6A #x1A9E723D
         #x57FCB9AF #x54C5B5FE #xBD2F908B #x98728791
         #x21FE8D0C #xB262388F #xD4AAAE09 #x0EB8AD81
         #x466576DC #x80181782 #x6BE8DAEB #x99305FAC
         #x9BE7B169 #xE81F25C3 ))

(defvar *licensing-public-key*
  (ecc-mul *ecc-gen* *licensing-private-key*))


(defvar *unlock-vDBM-private-key*
  (big32 #x01519520 #x2694F3BD #x70596133 #x06C000C0
         #x30266CE1 #xDA2F1C1D #x7C627F9C #x9FA0F6E0
         #x88BE69F6 #x2444CB30 #x57BFBEB4 #x41AC059A
         #xA435DED7 #x54C6714D #x7DE60A5A #x4942A718
         #x55D7C2E8 #xE658AB82 ))

(defvar *unlock-vDBM-public-key*
  (ecc-mul *ecc-gen* *unlock-vDBM-private-key*))


(defvar *unlock-CLAS-private-key*
  (big32 #x0125AB73 #xDB42174A #xF0A9C4B0 #xFCBDF018
         #x4AE89614 #x59B4B1C3 #x2E66CF63 #x93AA8634
         #xBA35A4E2 #x36FD4046 #x52FF3D25 #xFB15129A
         #x1A63BE66 #xD53555DA #x4E8724FA #x82383AB7
         #xD270327A #x9039B704 ))

(defvar *unlock-CLAS-public-key*
  (ecc-mul *ecc-gen* *unlock-CLAS-private-key*))


(defvar *unlock-vTuning-private-key*
  (big32 #x03D755CC #x7F8336EB #x27084087 #x7B54A4F3
         #x8A686C08 #x5DE1D2F6 #x217E6D17 #x8D54838A
         #xB15CADF5 #xBA78F82B #x090A916A #xAC01CF54
         #xD8B24CF4 #x3B262D86 #x6171F118 #x70A793F9
         #xCD97837F #x8F85F782 ))

(defvar *unlock-vTuning-public-key*
  (ecc-mul *ecc-gen* *unlock-vTuning-private-key*))


(defun compute-unlock-share-pt (user-id unlock-pt)
  ;; compute an unlock-pt share from a user id and the known unlock-pt
  ;; result is different for every user, yet computes to the same unlock-pt
  (ecc-solve-lagrange (list
                   (make-crypto-share
                    :x (aref *abscissa* 2)
                    :y unlock-pt)
                   (make-crypto-share
                    :x (aref *abscissa* 1)
                    :y (compute-user-pt user-id)))
                  (aref *abscissa* 0)))

(defun write-share-file (share-pt share-filename)
  (with-open-file (share-file share-filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (let ((snip "~%--- SNIP HERE ---~%"))
      (format share-file ";; machine generated -- do not modify")
      (format share-file snip)
      (write-sequence
       (encode-bytes-to-base64 share-pt)
       share-file)
      (format share-file snip))))
  
(defvar *vDBM-user-share-file*
  "com.Acudora.audiounit.vDBM")

(defvar *clas-user-share-file*
  "com.Acudora.audiounit.CLAS")

(defvar *vTuning-user-share-file*
  "com.Acudora.audiounit.vTuning")

(defun acudora-shares-folder ()
  (merge-pathnames "Acudora/shares/"
                   (sys:get-folder-path :appdata)))

(defun create-user-share-file (user-uuid unlock-pt bundle-id expiration)
  (let ((filename (merge-pathnames
                   (concatenate 'string "." bundle-id)
                   (acudora-shares-folder))))
    (ensure-directories-exist filename)
    (write-share-file
     (3ctr-hmac-encrypt-sequence
      (loenc:encode (list expiration
                          (ecc-div (compute-unlock-share-pt user-uuid unlock-pt)
                                   expiration)))
      (kdf 256 "User Share" bundle-id user-uuid))
     filename)))
  
(defun create-vDBM-user-share (user-uuid expiration)
  (create-user-share-file user-uuid
                          *unlock-vDBM-public-key*
                          *vDBM-user-share-file*
                          expiration))

(defun create-CLAS-user-share (user-uuid expiration)
  (create-user-share-file user-uuid
                          *unlock-clas-public-key*
                          *clas-user-share-file*
                          expiration))

(defun create-vTuning-user-share (user-uuid expiration)
  (create-user-share-file user-uuid
                          *unlock-vTuning-public-key*
                          *vTuning-user-share-file*
                          expiration))

(defun create-user-shares (user-uuid dd mm yy)
  (let ((expiration (encode-universal-time 0 0 0 dd mm yy)))
    (create-clas-user-share user-uuid expiration)
    (create-vTuning-user-share user-uuid expiration)
    (create-vDBM-user-share user-uuid expiration)))

(defvar *snippy-start* "---- START OF LICENSE BLOCK ----")
(defvar *snippy-stop*  "---- END OF LICENSE BLOCK ----")

;; --------------------------------------------------------

(defun find-random-multiple (pt)
  ;; determine a random offset so that pt + offset != inf
  (um:nlet iter ()
    (let* ((k      (convert-bytes-to-int (ctr-drbg 570)))
           (r      (ecc-mul *ecc-gen* k))
           (offset (ecc-mul *licensing-public-key* (* *ecc-h* k)))
           (z      (ecc-add pt offset)))
      (if (or (ecc-infinite-p offset)
              (ecc-infinite-p r)
              (ecc-infinite-p z))
          (go-iter)
        (values r z)) )))

;; -------------------------------------------------------
;; Use this to enable third party machines...
(defun encode-user-shares (user-uuid dd mm yyyy)
  (let ((enc-vDBM  (compute-unlock-share-pt user-uuid *unlock-vDBM-public-key*))
        (enc-clas  (compute-unlock-share-pt user-uuid *unlock-clas-public-key*))
        (enc-vTun  (compute-unlock-share-pt user-uuid *unlock-vTuning-public-key*))
        (expiration (encode-universal-time 0 0 0 dd mm yyyy)))
    (format t "~A~&~A~&~A~&"
            *snippy-start*
            (encode-bytes-to-base64
             (loenc:encode
              (loop for enc in (list enc-clas enc-vTun enc-vDBM)
                    for bid in (list *clas-user-share-file*
                                     *vTuning-user-share-file*
                                     *vDBM-user-share-file*)
                    collect
                    (multiple-value-bind (r z)
                        (find-random-multiple enc)
                      (3ctr-hmac-encrypt-sequence
                       (loenc:encode (list expiration r (ecc-mul z expiration)))
                       (kdf 256 "User Share"  bid user-uuid))))))
            *snippy-stop*) ))

(defun encode-eval-shares (dd mm yyyy)
  (encode-user-shares "E5EC8133-AEC7-5B94-9877-6A651B29262E" dd mm yyyy))

#|
;; Example for William's Mac
(encode-user-shares "13D198D4-DFCE-5A5E-A829-DEED520065D1" 1 1 2013)
==>
---- START OF LICENSE BLOCK ----
JwMXgiaCJy4KsMER4ZxVyCoURG6nSoK/KH3wkg14YAuK0N/eTAAMoh4EiZSvAAAAAAAAAAB2+55Z
WesQTbAiAh56Npn1LDJVrhoy7vNvlrh62BuBitEsYXdv3SNsxz0HXj53PLafFcKZddt8xuPqR+Xo
37VY873EyMYBrwVYO5BXtIHGkNuRBG+LI41uYYs9j0Ii7Zvcnkbx2FrtiPTuklNJJ8P1bPHI9Qna
B2krH6E6BEKzpUZ+GJf52byjSGQAwSIrxdJYwnLi0io100ZOnZWEa92SJx4gpKBHoeEpnKqgod2d
8VVjWAcs+XdL20PUBfHabUFn7nzPCKMHULxdYrkjPSdyphTxmMO+tUIalDrvTmk5whsLSaZUeQRr
xU4O/UYSWie+9ucOIQ4XgiaCJy4KsMER4ZxVyCoURG6n6fp/vsSOEBrDtY1SGBckWQAMoh4Ejwxu
AAAAAAAAAACXQpEb8dUrPYF3Q+bVFE5OF7/+ClsG5nNQgGu6caT/plhgtgLe/Sg27rxs9BDOi99C
mrvoRtZbyZZv8MfAhT9o35y+Q7QxYRYS1prbIqGWI1jc/yIau90xD7FPH1tp0Vjr8QxbOFc/8NY/
ocjHfCHfsGfa4W+Tfnv1LTv/KqyQ38E+fd61g3XYne+Jz5aIa6nf0BuE87yBGMMwPtTFi79acbon
yuVm+zL3xUHiJ+CvX3CqAAKQDDVbPC6+jjr42xJBd4ld4FvzJijAlBXAnfVhXWbhc7jIftmdTckQ
oZ6wdsccKqNOxSUzfN1hqCNozeOvxB2VVyUXgiWCJy4KsMER4ZxVyCoURG6nmh7CzxW0InLsbH1e
20ZzvgAMoh4ElIpKAAAAAAAAAACbTCjHXITnrIiFuDswbCw5fl699QjIfvnG8OJIPeqIygplHNLo
p8Tog6CksDNNft2DbxZoOh+najBKyP8oActuz5sEDQzGVFOZD9aOZujdvmhdyqOua5Gbdjqw+kIU
8HfxtYd6ITgQJdmUo7sLlXZE54CYvO4kJX1Cce9CFj9z6GTbFuUe7wmSZcYmmwoNQPwHgGRv0aBs
/p6SWFgv55hQ7ktNMc20O/iUCuDjcaGQJ5mMCcxmZSmSD+GNJ7dmhlXH/0YRqAxoKdgN8gkjZOgw
9aTKpPA2/v7n3yhOeDSLTpd1Lp+tQTV2cIqK4QRRgzE7mvrKGQ==
---- END OF LICENSE BLOCK ----
|#
;; -------------------------------------------------------

(defun save-user-share-file (bundle-id pt)
  (let ((filename (merge-pathnames
                   (concatenate 'string "." bundle-id)
                   (acudora-shares-folder))))
    (ensure-directories-exist filename)
    (write-share-file pt filename)))

(defun decode-and-save-user-shares (shares-enc)
  (let* ((from (+ (length *snippy-start*) (search *snippy-start* shares-enc)))
         (to   (search *snippy-stop* shares-enc :start2 from)))
    (destructuring-bind (clas-share-pt vtuning-share-pt vDBM-share-pt)
        (loenc:decode
         (decode-bytes-from-base64
          (subseq shares-enc from to)))
      (save-user-share-file *clas-user-share-file*      clas-share-pt)
      (save-user-share-file *vTuning-user-share-file*   vTuning-share-pt)
      (save-user-share-file *vDBM-user-share-file*      vDBM-share-pt)
      ) ))

;; ------------------------------------------------------------------------

;; --------------------------------------------
;; encrypt payload and write decryption share pt

(defun compute-decryption-share (share-pt encryption-pt)
  (multiple-value-bind (y1 y2 y3)
      (compute-decryption-share-coords)
    (ecc-solve-lagrange (list
                     (make-crypto-share
                      :x (aref *abscissa* 5)
                      :y encryption-pt)
                     (make-crypto-share
                      :x (aref *abscissa* 1)
                      :y share-pt)
                     (make-crypto-share
                      :x (aref *abscissa* 2)
                      :y y1)
                     (make-crypto-share
                      :x (aref *abscissa* 3)
                      :y y2)
                     (make-crypto-share
                      :x (aref *abscissa* 4)
                      :y y3))
                    (aref *abscissa* 0))))

(defun encrypt-osx-payload (filename-in filename-out unlock-pt bundle-id)
  ;; Main payload encrypted with randomly generated key.
  ;;
  ;; Accompanying share file contains encrypted share that regenerates
  ;; the random key through Lagrange interpolation over B-571, along with
  ;; hashes of the main files: decryptor program, crypto-lib, and main AU plugin
  ;;
  ;; Hashes are used on files that are unencrypted. All other files
  ;; are encrypted along with an HMAC
  ;;
  (let* ((encryption-pt (ecc-mul *ecc-gen* (convert-bytes-to-int (ctr-drbg 571))))
         (key           (encode-bytes-to-base64
                         (loenc:encode encryption-pt)) ))
    (with-sensitive-objects (key)
      (3ctr-hmac-encrypt-file filename-in filename-out key))
    (write-share-file
     (3ctr-hmac-encrypt-sequence
      (loenc:encode
       (compute-decryption-share unlock-pt encryption-pt))
      (kdf 256 "Signature Share" bundle-id))
     (concatenate 'string (namestring filename-out) "-x")) ))

(defvar *encrypted-honesty-lib*  ".T{EF2D1828-5E34-11E1-9D35-C82A14446EA7}")
(defvar *encrypted-clas-lib*     ".T{EF2D1829-5E34-11E1-9D35-C82A14446EA7}")
(defvar *encrypted-vTun-lib*     *encrypted-clas-lib*)
(defvar *encrypted-vdbm-lib*     *encrypted-clas-lib*)

(defmacro with-built-vDBM (&body body)
  `(let ((*auhost-path* "~/projects/Mac-Audio-Tools/Built/vDBM.component/Contents/MacOS/vDBM"))
     ,@body))

(defmacro with-built-CLAS (&body body)
  `(let ((*auhost-path* "~/projects/Mac-Audio-Tools/Built/CLAS.component/Contents/MacOS/CLAS"))
     ,@body))

(defmacro with-built-vTuning (&body body)
  `(let ((*auhost-path* "~/projects/Mac-Audio-Tools/Built/vTuning.component/Contents/MacOS/vTuning"))
     ,@body))

(defun host-file (fname)
  (merge-pathnames fname *auhost-path*))

(defun encrypt-decryptor (payload)
  (let ((encr     (namestring (host-file ".T{8ECB290E-5E32-11E1-9D35-C82A14446EA7}")))
        (decr-in  (namestring (host-file "decryptor")))
        (decr-out (namestring (host-file ".T{EF2D1828-5E34-11E1-9D35-C82A14446EA7}"))))
    (sys:call-system `(,encr
                       "ENC"
                       ,decr-in
                       ,decr-out
                       ,(concatenate 'string "file:" (namestring payload))))
    (delete-file decr-in))

  (let ((encr     (namestring (host-file ".T{8ECB290E-5E32-11E1-9D35-C82A14446EA7}")))
        (decr-in  (namestring (host-file "license-decryptor")))
        (decr-out (namestring (host-file ".T{EF2D1827-5E34-11E1-9D35-C82A14446EA7}"))))
    (sys:call-system `(,encr
                       "ENC"
                       ,decr-in
                       ,decr-out
                       ,(concatenate 'string "file:" (namestring payload))))
    (delete-file decr-in))
  )

;; ---------------------------------

#|
(defun encrypt-honesty (pubkey bundle-id)
  (let ((path-in (merge-pathnames "forced_honesty.dylib"
                                  *auhost-path*))
        (path-out (merge-pathnames *encrypted-honesty-lib*
                                   *auhost-path*)))
    (encrypt-osx-payload path-in path-out
                         pubkey
                         bundle-id)
    (delete-file path-in)
    (encrypt-decryptor path-out)))
|#

(defun encrypt-vDBM ()
  (with-built-vDBM
   (let ((path-in (merge-pathnames "vDBM-dsp.dylib"
                                   *auhost-path*))
         (path-out (merge-pathnames *encrypted-vDBM-lib*
                                    *auhost-path*)))
     (encrypt-osx-payload path-in path-out
                          *unlock-vDBM-public-key*
                          *vDBM-user-share-file*)
     (delete-file path-in)
     (encrypt-decryptor path-out))))

(defun encrypt-CLAS ()
  (with-built-clas
   (let ((path-in (merge-pathnames "CLAS-dsp.dylib"
                                   *auhost-path*))
         (path-out (merge-pathnames *encrypted-clas-lib*
                                    *auhost-path*)))
     (encrypt-osx-payload path-in path-out
                          *unlock-CLAS-public-key*
                          *clas-user-share-file*)
     (delete-file path-in)
     (encrypt-decryptor path-out))))

(defun encrypt-vTuning ()
  (with-built-vTuning
   (let ((path-in  (host-file "vTuning-dsp.dylib"))
         (path-out (host-file *encrypted-vTun-lib*)))
     (encrypt-osx-payload path-in path-out
                          *unlock-vTuning-public-key*
                          *vTuning-user-share-file*)
     (delete-file path-in)
     (encrypt-decryptor path-out))))

;; --------------------------------------------------------------------
#|
(defun delivered-encrypt-libraries ()
  (let ((status 0))
    (handler-case
        (progn
          (setf sys:*stack-overflow-behaviour* nil)
          (encrypt-clas)
          (encrypt-vTuning)
          (encrypt-vDBM))
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))
        

(defun delivered-encrypt-vDBM-libraries ()
  (let ((status 0))
    (handler-case
        (progn
          (setf sys:*stack-overflow-behaviour* nil)
          (encrypt-vDBM))
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))
        

(defun delivered-encrypt-clas-libraries ()
  (let ((status 0))
    (handler-case
        (progn
          (setf sys:*stack-overflow-behaviour* nil)
          (encrypt-clas))
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))
        

(defun delivered-encrypt-vTuning-libraries ()
  (let ((status 0))
    (handler-case
        (progn
          (setf sys:*stack-overflow-behaviour* nil)
          (encrypt-vTuning))
      (error (err)
        (setf status 1)
        (format t "~A~%" err)))
    (lw:quit :status status)))
|#


(defun delivered-encrypt-lib ()
  (let ((status 0))
    (labels ((usage ()
               (error "Usage: encrypt-libraries [CLAS | vDBM | vTuning]")))
      (handler-case
          (progn
            (unless (= 2 (length sys:*line-arguments-list*))
              (usage))
            (destructuring-bind (prog-name arg) sys:*line-arguments-list*
              (declare (ignore prog-name))
              (setf sys:*stack-overflow-behaviour* nil)
              (cond ((string-equal arg "CLAS")
                     (encrypt-CLAS))
                    ((string-equal arg "vDBM")
                     (encrypt-vDBM))
                    ((string-equal arg "vTuning")
                     (encrypt-vTuning))
                    (t (usage))) ))
        (error (err)
          (setf status 1)
          (format *error-output* "~A~%" err)))
      (lw:quit :status status))))

;; --------------------------------------------------------------------

#|
(create-user-shares (machid:get-machine-id)
                    1 1 2099)

(progn
  (encrypt-clas)
  (encrypt-vTuning)
  (encrypt-vDBM))

(with-built-clas
 (decrypt-library
  "~/projects/Mac-Audio-Tools/Built/CLAS.component/Contents/MacOS/CLAS-dsp.dylib.enc"
  "~/projects/Mac-Audio-Tools/Built/CLAS.component/Contents/MacOS/CLAS-dsp.dylib.dec"
  *clas-user-share-file*))

cd ~/projects/Mac-Audio-Tools/Built/CLAS.component/Contents/MacOS
.T{809C748C-5E32-11E1-9D35-C82A14446EA7} CLAS .T{EF2D1828-5E34-11E1-9D35-C82A14446EA7} junk .com.Acudora.audiounit.CLAS
diff junk CLAS-dsp.dylib

cd ~/projects/Mac-Audio-Tools/Built/vTuning.component/Contents/MacOS
.T{809C748C-5E32-11E1-9D35-C82A14446EA7} vTuning .T{F8DBF236-5E34-11E1-9D35-C82A14446EA7} junk .com.Acudora.audiounit.vTuning
diff junk vTuning-dsp.dylib
|#

#|
(let ((k (convert-bytes-to-int (ctr-drbg 571))))
  (equalp *unlock-clas-public-key*
          (ecc-div (ecc-mul *unlock-clas-public-key* k) k)))
|#


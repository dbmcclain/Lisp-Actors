;; vtuning-crypto.lisp -- Crypto to support VTuning on the Muse Receptor
;; DM/Acudora  11/11
;; ----------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------

(defstruct vtuning-environ
  payload-path
  staging-area-path)

(defun make-vt-environ ()
  (make-vtuning-environ
   :payload-path (shared-acudora-file "VTuningArchive.zip.aes")
   :staging-area-path 
   #+:MSWINDOWS
   #P"C:/Program Files (x86)/Steinberg/Cubase LE 4/VSTPlugins/VTuning/"
   #+:MAC
   #P"/Volumes/ramdisk/"))

(def-cached-var vt-environ (make-vt-environ))

#|
(defvar *vt-environ* nil)

(defun vt-environ ()
  (or *vt-environ*
      (setf *vt-environ* (make-vtuning-environ
                          :payload-path (shared-acudora-file "VTuningArchive.zip.aes")
                          :staging-area-path 
                          #+:MSWINDOWS
                          #P"C:/Program Files (x86)/Steinberg/Cubase LE 4/VSTPlugins/VTuning/"
                          #+:MAC
                          #P"/Volumes/ramdisk/"))))
|#

(defun vtuning-payload-path ()
  (vtuning-environ-payload-path (vt-environ)))

(defun staging-area-path ()
  (vtuning-environ-staging-area-path (vt-environ)))

(defun staging-path (fname)
  (merge-pathnames fname (staging-area-path)))

(defun fmem-path ()
  (staging-path "fmem-array.dat"))

(defun fcoffs-path ()
  (staging-path "fcoffs"))

(defun encrypted-strings-path ()
  (staging-path "encrypted-strings"))

(defun scoffs-path ()
  (staging-path "scoffs"))

(defun prog-path ()
  (staging-path "VTuning.dll"))

(defun img-path ()
  #+:MSWINDOWS "c:/Windows/receptor.jpg"
  #+:MAC     (staging-path "receptor.jpg"))

(defun coffs-path ()
  (staging-path "coffs"))

(defvar *license-path* nil)

;; -------------------------------------------------

(defun convert-number-to-16bytes (x)
  (let ((ans (make-ub-array 16
                            :initial-element 0)))
    (loop for ix from 15 downto 0 do
          (setf (aref ans ix) (ldb (byte 8 0) x)
                x             (ash x -8)))
    ans))

(defun encrypt-string (str &key (key *ecc-strings-public-key*))
  ;; take a string < 64 chars long and convert to
  ;; encrypted form 80 bytes long
  (assert (< (Length str) 64))
  (let* ((len (length str))
         (str (concatenate 'string str
                           (make-string (- 64 len)
                                        :initial-element (code-char 0))))
         (cnt  (uuid:uuid-to-integer (uuid:make-v1-uuid)))
         (enc  (logxor (convert-text-to-int571 str)
                       (ecc-pt-x (ecc-mul key cnt))))
         (encv (convert-int571-to-int8-array enc)))
    ;; first 16-bytes is the string UUID
    (concatenate 'vector (convert-number-to-16bytes cnt) (subseq encv 8))))
    
(defun decrypt-string (arr &key (key *ecc-strings-public-key*))
  ;; take an 80 byte vector and decrypt to a C string < 64 bytes
  (let* ((cnt (convert-bytes-to-int (subseq arr 0 16)))
         (enc (logxor (convert-bytes-to-int (subseq arr 16))
                      (ecc-pt-x (ecc-mul key cnt))))
         (encv (convert-int571-to-int8-array enc))
         (strv (subseq encv 8))
         (pos  (position 0 strv)))
    (map 'string #'code-char (subseq strv 0 pos))))

#|
(progn
  (terpri)
  (dolist (str '("fcoffs"
                 "VTuningLicense.txt"
                 "receptor.jpg"))
    (print-c-array (encrypt-string str))
    (terpri)))

(let ((arrs (mapcar #'encrypt-string '("fcoffs"
                                       "VTuningLicense.txt"
                                       "receptor.jpg"))))
  (terpri)
  (dolist (arr arrs)
    (print (decrypt-string arr))))
|#

;; -------------------------------------------------
;; Create the encrypted strings file

(defun create-strings-file (support-filename-strings)
  (let ((strs (mapcar #'encrypt-string support-filename-strings)))
    (with-open-file (f (encrypted-strings-path)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type 'ubyte)
                    
      (write-sequence (convert-int571-to-int8-array
                       (ecc-pt-x *ecc-strings-public-key*))
                      f)
      (write-sequence (convert-int571-to-int8-array
                       (ecc-pt-y *ecc-strings-public-key*))
                      f)
      (dolist (str strs)
        (write-sequence str f)))))

;; -------------------------------------------------
;; Create the FCOFFS and SCOFFS files

(defun encrypt-payloads (prog-key)
  (labels ((prep (key)
             (coerce
              (convert-int571-to-int8-array (ecc-pt-x key))
              'list)))
    
    ;; Encrypt the fmem array
    (let ((enckey (prep *ecc-vtuning-product-public-key*)))
      (when (= 127 
               (sys:call-system (format nil "aescrypt 0 ~S ~S hex:~{~2,'0X~}"
                                        (namestring (fmem-path))
                                        (namestring (fcoffs-path))
                                        enckey)))
        (error "BASH cannot locate AESCrypt")))
    
    ;; encrypt the strings
    (let ((enckey (prep (ecc-mul *ecc-gen* prog-key))))
      (sys:call-system (format nil "aescrypt 0 ~S ~S hex:~{~2,'0X~}"
                               (namestring (encrypted-strings-path))
                               (namestring (scoffs-path))
                               enckey))) ))

;; ---------------------------------------------------------------------------------

(defun compute-total-key (&rest keys)
  ;; sha2 sum of concatenated keys
  (convert-bytes-to-int
   (apply #'sha2-buffers
          (mapcar #'convert-hashint-to-32bytes keys)) ))

(defun compute-comp-key (&rest keys)
  (let ((total-key (apply #'compute-total-key keys)))
    (ecc-sub *ecc-vtuning-product-public-key*
             (ecc-mul *ecc-gen* total-key))))
  
;; -------------------------------------------------
;; Generate COFFS file

(defun generate-coffs-file (comp-key)
  (with-open-file (f (coffs-path)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :element-type 'ubyte)
    (write-sequence (convert-int571-to-int8-array
                     (ecc-pt-x comp-key))
                    f)
    (write-sequence (convert-int571-to-int8-array
                     (ecc-pt-y comp-key))
                    f)))

;; -------------------------------------------------
#|
(defun auth ()
  (equalp
   *ecc-vtuning-product-public-key*
   (ecc-add *compkey*
    (ecc-mul *ecc-gen* *totalkey*))))
(unless (auth)
  (error "Something is really wrong here!"))
|#
;; ---------------------------------------------------

(defun rebuild-vtuning-payload (&optional (license-fname "VTuningLicense.txt"))
  (create-strings-file (list "scoffs"
                             "coffs"
                             "fcoffs"
                             "receptor.jpg"
                             license-fname))
  (let* ((license-path  (staging-path license-fname))
         (prog-key      (sha2-key (prog-path)))
         (img-key       (sha2-key (img-path)))
         (txt-key       (sha2-key license-path)))
    (setf *license-path* license-path)
    (encrypt-payloads prog-key)
    (let* ((fc-key   (sha2-key (fcoffs-path)))
           (sc-key   (sha2-key (scoffs-path)))
           (comp-key (compute-comp-key prog-key
                                       img-key
                                       fc-key
                                       sc-key
                                       txt-key)))
      (generate-coffs-file comp-key)) ))

#|
(rebuild-vtuning-payload "VTuningLicense-F11110100878.txt")
(rebuild-vtuning-payload)
|#

(defun copy-vtuning-payload-to-receptor (receptor-path)
  (let ((acudora-dir (merge-pathnames
                        "Program Files/VST Plugins/Unsupported Plugins/Acudora/"
                        receptor-path))
        (windows-dir (merge-pathnames
                        "windows/"
                        receptor-path)))
    (ensure-directories-exist acudora-dir)
    (sys:call-system
     (format nil "cp ~S ~S ~S ~S ~S ~S"
             (namestring (prog-path))
             (namestring *license-path*)
             (namestring (fcoffs-path))
             (namestring (scoffs-path))
             (namestring (coffs-path))
             (namestring acudora-dir)))
    (sys:call-system
     (format nil "cp ~S ~S"
             (namestring (img-path))
             (namestring windows-dir))) ))
   
#|
(copy-vtuning-payload-to-receptor "/Volumes/Receptor HD/")
|#

;; --------------------------------------------------------------------
;; Authorization for VTuning...

(defun vtuning-auth-path ()
  (shared-acudora-file "VTuning-Authorizations"))

(defun read-vtuning-authorizations ()
  (with-open-file (fp (vtuning-auth-path)
                      :element-type 'ubyte
                      :direction :input)
    (loenc:deserialize fp)))

(defun write-vtuning-authorizations (auths)
  (with-open-file (fp (vtuning-auth-path)
                      :element-type 'ubyte
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (loenc:serialize auths fp)))

(def-cached-var vtuning-authorized (read-vtuning-authorizations))

#|
(defvar *vtuning-authorized* nil)

(defun vtuning-authorized ()
  (or *vtuning-authorized*
      (setf *vtuning-authorized* (read-vtuning-authorizations))))
|#

(defun find-vtuning-share (id)
  (let ((kpub (get-public-key id)))
    (find kpub (vtuning-authorized)
          :test #'ecc-pt=
          :key  #'first)))
  
(defun vtuning-share-x0 ()
  (convert-bytes-to-int
   (kdf *nbits*
        (ecc-pt-x *ecc-vtuning-product-public-key*)
        (ecc-pt-y *ecc-vtuning-product-public-key*))))

(defun make-vtuning-share (id auth-id)
  ;; *** keep private ***
  (unless (find-vtuning-share id)
    ;; if not already authorized...
    (get-private-key auth-id) ;; force admin to enter password
    (let* ((kpub (get-public-key id))
           (x    (convert-bytes-to-int
                  (kdf *nbits*
                       (ecc-pt-x kpub)
                       (ecc-pt-y kpub)
                       (ctr-drbg *nbits*))))
           (x0   (vtuning-share-x0))
           (y    (solve-gf-lagrange x
                                    (make-crypto-share
                                     :x x0
                                     :y *ecc-vtuning-product-private-key*)
                                    (make-crypto-share
                                     :x (ecc-pt-x *ecc-vtuning-product-public-key*)
                                     :y (ecc-pt-y *ecc-vtuning-product-public-key*)))))
      (push (list kpub (make-crypto-share :x x :y y)) *vtuning-authorized*)
      (write-vtuning-authorizations *vtuning-authorized*))))

(defun remove-vtuning-share (id auth-id)
  (when (find-vtuning-share id)
    ;; if really authorized...
    (get-private-key auth-id) ;; force admin to enter password
    (let ((kpub (get-public-key id)))
      (setf *vtuning-authorized* (remove kpub *vtuning-authorized*
                                         :test #'ecc-pt=
                                         :key  #'first))
      (write-vtuning-authorizations *vtuning-authorized*))))

(defun get-vtuning-share (id)
  (or (second (find-vtuning-share id))
      (error "Not authorized")))

(defun authorize-for-vtuning (id)
  (let* ((share (get-vtuning-share id))
         (x0    (vtuning-share-x0)))
    (solve-gf-lagrange x0
                       share
                       (make-crypto-share
                        :x (ecc-pt-x *ecc-vtuning-product-public-key*)
                        :y (ecc-pt-y *ecc-vtuning-product-public-key*))) ))

#|
(ignore-errors
  (read-vtuning-authorizations))
|#

;; --------------------------------------------------------------------
;; RAMDisk...

(defun mount-ramdisk (&optional (mbsize 500))
  (sys:call-system
   (format nil "diskutil erasevolume HFS+ \"ramdisk\" `hdiutil attach -nomount ram://~D`"
           (* mbsize 2048))))

(defun erase-ramdisk ()
  (sys:call-system
   "diskutil erasevolume HFS+ ramdisk /Volumes/ramdisk"))

(defun unmount-ramdisk ()
  (erase-ramdisk)
  (sys:call-system
   "hdiutil detach /Volumes/ramdisk"))

#|
(defmacro with-transient-ramdisk (&body body)
  `(do-with-transient-ramdisk (lambda () ,@body)))

(defun do-with-transient-ramdisk (fn)
  (mount-ramdisk)
  (unwind-protect
      (funcall fn)
    (unmount-ramdisk)))
|#

(um:defwrapper with-transient-ramdisk ()
  (mount-ramdisk)
  (unwind-protect
      &body
    (unmount-ramdisk)))

#|
(defun tst ()
  (with-transient-ramdisk ()
    (sleep 3)))
|#
;; --------------------------------------------------------------------
;; VTuning Installation...

#|
  ;;
  ;; create the encrypted payload archive
  ;;
(let ((id "david@acudora.com"))
  (aes-encrypt-file-with-signature "./VTuning/crypto/VTuningArchive.zip"
                                   "./VTuning/crypto/VTuningArchive.zip.aes"
                                   (authorize-for-vtuning id)
                                   id))
|#

(defun copy-vtuning-payload-archive-to-ramdisk (id)
  ;;
  ;; Decrypt the VTuning encrypted archive to RamDisk,
  ;; then unzip the decrypted archive
  ;;
  (let ((dest-path (staging-path "VTuningArchive.zip")))
    (aes-decrypt-file-with-signature
     (vtuning-payload-path)
     dest-path
     (authorize-for-vtuning id))
    (sys:call-system
     (format nil
             "unzip ~S -d ~S"
             (namestring dest-path)
             (namestring (staging-area-path)))) ))
   
#|
(progn
  (mount-ramdisk)
  (copy-vtuning-payload-archive-to-ramdisk "david@acudora.com"))
(unmount-ramdisk)  
|#


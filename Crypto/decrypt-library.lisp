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

;; --------------------------------------------------------------------

(defvar *live-decryption* nil)
(defvar *auhost-path*     nil)

;; Decryptor module
(defvar *decryptor*     ".T{809C748C-5E32-11E1-9D35-C82A14446EA7}")

;; ForcedHonesty module
(defvar *forced-honesty* ".T{EF2D182A-5E34-11E1-9D35-C82A14446EA7}")

(defun image-directory ()
  (make-pathname
   :directory (pathname-directory (lw:lisp-image-name))))

(defun exec-directory ()
  (make-pathname
   :directory (pathname-directory (auhost-path))))

(defun forced-honesty-path ()
  (if *live-decryption*
      (merge-pathnames *forced-honesty* (image-directory))
    (merge-pathnames *forced-honesty* *auhost-path*)))

(defun decryptor-path ()
  (if *live-decryption*
      (lw:lisp-image-name)
    (merge-pathnames *decryptor* *auhost-path*)))

(defun auhost-path ()
  (or *live-decryption*
      *auhost-path*))

;; -----------------------------------------------------
(fli:disconnect-module :cryptolib)
(fli:get-embedded-module :cryptolib
                         (translate-logical-pathname 
                          "PROJECTS:DYLIB;libLispCrypto.dylib"))
;; -----------------------------------------------------

(defun live-connect-to-cryptolib ()
  (fli:install-embedded-module :cryptolib))

(defun sha-len (path)
  (with-open-file (f path
                     :direction :input
                     :element-type 'ubyte)
    (ecc-mul *ecc-gen*
             (+ (file-length f)
                (ash (convert-bytes-to-int (shad2-file path)) 32)) )))
       
(defun compute-decryption-share-coords ()
  (values (sha-len (forced-honesty-path))
          (sha-len (decryptor-path))
          (sha-len (auhost-path))))
         
(defvar *abscissa*
  (vector
   (big32 #x03192D83 #xC4345CD8 #xEE55A208 #x35D5C87F
          #xCC3F2F36 #xD56AEE0B #xFD9C2C67 #x2F6DE318
          #xFED5B988 #x235797A2 #x04D869C9 #x5BFEDB97
          #x6E894CAD #x54FAB41B #x62F84ECF #x7EE5DEF0
          #x76B5907B #xB409992F )
   (big32 #x0455BA88 #xEAEAA81E #xF387EAE4 #x8A377660
          #x6E03D78D #x8B992EF5 #x70E3827D #xC0F3C4E8
          #x60320C3B #xC46D6026 #x12E57523 #xD8EAA2AC
          #x3F5A3488 #x04D0488C #x4F57A30F #xCD2A6476
          #x0040C16D #x1A592E0A )
   (big32 #x06DB9DBE #x70037CD9 #xE312809C #xF5213750
          #x50F32D3D #x71B66B5B #x6284B0F5 #xBFFC4B77
          #xD6D50B9E #x90A55F49 #xC84E271C #x63831ED3
          #x01A2A19F #xE2D0E1AA #x0BF18825 #x1D2CBD17
          #x4244784D #xD18B542C )
   (big32 #x0099EBE0 #xE14350C0 #x52EF31FE #x6AC5BB5A
          #x03C69878 #x28088A63 #xBB0A5819 #x4F9958FF
          #xD9343ED3 #x9EABE75B #x42A15D01 #x3511211B
          #x44100F81 #x75E87AA4 #xE97B764D #x7CB19C8C
          #x0D5CBED8 #x284C5918 )
   (big32 #x06FE181B #x87B56048 #x1CA9CA19 #x7CED82ED
          #xCD785222 #xC7E9422E #x96BA752B #x56883AE8
          #x728355F2 #xEF4C780C #x9DEBB1A9 #xCE7C50A0
          #x7329B8C4 #xDF2B30CF #x110960FC #x18C1D5E2
          #x91589146 #x2C7C94E6 )
   (big32 #x06D66645 #x1D51A3DE #xEAE257E7 #x0DCCC90E
          #xA52A3ADF #x7F985ACF #x3BF862D4 #x87149EA0
          #x70BC27DC #x28CE821B #xB0DFCA65 #x517506D8
          #x860794D9 #x1A46D166 #xA77834BF #xD3D85A33
          #xF1DE01CC #x68F9DBAA )
   ))

(defun compute-decryption-pt-from-shares (unlock-pt-share decryption-pt-share)
  (multiple-value-bind (y1 y2 y3)
      (compute-decryption-share-coords)
    (solve-lagrange (list
                     (make-crypto-share
                      :x (aref *abscissa* 1)
                      :y unlock-pt-share)
                     (make-crypto-share
                      :x (aref *abscissa* 2)
                      :y y1)
                     (make-crypto-share
                      :x (aref *abscissa* 3)
                      :y y2)
                     (make-crypto-share
                      :x (aref *abscissa* 4)
                      :y y3)
                     (make-crypto-share
                      :x (aref *abscissa* 0)
                      :y decryption-pt-share))
                    (aref *abscissa* 5))))
  
(defun compute-user-pt (uid)
  (ecc-mul *ecc-gen*
           (1+ (ash (uuid:uuid-to-integer (uuid:make-uuid-from-string uid)) 1)) ))

(defun compute-product-unlock-pt (user-id unlock-pt-share)
  ;; produce an unlock pt from a user-id and its unlock-pt-share
  (solve-lagrange (list
                   (make-crypto-share
                    :x (aref *abscissa* 1)
                    :y (compute-user-pt user-id))
                   (make-crypto-share
                    :x (aref *abscissa* 0)
                    :y unlock-pt-share))
                  (aref *abscissa* 2)))

(defun read-share-file (filename key)
  (with-open-file (fin filename
                       :direction :input)
    (let* ((flen (file-length fin))
           (buf  (make-string flen)))
      (read-sequence buf fin)
      (let* ((snip "--- SNIP HERE ---")
             (start (search snip buf
                            :test #'char-equal))
             (end   (search snip buf
                            :test #'char-equal
                            :from-end t)))
        (loenc:decode
         (3ctr-hmac-decrypt-sequence
          (decode-bytes-from-base64 (subseq buf (+ start (length snip)) end))
          key))
        ))))

(defun acudora-shares-folder ()
  (merge-pathnames "Acudora/shares/"
                   (sys:get-folder-path :appdata)))

(defun get-unlock-share (bundle-id key)
  (destructuring-bind (expiration pt)
      (read-share-file (merge-pathnames
                        (concatenate 'string "." bundle-id)
                        (acudora-shares-folder))
                       key)
    (let* ((now (get-universal-time))
           (ans (ecc-mul (cvt-point (shiftf pt nil)) (max now expiration))))
      (if (> now expiration)
          (multiple-value-bind (_s _m _h dd mm yy)
              (decode-universal-time expiration)
            (declare (ignore _s _m _h))
            (error "License for ~A expired on ~2,'0D/~2,'0D/~4,'0D"
                   bundle-id dd mm yy))
        ans)) ))

(defun decrypt-library (filename-in filename-out bundle-id)
  (let* ((user-uuid     (
                         #-:ACUDORA-EVAL-LICENSE or
                         #+:ACUDORA-EVAL-LICENSE and
                         (machid:get-machine-id)
                         "E5EC8133-AEC7-5B94-9877-6A651B29262E"))
         (unlock-share  (get-unlock-share bundle-id
                                          (kdf 256 "User Share" bundle-id user-uuid)))
         (unlock-pt     (compute-product-unlock-pt user-uuid unlock-share))
         (inp-file      (merge-pathnames
                         (string filename-in)
                         (image-directory)))
         (decrypt-share (read-share-file (merge-pathnames
                                          (concatenate 'string (string filename-in) "-x")
                                          (image-directory))
                                         (kdf 256 "Signature Share" bundle-id)))
         (decrypt-pt    (compute-decryption-pt-from-shares unlock-pt decrypt-share))
         (key           (encode-bytes-to-base64
                         (loenc:encode decrypt-pt))))
    (with-sensitive-objects (key unlock-share unlock-pt decrypt-share decrypt-pt)
      (3ctr-hmac-decrypt-file inp-file filename-out key)) ))

;; ------------------------------------------------------------------

(defun delivered-decrypt-library ()
  ;; bundle-id argument string
  ;;   -- dotted bundle-id is the name of the appdata share file
  (let ((status 0))
    (handler-case
        (progn
          (unless (= 5 (length sys:*line-arguments-list*))
            (error "Usage: ~A <exe-file> <from-file> <to-file> <bundle-id>"
                   (pathname-name (car sys:*line-arguments-list*))))
          (destructuring-bind (progname exename fromfile tofile bundle-id)
              sys:*line-arguments-list*
            (declare (ignore progname))
            ;; (format t "Arg 0: ~A~%" (lispworks:lisp-image-name ))
            #|
            (print sys:*line-arguments-list*)
            (format t "~%Progname: ~A" progname)
            (format t "~%Exe:      ~A" exename)
            (format t "~%From:     ~A" fromfile)
            (format t "~%To:       ~A" tofile)
            (format t "~%Bundle:   ~A" bundle-id)
            (terpri)
            |#
            (setf sys:*stack-overflow-behaviour* nil)
            
            (let ((*live-decryption* exename))
              #|
              (format t "libpath:   ~A~%" (forced-honesty-path))
              (format t "decryptor: ~A~%" (decryptor-path))
              (format t "au host:   ~A~%" (auhost-path))
              |#
              
              (let ((imgdir (directory (image-directory)))
                    (excdir (directory (exec-directory))))
                (unless (and (= 7 (length imgdir))
                             (equalp imgdir excdir))
                  ;; check for impostors
                  (error "Invalid directory contents")))
              
              (live-connect-to-cryptolib)
              (decrypt-library fromfile tofile bundle-id)
              )))
          
      (error (err)
        (setf status 1)
        (format *error-output* "~A~%" err)))
    (lw:quit :status status)))

;; ---------------------------------------------------------------

#|
;; examples

cd ~/projects/Mac-Audio-Tools/Built/CLAS.component/Contents/MacOS
.T{809C748C-5E32-11E1-9D35-C82A14446EA7} CLAS .T{EF2D1828-5E34-11E1-9D35-C82A14446EA7} junk com.Acudora.audiounit.CLAS
diff junk CLAS-dsp.dylib

cd ~/projects/Mac-Audio-Tools/Built/vTuning.component/Contents/MacOS
.T{809C748C-5E32-11E1-9D35-C82A14446EA7} vTuning .T{EF2D1829-5E34-11E1-9D35-C82A14446EA7} junk com.Acudora.audiounit.vTuning
diff junk vTuning-dsp.dylib
|#

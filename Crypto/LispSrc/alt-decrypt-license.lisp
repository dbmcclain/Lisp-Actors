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

#+:DECRYPT-DELIVERY
(progn
  (fli:disconnect-module :cryptolib)
  (fli:get-embedded-module :cryptolib
                           (translate-logical-pathname 
                            "PROJECTS:LIB;libLispCrypto.dylib")))
;; -----------------------------------------------------

(defun live-connect-to-cryptolib ()
  (fli:install-embedded-module :cryptolib))

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

;; --------------------------------------------------------

(defvar *licensing-public-key*
  (make-ecc-pt
   :x (big32 #x02F1C74E #x8664589E #xB37216F2 #x67935750
             #x1C2CD181 #x2723418A #x27BD62AE #xB49216A2
             #x647A39D7 #x854D6063 #xBB1682B8 #x0E3FADEA
             #x1F6C8F5A #x77FB1319 #x3A8631A2 #x9540BF41
             #xC87A5115 #xBB363F32 )
   :y (big32 #x00975C45 #xD3CE3DA8 #xDE1A1CEA #x4285E2C1
             #xCAD1415F #xD88849C4 #x6AFB04C3 #x899489E9
             #x47C9AC9C #x707F7E58 #xAE246101 #x86BC7DBF
             #x2D45BB1D #x408D5A3D #x09D9A9C3 #x90C7E6B0
             #xFA87DDC6 #x7A93C6F4 )))

;; --------------------------------------------------------

(defun find-random-multiple (r z)
  ;; determine random multiple so that
  ;;   pt + k*pubkey != inf
  ;; and
  ;;   r + k*gen != inf
  (let ((uuid (uuid:uuid-to-integer
                      (uuid:make-uuid-from-string
                       (machid:get-machine-id)))))
    (um:nlet iter ()
      (let* ((k    (gf* uuid (convert-bytes-to-int (ctr-drbg 570))))
             (offr (ecc-mul *ecc-gen* k))
             (r2   (ecc-add r offr))
             (offz (ecc-mul *licensing-public-key* (* *ecc-h* k)))
             (z2   (ecc-add z offz)))
        (if (or (ecc-infinite-p offr)
                (ecc-infinite-p r2)
                (ecc-infinite-p offz)
                (ecc-infinite-p z2))
            (go-iter)
          (values r2 z2)) ))))

;; --------------------------------------------------------

(defun get-unlock-share (user-uuid bundle-id)
  (destructuring-bind (expiration r pt)
      (read-share-file (merge-pathnames
                        (concatenate 'string "." bundle-id)
                        (acudora-shares-folder))
                       (kdf 256 "User Share" bundle-id user-uuid))
    (let* ((now (get-universal-time))
           (ans (ecc-div (cvt-point (shiftf pt nil)) (max now expiration))))
      (if (> now expiration)
          (multiple-value-bind (_s _m _h dd mm yy)
              (decode-universal-time expiration)
            (declare (ignore _s _m _h))
            (error "License for ~A expired on ~2,'0D/~2,'0D/~4,'0D"
                   bundle-id dd mm yy))
        (multiple-value-bind (r2 z2)
            (find-random-multiple r ans)
          (list user-uuid r2 z2) ))) ))

(defun decrypt-license (filename-out bundle-id)
  (let* ((user-uuid     (
                         #-:ACUDORA-EVAL-LICENSE or
                         #+:ACUDORA-EVAL-LICENSE and
                         (machid:get-machine-id)
                         "E5EC8133-AEC7-5B94-9877-6A651B29262E"))
         (unlock-share  (get-unlock-share user-uuid bundle-id)))
    (with-open-file (f filename-out
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-sequence (encode-bytes-to-base64
                       (loenc:encode unlock-share))
                      f)) ))

;; ------------------------------------------------------------------

(defun delivered-decrypt-license ()
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
            (declare (ignore progname exename fromfile))
            
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
            ;; (print "Hello from alt-decrypt-license")
            (setf sys:*stack-overflow-behaviour* nil)
            (live-connect-to-cryptolib)
            (decrypt-license tofile bundle-id)
            ))
          
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

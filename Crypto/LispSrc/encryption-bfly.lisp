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

#|
;; --------------------------------------------------------------------
;; This portion belongs on the central Acudora key server

(defvar *member-ids*
  (make-hash-table :test 'equalp))

(defun get-ibe-decryption-key (ckeys my-id)
  (handler-case
      (let ((kpub  (get-public-key my-id)))
        (destructuring-bind (ks to-id)
            (dh-decrypt ckeys *ecc-acudora-private-key*)
          (with-sensitive-objects (ks)
            (if (or (equalp to-id my-id)
                    (member my-id (gethash to-id *member-ids*)
                            :test 'equalp))
                (values (dh-encrypt ks kpub) t)
              ;; else
              (error "Not intended recipient"))) ))
    (error (err)
      (values nil err))))

(defun start-key-server ()
  ;; (com.sd.butterfly.int::initialize-api)
  (com.sd.butterfly.int:lw-start-butterfly)
  (bfly.name-server::start :acudora-key-server))
  
;; --------------------------------------------------------------------
|#
(defun decrypt-ibe (cmsg my-id)
  (let ((kpriv (get-private-key my-id))
        ;; (srv   :acudora-key-server)
        #||#
        (srv   (bfly:remote-service
                ;; "granite.local"
                ;; "roadrunner"
                ;; "10.0.1.200"
                ;; "artemis.local"
                "localhost"
                :name :acudora-key-server))
        #||#
        )
    (when kpriv
      (destructuring-bind (etype sig ckeys cmsg)
          (decode-object-from-base64 cmsg)
        (assert (uuid:uuid= etype *ibe-encryption*))
        (multiple-value-bind (uuid kpub)
            (check-signature-with-cmsg cmsg sig)
          (multiple-value-bind (cks err)
              (com.sd.butterfly.bb:with-rpc-timeout 60
                (bfly:call-sync srv :get-decryption-key ckeys my-id))
            (unless cks
              (error err))
            (let* ((ks  (dh-decrypt cks kpriv)))
              (with-sensitive-objects (ks)
                (values (decrypt-msg cmsg ks)
                        (uuid:when-created uuid)
                        kpub))) ))) )))


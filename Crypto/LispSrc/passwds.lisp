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

;; -----------------------------------------------------------------------------
;;

(defvar *progress* nil)

(um:defmacro! with-progress-bar ((&rest keys) &body body)
  `(let ((,g!fn (lambda ()
                  ,@body)))
     (if *progress*
         (funcall ,g!fn)
       (progress-bar:with-progress-bar (*progress* ,@keys)
         (funcall ,g!fn)))) )

(defun update-progress (&optional (nincr 1))
  (when *progress*
    (capi:execute-with-interface *progress*
                                 'progress-bar:incr-value *progress* nincr)))

;; -------------------------------------------------------------------------------------------

(defstruct passwd
  hid salt off kpub hmac locked)

;; ----------------------------------------------------------

(defmacro with-kdf-fields ((fields keys) &body body)
  ;; fields are keying entities with stated bit-widths
  ;; keys is a list of keys
  (let* ((names (mapcar #'first fields))
         (bits  (mapcar #'second fields))
         (tbits (reduce #'+ bits))
         (bytes (mapcar (um:rcurry #'truncate 8) bits))
         (ks    (gensym)))
    (assert (every (lambda (x)
                     (zerop (rem x 8)))
                   bits))
    `(let* ((,ks  (apply #'kdf ,tbits (um:mklist ,keys)))
            ,@(mapcar (let ((start 0))
                        (lambda (name bytes)
                          (let ((end (+ start bytes)))
                            (prog1
                                `(,name  (subseq ,ks ,start ,end))
                              (setf start end)))))
                      names bytes))
       (with-sensitive-objects (,ks ,@names)
                               ,@body) )))

;; ----------------------------------------------------------

(defun lock-passwd (pwd)
  (unless (passwd-locked pwd)
    (let* ((hid    (passwd-hid pwd))
           (salt   (passwd-salt pwd)))
      (with-kdf-fields (((km  256)
                         (ke  256)
                         (iv  128))
                        (list hid salt))
        (let ((hmac   (ironclad:make-hmac km :sha256))
              (cipher (ironclad:make-cipher :aes
                                            :key  ke
                                            :mode :cbc
                                            :initialization-vector iv))
              (off    (convert-int571-to-80bytes (passwd-off pwd)))
              (kpub   (make-ecc-pt
                       :x (convert-int571-to-80bytes (ecc-pt-x (passwd-kpub pwd)))
                       :y (convert-int571-to-80bytes (ecc-pt-y (passwd-kpub pwd))))))
          (with-sensitive-objects (hmac cipher)
            (safe-update-hmac hmac hid salt off
                              (ecc-pt-x kpub)
                              (ecc-pt-y kpub))
            (let ((dig (ironclad:hmac-digest hmac)))
              (safe-encrypt-in-place cipher off
                                     (ecc-pt-x kpub)
                                     (ecc-pt-y kpub)
                                     dig)
              (setf (passwd-off  pwd)   off
                    (passwd-kpub pwd)   kpub
                    (passwd-hmac pwd)   dig
                    (passwd-locked pwd) t) ))))))
  pwd)
 
(defun encode-passwd (pwd stream)
  (lock-passwd pwd)
  (write-sequences stream
                   (passwd-hid  pwd)
                   (passwd-salt pwd)
                   (passwd-off  pwd)
                   (ecc-pt-x (passwd-kpub pwd))
                   (ecc-pt-y (passwd-kpub pwd))
                   (passwd-hmac pwd) ))

(defun write-passwds-file (pwds)
  (with-open-file (fp (pwd-file)
                      :direction :output
                      :element-type 'ubyte
                      :if-exists    :supersede
                      :if-does-not-exist :create)
    (write-int (length pwds) 2 fp)
    (dolist (pwd pwds)
      (encode-passwd pwd fp))
    (sys:call-system-showing-output
     (format nil "chmod 0600 ~S"
             (namestring (pwd-file))))))

(defstruct pubkey
  hid
  key
  (validated nil))

(defun validate-pubkey (k)
  (unless (pubkey-validated k)
    (ecc-validate-public-key (pubkey-key k))
    (setf (pubkey-validated k) t)))

(defun encode-public-key (key)
  ;; encoding with HMAC since exposed to public
  (validate-pubkey key)
  (let* ((name  (pubkey-hid key))
         (point (ubstream:with-output-to-ubyte-stream (s)
                  (write-point (pubkey-key key) s)))
         (hmac  (let* ((km   (kdf 256 name))
                       (hmac (ironclad:make-hmac km :sha256)))
                  (safe-update-hmac hmac name point)
                  (ironclad:hmac-digest hmac))))
    (encode-bytes-to-base64
     (ubstream:with-output-to-ubyte-stream (s)
       (write-sequences s hmac name point) ))))

(defun write-public-keys-file (pkeys)
  (with-progress-bar ()
    (with-open-file (fp (pubkey-file)
                        :direction :output
                        :if-exists    :supersede
                        :if-does-not-exist :create)
      (format fp
              #>.end
;; Acudora, Inc., Internal Public Key Directory
;;
;; ** DO NOT EDIT **
;; ** FILE IS MACHINE GENERATED AND MAINTAINED **
;; ----------------------------------------------

.end)
      (dolist (key pkeys)
        (let ((data (encode-public-key key)))
          (format fp "pkey:~%")
          (write-sequence data fp)
          (format fp "~%~%"))
        (update-progress 5)) )
    (sys:call-system-showing-output
     (format nil "chmod 0666 ~S"
             (namestring (pubkey-file))))))
  
;; ---------------------------------------------------------------------
;;

(defun unlock-passwd (pwd)
  (when (passwd-locked pwd)
    (let* ((hid    (passwd-hid pwd))
           (salt   (passwd-salt pwd)))
      (with-kdf-fields (((km  256)
                         (ke  256)
                         (iv  128))
                        (list hid salt))
        (let* ((hmac   (ironclad:make-hmac km :sha256))
               (cipher (ironclad:make-cipher :aes
                                             :key ke
                                             :mode :cbc
                                             :initialization-vector iv))
               (off    (passwd-off pwd))
               (kpubx  (ecc-pt-x (passwd-kpub pwd)))
               (kpuby  (ecc-pt-y (passwd-kpub pwd)))
               (m      (passwd-hmac pwd)))
          (with-sensitive-objects (hmac cipher)
            (safe-decrypt-in-place cipher off kpubx kpuby m)
            (safe-update-hmac hmac hid salt off kpubx kpuby)
            (unless (equalp m (ironclad:hmac-digest hmac))
              (error "Passwd for [~A] is damaged" (convert-bytes-to-int hid)))
            (setf (passwd-off  pwd) (convert-bytes-to-int off)
                  (passwd-kpub pwd) (make-ecc-pt
                                     :x (convert-bytes-to-int kpubx)
                                     :y (convert-bytes-to-int kpuby))
                  (passwd-hmac pwd) m
                  (passwd-locked pwd) nil) )))))
  pwd)
    
(defun decode-passwd (stream)
  (let* ((hid    (read-nvector 32 stream))
         (salt   (read-nvector 64 stream))
         (off    (read-nvector 80 stream))
         (kpubx  (read-nvector 80 stream))
         (kpuby  (read-nvector 80 stream))
         (m      (read-nvector 32 stream)))
    (make-passwd
     :hid    hid
     :salt   salt
     :off    off
     :kpub   (make-ecc-pt :x kpubx :y kpuby)
     :hmac   m
     :locked t) ))

(defun read-passwds-file ()
  (when (probe-file (pwd-file))
    (with-open-file (fp (pwd-file)
                        :direction :input
                        :element-type 'ubyte)
      (let ((np (read-int 2 fp)))
        (loop repeat np collect
              (decode-passwd fp)) ))))

(defun read-public-keys-file ()
  (with-progress-bar ()
    (when (probe-file (pubkey-file))
      (let ((damage-seen  nil)
            (pkeys        nil))
        (with-open-file (fp (pubkey-file)
                            :direction :input)
          (labels ((next-line ()
                     (read-line fp nil :eof)))
            
            (um:nlet iter ((keys   nil)
                           (s      (next-line)))
              
              (labels ((check-key (bytes)
                         (if (and (= (length bytes) #.(+ 32 32 72 72))
                                  (let* ((h    (subseq bytes  0 32))
                                         (hid  (subseq bytes 32 64))
                                         (km   (kdf 256 hid))
                                         (hmac (ironclad:make-hmac km :sha256)))
                                    (safe-update-hmac hmac (subseq bytes 32))
                                    (equalp h (ironclad:hmac-digest hmac))))
                             t
                           ;; else
                           (progn
                             (setf damage-seen t)
                             (format t "Public key is invalid")
                             nil)))
                       
                       (add-key (bytes)
                         (if (check-key bytes)
                             (ubstream:with-input-from-ubyte-stream (s bytes :start 32)
                               (let* ((hid  (read-nvector 32 s))
                                      (pt   (read-point s)))
                                 (update-progress 5)
                                 (cons (make-pubkey
                                        :hid hid
                                        :key pt)
                                       keys)))
                           ;; else
                           keys)))
                
                (cond ((eq :eof s)  (setf pkeys (nreverse keys)))
                      
                      ((and (plusp (length s))
                            (char= #\; (char s 0)))
                       (go-iter keys (next-line)))
                      
                      ((and (>= (length s) 5)
                            (string-equal "pkey:" s :end2 5))
                       (let* ((eof  nil)
                              (data (do ((lines nil)
                                         (s (next-line) (next-line)))
                                        ((or (setf eof (eq :EOF s))
                                             (zerop (length s)))
                                         (decode-bytes-from-base64
                                          (apply #'concatenate 'string (nreverse lines))))
                                      (push s lines))))
                         (go-iter (add-key data)
                                  (if eof :EOF (next-line))) ))
                      
                      ((plusp (length s))
                       (setf damage-seen t)
                       (go-iter keys
                                (next-line)))
                      
                      (t (go-iter keys
                                  (next-line)))
                      )))
            ))
        (when damage-seen
          (write-public-keys-file pkeys))
        pkeys)) ))

(def-cached-var public-keys (read-public-keys-file) *public-keys*)
(def-cached-var passwds     (read-passwds-file)     *passwds*)

#|
(defvar *public-keys* nil)

(defun public-keys ()
  (or *public-keys*
      (setf *public-keys* (read-public-keys-file))))

(defvar *passwds*     nil)

(defun passwds ()
  (or *passwds*
      (setf *passwds* (read-passwds-file))))
|#

#|
(progn
  (read-passwds-file)
  (read-public-keys-file))
|#

#|
(setf *passwds* nil
      *public-keys* nil)
|#

;; -----------------------------------------------------------------------------
;;

(defun make-hash-id (id)
  (kdf 256 (string-upcase id)))

(defun find-public-key-for-hash-id (hid)
  (find hid (public-keys)
        :key  #'pubkey-hid
        :test #'equalp))

(defun find-public-key-for-id (id)
  (let ((hid  (make-hash-id id)))
    (find-public-key-for-hash-id hid)))

(defun get-public-key (id)
  (let ((entry (find-public-key-for-id id)))
    (unless entry
      (error "Can't find public key for ~A" id))
    (validate-pubkey entry)
    (pubkey-key entry)))

(defun find-passwd-entry-for-hash-id (hid)
  (find hid (passwds)
        :key  #'passwd-hid
        :test #'equalp))

(defun find-passwd-entry-for-id (id)
  (let ((hid (make-hash-id id)))
    (find-passwd-entry-for-hash-id hid)))

(defun get-passwd-info (id)
  (let ((entry-pwd (find-passwd-entry-for-id id)))
    (unless entry-pwd
      (error "Invalid ID: ~A" id))
    (unlock-passwd entry-pwd)))

(defun create-salt ()
  (kdf 512 (uuid:uuid-to-byte-array (uuid:make-v1-uuid))
       (ctr-drbg *nbits*)))

(defun compute-x0 (id passwd salt)
  (convert-bytes-to-int
   (kdf *nbits* id passwd salt)))

(defun compute-off (x0 kpriv kpub)
  (solve-gf-lagrange 0
                     kpub
                     (list x0 kpriv)))

;; --------------------------------------------------------

(defun make-passwd-entry (id passwd)
  (let ((hid  (make-hash-id id)))
    (when (or (find-passwd-entry-for-hash-id hid)
              (find-public-key-for-hash-id hid))
      (error "ID already in use"))
    (let* ((salt   (create-salt))
           (x0     (compute-x0 hid passwd salt))
           (kpriv  (ctr-drbg-int *nbits*))
           (kpub   (ecc-mul *ecc-gen* kpriv))
           (off    (compute-off x0 kpriv kpub)))
      (push (make-passwd
             :hid  hid
             :salt salt
             :off  off
             :kpub kpub
             :hmac nil
             :locked nil)
            *passwds*)
      (push (make-pubkey
             :hid hid
             :key kpub
             :validated t)
            *public-keys*)
      (sort-keys)) ))

(defun update-pwd-files ()
  (with-progress-bar ()
    (write-passwds-file (passwds))
    (write-public-keys-file (public-keys))))

(defun make-random-name ()
  (let ((s (make-string (random-between 5 15))))
    (loop for ix from 0 below (length s) do
          (setf (char s ix) (code-char (random-between #x20 128)) ))
    s))

(defun obfuscate-pwds ()
  (loop repeat 10 do
        (make-passwd-entry (make-random-name) (make-random-name)))
  (sort-keys))

(defun sort-keys ()
  (setf *passwds* (sort (passwds) #'<
                        :key (lambda (pwd)
                               (convert-bytes-to-int (passwd-hid pwd))))
        *public-keys* (sort (public-keys) #'<
                            :key (lambda (pkey)
                                   (convert-bytes-to-int (pubkey-hid pkey))) )))

;; --------------------------------------------------------

(defun ask-user-for-password ()
  (capi:prompt-for-string "Enter Password"
                          :pane-class 'capi:password-pane))

(defun compute-kpriv (x0 off kpub)
  (let ((kpriv (solve-gf-lagrange x0
                                  (make-crypto-share
                                   :x (ecc-pt-x kpub)
                                   :y (ecc-pt-y kpub))
                                  (make-crypto-share
                                   :x 0
                                   :y off))))
    (unless (ecc-pt= (ecc-mul *ecc-gen* kpriv) kpub)
      (error "Invalid password"))
    kpriv))

(defun get-private-key (id &optional passwd (challenge 0))
  (let* ((pwd (get-passwd-info id))
         (passwd (or passwd
                     (ask-user-for-password))))
    (when passwd
      (let* ((kpub  (passwd-kpub pwd))
             (x0    (compute-x0 (passwd-hid pwd)
                                passwd
                                (passwd-salt pwd)))
             (off   (passwd-off pwd)))
        (prog1
            (solve-gf-lagrange 0
                               (make-crypto-share
                                :x (ecc-pt-x kpub)
                                :y (ecc-pt-y kpub))
                               (make-crypto-share
                                :x challenge
                                :y (compute-kpriv x0 off kpub)))
          (update-progress 10)) ))))
  
;; --------------------------------------------------------

(defun change-passwd (id passwd-old passwd-new)
  (let* ((pwd   (get-passwd-info id))
         (kpub  (passwd-kpub pwd))
         (hid   (passwd-hid pwd))
         (x0    (compute-x0 hid passwd-old (passwd-salt pwd)))
         (kpriv (compute-kpriv x0 (passwd-off pwd) kpub))
         (new-salt (create-salt))
         (new-x0   (compute-x0 hid
                               passwd-new
                               new-salt))
         (new-off  (compute-off new-x0 kpriv kpub)))
    (setf (passwd-salt pwd) new-salt
          (passwd-off  pwd) new-off)
    (write-passwds-file (passwds))))

(defun remove-id (id)
  (let ((entry-pwd (find-passwd-entry-for-id id)))
    (when entry-pwd
      (setf *passwds* (remove entry-pwd (passwds)))
      (write-passwds-file *passwds*)))
  (let ((entry-pkey (find-public-key-for-id id)))
    (when entry-pkey
      (setf *public-keys* (remove entry-pkey (public-keys)))
      (write-public-keys-file *public-keys*))) )
    
(defun read-key-files ()
  (with-progress-bar ()
    (setf *public-keys* (read-public-keys-file)
          *passwds*     (read-passwds-file) )))

#|
(defun init-crypto ()
  (when (mp:list-all-processes)
    (unless *drbg-thread*
      (start-drbg-thread)
      (read-key-files))))

(init-crypto)
|#

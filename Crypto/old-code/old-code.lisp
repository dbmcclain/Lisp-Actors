#|
(defvar *ecc-acudora-locator*
  (LIST (big32 #x02172ECA #x87D14891 #x34BA3D67 #xC896F04F
               #x0CB44727 #xDC406826 #x9EAB4338 #x89886C2B
               #x8A7B731C #x31F905BC #x0CAB8432 #x8C590706
               #x24A02B79 #x1C23E4DB #x68C36DA1 #x3527E141
               #xCE5474BE #x81337863 )
        (big32 #x047CA02E #x8A847DAD #x05BACC60 #x00185FAD
               #xEC893BE9 #xCE2875CC #x04FD9E08 #x496E8A49
               #x80A64E96 #xFC0336C6 #x93CE4981 #x831A9FDF
               #xFA9DD67E #xFDB18DC8 #x0E77C36C #x7F1E0142
               #xECD8F7B0 #xFC5237E2 )))

(defun generate-locator (kpriv &rest pts)
  (let* ((gfx  (make-gf571-lagrange-interpolator `((0 ,kpriv) ,@pts)))
         (xloc (ecc-random-key))
         (yloc (funcall gfx xloc)))
    (list xloc yloc)))

(defun locate-private-key (kloc &rest pts)
  (solve-gf571-lagrange `(,kloc ,@pts)))
|#
#|
(format pt (generate-locator *ecc-acudora-private-key* *ecc-acudora-public-key*))
|#

;; (defvar *locators*    nil)

#|
  ;; probably not needed since file is private
(defun encode-passwd (pwd)
  (destructuring-bind (id hash salt) pwd
    (let* ((data  (ubstream:with-output-to-ubyte-stream (s)
                    (write-sequence hash s)
                    (write-sequence salt s)))
           (hmac  (let* ((km   (kdf 32 id))
                         (hmac (ironclad:make-hmac km :sha256)))
                    (ironclad:update-hmac hmac data)
                    (ironclad:hmac-digest hmac)))
           (hdata (encode-bytes-to-base64
                   (ubstream:with-output-to-ubyte-stream (s)
                     (write-sequence hmac s)
                     (write-sequence data s)) )))
      (values id hdata))))
|#

#|
(defun write-locators-file ()
  (ensure-directories-exist *pwd-dir*)
  (with-open-file (fp (merge-pathnames ".locators" *pwd-dir*)
                      :direction :output
                      :element-type '(unsigned-byte 8)
                      :if-exists    :supersede
                      :if-does-not-exist :create)
    (loenc:serialize *locators* fp)))
|#

#|
(progn
  (setf *passwds*
        (list
         (LIST "Dave"
               (READ-BLOB "54DAE6CEB938491CC538A78D0530C61FCFB35C3595D4590185786B4314EBA8E4")
               (READ-BLOB "F07E942018FE11E1847900254BAF81A0"))
         ))
  (setf *locators*
        (list
         (LIST (READ-BLOB "0579861DC4000D4DC74084C18A1E2674DCD3FB041C1B7C00B2637F2F873CD632")
               #>.end
               D35FFFAB2957D15E2E1BA46E5CBF883C69E64F8293BC518DDFD17A503CC36287
               FB4B6934FAFA7013FFB020E916F575F2A835FBFBA030F30362F631DEF5303AC8
               0F057B5A5FA82E075D7D263833AF4E1D3D9BDDDCEB5FD8179F040CABC7280AB8
               9C98B523E4030436115FFC589AFA4656AE13C628AA1396198BDE96519448DF87
               FBF156D5E91D48B0A01A0B1E5E4FD11749CA7DFCAF4E673FF2BD131253C17630
               DD6F72B079850F1133A034CC18433CC1
               .end)
         ))
  (setf *public-keys*
        (list
         (LIST "Dave"
               (LIST (big32 #x0439DC85 #x74B97155 #x23088941 #xAD2B78F3
                            #x438446AE #xEB86981A #x216AB7DE #x5A2E4AAB
                            #x69C1F4E1 #xD0B71D8C #x3CBF2A7A #xE249A5AC
                            #xEB76E4D8 #x3B9732C9 #x8AD373E6 #xC8C9CD72
                            #xB94357A9 #x11B4AE2D )
                     (big32 #x015FEEF7 #xA6C332CF #x6749FE1F #x6216DB23
                            #xD3A44720 #xDEAC77D8 #xF0332EF2 #x60F5DBFE
                            #xBF8224A5 #xE088246A #x2888213E #xC690F8BE
                            #xF7021FA6 #xA3FB2CDC #x9E36BDCE #x42B265BB
                            #xED6D93C8 #xAA4D2436 )))
         ))
  (write-passwds-file)
  (write-locators-file)
  (write-public-keys-file))
|#

#|
(defun read-locators-file ()
  (with-open-file (fp (merge-pathnames ".locators" *pwd-dir*)
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (setf *locators* (loenc:deserialize fp))))
|#

(defun make-passwd-entry (id passwd)
  (when (find-passwd-entry-for-id id)
    (error "ID already in use"))
  (let* ((salt   (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))
         (hash   (kdf 256 passwd salt))
         (kpriv  (ecc-random-key))
         (kpub   (validate-public-key (ecc-mul *ecc-gen* kpriv)))
         (x0     (convert-bytes-to-int (kdf *nbits* passwd hash (car kpub))))
         (off    (convert-int571-to-int8-array (solve-gf571-lagrange 0 `(,x0 ,kpriv) kpub)))
         #|
         (loc    (generate-locator kpriv kpub))
         (klook  (kdf 256 passwd hash))
         (encloc (let* ((ks     (kdf #.(* 8 (+ 32 16)) (car kpub) (cadr kpub)))
                        (iv     (subseq ks 0 16))
                        (ke     (subseq ks 16))
                        (encloc (ubstream:with-output-to-ubyte-stream (s)
                                  (write-point loc s))) ;; just happens to take multiple of 16-bytes
                        (cipher (ironclad:make-cipher :aes
                                                      :key  ke
                                                      :mode :cbc
                                                      :initialization-vector iv)))
                   (ironclad:encrypt-in-place cipher encloc)
                   encloc))
         |#
         )
    (push (list id hash salt off) *passwds*)
    #|
    (push (list klook (encode-bytes-to-base64 encloc)) *locators*)
    |#
    (push (list id kpub) *public-keys*)
    (write-passwds-file)
    ;; (write-locators-file)
    (write-public-keys-file)))

(defun find-passwd-info (id passwd)
  (let ((entry-pwd (find-passwd-entry-for-id id)))
    (unless entry-pwd
      (error "Invalid ID"))
    (destructuring-bind (id hash salt off) entry-pwd
      (declare (ignore off))
      (unless (equalp hash (kdf 256 passwd salt))
        (error "Invalid Password"))
      (let ((entry-pkey (find id *public-keys*
                              :key #'first
                              :test #'string-equal)))
        (unless entry-pkey
          (error "Missing public key"))
        #|
        (let* ((klook (kdf 256 passwd hash))
               (entry-locator (find klook *locators*
                                :key #'first
                                :test #'equalp)))
          (unless entry-locator
            (error "Missing key info"))
          (list entry-pwd entry-pkey entry-locator) )
        |#
        (list entry-pwd entry-pkey))
      )))

#|
(capi:popup-confirmer (make-instance 'capi:password-pane) "Enter Password")
|#

(defun get-private-key (id &optional passwd (challenge 0))
  (let ((passwd (or passwd
                    (ask-user-for-password))))
    (when passwd
      #|
      (destructuring-bind (entry-pwd entry-pkey entry-locator)
          (find-passwd-info id passwd)
        (declare (ignore entry-pwd))
        (let* ((kloc   (cadr entry-locator))
               (kpub   (cadr entry-pkey))
               (ks     (kdf #.(* 8 (+ 32 16)) (car kpub) (cadr kpub)))
               (iv     (subseq ks 0 16))
               (ke     (subseq ks 16))
               (buf    (decode-bytes-from-base64 kloc))
               (cipher (ironclad:make-cipher :aes
                                             :key  ke
                                             :mode :cbc
                                             :initialization-vector iv)))
          (ironclad:decrypt-in-place cipher buf)
          (let* ((ptloc (ubstream:with-input-from-ubyte-stream (s buf)
                         (read-point s))))
            (gf/ (solve-gf571-lagrange (list kpub ptloc))
                 challenge)) ))
        |#
      (destructuring-bind (entry-pwd entry-pkey)
          (find-passwd-info id passwd)
        (destructuring-bind (id hash salt off) entry-pwd
          (declare (ignore id salt))
          (let* ((kpub  (cadr entry-pkey))
                 (x0    (convert-bytes-to-int (kdf *nbits* passwd hash (car kpub))))
                 (off   (convert-bytes-to-int off)))
            (solve-gf571-lagrange challenge
                                  kpub
                                  (list 0
                                        (solve-gf571-lagrange x0
                                                              (list 0 off)
                                                              kpub))) )))
      )))


(defun change-passwd (id passwd-old passwd-new)
  #|
  (destructuring-bind (entry-pwd entry-pkey entry-locator)
      (find-passwd-info id passwd-old)
    (declare (ignore entry-pkey))
    (let* ((salt   (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))
           (hash   (kdf 256 passwd-new salt))
           (klook  (kdf 256 passwd-new hash)))
      (setf (second entry-pwd) hash
            (third  entry-pwd) salt
            (first  entry-locator) klook)
      (write-passwds-file)
      (write-locators-file)))
  |#
  (destructuring-bind (entry-pwd entry-pkey)
      (find-passwd-info id passwd-old)
    (destructuring-bind (id hash salt off) entry-pwd
        (declare (ignore id salt))
      (let* ((kpub   (cadr entry-pkey))
             (x0     (convert-bytes-to-int (kdf *nbits* passwd-old hash (car kpub))))
             (off    (convert-bytes-to-int off))
             (kpriv  (solve-gf571-lagrange x0
                                           kpub
                                           (list 0 off)))
             (salt   (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))
             (hash   (kdf 256 passwd-new salt))
             (x0     (convert-bytes-to-int (kdf *nbits* passwd-new) (car kpub)))
             (off    (convert-int571-to-int8-array
                      (solve-gf571-lagrange 0
                                            (list x0 kpriv)
                                            kpub))))
        (setf (second entry-pwd) hash
              (third  entry-pwd) salt
              (fourth entry-pwd) off)
        (write-passwds-file))))
    )

;; -----------------------------------------------------------------------------
;;

#|
(defun ecc-encrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (let ((key (ecc-mul *ecc-acudora-public-key* (make-key-from-plaintext key))))
    (with-open-file (fin fname-in
                         :direction :input
                         :element-type '(unsigned-byte 8))
      (with-open-file (fout fname-out
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
        
        (destructuring-bind (k1 k2) (generate-shares 2 key 1)
          (destructuring-bind (x (ptx pty)) k1
            (write-sequence (convert-int571-to-int8-array x)   fout)
            (write-sequence (convert-int571-to-int8-array ptx) fout)
            (write-sequence (convert-int571-to-int8-array pty) fout)
            (aes-encrypt-file fin fout fname-in (car key)))
          k2)))))

(defun ecc-decrypt-file (fname-in fname-out key)
  (check-paths-not-equal fname-in fname-out)
  (with-open-file (fin fname-in
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (with-open-file (fout fname-out
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
      (let* ((x   (make-array 72 :element-type '(unsigned-byte 8)))
             (ptx (make-array 72 :element-type '(unsigned-byte 8)))
             (pty (make-array 72 :element-type '(unsigned-byte 8))))
        (read-sequence x   fin)
        (read-sequence ptx fin)
        (read-sequence pty fin)
        (let* ((k2 (list (convert-vector-to-int x)
                         (list (convert-vector-to-int ptx)
                               (convert-vector-to-int pty))))
               (k  (solve-lagrange (list k2 key))))
          (aes-decrypt-file fin fout (car k)) )))))
|#
#|
(directory "./")
(let* ((dir (pathname "./VTuning/crypto/tools/"))
       (f1  (merge-pathnames "secret-sharing.lisp" dir))
       (f2  (merge-pathnames "plaintextjunk" dir))
       (f3  (merge-pathnames "cryptojunk"   dir))
       (f4  (merge-pathnames "uncryptojunk" dir)))
  (sys:call-system-showing-output (format nil "cp ~S ~S"
                                          (namestring f1)
                                          (namestring f2)))
  (aes-encrypt-file f2 f3 "ic181x20")
  (aes-decrypt-file f3 f4 "ic181x20")
  (sys:call-system-showing-output (format nil "diff ~S ~S"
                                          (namestring f2)
                                          (namestring f4))))
                               
(labels ((prep (key)
           (convert-int571-to-int8-array (car key))))
  (let* ((dir (pathname "./VTuning/crypto/"))
         (f1  (merge-pathnames "encrypted-strings" dir))
         (f2  (merge-pathnames "cryptojunk" dir))
         (f3  (merge-pathnames "uncryptojunk" dir))
         (key (prep (ecc-mul *ecc-gen* *progkey*))))
    #||#
    (aes-encrypt-file *encrypted-strings-path* f2 key)
    (aes-decrypt-file f2 f3 key)
    (sys:call-system-showing-output (format nil "diff ~S ~S"
                                            (namestring f1)
                                            (namestring f3)))
    (sys:call-system-showing-output (format nil "diff ~S ~S"
                                            (namestring
                                             (merge-pathnames
                                              "scoffs" dir))
                                            (namestring f2)))
    #||#
    (aes-decrypt-file (merge-pathnames
                       "scoffs" dir)
                      (merge-pathnames
                       "junk" dir)
                      key)))
                               
|#

#|
(defun pbkdf2 (pwd nbits)
  (let* ((niter (random-between 1000 10000))
         (salt  (convert-int-to-nbytes (random-between 0    #.(ash 1 128)))
         (ngrps (ceiling nbits 32))
         (nrem  (- nbits (* (1- ngrps) 32)))
         (ts    #()))
    (labels ((f (i)
               (let ((ans (make-array 32
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0))
                     (dig (ironclad:make-digest
                 
                 (um:nlet iter ((ctr 0))
               (let ((dig (ironclad:make-digest :sha256)))
    
  ... TBD ...
|#


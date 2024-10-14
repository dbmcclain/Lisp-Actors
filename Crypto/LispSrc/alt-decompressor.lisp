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

(defvar *live-decryption* nil)
(defvar *auhost-path*     nil)

(defun auhost-path ()
  (or *live-decryption*
      *auhost-path*))

(defun image-directory ()
  (make-pathname
   :directory (pathname-directory (lw:lisp-image-name))))

(defun exec-directory ()
  (make-pathname
   :directory (pathname-directory (auhost-path))))

(defun host-file (fname)
  (namestring
   (merge-pathnames fname (exec-directory))))

(defun delivered-alt-decompressor ()
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
            ;; (setf sys:*stack-overflow-behaviour* nil)
            
            (let* ((*live-decryption* exename)
                   ;; (imgdir (directory (image-directory)))
                   (excdir (directory (exec-directory))))
              ;;
              ;; -- DM/Acudora 02/13 ---
              ;; removing the imgdir check slightly softens our security,
              ;; but it permits us to run a gzexe'd binary which both saves space
              ;; and makes it slightly more secure against static analysis
              ;;
              (unless (and (= 8 (length excdir))
                           ;; (equalp imgdir excdir)
                           )
                ;; check for impostors
                (error "Invalid directory contents"))
              
              ;; insert action here...
              (let* ((tmp1    (namestring (hcl:create-temp-file)))
                     ;; (tmp1.gz (concatenate 'string tmp1 ".gz"))
                     )
                (unwind-protect
                    (let* ((fast-decryptor    (host-file ".T{8ECB290E-5E32-11E1-9D35-C82A14446EA7}"))
                           (license-decryptor (host-file ".T{EF2D1827-5E34-11E1-9D35-C82A14446EA7}"))
                           (slow-decryptor    (host-file ".T{EF2D1828-5E34-11E1-9D35-C82A14446EA7}"))
                           (payload           (host-file ".T{EF2D1829-5E34-11E1-9D35-C82A14446EA7}")))

                      ;; using gzexe'd binaries... let it unpack itself...
                      ;; (print "Hello from alt-decompressor")
                      (sys:call-system `(,fast-decryptor
                                         "DEC"
                                         ,license-decryptor
                                         ,tmp1
                                         ,(concatenate 'string "file:" payload)
                                         ))
                      (sys:call-system `("/bin/chmod" "u+x" ,tmp1))
                      (sys:call-system  (list* tmp1 (cdr sys:*line-arguments-list*)))

                      (sys:call-system `(,fast-decryptor
                                         "DEC"
                                         ,slow-decryptor
                                         ,tmp1
                                         ,(concatenate 'string "file:" payload)
                                         ))
                      (sys:call-system `("/bin/chmod" "u+x" ,tmp1))
                      (sys:call-system  (list* tmp1 (cdr sys:*line-arguments-list*)))


                      #|
                      ;; this works just fine now...
                      (sys:call-system `(,fast-decryptor
                                         "DEC"
                                         ,slow-decryptor
                                         ,tmp1.gz
                                         ,(concatenate 'string "file:" fast-decryptor)
                                         ))
                      (sys:call-system `("/usr/bin/gunzip" "-f" ,tmp1.gz))
                      (sys:call-system `("/bin/chmod" "u+x" ,tmp1))
                      (sys:call-system  (list* tmp1 (cdr sys:*line-arguments-list*)))
                      |#
                      
                      #|
                      ;; this works against the bare uncompressed, unecrypted decryptor
                      (sys:call-system (list* slow-decryptor (cdr sys:*line-arguments-list*)))
                      |#

                      #|
                      ;; this works against the compressed decryptor
                      (sys:call-system `("/bin/cp" "-f" ,slow-decryptor ,tmp1.gz))
                      (sys:call-system `("/usr/bin/gunzip" "-f" ,tmp1.gz))
                      (sys:call-system `("/bin/chmod" "u+x" ,tmp1))
                      (sys:call-system  (list* tmp1 (cdr sys:*line-arguments-list*)))
                      |#
                      ;; (format *error-output* "calling decryptor: ~A" tmp1)
                      )
                  (delete-file tmp1)))
              )))
      
      (error (err)
        (setf status 1)
        (format *error-output* "~A~%" err)))
    (lw:quit :status status)))


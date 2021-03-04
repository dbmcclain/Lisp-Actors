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

(defun get-ks-table ()
  (or (prevo:get-root-object :acudora-key-server)
      (prevo:add-root-object :acudora-key-server
                             (make-hash-table :test 'equalp))))
  
(def-cached-var ks-tbl (get-ks-table))

#|
(defvar *ks-tbl*  nil)

(defun init-key-server ()
  (setf *ks-tbl*
        (or (prevo:get-root-object :acudora-key-server)
            (prevo:add-root-object :acudora-key-server
                                   (make-hash-table :test 'equalp)))))
|#

(defun get-value (key &optional default)
  (gethash key (ks-tbl) default))

(defun set-value (key val)
  (prevo:mutate-object (ks-tbl) key val))

(defsetf get-value (key) (val)
  `(set-value ,key ,val))

(defun remove-key (key)
  (remhash key (ks-tbl))
  (prevo:add-root-object :acudora-key-server (ks-tbl)))

(defun remove-keys (&rest key-list)
  (dolist (key key-list)
    (remhash key (ks-tbl)))
  (prevo:add-root-object :acudora-key-server (ks-tbl)))

;; ---------------------------------------------------

(defun start-key-server ()
  (setf sys:*stack-overflow-behaviour* nil)
  ;; (init-crypto)
  (com.sd.butterfly.int:lw-start-butterfly)
  (bfly:init-server)
  ;; (init-key-server)
  ;; (bfly.name-server::start :name-server)
  (let* ((launcher (make-service-launcher :acudora-key-server))
         (pid      (funcall launcher)))
    #||#(com.sd.butterfly.ka:keep-alive pid launcher)#||#
    pid))

(defun make-service-launcher (name)
  (lambda ()
    (bfly:make-service (:bfly
                        :register name
                        :trap-exits-p t)
      ()
      (:get-decryption-key (ckeys my-id)
         (get-ibe-decryption-key ckeys my-id))
      (:get-value (key)
       (get-value key))
      (:set-value (key val)
       (setf (get-value key) val))
      (:remove-key (key)
       (remove-key key))
      (:remove-keys (&rest keys)
       (apply #'remove-keys keys))
      (:get-values ()
       (ks-tbl))
      )))

(defun valid-id (my-id to-id)
  (let ((to-ids (um:split-string to-id
                                 :delims (list #\, #\; #\Space #\Return #\Page #\VT #\Newline #\Tab))))
    (multiple-value-bind (to-ids expiry)
        (get-expiry to-ids)
      (if (or (null expiry)
              (< (get-universal-time) expiry))
          (or (member my-id to-ids
                      :test #'equalp)
              (belongs-to my-id to-ids))
        (error "Message expired")) )))

(defun belongs-to (my-id to-ids)
  (when to-ids
    (let* ((key  (car to-ids))
           (mems (um:mklist (get-value key))))
      (or (member my-id mems
                  :test 'equalp)
          (belongs-to my-id (cdr to-ids))) )))

(defun get-expiry (ids)
  (let* ((pos (or (position "expires" ids
                            :test 'equalp)
                  (position "until" ids
                            :test 'equalp))))
    (if (and pos
             (< (1+ pos) (length ids)))
        (let* ((expiry (elt ids (1+ pos))))
          (values (remove expiry
                          (remove (elt ids pos) ids))
                  (convert-to-date expiry)))
      ids)))

(defun convert-to-date (str)
  (labels ((year (yr)
             (if (< yr 100)
                 (+ yr 2000)
               yr)))
    (let ((dmy (um:split-string str :delims '(#\/ #\- #\.))))
      (case (length dmy)
        (1 (let ((yr (read-from-string (car dmy))))
             (when (integerp yr)
               (encode-universal-time 0 0 0 1 1 (year yr))) ))
        
        (2 (let ((yr (read-from-string (cadr dmy)))
                 (mo (read-from-string (car dmy))))
             (when (and (integerp yr)
                        (integerp mo))
               (encode-universal-time 0 0 0 1 mo (year yr)))))
        
        (3 (let ((day (read-from-string (second dmy)))
                 (mo  (read-from-string (first dmy)))
                 (yr  (read-from-string (third dmy))))
             (when (and (integerp day)
                        (integerp mo)
                        (integerp yr))
               (encode-universal-time 0 0 0 day mo (year yr)))))
        
        (t nil)))))

(defun get-ibe-decryption-key (ckeys my-id)
  (handler-case
      (let ((kpub  (get-public-key my-id)))
        (destructuring-bind (ks to-id)
            (dh-decrypt ckeys *ecc-acudora-private-key*)
          (with-sensitive-objects (ks)
            (if (valid-id my-id to-id)
                (values (dh-encrypt ks kpub) t)
              ;; else
              (error "Not intended recipient"))) ))
    (error (err)
      (values nil err))))

#|
(com.sd.butterfly.bb:call-sync "acudora-key-server@10.0.1.200"
                               :set-value "programmers"
                               '("david@acudora.com"
                                 "daniel@acudora.com"))

(com.sd.butterfly.bb:call-sync "acudora-key-server@10.0.1.200"
                               :get-value "programmers")

(inspect (com.sd.butterfly.bb:call-sync "acudora-key-server@10.0.1.200" :get-values))
|#

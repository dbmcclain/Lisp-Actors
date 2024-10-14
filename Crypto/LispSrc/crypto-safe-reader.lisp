;; crypto-safe-reader.lisp -- safe read back of persisted tagged crypto items
;;
;; DM/Emotiq 08/18
;; ---------------------------------------------------------
#|
Copyright (c) 2018 Emotiq AG

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

(in-package :pbc)

;; ---------------------------------------------------

(defclass address (base58)
  ())

(defun addr (obj)
  (change-class (base58 obj) 'address))

(defun addr-str (addr)
  (base58-str addr))
  
;; ---------------------------------------------------

(defun make-pkey (hex-str)
  (make-instance 'public-key
                 :val (bev-vec hex-str)))

(defun make-skey (hex-str)
  (make-instance 'secret-key
                 :val (bev-vec hex-str)))

(defun make-sig (hex-str)
  (make-instance 'signature
                 :val (bev-vec hex-str)))

(defun make-addr (base58-str)
  (make-instance 'address
                 :str  base58-str))

;; ---------------------------------------------------

(defmethod print-object ((obj public-key) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream "~&(make-pkey ~S)" (hex-str obj)))
    (call-next-method)))

(defmethod print-object ((obj secret-key) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream "~&(make-skey ~S)" (hex-str obj)))
    (call-next-method)))

(defmethod print-object ((obj signature) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream "~&(make-sig ~S)" (hex-str obj)))
    (call-next-method)))

(defmethod print-object ((addr address) out-stream)
  (if *print-readably*
      (with-standard-io-syntax
        (format out-stream "(make-addr ~S)" (base58-str addr)))
    (call-next-method)))


(defun read-safely (in-stream)
  (let ((*read-eval* nil)
        (*package*   (find-package :pbc)))
    (labels ((eval-it (inp)
               (cond ((atom inp) inp)

                     ((and (eql (car inp) 'make-addr)
                           (stringp (cadr inp))
                           (every 'validate-base58-string (cadr inp))
                           (null (cddr inp)))
                      (make-addr (cadr inp)))
                     
                     ((and (member (car inp) '(make-pkey make-skey make-sig))
                           (stringp (cadr inp))
                           (every (um:rcurry 'digit-char-p 16) (cadr inp))
                           (null (cddr inp)))
                      ;; one of (MAKE-PKEY str), (MAKE-SKEY str),
                      ;; (MAKE-SIG str), with hex string
                      (funcall (car inp) (cadr inp)))

                     (t
                      ;; allow everything else to be a list of one of
                      ;; the above, or NIL
                      (mapcar #'eval-it inp))
                     )))
      (eval-it (read in-stream nil nil nil))
      )))

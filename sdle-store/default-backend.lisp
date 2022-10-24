;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; The sdle-store backend. 
(in-package :sdle-store)

;; -----------------------------------------------------------
;;  Type code constants

(defconstant +referrer-code+            (register-code  1 'referrer))
(defconstant +special-float-code+       (register-code  2 'special-float))
(defconstant +unicode-string-code+      (register-code  3 'unicode-string))
(defconstant +positive-integer-code+    (register-code  4 'positive-integer))
(defconstant +negative-integer-code+    (register-code  5 'negative-integer))
(defconstant +simple-string-code+       (register-code  6 'simple-string))
(defconstant +float-code+               (register-code  7 'float))
(defconstant +ratio-code+               (register-code  8 'ratio))
(defconstant +character-code+           (register-code  9 'character))
(defconstant +complex-code+             (register-code 10 'complex))
(defconstant +symbol-code+              (register-code 11 'symbol))
(defconstant +cons-code+                (register-code 12 'cons))
(defconstant +pathname-code+            (register-code 13 'pathname))
(defconstant +hash-table-code+          (register-code 14 'hash-table))
(defconstant +standard-object-code+     (register-code 15 'standard-object))
(defconstant +condition-code+           (register-code 16 'condition))
(defconstant +structure-object-code+    (register-code 17 'structure-object))
(defconstant +standard-class-code+      (register-code 18 'standard-class))
(defconstant +built-in-class-code+      (register-code 19 'built-in-class))
(defconstant +array-code+               (register-code 20 'array))
(defconstant +simple-vector-code+       (register-code 21 'simple-vector))
(defconstant +package-code+             (register-code 22 'package))
(defconstant +simple-byte-vector-code+  (register-code 23 'simple-byte-vector))
(defconstant +keyword-code+             (register-code 24 'keyword))

(defconstant +built-in-function-code+   (register-code 25 'built-in-function))
(defconstant +function-code+            (register-code 26 'function nil))
(defconstant +gf-code+                  (register-code 27 'generic-function nil))

;; Used by SBCL and CMUCL.
(defconstant +structure-class-code+     (register-code 28 'structure-class))
(defconstant +struct-def-code+          (register-code 29 'struct-def))

(defconstant +gensym-code+              (register-code 30 'gensym))

(defconstant +unicode-base-string-code+ (register-code 31 'unicode-base-string))
(defconstant +simple-base-string-code+  (register-code 32 'simple-base-string))

(defconstant +nil-code+                 (register-code 33 'nil-object))
(defconstant +t-code+                   (register-code 34 't-object))
(defconstant +0-code+                   (register-code 35 '0-object))
(defconstant +1-code+                   (register-code 36 '1-object))
(defconstant +2-code+                   (register-code 37 '2-object))
(defconstant +-1-code+                  (register-code 38 '-1-object))
(defconstant +proper-list-code+         (register-code 39 'proper-list))
(defconstant +unbound-slot-code+        (register-code 40 'unbound-slot))
(defconstant +rawbytes-code+            (register-code 41 'rawbytes))
(defconstant +tree-code+                (register-code 42 'tree))

(um:defconstant+ $unbound-marker #(:unbound-marker))

(defmethod before-store (obj)
  obj)

(defmethod after-retrieve (obj)
  obj)

;; setups for type code mapping
(defun output-type-code (code stream)
  (declare (type ub32 code))
  (store-count code stream))

(declaim (inline read-type-code))
(defun read-type-code (stream)
  (read-count stream))

(defmethod referrerp ((backend sdle-store) (reader t))
  ;; (declare (xoptimize speed (safety 0) (space 0) (debug 0)))
  (eql reader 'referrer))

(defparameter *restorers* (restorers (find-backend 'sdle-store)))

;; get-next-reader needs to return a symbol which will be used by the
;; backend to lookup the function that was defined by
;; defrestore-sdle-store to restore it, or nil if not found. 
(defun lookup-code (code)
  ;; (declare (xoptimize speed (safety 0) (space 0) (debug 0)))
  (gethash code *restorers*))

(defmethod get-next-reader ((backend sdle-store) (stream stream))
  ;; (declare (xoptimize speed (safety 0) (space 0) (debug 0)))
  (let ((type-code (read-type-code stream)))
    (or (lookup-code type-code)
        (error "Type code ~A is not registered." type-code))))


;; referrer, Required for a resolving backend
(defmethod store-referrer ((backend sdle-store) (ref t) (stream t))
  (output-type-code +referrer-code+ stream)
  (store-count ref stream))

(defrestore-sdle-store (referrer stream)
  (make-referrer :val (read-count stream)))



;; We need this for circularity stuff.
(defmethod int-or-char-p ((backend sdle-store) (type symbol))
  ;; (declare (xoptimize speed (safety 0) (space 0) (debug 0)))
  (member type '(positive-integer
                 negative-integer
                 character
                 nil-object
                 t-object
                 0-object
                 1-object
                 2-object
                 -1-object
                 unbound-slot)
          ))

;; --------------------------------------------------------------
;; counts = implicit unsigned integers as 7-bit encodings
;; All but last byte has MSB set.

(defun store-count (count stream)
  (declare (type integer count))
  (declare (optimize (speed 3) (debug 0)))
  (cond
   ((< count 128)
    (write-byte count stream))
   (t (let ((nb (ceiling (integer-length count) 7)))
        (declare (type (unsigned-byte *) nb))
        (um:nlet iter ((ix (* 7 (1- nb))))
          (declare (type (unsigned-byte *) ix))
          (if (zerop ix)
              (write-byte (ldb (byte 7 0) count) stream)
            (progn
              (write-byte (logior #x80
                                  (ldb (byte 7 ix) count))
                          stream)
              (go-iter (- ix 7))) )) )) ))

(defun read-count (stream)
  (declare (optimize (speed 3) (debug 0)))
  (let ((x (read-byte stream)))
    (declare (type (unsigned-byte 8) x))
    (cond
     ((< x 128) x)
     (t (um:nlet iter ((val (logand x #x7f)))
          (declare (type (unsigned-byte *) val))
          (let* ((x        (read-byte stream))
                 (next-val (logior (logand x #x7f) (ash val 7))))
            (declare (type (unsigned-byte 8) x)
                     (type (unsigned-byte *) next-val))
            (if (logbitp 7 x)
                (go-iter next-val)
              next-val) ))) )))

#|
;; --------------------------------------------------------------
;; counts = implicit unsigned integers
;; 0 - 128 as itself
;; otherwise (NB | #x80) XH ... XL, for NB bytes
;; If NB >= 127, then #xFF (nbcount) XH ... XL where NB is itself encoded
;; as a count

(defun store-count (count stream)
  (declare (type integer count))
  (declare (optimize (speed 3) (debug 0)))
  (cond
   ((<= count 128)
    (write-byte count stream))

  ((<= count 255)
   (write-byte #x81 stream)
   (write-byte count stream))

  (t
   (let* ((nb (ceiling (integer-length count) 8)))
     (declare (fixnum nb))
     (if (< nb 127)
         (write-byte (logior #x80 nb) stream)
       ;; else
       (progn
         (write-byte #xff stream)
         (store-count nb stream)))
     
     (um:perform iter ((ix (* 8 (1- nb))))
       (write-byte (ldb (byte 8 ix) count) stream)
       (if (> ix 0)
           (iter (- ix 8))))
     ))
  ))

(defun read-count (stream)
  (declare (optimize (speed 3) (debug 0)))
  (let ((x (read-byte stream)))
    (declare (type (unsigned-byte 8) x))

    (cond
     ((<= x 128) x)
     ((= x #x81) (read-byte stream))
     (t
      (let* ((nb (if (= x #xff)
                     (read-count stream)
                   ;; else
                   (logand x #x7f))))
        (um:perform iter ((ix (* 8 (1- nb)))
                          (val 0))
          (if (< ix 0)
              val
            (iter (- ix 8) (dpb (read-byte stream)
                                (byte 8 ix)
                                val))
            ))))
     )))
|#

;; --------------------------------------------------------------

(defstore-sdle-store (obj integer stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (declare (type integer obj) (stream stream)
           ;; (xoptimize speed)
	   )
  (output-type-code (if (minusp obj)
                        +negative-integer-code+
                      +positive-integer-code+)
                    stream)
  (store-count (abs obj) stream))

(defrestore-sdle-store (positive-integer stream)
  ;; (declare (xoptimize speed))
  (read-count stream))

(defrestore-sdle-store (negative-integer stream)
  ;; (declare (xoptimize speed))
  (- (read-count stream)))

;; --------------------------------------------------------------
;; Floats (*special-floats* are setup in the custom.lisp files)

(defconstant +short-float-inf+ 0)
(defconstant +short-float-neg-inf+ 1)
(defconstant +short-float-nan+ 2)

(defconstant +single-float-inf+ 3)
(defconstant +single-float-neg-inf+ 4)
(defconstant +single-float-nan+ 5)

(defconstant +double-float-inf+ 6)
(defconstant +double-float-neg-inf+ 7)
(defconstant +double-float-nan+ 8)

(defconstant +long-float-inf+ 9)
(defconstant +long-float-neg-inf+ 10)
(defconstant +long-float-nan+ 11)

(defvar *special-floats* nil)

;; Implementations are to provide an implementation for the create-float-value
;; function

#-(or lispworks sbcl)
(defun create-float-values (value &rest codes)
  "Returns a alist of special float to float code mappings."
  (declare (ignore value codes))
  nil)

(defun setup-special-floats ()
  (setf *special-floats*
        (nconc (create-float-values most-negative-short-float +short-float-inf+
                                    +short-float-neg-inf+ +short-float-nan+)
               (create-float-values most-negative-single-float +single-float-inf+
                                    +single-float-neg-inf+ +single-float-nan+)
               (create-float-values most-negative-double-float +double-float-inf+
                                    +double-float-neg-inf+ +double-float-nan+)
               (create-float-values most-negative-long-float +long-float-inf+
                                    +long-float-neg-inf+ +long-float-nan+))))

(defstore-sdle-store (obj float stream)
  ;; (declare (xoptimize speed))
  (block body
    (let (significand exponent sign)
      (handler-bind (((or simple-error
                          arithmetic-error
                          type-error)
                      #'(lambda (err)
                          (declare (ignore err))
                          (when-let (type (cdr (assoc obj *special-floats*)))
                            (output-type-code +special-float-code+ stream)
                            (write-byte type stream)
                            (return-from body)))))
        (multiple-value-setq (significand exponent sign)
            (integer-decode-float obj))
        (output-type-code +float-code+ stream)
        (write-byte (float-type obj) stream)
        (store-object (if (minusp sign) (- significand) significand) stream)
        (store-count (float-radix obj) stream)
        (store-object exponent stream) ))))

(defrestore-sdle-store (float stream)
  (let ((type  (get-float-type (read-byte stream)))
        (mant  (restore-object stream))
        (radix (read-count stream))
        (expon (restore-object stream)))
    (if (= radix (float-radix type))
        (scale-float (float mant type) expon)
      (* (float mant type) (expt (float radix type) expon)) )))

(defrestore-sdle-store (special-float stream)
  (or (car (rassoc (read-byte stream) *special-floats*))
      (restore-error "Float ~S is not a valid special float.")))

;; -------------------------------------------------------
;; ratio
(defstore-sdle-store (obj ratio stream)
  (output-type-code +ratio-code+ stream)
  (store-object (numerator obj) stream)
  (store-object (denominator obj) stream))

(defrestore-sdle-store (ratio stream)
  (/ (the integer (restore-object stream))
     (the integer (restore-object stream))))

;; chars
(defstore-sdle-store (obj character stream)
  (output-type-code +character-code+ stream)    
  (store-object (char-code obj) stream))

(defrestore-sdle-store (character stream)
  (code-char (restore-object stream)))

;; complex
(defstore-sdle-store (obj complex stream)
  (output-type-code +complex-code+ stream)    
  (store-object (realpart obj) stream)
  (store-object (imagpart obj) stream))

(defrestore-sdle-store (complex stream)
  (complex (restore-object stream)
           (restore-object stream)))

;; -------------------------------------------------------

(defmethod backend-store-object ((backend resolving-backend) (obj (eql 0)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql 1)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql 2)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql -1)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql nil)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql t)) (place t))
  (internal-store-object backend obj place))
       
(defmethod backend-store-object ((backend resolving-backend) (obj (eql $unbound-marker))
                                 (place t))
  (internal-store-object backend obj place))
       

(defstore-sdle-store (obj (eql 0) stream)
  (output-type-code +0-code+ stream))

(defrestore-sdle-store (0-object stream)
  (declare (ignore stream))
  0)


(defstore-sdle-store (obj (eql 1) stream)
  (output-type-code +1-code+ stream))

(defrestore-sdle-store (1-object stream)
  (declare (ignore stream))
  1)


(defstore-sdle-store (obj (eql 2) stream)
  (output-type-code +2-code+ stream))

(defrestore-sdle-store (2-object stream)
  (declare (ignore stream))
  2)


(defstore-sdle-store (obj (eql -1) stream)
  (output-type-code +-1-code+ stream))

(defrestore-sdle-store (-1-object stream)
  (declare (ignore stream))
  -1)


(defstore-sdle-store (obj (eql $unbound-marker) stream)
  (output-type-code +unbound-slot-code+ stream))

(defrestore-sdle-store (unbound-slot stream)
  (declare (ignore stream))
  $unbound-marker)


;; -------------------------------------------------------

;; symbols
(defstore-sdle-store (obj symbol stream)
  ;; (declare (xoptimize speed))
  (cond ((keywordp obj)
         (output-type-code +keyword-code+ stream)
         (store-object (symbol-name obj) stream))

        ((symbol-package obj)
         (output-type-code +symbol-code+ stream)
         (store-object (package-name (symbol-package obj))
                       stream)
         (store-object (symbol-name obj) stream))

        ;; Symbols with no home package 
        (t 
           (output-type-code +gensym-code+ stream)
           (store-object (symbol-name obj) stream))))

(defrestore-sdle-store (symbol stream)
  (let* ((pkg-name (restore-object stream))
         (sym      (restore-object stream))
         ;; (pkg      (find-package pkg-name)) ;; DM/RAL 07/09
         (pkg      (find-or-make-package pkg-name))
         )
    (values (intern (string sym) pkg)) ))

;; added DM/RAL 07/09
(defun find-or-make-package (pkg-name)
  (or (find-package pkg-name)
      (make-package pkg-name :use '(:COMMON-LISP))))

(defrestore-sdle-store (gensym stream)
  (make-symbol (restore-object stream)))

(defrestore-sdle-store (keyword stream)
  (values (intern (string (restore-object stream)) :keyword)))

;; -------------------------------------------------------

(defstore-sdle-store (obj (eql nil) stream)
  (output-type-code +nil-code+ stream))

(defrestore-sdle-store (nil-object stream)
  (declare (ignore stream))
  nil)


(defstore-sdle-store (obj (eql t) stream)
  (output-type-code +t-code+ stream))

(defrestore-sdle-store (t-object stream)
  (declare (ignore stream))
  t)

;; -------------------------------------------------------

;; Lists
(defun dump-proper-list (list length stream)
  (output-type-code +proper-list-code+ stream)
  (store-count length stream)
  (loop repeat length
        for x in list do
        (store-object x stream)))

(defun dump-general-list (list length last stream)
  (declare ;; (xoptimize speed (safety 1) (debug 0))
           (type cons list))
  (output-type-code +cons-code+ stream)
  (store-count length stream)
  (loop repeat length 
        for x on list do
        (store-object (car x) stream))
  (store-object last stream))

(defun restore-proper-list (stream)
  (let (ret)
    (setf ret
          (loop repeat (read-count stream)
                for pos from 0
                collect
                (let ((obj (restore-object stream)))
                  (when (and *check-for-circs*
                             (referrer-p obj))
                    (let ((pos pos))
                      (push (delay
                             (setf (nth pos ret)
                                   (referred-value obj *restored-values*)))
                            *need-to-fix*)))
                  obj))
          )))

(defun restore-general-list (stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (let* ((conses (read-count stream))
         (ret ())
         (tail ret))
    (dotimes (x conses)
      (let ((obj (restore-object stream)))
        ;; we can't use setting here since we wan't to
        ;; be fairly efficient when adding objects to the
        ;; end of the list.
        (when (and *check-for-circs*
                   (referrer-p obj))
          (let ((x x))
            (push (delay
                      (setf (nth x ret)
                            (referred-value obj *restored-values*)))
                  *need-to-fix*)))
        (if ret
            (setf (cdr tail) (list obj) 
                  tail (cdr tail))
            (setf ret (list obj)
                  tail (last ret)))))
    (let ((last1 (restore-object stream)))
      ;; and check for the last possible circularity
      (if (and *check-for-circs*
               (referrer-p last1))
          (push (delay
                    (setf (cdr tail)
                          (referred-value last1 *restored-values*)))
                *need-to-fix*)
          (setf (cdr tail) last1)))
    ret))

#|
(defstore-sdle-store (list cons stream)
  (multiple-value-bind (length last) (safe-length list)
    (if last
        (dump-general-list list length last stream)
      (dump-proper-list list length stream))))
|#
#|
(defun dump-tree (list stream)
  (um:nlet iter ((lst  list)
                 (len  1))
    (let ((tl (cdr lst)))
      (cond ((null tl)
             (dump-proper-list list len stream))
            ((get-ref tl)
             (dump-general-list list len tl stream))
            ((consp tl)
             (go-iter tl (1+ len)))
            (t
             (dump-general-list list len tl stream))
            ))
    ))
|#

(defun dump-tree (list stream)
  (output-type-code +tree-code+ stream)
  (store-object (car list) stream)
  (store-object (cdr list) stream))

(defun restore-tree (stream)
  (let* ((hd (restore-object stream))
         (tl (restore-object stream)))
    (cons hd tl)))

(defstore-sdle-store (list cons stream)
  (dump-tree list stream))

(defrestore-sdle-store (cons stream)
  (restore-general-list stream))

(defrestore-sdle-store (proper-list stream)
  (restore-proper-list stream))

(defrestore-sdle-store (tree stream)
  (restore-tree stream))


;; -------------------------------------------------------

;; pathnames
(defstore-sdle-store (obj pathname stream)
  (output-type-code +pathname-code+ stream)
  (store-object #-sbcl (pathname-host obj)
                #+sbcl (host-namestring obj) stream)
  (store-object (pathname-device obj) stream)
  (store-object (pathname-directory obj) stream)
  (store-object (pathname-name obj) stream)
  (store-object (pathname-type obj) stream)
  (store-object (pathname-version obj) stream))

(defrestore-sdle-store (pathname stream)
  (make-pathname
   :host (restore-object stream)
   :device (restore-object stream)
   :directory (restore-object stream)
   :name (restore-object stream)
   :type (restore-object stream)
   :version (restore-object stream)))


;; -------------------------------------------------------

;; hash tables
(defstore-sdle-store (obj hash-table stream)
  ;; (declare (xoptimize speed))
  (output-type-code +hash-table-code+ stream)    
  (store-object (hash-table-rehash-size obj) stream)
  (store-object (hash-table-rehash-threshold obj) stream)
  (store-count  (hash-table-size obj) stream)
  (store-object (hash-table-test obj) stream)
  (store-count  (hash-table-count obj) stream)
  (loop for key being the hash-keys of obj
        using (hash-value value) do
        (store-object key stream)
        (store-object value stream)))

(defrestore-sdle-store (hash-table stream)
  (let ((rehash-size (restore-object stream))
        (rehash-threshold (restore-object stream))
        (size (read-count stream))
        (test (restore-object stream))
        (count (read-count stream)))
    (declare (type integer count size))
    (let ((hash (make-hash-table :test test
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold
                                 :size size)))
      (resolving-object (x hash)
        (loop repeat count do
              ;; Unfortunately we can't use the normal setting here
              ;; since there could be a circularity in the key
              ;; and we need to make sure that both objects are 
              ;; removed from the stream at this point.
              (setting-hash (restore-object stream) 
                            (restore-object stream))))
      hash)))

;; -------------------------------------------------------

;; The dumping of objects works by serializing the type of the object which
;; is followed by applicable slot-name and value (depending on whether the
;; slot is bound, it's allocation and *store-class-slots*). Once each slot
;; is serialized a counter is incremented which is stored at the end.
;; When restoring the object a new instance is allocated and then
;; restore-type-object starts reading objects from the stream.
;; If the restored object is a symbol the it names a slot and it's value
;; is pulled out and set on the newly allocated object.
;; If the restored object is an integer then this is the end marker
;; for the object and the number of slots restored is checked against
;; this counter.

#|
;; Allegro Tests...
(defstruct thing a b c)
(defparameter x (make-thing :a 1 :b 2 :c 3))
(ubstream:with-output-to-ubyte-stream (s) (loenc:serialize x s))
|#

;; Object and Conditions
#|
(defun store-type-object (obj stream)
  ;; (declare (xoptimize speed))
  (let ((all-slots (remove-if
                    (complement (lambda (slot)
                                  (let ((slot-name (slot-definition-name slot)))
                                    (and (slot-boundp obj slot-name)
                                         (or *store-class-slots*
                                             (not (eql (slot-definition-allocation slot)
                                                       :class)))))))
                    (serializable-slots obj)) ))
    (store-object (type-of obj) stream)
    (store-count (length all-slots) stream)
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
        (store-object slot-name stream)
        (store-object (slot-value obj slot-name) stream) )) ))
|#

;; DM/RAL 07/09
;; store slots names then slot values to help the restorer when
;; the class is not recognized...
(defun store-type-object (obj stream)
  ;; (declare (xoptimize speed))
  (let ((all-slots (remove-if
                    (complement (lambda (slot)
                                  (let ((slot-name (slot-definition-name slot)))
                                    (and (slot-boundp obj slot-name)
                                         (or *store-class-slots*
                                             (not (eql (slot-definition-allocation slot)
                                                       :class)))))))
                    (serializable-slots obj)) ))
    (store-object (class-name (class-of obj)) stream)
    (store-count (length all-slots) stream)
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
        (store-object slot-name stream)))
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
        (store-object (slot-value obj slot-name) stream) )) ))

(defstore-sdle-store (obj standard-object stream)
  (let ((obj (before-store obj)))
    (cond ((typep obj 'standard-object)
           (output-type-code +standard-object-code+ stream)    
           (store-type-object obj stream))
          (t
           (store-object obj stream))
          )))

(defstore-sdle-store (obj condition stream)
  (output-type-code +condition-code+ stream)    
  (store-type-object obj stream))

#|
(defun restore-type-object (stream)
  ;; (declare (xoptimize speed))
  (let* ((class (find-class (restore-object stream)))
         (new-instance (allocate-instance class))
         (count (read-count stream)))
    (resolving-object (obj new-instance)
      (loop repeat count
            do
            (let ((slot-name (restore-object stream)))
              ;; slot-names are always symbols so we don't
              ;; have to worry about circularities
              (setting (slot-value obj slot-name) (restore-object stream)))))
    (after-retrieve new-instance)))

(defrestore-sdle-store (standard-object stream)
  (restore-type-object stream))

(defrestore-sdle-store (condition stream)
  (restore-type-object stream))
|#

;; DM/RAL 07/09
;; if we try to restore an object of an unknown class
;; then just dummy up a new class defintion containing the
;; indicated slots.
;; -----------------------------------------------------------

(defun restore-type-object (stream obj-type)
  ;; (declare (xoptimize speed))
  (let* ((class-name   (restore-object stream))
         (count        (read-count stream))
         (slot-names   (loop repeat count
                             collect (restore-object stream)))
         (class        (find-or-create-class class-name obj-type slot-names))
         (new-instance (allocate-instance class)))
    
    (resolving-object (obj new-instance)
      (loop for slot-name in slot-names
            do
            ;; slot-names are always symbols so we don't
            ;; have to worry about circularities
            (let ((val (restore-object stream)))
              (when #+:LISPWORKS (clos:slot-exists-p new-instance slot-name)
                    #-:LISPWORKS (slot-exists-p new-instance slot-name)
                (setting (slot-value obj slot-name) val))) ))
    (after-retrieve new-instance)))

(defun find-or-create-class (class-name obj-type slot-names)
  (or (find-class class-name nil)
      (ensure-class class-name
                    :direct-slots (mapcar (lambda (slot-name)
                                            `(:name       ,slot-name
                                              :allocation :instance
                                              :initargs   nil
                                              :readers    nil
                                              :type       t
                                              :writers    nil))
                                          slot-names)
                    :direct-superclasses (list obj-type)
                    :metaclass 'standard-class) ))

(defrestore-sdle-store (standard-object stream)
  (restore-type-object stream 'standard-object))

(defrestore-sdle-store (condition stream)
  (restore-type-object stream 'condition))


;; -------------------------------------------------------
;; classes

(defstore-sdle-store (obj standard-class stream)
  (output-type-code +standard-class-code+ stream)
  (store-standard-class obj stream))

(defun store-standard-class (obj stream)
  (store-object (class-name obj) stream)
  (store-object (mapcar #'get-slot-details (class-direct-slots obj))
                stream)
  (store-object (mapcar (if *store-class-superclasses*
                            #'identity 
                            #'class-name)
                        (class-direct-superclasses obj))
                stream)
  (store-object (class-name (class-of obj)) stream))

(defrestore-sdle-store (standard-class stream)
  (restore-standard-class stream))

(defun restore-standard-class (stream)
  (let* ((class (restore-object stream))
         (slots (restore-object stream))
         (supers (restore-object stream))
         (meta (restore-object stream))
         (keywords '(:direct-slots :direct-superclasses
                     :metaclass))
         (final (loop for keyword in keywords
                      for slot in (list slots 
                                        (or supers
                                            (list 'standard-object))
                                        meta)
                      nconc (list keyword slot))))
    (cond ((find-class class nil)
           (cond (*nuke-existing-classes*
                  (apply #'ensure-class class final)
                  #+(and clisp (not mop)) (add-methods-for-class class slots))
                 (t (find-class class))))
          (t (apply #'ensure-class class final)
             #+(and clisp (not mop)) (add-methods-for-class class slots))
          )))

;; built in classes

(defstore-sdle-store (obj built-in-class stream)
  (output-type-code +built-in-class-code+ stream)
  (store-object (class-name obj) stream))

#-ecl ;; for some reason this doesn't work with ecl
(defmethod internal-store-object ((backend sdle-store) (obj (eql (find-class 'hash-table))) stream)
  (output-type-code +built-in-class-code+ stream)
  (store-object 'cl:hash-table stream))

(defrestore-sdle-store (built-in-class stream)
  (find-class (restore-object stream)))


;; -------------------------------------------------------
;; Arrays, vectors and strings.
(defstore-sdle-store (obj array stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (typecase obj
    (simple-base-string (store-simple-base-string obj stream))
    (simple-string (store-simple-string obj stream))
    (simple-vector (store-simple-vector obj stream))
    ((simple-array (unsigned-byte 8) (*)) (store-simple-byte-vector obj stream))
    (t (store-array obj stream))))


(defun store-array (obj stream)
  (declare ;; (xoptimize speed (safety 0) (debug 0))
           (type array obj))
  (output-type-code +array-code+ stream)
  (if (and (= (array-rank obj) 1)
           (array-has-fill-pointer-p obj))
      (store-object (fill-pointer obj) stream)
    (store-object nil stream))
  (store-object (array-element-type obj) stream)
  (store-object (adjustable-array-p obj) stream)
  (store-object (array-dimensions obj) stream)
  (dolist (x (multiple-value-list (array-displacement obj)))
    (store-object x stream))
  (store-count (array-total-size obj) stream)
  (loop for x from 0 below (array-total-size obj) do
        (store-object (row-major-aref obj x) stream)))

 


(defrestore-sdle-store (array stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (let* ((fill-pointer (restore-object stream))
         (element-type (restore-object stream))
         (adjustable (restore-object stream))
         (dimensions (restore-object stream))
         (displaced-to (restore-object stream))
         (displaced-offset (restore-object stream))
         (size (read-count stream))
         (res (make-array dimensions  
                          :element-type element-type
                          :adjustable adjustable
                          :fill-pointer fill-pointer)))
    (declare (type cons dimensions) (type array-tot-size size))
    (when displaced-to 
      (adjust-array res dimensions :displaced-to displaced-to
                    :displaced-index-offset displaced-offset))
    (resolving-object (obj res)
      (loop for x from 0 below size do
            (let ((pos x))
              (setting (row-major-aref obj pos) (restore-object stream)))))))

(defun store-simple-vector (obj stream)
  (declare ;; (xoptimize speed (safety 0) (debug 0))
           (type simple-vector obj))
  (output-type-code +simple-vector-code+ stream)
  (store-count (length obj) stream)
  (loop for x across obj do
    (store-object x stream)))

(defrestore-sdle-store (simple-vector stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (let* ((size (read-count stream))
         (res (make-array size)))
    (declare (type array-size size))
    (resolving-object (obj res)
      (dotimes (i size)
        ;; we need to copy the index so that
        ;; it's value at this time is preserved.
        (let ((x i)) 
          (setting (aref obj x) (restore-object stream)))))
    res))

(defun store-simple-byte-vector (obj stream)
  (declare ;; (xoptimize speed (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 8) (*)) obj))
  (output-type-code +simple-byte-vector-code+ stream)
  (store-count (length obj) stream)
  (loop for x across obj do
        (write-byte x stream)))
 
(defrestore-sdle-store (simple-byte-vector stream)
  ;; (declare (xoptimize speed (safety 1) (debug 0)))
  (let* ((size (read-count stream))
         (res (make-array size :element-type '(unsigned-byte 8))))
    (declare (type array-size size))
    (resolving-object (obj res)
      (dotimes (i size)
        ;; we need to copy the index so that
        ;; it's value at this time is preserved.
        (let ((x i)) 
          (setting (aref obj x) (read-byte stream)))))
    res))

;; Dumping (unsigned-byte 32) for each character seems
;; like a bit much when most of them will be 
;; base-chars. So we try to cater for them.
(defconstant +char-marker+ (code-char 255)
  "Largest character that can be represented in 8 bits")

(defun unicode-string-p (string)
  "An implementation-independent predicate on STRING, which must be of
   type simple-string, as to whether it contains any characters with
   code > 255. It may use implementation-dependent code and/or
   knowledge to achieve this result, but only as an optimization."
  (declare ;; (xoptimize speed (safety 0) (debug 0))
   (type simple-string string))
  (and

   #+cmu nil                      ; cmucl doesn't support unicode yet.
   #+:LISPWORKS (if (typep string 'lw:8-bit-string)
                    nil           ; can't have char > 255
                    t)            ; might have, must check

   ;; Return true as soon as one char > 255 encountered:
   (loop for i fixnum from 0 below (the fixnum (length string))
           thereis (char> (char string i) +char-marker+))))

(defun store-simple-string (obj stream)
  (declare (type simple-string obj)
           ;; (xoptimize speed (safety 1) (debug 0))
	   )
  (cond ((unicode-string-p obj)
         (output-type-code +unicode-string-code+ stream)
         (dump-string #'store-count obj stream))
        (t (output-type-code +simple-string-code+ stream)
           (dump-string #'write-byte obj stream))))

(defun store-simple-base-string (obj stream)
  (declare (type simple-string obj)
           ;; (xoptimize speed (safety 1) (debug 0))
	   )
  (cond ((unicode-string-p obj)
         (output-type-code +unicode-base-string-code+ stream)
         (dump-string #'store-count obj stream))
        (t (output-type-code +simple-base-string-code+ stream)
           (dump-string #'write-byte obj stream))))

(defun dump-string (dumper obj stream)
  (declare (simple-string obj) (function dumper) (stream stream)
           ;; (xoptimize speed (safety 1) (debug 0))
	   )
  ;; (store-count (the array-size (length obj)) stream)
  (store-count (length obj) stream)
  (loop for x across obj do (funcall dumper (char-code x) stream)))

(defrestore-sdle-store (simple-string stream)
  ;; (declare (xoptimize speed))
  (undump-string #'read-byte 'character stream))

(defrestore-sdle-store (unicode-string stream)
  ;; (declare (xoptimize speed))
  (undump-string #'read-count 'character stream))

(defrestore-sdle-store (simple-base-string stream)
  ;; (declare (xoptimize speed))
  (undump-string #'read-byte 'base-char stream))

(defrestore-sdle-store (unicode-base-string stream)
  ;; (declare (xoptimize speed))
  (undump-string #'read-count 'base-char stream))

(defun undump-string (reader type stream)
  (declare (type function reader) (type stream stream)
           ;; (xoptimize speed (safety 1) (debug 0))
	   )
  (let* ((length (the array-size (read-count stream)) )
         (res (make-string length :element-type type)))
    (declare (type simple-string res))
    (dotimes (x length)
      (setf (schar res x) (code-char (funcall reader stream))))
    res))

;; -------------------------------------------------------
;; packages (from Thomas Stenhaug)

(defstore-sdle-store (obj package stream)
  (output-type-code +package-code+ stream)  
  (store-object (package-name obj) stream)
  (store-object (package-nicknames obj) stream)
  (store-object (mapcar (if *store-used-packages* #'identity #'package-name)
                        (package-use-list obj))
                stream)
  (store-object (internal-symbols obj) stream)
  (store-object (package-shadowing-symbols obj) stream)
  (store-object (external-symbols obj) stream))

(defun remove-remaining (times stream)
  ;; (declare (xoptimize speed) (type fixnum times))
  (dotimes (x times)
    (restore-object stream)))

(defrestore-sdle-store (package stream)
  (let* ((package-name (restore-object stream))
         (existing-package (find-package package-name)))
    (cond ((or (not existing-package)
               *nuke-existing-packages*)
           (restore-package package-name stream :force *nuke-existing-packages*))
          (t (remove-remaining 5 stream)
             existing-package))))

(defun internal-symbols (package)
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0))
        (used (package-use-list package)))
    (do-symbols (symbol package)
      (unless (find (symbol-package symbol) used)
        (vector-push-extend symbol acc)))
    acc))

(defun external-symbols (package)
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol package)
      (vector-push-extend symbol acc))
    acc))

(defun restore-package (package-name stream &key force)
  (when (and force
             (find-package package-name))
    (delete-package package-name))
  (let ((package (make-package package-name
			       :nicknames (restore-object stream)
			       :use (restore-object stream))))
    (loop for symbol across (restore-object stream) do
      (import symbol package))
    (shadow (restore-object stream) package)
    (loop for symbol across (restore-object stream) do
      (export symbol package))
    package))

;; -------------------------------------------------------
;; Function storing hack.
;; This just stores the function name if we can find it
;; or signal a store-error.
(defun parse-name (name)
  (let ((name (subseq name 21)))
    (declare (type simple-string name))
    (if (search name "SB!" :end1 3)
        (replace name "SB-" :end1 3)
        name)))

#+sbcl
(defvar *sbcl-readtable* (copy-readtable nil))
#+sbcl
(set-macro-character #\#
		     #'(lambda (c s) 
			 (declare (ignore c s))
			 (store-error "Invalid character in function name."))
                     nil
                     *sbcl-readtable*)

(defun get-function-name (obj)
  (multiple-value-bind (l cp name) (function-lambda-expression obj)
    (declare (ignore l))
    (cond ;; handle (SB-C::&OPTIONAL-DISPATCH MAKE-FOO) names introduced around 1.0.15
          #+xx_sbcl
          ((and name
		(consp name)
		(not (cddr name))
		(eql (first name) 'SB-C::&OPTIONAL-DISPATCH))
           (second name))
          
          ;; normal names and (setf name)
          ((and (not cp) ;; can't be a closure DM/RAL 08/16
                name
                (or (symbolp name)
                    (and (consp name)
                         (eq (car name) 'cl:setf)
                         (um:single (cdr name))
                         (symbolp (cadr name)))
                    ))
                name)

          ;;  Try to deal with sbcl's naming convention
          ;; of built in functions (pre 0.9)
          #+xx_sbcl
          ((and name
                (stringp name)
                (search "top level local call " (the simple-string name)))
           (let ((new-name (parse-name name))
                 (*readtable* *sbcl-readtable*))
             (unless (string= new-name "")
               (handler-case (read-from-string new-name)
                 (error (c)
                   (declare (ignore c))
                   (store-error "Unable to determine function name for ~A."
                                obj))))))

          (t (store-error "Unable to determine function name for ~A."
                          obj)))))  

#-clisp
(defstore-sdle-store (obj function stream)
  (cond ((typep obj 'standard-object) ;; true for funcallable instances
         (let ((obj (before-store obj)))
           (cond ((functionp obj)
                  (store-function obj stream))
                 (t
                  (store-object obj stream))
                 )))
        (t
         (store-function obj stream))
        ))

#-clisp
(defun store-function (obj stream)
  (output-type-code +function-code+ stream)
  (store-object (get-function-name obj) stream))

#-clisp
(defrestore-sdle-store (function stream)
  (restore-function stream))

(defun restore-function (stream)
  (function-for-name (restore-object stream)))

(defun function-for-name (name)
  ;; DM/RAL 08/16
  ;; on restore we try to avoid throwing an error.
  ;; If the name on restore cannot be turned back into a function,
  ;; then let's produce a function that will later throw an error.
  ;;
  ;; There's a good chance that the function will never be evaluated,
  ;; e.g., a function that is embedded in a condition object.
  (multiple-value-bind (f e)
      (ignore-errors
        (fdefinition name))
    (if e
        (lambda (&rest args)
          (declare (ignore args))
          (error e))
      ;; else
      f)))

;; Generic function, just dumps the gf-name
(defstore-sdle-store (obj generic-function stream)
  (output-type-code +gf-code+ stream)
  (aif (generic-function-name obj)
      (store-object it stream)
    (store-error "No generic function name for ~A." obj)))

(defrestore-sdle-store (generic-function stream)
  (restore-function stream))


(setf *default-backend* (find-backend 'sdle-store))

;; -----------------------------------------------------------
;; RawBytes Objects

(defstore-sdle-store (obj rawbytes stream)
  ;; rawbytes must have been previously encoded
  (output-type-code +rawbytes-code+ stream)
  (store-count (length (rawbytes-bytes obj)) stream)
  (write-sequence (rawbytes-bytes obj) stream))

(defrestore-sdle-store (rawbytes stream)
  (let* ((nb    (read-count stream))
         (bytes (make-array nb :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (make-rawbytes :bytes bytes)))

;; EOF

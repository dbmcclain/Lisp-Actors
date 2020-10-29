(in-package :um)
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

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------------------------------------

(defclass <lockable-mixin> ()
  ((lock  :reader lm-lock  
	  :initform
          #+:sbcl (sb-thread:make-mutex) 
	  #+:LISPWORKS (mp:make-lock :sharing t)
	  #+:ALLEGRO   (mp:make-sharable-lock)
          #+:CLOZURE   (ccl:make-read-write-lock))))

(defclass <abstract-kv> ()
  ((tbl   :accessor lm-val :initform nil)))

(defmethod initialize-instance :after ((obj <abstract-kv>) &key initial &allow-other-keys)
  (when initial ;; should be a canonical plist
    (set-kvs obj initial)))

(defclass <plist>  (<abstract-kv>)
  ())

(defclass <alist>  (<abstract-kv>)
  ())

(defclass <map>    (<abstract-kv>)
  ((tbl :accessor lm-val :initform (maps:empty))))

(defclass <hash-table> (<abstract-kv>)
  ((tbl :accessor lm-val :initform (make-hash-table))))

(defclass <shared-plist> (<lockable-mixin> <plist>)
  ())

(defclass <shared-alistl> (<lockable-mixin> <alist>)
  ())

(defclass <shared-map> (<lockable-mixin> <map>)
  ())

(defclass <shared-hash-table> (<lockable-mixin> <hash-table>)
  ())

(defvar *unique*  (load-time-value #() t))

(defgeneric get-kv (obj key &optional default)
  #+:LISPWORKS
  (:method ((obj <lockable-mixin>) key &optional default)
   (mp:with-sharing-lock ((lm-lock obj))
     (call-next-method)))
  
  #+:ALLEGRO
  (:method ((obj <lockable-mixin>) key &optional default)
   (mp:with-sharable-lock (:shared (lm-lock obj))
     (call-next-method)))

  #+:CLOZURE
  (:method ((obj <lockable-mixin>) key &optional default)
   (ccl:with-read-lock ((lm-lock obj))
     (call-next-method)))

  (:method ((obj <plist>) key &optional default)
   (let ((ans (getf (lm-val obj) key *unique*)))
     (if (eq ans *unique*)
         (values default nil)
       (values ans t))))

  (:method ((obj <alist>) key &optional default)
   (let ((ans (assoc key (lm-val obj))))
     (if ans
         (values (cdr ans) t)
       (values default nil))))

  (:method ((obj <shared-map>) key &optional default)
   (maps:find (lm-val obj) key default))
  
  (:method ((obj <map>) key &optional default)
   (maps:find (lm-val obj) key default))

  (:method ((obj <hash-table>) key &optional default)
   (gethash key (lm-val obj default)))

  #+:LISPWORKS
  (:method ((obj hash-table) key &optional default)
   (hcl:with-hash-table-locked obj
     (gethash key obj default)))

  #+:CLOZURE
  (:method ((obj hash-table) key &optional default)
     (gethash key obj default))
  
  ;; ALLEGRO-FIXME - need a variant for hash-tables here...
  )

(defgeneric set-kv (obj key val)
  #-(or CLOZURE sbcl) ;; assuming the meaning is LispWorks?
  (:method ((obj <lockable-mixin>) key val)
   (mp:with-exclusive-lock ((lm-lock obj))
     (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) key val)
   (sb-thread:with-recursive-lock ((lm-lock obj))
     (call-next-method)))
  

  #+:CLOZURE
  (:method ((obj <lockable-mixin>) key val)
   (ccl:with-write-lock ((lm-lock obj))
     (call-next-method)))
  
  (:method ((obj <plist>) key val)
   (setf (getf (lm-val obj) key) val))

  (:method ((obj <alist>) key val)
   (let ((ans (assoc key (lm-val obj))))
     (if ans
         (setf (cdr ans) val)
       (setf (lm-val obj) (acons key val (lm-val obj))))
     val))

  (:method ((obj <map>) key val)
   (setf (lm-val obj) (maps:add (lm-val obj) key val))
   val)

  (:method ((obj <hash-table>) key val)
   (setf (gethash key (lm-val obj)) val))

  #+:LISPWORKS
  (:method ((obj hash-table) key val)
   (hcl:with-hash-table-locked obj
     (setf (gethash key obj) val)))

  #+:CLOZURE
  (:method ((obj hash-table) key val)
     (setf (gethash key obj) val))

  #+(or sbcl)
  (:method ((obj hash-table) key val)
     (setf (gethash key obj) val))

  ;; ALLEGRO-FIXME - need a variant for hash-tables here...
   )

(defsetf get-kv set-kv)

(defgeneric iter-kv (obj fn)
  #+:LISPWORKS
  (:method ((obj <lockable-mixin>) fn)
   (mp:with-sharing-lock ((lm-lock obj))
     (call-next-method)))

  #+:ALLEGRO
  (:method ((obj <lockable-mixin>) fn)
   (mp:with-sharable-lock (:shared (lm-lock obj))
     (call-next-method)))
  
  #+:CLOZURE
  (:method ((obj <lockable-mixin>) fn)
   (ccl:with-read-lock ((lm-lock obj))
     (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) fn)
   (sb-thread:with-recursive-lock ((lm-lock obj))
     (call-next-method)))

  (:method ((obj <plist>) fn)
   (um:nlet-tail iter ((lst (lm-val obj)))
     (when lst
       (destructuring-bind (k v &rest tl) lst
         (funcall fn k v)
         (iter tl))
       )))

  (:method ((obj <alist>) fn)
   (map nil (um:lambda* ((k . v))
              (funcall fn k v))
        (lm-val obj)))

  (:method ((obj <map>) fn)
   (maps:iter (lm-val obj) fn))

  (:method ((obj <hash-table>) fn)
   (maphash fn (lm-val obj)))

  (:method ((obj hash-table) fn)
   (maphash fn obj))
  )
                       
(defgeneric ensure-kv (obj key val)
  #-(or CLOZURE sbcl) ;; assuming the meaning is LispWorks?
  (:method (key (obj <lockable-mixin>) val)
    (mp:with-exclusive-lock ((lm-lock val))
      (call-next-method)))

  #+:CLOZURE
  (:method ((obj <lockable-mixin>) key val)
    (ccl:with-write-lock ((lm-lock val))
      (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) key val)
    (sb-thread:with-recursive-lock ((lm-lock val))
      (call-next-method)))
  
  (:method ((obj <plist>) key val)
   (let ((ans (getf (lm-val obj) key *unique*)))
     (when (eq ans *unique*)
       (setf ans val
             (getf (lm-val obj) key) val))
     ans))

  (:method ((obj <alist>) key val)
   (let ((ans (assoc key (lm-val obj))))
     (if ans
         (setf ans (cdr ans))
       (setf (lm-val obj) (acons key val (lm-val obj))
             ans val))
     ans))

  (:method ((obj <map>) key val)
   (let ((ans (maps:find (lm-val obj) key *unique*)))
     (when (eq ans *unique*)
       (setf (lm-val obj) (maps:add (lm-val obj) key val)
             ans val))
     ans))

  (:method ((obj <hash-table>) key val)
   (let ((ans (gethash key (lm-val obj) *unique*)))
     (when (eq ans *unique*)
       (setf (gethash key (lm-val obj)) val
             ans val))
     ans))
  
  #+:LISPWORKS
  (:method ((obj hash-table) key val)
   (hcl:gethash-ensuring key obj (constantly val)))
  
  ;; ALLEGRO-FIXME - need a variant for hash-tables here...
  )


(defgeneric remove-key (obj key)
  #-(or CLOZURE sbcl) ;; assuming the meaning is LispWorks?
  (:method ((obj <lockable-mixin>) key)
   (mp:with-exclusive-lock ((lm-lock obj))
     (call-next-method)))

  #+:CLOZURE
  (:method ((obj <lockable-mixin>) key)
   (ccl:with-write-lock ((lm-lock obj))
     (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) key)
   (sb-thread:with-recursive-lock ((lm-lock obj))
     (call-next-method)))
  
  (:method ((obj <plist>) key)
   (remf (lm-val obj) key))

  (:method ((obj <alist>) key)
   (setf (lm-val obj) (delete key (lm-val obj)
                              :key #'car)))

  (:method ((obj <map>) key)
   (setf (lm-val obj) (sets:remove (lm-val obj) key)))

  (:method ((obj <hash-table>) key)
   (remhash key (lm-val obj)))

  #+:LISPWORKS
  (:method ((obj hash-table) key)
   (hcl:with-hash-table-locked obj
     (remhash key obj)))
  
  ;; ALLEGRO-FIXME - need a variant for hash-tables here...
  )

;; -----------------------------------------------------------

(defun plist-p (plist)
  (and (listp plist)
       (every (um:compose #'symbolp #'car)
              (um:group plist 2))))

(defun plist-to-map (plist)
  (assert (plist-p plist))
  (um:nlet-tail iter ((lst (reverse plist))
                      (map (maps:empty)))
    (if lst
        (destructuring-bind (v k . tl) lst
          (iter tl (maps:add map k v)))
      map)))

(defun alist-to-map (alist)
  (foldl (um:lambda* ((k . v) map)
           (maps:add map k v))
         (reverse alist) (maps:empty)))

(defun hash-table-to-map (tbl)
  (let ((map (maps:empty)))
    (maphash #'(lambda (k v)
                 (setf map (maps:add map k v)))
             tbl)
    map))

(defun merge-maps (new-map old-map)
  (sets:union new-map
              (sets:diff old-map new-map)))

(defun map-to-plist (map)
  (um:accum acc
    (maps:iter map
               #'(lambda (k v)
                   (acc k)
                   (acc v)))
    ))

(defun map-to-alist (map)
  (um:accum acc
    (maps:iter map
               #'(lambda (k v)
                   (acc (cons k v))))
    ))

(defun map-to-hash-table (map &optional (tbl (make-hash-table)))
  (maps:iter map #'(lambda (k v)
                     (setf (gethash k tbl) v)))
  tbl)

(defgeneric merge-kvs (obj new-kvs)
  ;; new-kvs should be a canonical plist

  #-(or CLOZURE sbcl) ;; assuming the meaning is LispWorks?
  (:method ((obj <lockable-mixin>) new-kvs)
   (mp:with-exclusive-lock ((lm-lock obj))
     (call-next-method)))
  
  #+:CLOZURE
  (:method ((obj <lockable-mixin>) new-kvs)
   (ccl:with-write-lock ((lm-lock obj))
     (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) new-kvs)
   (sb-thread:with-recursive-lock ((lm-lock obj))
     (call-next-method)))

  (:method ((obj <plist>) new-kvs)
   (setf (lm-val obj)
         (map-to-plist
          (merge-maps (plist-to-map new-kvs)
                      (plist-to-map (lm-val obj))))))

  (:method ((obj <alist>) new-kvs)
   (setf (lm-val obj)
         (map-to-alist
          (merge-maps (plist-to-map new-kvs)
                      (alist-to-map (lm-val obj))))))

  (:method ((obj <map>) new-kvs)
   (setf (lm-val obj)
         (merge-maps (plist-to-map new-kvs) (lm-val obj))))

  (:method ((obj <hash-table>) new-kvs)
   (setf (lm-val obj)
         (map-to-hash-table
          (merge-maps (plist-to-map new-kvs)
                      (hash-table-to-map (lm-val obj))))))

  (:method ((obj hash-table) new-kvs)
   (map-to-hash-table (plist-to-map new-kvs) obj))
  )

(defgeneric set-kvs (obj new-kvs)
  ;; new-kvs should be canonical plist

  #-(or CLOZURE sbcl) ;; assuming the meaning is LispWorks?
  (:method ((obj <lockable-mixin>) new-kvs)
   (mp:with-exclusive-lock ((lm-lock obj))
     (call-next-method)))

  #+:CLOZURE
  (:method ((obj <lockable-mixin>) new-kvs)
   (ccl:with-write-lock ((lm-lock obj))
     (call-next-method)))

  #+(or sbcl)
  (:method ((obj <lockable-mixin>) new-kvs)
    (sb-thread:with-recursive-lock ((lm-lock obj))
      (call-next-method)))

  (:method ((obj <plist>) new-kvs)
   (setf (lm-val obj) (map-to-plist (plist-to-map new-kvs))))

  (:method ((obj <alist>) new-kvs)
   (setf (lm-val obj) (map-to-alist (plist-to-map new-kvs))))

  (:method ((obj <map>) new-kvs)
   (setf (lm-val obj) (plist-to-map new-kvs)))

  (:method ((obj <hash-table>) new-kvs)
   (setf (lm-val obj) (map-to-hash-table (plist-to-map new-kvs))))

  (:method ((obj hash-table) new-kvs)
   (clrhash obj)
   (map-to-hash-table (plist-to-map new-kvs) obj))
  )

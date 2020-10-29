
(in-package :sdle-store)

(defbackend sdle-store
            :magic-number      (um:magic-word "SDLE")
            :stream-type       '(unsigned-byte 8)
            :old-magic-numbers ()
            :extends           (resolving-backend)
            :fields            ((restorers :accessor restorers 
                                           :initform (make-hash-table :size 100))))
  
(defun register-code (code name &optional (errorp nil))
  (let ((tbl (restorers (find-backend 'sdle-store))))
    (if (and errorp
             (gethash code tbl))
        (error "Code ~A is already defined for ~A." code name)
      (setf (gethash code tbl) name))
    code))


(defun next-available-code ()
  (let ((vals (loop for ix from 1 below 256 collect ix)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (setf vals (delete k vals)))
             (restorers (find-backend 'sdle-store)) )
    (if vals
        (first vals)
      (error "No more available codes"))
    ))

(defstruct rawbytes
  bytes)

;; -- end of default-backend-decls.lisp -- ;;

#|
(let* ((codes (make-array 256)))
  (maphash (lambda (k v)
             (setf (aref codes k) (cons k v)))
           (restorers (find-backend 'sdle-store)))
  (let* ((*print-length* Nil))
    (pprint codes)))
|#
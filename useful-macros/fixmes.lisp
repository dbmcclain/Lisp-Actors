;; fixmes.lisp - a collection of FIX-ME's to overcome some Lisp problems...
;;
;; DM/RAL 02/21
;; ----------------------------------------------------------------------

(defpackage :fixmes)

(in-package :fixmes)

;; -----------------------------------------------------------------
;; First up... DSPEC:Save-Tags-Database can run into problems when it
;; tags a method which uses EQL specializers.
;;
;; It currently attempts to serialize those EQL args, and will
;; reconstruct them on readback of the database. This is not a problem
;; for simple atoms - symbols, numbers, strings, etc.
;;
;; But if an EQL arg is a composite object, then it will produce a
;; deep copy of the original - if it can even save the original.
;; Sometimes those composite objects may contain a function, which is
;; not serializable, and will cause the save operation to bomb out.
;;
;; --------------------------------------------------------------------------
;; Try to prevent DSPEC from saving risky compound EQL method specializers

(defvar *saving-tags* nil)

(lw:defadvice (dspec:save-tags-database :ensure-safe-serializing :around)
    (pathname)
  (let ((*saving-tags* t))
    (lw:call-next-advice pathname)))

(defgeneric eql-spec-needs-proxy-p (obj)
  (:method (obj)
   nil)
  (:method ((obj function))
   t)
  (:method ((obj stream))
   t)
  (:method ((obj uuid:uuid))
   nil)
  (:method ((obj standard-object))
   t)
  (:method ((obj pathname))
   nil)
  (:method ((obj structure-object))
   t)
  (:method ((obj array))
   t)
  (:method ((obj clos::system-object))
   t))

(defvar *allow-hash-tables* t)

(defmethod scrub (obj)
  (if (eql-spec-needs-proxy-p obj)
      (gensym (format nil "~A-"
                      (class-name (class-of obj))))
    obj))

(defmethod scrub ((obj hash-table))
  (if *allow-hash-tables*
      (restart-case
          (progn
            (maphash (lambda (k v)
                       (let ((new-v (scrub v)))
                         (unless (eq v new-v)
                           (invoke-restart 'needs-new-table k new-v))))
                     obj)
            obj)
        (needs-new-table (start-k new-v)
          (let ((new-tbl (make-hash-table
                          :test (hash-table-test obj))))
            (maphash (lambda (k v)
                       (setf (gethash k new-tbl)
                             (cond
                              ((eql k start-k)
                               (setf start-k nil)
                               new-v)
                              (start-k  v)
                              (t        (scrub v))
                              )))
                     obj)
            new-tbl)))
    ;; else
    (call-next-method)))

(defmethod scrub ((obj vector))
  (restart-case
      (progn
        (loop for ix from 0
              for x across obj
              do
              (let ((new-x (scrub x)))
                (unless (eq x new-x)
                  (invoke-restart 'needs-fresh-vector ix new-x))
                ))
        obj)
           
    (needs-fresh-vector (ix new-x)
      (let ((new-obj  (copy-seq obj)))
        (setf (aref new-obj ix) new-x)
        (loop for ix from (1+ ix) below (length obj) do
              (setf (aref new-obj ix) (scrub (aref obj ix))))
        new-obj))
    ))

(defmethod scrub ((obj cons))
  ;; functionally pure scrubber replacing EQL specializers that
  ;; cannot be meaningfully serialized without risk.
  (destructuring-bind (hd . tl) obj
    (cond ((and (eq 'eql hd)
                (consp tl)
                (null (cdr tl)))
           (let* ((arg     (car tl))
                  (*allow-hash-tables* nil)
                  (new-arg (scrub arg)))
             (if (eq arg new-arg)
                 obj
               (list 'eql new-arg))
             ))
          
          (t
           (let ((new-hd (scrub hd))
                 (new-tl (scrub tl)))
             (if (and (eq hd new-hd)
                      (eq tl new-tl))
                 obj
               (cons new-hd new-tl))))
          )))

(lw:defadvice (dump-forms-to-file :filter-dspec-methods :around)
    (pathname forms &rest args)
  (when *saving-tags*
    ;; (setf *the-forms* forms)
    (setf forms (scrub forms)))
  (apply #'lw:call-next-advice pathname forms args))

#|
(cd "PROJECTS:LISP;")
(dspec:save-tags-database "diddly.fasl")

(defmethod tst (x)
  (print x))
(defmethod tst ((x (eql #'tst)))
  (print :yes!))
(tst 51)
(tst #'tst)
 |#



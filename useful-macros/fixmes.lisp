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

(defpackage :dspec/unserialized)

(defvar *unserialized* (make-hash-table
                        :test #'eql))

(defvar *saving-tags* nil)

(lw:defadvice (dspec:save-tags-database :ensure-safe-serializing :around)
    (pathname)
  (let ((*saving-tags* t))
    (lw:call-next-advice pathname)))

(defun needs-proxy-p (x)
  ;; objects which may pose risks to serialization
  (or (functionp x)
      (streamp x)
      (typep x 'standard-object)
      (typep x 'structure-object)
      (typep x 'array)
      (typep x 'clos::system-object)))

(defun not-needs-proxy-p (x)
  ;; objects which can serialize without risk
  (or (pathnamep x)
      (typep x 'uuid:uuid)))

(defun lookup-unserialized-symbol (obj)
  (or (gethash obj *unserialized*)
      (let ((sym (intern (string (gensym (format nil "~A-"
                                         (class-name
                                          (class-of obj)))))
                         :dspec/unserialized)))
        (export sym :dspec/unserialized)
        (setf (symbol-value sym) obj
              (gethash obj *unserialized*) sym))))

(defgeneric scrub (obj)
  (:method (obj)
   (cond
    ((not-needs-proxy-p obj)  obj)
    ((needs-proxy-p obj)     (lookup-unserialized-symbol obj))
    (t                        obj)
    ))
  (:method ((obj string))
   obj))

(defvar *allow-hash-tables* t)

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
                          :test (hash-table-test obj)))
                (state :early))
            (maphash (lambda (k v)
                       (setf (gethash k new-tbl)
                             (case state
                               ((:early)
                                (if (eql k start-k)
                                    (progn
                                      (setf state :late)
                                      new-v)
                                  v))
                               ((:late)
                                (scrub v))
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
  ;; Warning: obj may be a dotted pair
  (destructuring-bind (hd . tl) obj
    (if (and (eq 'eql hd)
             (consp tl)
             (null (cdr tl)))
        (let* ((arg  (car tl))
               (*allow-hash-tables* nil)
               (new-arg (scrub arg)))
          (if (eq arg new-arg)
              obj
            (list 'eql new-arg)))
      ;; else
      (let ((new-hd (scrub hd)))
        (if (eq hd new-hd)
            (let ((new-tl (scrub tl)))
              (if (eq tl new-tl)
                  obj
                (cons hd new-tl)))
          ;; else
          (cons new-hd (scrub tl)))
        ))))

(lw:defadvice (dump-forms-to-file :filter-dspec-methods :around)
    (pathname forms &rest args)
  (when *saving-tags*
    (setf forms (scrub forms)))
  (apply #'lw:call-next-advice pathname forms args))

#|
(cd "PROJECTS:LISP;")
(dspec:save-tags-database "diddly.fasl")

(defmethod tst (x)
  (print x))
(defmethod tst ((x (eql 'tst)))
  (print :yes!))
(tst 51)
(tst 'tst)
 |#



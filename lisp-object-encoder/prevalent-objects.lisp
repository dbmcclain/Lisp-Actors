;; prevalent-objects.lisp
;; --------------------------------------------------------------------------------------
;; Prevalent object metaclass. For Persistent Object Prevalence --
;; -- all objects memory resident,
;; -- changes to slots are logged.
;; -- Initialization reads last committed data file
;;    and then runs the existing log entries to bring objects back up
;;    to most recent versions.
;;
;; Not transactioned. Single user only.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; DM/RAL 09/03  extensive modification/improvement
;; --------------------------------------------------------------------------------------
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

;; --------------------------------------------------------------------------------------
(in-package :prevalent-object)
;; --------------------------------------------------------------------------------------

;; ------------------------------------------------------------

;; the magic word for .snap snapshot files
(defconstant +prevalent-objects-magic+ (magic-word "SDPO"))  ;; RAL Objects

;; ----------------------------------------------------------------------------
;; prevalent-object -- the root class of all classes containing prevalent slots

(defclass prevalent-object ()
  ()
  (:metaclass prevalent-class)
  (:documentation "Classes of metaclass PREVALENT-CLASS automatically
inherit from this class."))

;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-backend 'prevo-back-end)
    (defbackend prevo-back-end
                :magic-number +prevalent-objects-magic+
                :extends      (loe-back-end))))

#|
(defstore-prevo-back-end (x symbol stream qualifier) body)
(defrestore-prevo-back-end (symbol place qualifier) body)
(defstruct point x y)
(multiple-value-bind (create-form init-form)
    (make-load-form-saving-slots (make-point :x 15 :y 18))
  (pprint create-form)
  (pprint "================")
  (pprint init-form))
|#

(defun serialize-to-logfile (obj)
  (when-let (logfile (current-prevalent-logfile))
    (file-position logfile (system-logpos *prevalent-system*))
    (serialize obj logfile
               :prefix-length 4
               :backend       'prevo-back-end)
    (setf (system-logpos *prevalent-system*) (file-position logfile))
    (force-output logfile) ;; not really needed here... (optimistic)
    ))

;; -------------------------------------------------------------------

(defclass admin-spec ()
  ;; a convenient root class for all administrative objects
  ((object    :accessor admin-spec-object   :initarg :object)
   ))

(defclass update-spec (admin-spec)
  ((at-spec   :accessor update-spec-at-spec     :initarg :at-spec)
   (value     :accessor update-spec-value       :initarg :value)))

(defclass remove-item-spec (admin-spec)
  ((key         :accessor remove-item-spec-key        :initarg :key)))

(defclass makunbound-spec (admin-spec)
  ((slot-name   :accessor makunbound-spec-slot-name   :initarg :slot-name)))

;; ------------------------------------------------------------

(defun make-update-spec (object at-spec value)
  (make-instance 'update-spec
                 :object    object
                 :at-spec   at-spec
                 :value     value))

(defun make-remove-item-spec (object key)
  (make-instance 'remove-item-spec
                 :object object
                 :key    key))

(defun make-makunbound-spec (object slot-name)
  (make-instance 'makunbound-spec
                 :object    object
                 :slot-name slot-name))

;; -------------------------------

(defvar *oid-incr*      0)
(defvar *last-oid-time* 0)

(defun get-new-oid ()
  ;; what happens if the clock is wrong and later gets reset?
  (let ((now (get-universal-time)))
    (if (= now *last-oid-time*)
        (incf *oid-incr*)
      (setf *last-oid-time* now
            *oid-incr*      0))
    (+ (ash now 30) *oid-incr*))) ;; room for 1G objects per second

(defun when-created (oid)
  (multiple-value-bind (ss mm hh d m y) (decode-universal-time (ash oid -30))
    (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            y m d hh mm ss)))

;; -----------------------------------------------------------------

(defun make-known-to-system (oid obj)
  ;; returns obj
  (setf (gethash obj (system-obj-table *prevalent-system*)) oid
        (gethash oid (system-oid-table *prevalent-system*)) obj))
    
;; ------------------------------------------------------------

(defun oid-for-object (obj)
  (gethash obj (system-obj-table *prevalent-system*)))

(defun obj-for-oid (oid)
  (gethash oid (system-oid-table *prevalent-system*)))

;; ------------------------------------------------------------

(defconstant +assoc-oid-code+           (register-code 111 'object-association))
(defconstant +object-proxy-code+        (register-code 112 'object-proxy))
(defconstant +update-spec-code+         (register-code 113 'update-spec))
(defconstant +remove-item-code+         (register-code 114 'remove-item-spec))
(defconstant +makunbound-spec-code+     (register-code 115 'makunbound-spec))

;; ------------------------------------------------------------
;; object-proxy serialization -- occurs when serializing a persitent object
;; that points to another prevalent object

(defrestore (object-proxy stream)
  (obj-for-oid (read-count stream)))

;; ------------------------------------

(defrestore (object-association stream)
  (let ((oid (read-count stream))
        (obj (restore-object stream)))
    (make-known-to-system oid obj)
    obj))

;; ------------------------------------

(defmethod sdle-store:referrerp :around ((backend prevo-back-end) reader)
  ;; associations should not advance the internal object counter in sdle-store
  (or (eql reader 'object-association)
      (call-next-method)))

;; -----------------------------------------------------------------

(defun do-with-safe-object-marshaling (stream alternate-object fn)
  (let ((file-pos (file-position stream)))
    (handler-case
        (funcall fn)
      (error (exn)
        (print (format-error exn))
        (file-position stream file-pos)
        (store-object alternate-object stream))
      )))

(defmacro with-safe-object-marshaling ((stream &optional alternate-object)
                                       &body body)
  `(do-with-safe-object-marshaling ,stream
                                   ,alternate-object
                                   #'(lambda ()
                                       ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-safe-object-marshaling" 2 2)

;; -----------------------------------------------------------------

(deftype no-oids-needed ()
  '(or number
       character
       null
       (eql t)
       admin-spec))

;; -----------------------------------------------------------------

;; local special used as flag to avoid stuffing the OID tables with symbol namestrings
(defvar *in-symbol* nil)

(defmethod sdle-store:backend-store-object :around ((backend prevo-back-end)
                                                    obj stream)
  ;; why are we doing this?  We are trying to conserve external
  ;; storage space in the marshalling of data. As such, we should
  ;; automatically assign OID's to any objects that are:
  ;;
  ;; a. large, or
  ;; b. have mutable structure,
  ;; c. potentially shared
  ;;
  ;; So this essentially means everything in Lisp, except for numbers
  ;; and characters.
  ;;
  ;; And if an object already is known to be shared, by already having
  ;; an OID, then be sure to record just that OID reference. Its
  ;; original structure will have already been saved along with its
  ;; OID association.
  ;;
  ;; We are also trying to safely back out of a logging transaction in
  ;; the event of a failure. By default, we unroll the attempted store
  ;; and record a nil in its place, after issuing a notification.
  ;;
  (with-safe-object-marshaling (stream)
      (cond ((typep obj 'no-oids-needed)
             ;; (format t "~%SDLE-NBR-CHAR ~A" obj)
             (call-next-method))

            ((symbolp obj)
             (let ((*in-symbol* t)) ;; avoids stuffing the oid tables with symbol namestrings
               (call-next-method)))
            
            ((not *in-symbol*)
             ;; (format t "~%SDLE-Logging ~A" obj)
             (if-let (oid (oid-for-object obj))
                 (progn
                   (store-count +object-proxy-code+ stream)
                   (store-count oid stream))
               ;; else
               (let ((oid (get-new-oid)))
                 (make-known-to-system oid obj)
                 (store-count +assoc-oid-code+ stream)
                 (store-count oid stream)
                 (call-next-method))
               ))

            (t  (call-next-method))
            )))

;; ------------------------------------------------------------
;; update-spec serialization -- sent to the logfile for every mutation
;; of a perstistent slot

(defstore (obj update-spec stream)
  (let* ((per-obj    (admin-spec-object obj))
         (at-spec    (update-spec-at-spec obj))
         (value      (update-spec-value obj)))
    ;; (print (list per-obj at-spec value))
    (store-count  +update-spec-code+ stream)
    (store-object per-obj stream)
    (store-object at-spec stream)
    (store-object value stream)
    ))

(defrestore (update-spec stream)
  (let* ((obj         (restore-object stream))
         (at-spec     (restore-object stream))
         (value       (restore-object stream))
         (*restoring* t))
    ;; (print (list obj at-spec value))
    (update-object obj at-spec value)))

(defmethod log-update (object at-spec value)
  ;; this is also called from persistent-slot mutation
  (when (oid-for-object object)
    (serialize-to-logfile (make-update-spec object at-spec value))))

;; ------------------------------------------------------
;; non-prevalent objects

(defmethod update-object ((object standard-object) slot-name value)
  (setf (slot-value object slot-name) value))

(defmethod update-object ((object structure-object) slot-name value)
  (setf (slot-value object slot-name) value))

(defmethod update-object ((object sequence) nth value)
  (setf (elt object nth) value))

(defmethod update-object ((object array) (pos list) value)
  (setf (apply #'aref object pos) value))

(defmethod update-object ((object array) pos value)
  (setf (row-major-aref object pos) value))

(defmethod update-object ((object hash-table) key value)
  (setf (gethash key object) value))

;; API
(defun mutate-object (object at-spec value)
  (with-locked-system
    (open-system)
    (update-object object at-spec value)
    (log-update object at-spec value)))

;; -------------------------------------------------------------

(defstore (rrs remove-item-spec stream)
  (store-count +remove-item-code+ stream)
  (store-object (admin-spec-object rrs) stream)
  (store-object (remove-item-spec-key rrs) stream))

(defrestore (remove-item-spec stream)
  (let* ((object      (restore-object stream))
         (key         (restore-object stream))
         (*restoring* t))
    (remove-item-from-object object key)))

(defmethod log-remove (object key)
  (when (oid-for-object object)
    (serialize-to-logfile (make-remove-item-spec object key))))
    
(defmethod remove-item-from-object ((object hash-table) key)
  (remhash key object))

;; API
(defun remove-object (object at-spec)
  (with-locked-system
    (open-system)
    (remove-item-from-object object at-spec)
    (log-remove object at-spec)))

;; ------------------------------------------------------------
;; unbound-spec serialization -- sent to the logfile on slot-makunbound

(defstore (obj makunbound-spec stream)
  (let* ((pobj      (admin-spec-object obj))
         (slot-name (makunbound-spec-slot-name obj)))
    (store-count  +makunbound-spec-code+ stream)
    (store-object pobj stream)
    (store-object slot-name stream)
    ))

(defrestore (makunbound-spec stream)
  (let* ((obj         (restore-object stream))
         (slot-name   (restore-object stream))
         (*restoring* t))
    (slot-makunbound obj slot-name)))
    
(defmethod log-makunbound (object slot-name)
  (when (oid-for-object object)
    (serialize-to-logfile (make-makunbound-spec object slot-name))))

;; ------------------------------------------------------------
;; close the currently open logfile and mark nil so that
;; future logging requests are ignored.
;;
;; Why would we want to do this? DM 01/10
;;  answers:
;;   1. at Lisp shut down we need to ensure that the logfile has been
;;      flushed to disk and closed

(defun close-logfile (&rest ignored)
  "Close-system closes out the current log file and disables further
logging of changes to the database."
  (declare (ignore ignored))
  (with-locked-system
    (when-let (logfile (and  *prevalent-system*
                                (current-prevalent-logfile)))
      (close logfile)
      (setf (system-logfile *prevalent-system*) nil))) )

(defun reset-oids ()
  (clrhash (system-oid-table *prevalent-system*))
  (clrhash (system-obj-table *prevalent-system*)))

;; API
(defun close-system ()
  (with-locked-system
    (close-logfile)
    (setf *prevalent-system* nil)))

;; ------------------------------------------------------------

(defun get-directory (filename)
  (pathname-directory
   (merge-pathnames filename
                    (sys:get-folder-path :documents)
                    ;; (translate-logical-pathname "PROJECTS:LISP;")
                    )))

;; ------------------------------------------------------------
;; save-system -- save the entire known universe of prevalent objects
;; to a new .snap snapshot file, rename the existing .snap snapshot
;; file and its associated logfile, and then create a new logfile for
;; recording future mutations.

;; API
(defun save-system (&optional (filename *default-prevalent-filename*))
  (with-locked-system
    (when *prevalent-system*
      (let ((dir   (get-directory filename))
            (fname (pathname-name filename)))
        
        (close-logfile)
        (reset-oids)
        (setf (system-directory *prevalent-system*) dir)
        (ensure-directories-exist (make-pathname
                                   :directory dir))
        
        ;; snapshot the system
        (with-open-file (stream (make-pathname
                                 :directory dir
                                 :name      fname
                                 :type      "snap")
                                :direction :output
                                :if-exists :rename
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
          
          (serialize (system-root-objects *prevalent-system*)
                     stream
                     :use-magic +prevalent-objects-magic+
                     :backend   'prevo-back-end))
        
        ;; setup a new log file
        (let ((logfile (open (make-pathname
                              :directory dir
                              :name      fname
                              :type      "log")
                             :direction :output
                             :if-exists :rename
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))))
          (setf (system-logfile *prevalent-system*) logfile
                (system-logpos  *prevalent-system*) 0)
          )))))

;; ------------------------------------------------------------
;; restore-system -- open a new prevalent repository deserialize all
;; contained objects by reading in the base .snap snapshot and then
;; applying all logfile entries against that restored snapshot.  At
;; the end of this process, the logfile remains open for additional
;; entries.

;; API
(defun restore-system (&optional (filename *default-prevalent-filename*))
  (with-locked-system
    (close-logfile)
    (let ((dir   (get-directory filename))
          (fname (pathname-name filename)))
      
      (ensure-directories-exist (make-pathname
                                 :directory dir))
      
      (setf *prevalent-system* (make-instance 'system)
            (system-directory *prevalent-system*) dir)
      
      ;; restore from last snapshot
      (with-open-file (stream (make-pathname
                               :directory dir
                               :name      fname
                               :type      "snap")
                              :direction :input
                              :if-does-not-exist nil
                              :element-type '(unsigned-byte 8))
        (setf (system-root-objects *prevalent-system*)
              (or (and stream
                       (deserialize stream
                                    :use-magic +prevalent-objects-magic+
                                    :backend   'prevo-back-end))
                  ;; else -- starting out cold. Ensure we have an OID for the root table
                  (make-known-to-system 0 (make-hash-table)))
              ))
      
      (let ((log-length   0)
            (logfile-name (make-pathname
                           :directory dir
                           :name      fname
                           :type      "log")))
        ;; replay the log file
        (with-open-file (stream logfile-name
                              :direction :input
                              :if-does-not-exist nil
                              :element-type '(unsigned-byte 8))
          (when stream
            (setf log-length (replay-logfile stream))
            
            (unless (= log-length (file-length stream))
              (warn "Logfile playback was truncated: ~A~%Some data may have been lost."
                    logfile-name))
            ))
        
        ;; setup new log file
        (let ((logfile (open (make-pathname
                              :directory dir
                              :name      fname
                              :type      "log")
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))))
          (setf (system-logfile *prevalent-system*) logfile
                (system-logpos  *prevalent-system*) log-length))
        ))))

;; ------------------------------------------------------------------

#| ;; for debugging
(defmacro without-handler-case (clause &rest traps)
  (declare (ignore traps))
  `(progn ,clause))
|#

(defun replay-logfile (f)
  ;; by replaying the logfile we mutate the system up to its last
  ;; known state
  (let ((last-pos 0))
    (handler-case
      (loop for ans = (deserialize f
                                   :prefix-length 4
                                   :backend 'prevo-back-end)
            do (setf last-pos (file-position f))
            until (eq ans f))  ;; reading returns stream f on EOF
      (error (exn)
        (print (format-error exn))))
    last-pos))

;; ------------------------------------------------------------

#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))
  (lw:define-action "When quitting image"
                    "Close prevalent store"
                    'close-system))

;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; -------------------------------------------------------------------

#|
;; tests...

(defclass per-class-1 ()
  ((a :accessor a
      :initarg  :a
      :initform 15
      :allocation :prevalent)
   (b :accessor b
      :initform 32
      :initarg  :b)
   (c :accessor c
      :initform "HiC"
      :initarg :c
      :allocation :prevalent))
  (:metaclass prevalent-class))

(defstruct thing a b c)

(open-system)
(root-keys)

(add-root-object :x (make-instance 'per-class-1
                                   :a 19))

(add-root-object :x2 (make-instance 'per-class-1
                                    :b 23))

(setf (a (get-root-object :x)) (get-root-object :x2))

(add-root-object :x3 (make-instance 'per-class-1
                                    :a (get-root-object :x)))

(add-root-object :x4 (make-thing :a 1 :b :two :c (complex 1.2 2.3)))


(save-system)
(close-system)

(open-system)
(add-root-object :x '(a b c))
|#

;; ---------------------------------------------------------------------
#|
(defstruct stock
  date
  open
  high
  low
  close
  volume)

(defun grab-stock (stock-name)
  (let* ((data (stocks::get-data (stocks::market-data
                                  (concatenate 'string stock-name ".csv"))))
         (stock (make-stock
                 :date   (reverse (csv:get-column "Date" data))
                 :open   (stocks::get-numeric-column "Open"   data)
                 :high   (stocks::get-numeric-column "High"   data)
                 :low    (stocks::get-numeric-column "Low"    data)
                 :close  (stocks::get-numeric-column "Close"  data)
                 :volume (stocks::get-numeric-column "Volume" data))))
    stock))

(add-root-object :vix (grab-stock "VIX"))

|#
;; ---------------------------------------------------------------------

#|
(defun get-sorted-hashtable-mappings (ht)
  (let ((map (maps:empty)))
    (maphash (lambda (k v)
               (setf map (maps:add k v map)))
             ht)
    map))

(defun show-hashtable-mappings (ht)
  (maps:iter (lambda (k v)
               (pprint (list k v)))
             (get-sorted-hashtable-mappings ht)))

(show-hashtable-mappings (get-root-object :com.sd.fmt.session))
(show-hashtable-mappings (get-root-object :com.sd.k3))
(show-hashtable-mappings (system-obj-table *prevalent-system*))
(inspect *prevalent-system*)

|#
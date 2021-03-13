;; tl2.lisp -- A very simple, but fast, STM
;; 
;; For Read-only transactions:
;;   about 2.8M trans/sec under no contention
;;   about 0.36 microsec/trans
;;
;; For Read/Write transactions:
;;     About 270K trans/sec under no contention
;;     about 3.7 microsec/trans

;; DM/RAL 12/19
;; -------------------------------------------------------

(defpackage :tl2
  (:use :common-lisp)
  (:export
   :atomic
   :orelse
   :check
   :retry
   :abort-transaction
   :tvar
   :make-tvar
   :get-tvar
   :set-tvar
   :release-tvar
   :tvar-val
   ))

(in-package :tl2)

(declaim (optimize (speed 3) #|(safety 0)|# #+:LISPWORKS (float 0)))

;; ----------------------------------------------------

(defvar *tvar-id*  0)
  
(defstruct (tvar
            (:constructor make-tvar (&optional val)))
  (order-id  (sys:atomic-fixnum-incf *tvar-id*) :read-only t :type fixnum)
  (ver       0)
  val) ;; see NOTE below...

;; NOTE: Mutators should use COW semantics so that old not eq new.
;; E.g., it would be a mistake to perform:
;;
;;     (setf (get-tvar x) (nconc (get-tvar x) another-list))
;;
;; because that would immediately update the old value in-place before
;; its version can be changed (if, indeed, the commit would succeed -
;; it might not).
;;
;; Instead, use FPL read-only semantics as in:
;;
;;     (setf (get-tvar x) (append (get-tvar x) another-list))
;;
;; On successful commit, the new value will overwrite the old value in
;; the TVAR, but not before. Until that time, anyone can read the VAL
;; slot and be assured of getting a valid value.
;;
;; Coordinated changes should always use GET-TVAR inside of an ATOMIC
;; transaction to do this. That is sensitive to commits in progress.
;;
;; ATOMIC transactions should be sure to perform only idempotent
;; operations, not drop-the-bomb, because the transaction may be
;; retried before reaching a successful commit.

(defun cas-ver (tvar old new)
  (sys:compare-and-swap (tvar-ver tvar) old new))

;; ----------------------------------------------------

(defvar *current-transaction*  nil)
(defvar *global-version*       0)

(defvar *ntrans*  0)  ;; total transactions attempted
(defvar *ncomms*  0)  ;; cumm nbr successful commits
(defvar *nrolls*  0)  ;; cumm nbr retrys

(defstruct (transaction
            (:constructor %make-transaction))
  ver rw-map)

(defun make-transaction (rwmap)
  (sys:atomic-fixnum-incf *ntrans*)
  (%make-transaction
   :ver    (if *current-transaction*
               (transaction-ver *current-transaction*)
             *global-version*)
   :rw-map rwmap))

;; ----------------------------------------------------

(define-condition retry-exn ()
  ())

(define-condition retry-for-write-exn ()
  ())

(define-condition abort-exn ()
  ((arg  :reader abort-exn-retval :initarg :retval :initform nil)))

(defun retry ()
  (sys:atomic-fixnum-incf *nrolls*)
  (error (load-time-value
          (make-condition 'retry-exn)
          t)))

(defun retry-for-write ()
  (sys:atomic-fixnum-incf *nrolls*)
  (error (load-time-value
          (make-condition 'retry-for-write-exn)
          t)))

(defun abort-transaction (&optional retval)
  (if retval
      (error 'abort-exn :retval retval)
    ;; else
    (error (load-time-value
            (make-condition 'abort-exn)
            t))))

;; ----------------------------------------------------

(defun extract-tvar-ver (x)
  (loop
   (let ((ver (tvar-ver x)))
     ;; versions are integers, but the slot might be pointing to a
     ;; read/write entry during locked update
     (when (integerp ver)
       (unless (<= ver (transaction-ver *current-transaction*))
         ;; someone either has it locked, in which case the version will
         ;; be beyond our transaction version, or else it is already
         ;; beyond our transaction version
         (retry))
       (return ver)))
   ))

(defun extract-tvar (x)
  (loop
   (let ((ver (extract-tvar-ver x))
         (val (tvar-val x)))
     (sys:ensure-loads-after-loads)
     (when (eq ver (tvar-ver x))
       (return (values ver val)))
     )))

;; ----------------------------------------------------
;; entries in the transaction read/write table
;; we use a map so that tvars are iterated in deterministic order

(defstruct entry
  (tvar  nil  :type tvar   :read-only t)
  (ver   0    :type fixnum :read-only t)
  val
  written-p
  )

(defun find-entry (x)
  (maps:find (transaction-rw-map *current-transaction*)
             (tvar-order-id x)))

;; ------------------------------------------------------

(defun get-tvar (x)
  (if (transaction-rw-map *current-transaction*)
      (um:if-let (entry (find-entry x))
          (entry-val entry)
        ;; else
        (multiple-value-bind (ver val) (extract-tvar x)
          ;; the transaction read list
          (maps:addf
           (transaction-rw-map *current-transaction*)
           (tvar-order-id x) (make-entry
                              :tvar x
                              :ver  ver
                              :val  val))
          val))
    ;; else - so far just read-only
    (multiple-value-bind (ver val) (extract-tvar x)
      (declare (ignore ver))
      val)))

;; ------------------------------------------------------

(defun release-tvar (x)
  ;; remove an entry from the read list
  (um:when-let (entry (find-entry x))
    (unless (entry-written-p entry)
      (maps:removef
       (transaction-rw-map *current-transaction*)
       (tvar-order-id x))
      )))

;; ------------------------------------------------------

(defun set-tvar (x new-val)
  (unless (transaction-rw-map *current-transaction*)
    (retry-for-write))
  (um:if-let (entry (find-entry x))
      (setf (entry-written-p entry) t
            (entry-val entry)       new-val)
    ;; else
    (let ((ver (extract-tvar-ver x)))
      ;; the transaction write map
      ;; map used to keep refs in total order
      ;; (pay as you go sorting)
      (maps:addf
       (transaction-rw-map *current-transaction*)
       (tvar-order-id x) (make-entry
                          :tvar x
                          :ver  ver
                          :val  new-val
                          :written-p t))
      new-val)))

(defsetf get-tvar set-tvar)

;; ------------------------------------------------------

(defun commit-transaction (trans)
  (let ((rw-map (transaction-rw-map trans)))
    (labels
        ((decide (wrver)
           (flet
               ((patch-up (key entry)
                  (declare (ignore key))
                  (let ((tvar  (entry-tvar entry))
                        (ver   (entry-ver  entry)))
                    (if (and wrver
                             (entry-written-p entry))
                        (progn
                          (setf (tvar-val tvar) (entry-val entry))
                          (sys:ensure-stores-after-stores)
                          (setf (tvar-ver tvar) wrver))
                      ;; else
                      (cas-ver tvar entry ver))
                    )))
             (maps:iter rw-map #'patch-up)
             (return-from commit-transaction wrver)))
         
         (acquire (key entry)
           (declare (ignore key))
           (let ((tvar  (entry-tvar entry))
                 (ver   (entry-ver entry)))
             (um:nlet iter ()
               (cas-ver tvar ver entry)
               (let ((v (tvar-ver tvar)))
                 (cond
                  ((eq v entry))  ;; we got it

                  ((not (integerp v))  ;; currently locked - wait to see if it aborts
                   (commit-transaction v)
                   (go-iter))
                  
                  (t
                   ;; tvar was updated ahead of us
                   (decide nil))
                  )))
             )))

      (mp:with-interrupts-blocked
        (maps:iter rw-map #'acquire)
        (decide (sys:atomic-fixnum-incf *global-version*)))
      )))

(defun commit ()
  (when (transaction-rw-map *current-transaction*)
    (unless (commit-transaction *current-transaction*)
      (retry)))
  (sys:atomic-fixnum-incf *ncomms*))

(defun merge-trans (parent trans)
  (setf (transaction-rw-map parent) (transaction-rw-map trans)))

(defun do-orelse (&rest fns)
  (let* ((parent *current-transaction*)
         (rwmap  (when parent
                   (transaction-rw-map parent))))
    (um:nlet iter ((rest-fns fns))
      (when rest-fns
        (let ((*current-transaction* (make-transaction rwmap)))
          (handler-case
              (multiple-value-bind (ans ok)
                  (funcall (car rest-fns))
                (if ok
                    (progn
                      (if parent
                          (merge-trans parent *current-transaction*)
                        (commit))
                      (return-from do-orelse (values ans t)))
                  ;; else
                  (return-from do-orelse ans)))
            
            (retry-exn ()
              (go-iter (cdr rest-fns)))

            (retry-for-write-exn ()
              (when parent
                (retry-for-write))
              (setf rwmap (maps:empty))
              (go-iter fns))
            
            (abort-exn (exn)
              (return-from do-orelse (abort-exn-retval exn)))
            )))
      (if parent
          (retry)
        (go-iter fns))
      )))

(defmacro atomic (&body body)
  `(do-orelse (lambda ()
                (values (progn
                          ,@body)
                        t))))

(defmacro orelse (&rest clauses)
  `(do-orelse ,@(mapcar #`(lambda ()
                            (values ,a1 t))
                        clauses)))

(defmacro check (expr)
  `(unless ,expr
     (retry)))

;; -------------------------------------------------

;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!
#|
(progn
  (defun show-rolls (&optional (duration 1))
    (let ((pcnt (if (zerop *ncomms*)
                    0
                  (/ *nrolls* *ncomms* 0.01)))
          (rate (if (zerop *ncomms*)
                    0
                  (/ *ncomms* duration))))
      (list :retrys *nrolls*
            :commits   *ncomms*
            :percent-retrys pcnt
            :commits-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
            :duration duration
            :commits-per-sec  rate)))
  
  (defun reset ()
    (setf *nrolls* 0)
    (setf *ncomms* 0))
  
  (defparameter *a* (make-tvar 0))
  (defparameter *b* (make-tvar 0))

  (defparameter xretries 0)
  
  (defun check-invariant ()
    (let (a b)
      (atomic
        (setf a (get-tvar *a*)
              b (get-tvar *b*))
        (check (= b (* 2 a))))
      (when (/= b (* 2 a))
        ;; (format t "~%a = ~A, b = ~A  (~A)" a b (mp:get-current-process))
        (bfly:log-info :SYSTEM-LOG "Invariant broken: A = ~A, B = ~A" a b))))
  
  (defun common-code (delta)
    (atomic
      (let* ((a  (+ delta (get-tvar *a*)))
             (b  (* 2 a)))
        (setf (get-tvar *a*) a
              (get-tvar *b*) b)
        )))

  (defparameter *ct* 1000000)
  
  (defun count-up ()
    (loop repeat *ct* do (common-code 1))
    (check-invariant))
  
  (defun count-down ()
    (loop repeat *ct* do (common-code -1))
    (check-invariant))
  
  (defun checker (&rest procs)
    (let ((start (usec:get-time-usec)))
      (loop while (some #'mp:process-alive-p procs)
            do (check-invariant))
      (let ((stop (usec:get-time-usec)))
        (bfly:log-info :SYSTEM-LOG (show-rolls (* 1e-6 (- stop start))))) ))
  
  (defun tst0 ()
    (bfly:log-info :SYSTEM-LOG "Start FSTM Test...")
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0)
          xretries 0)
    (reset)
    (bfly:spawn #'checker
                :name :checker
                :args (mapcar #'bfly:pid-proc
                              (list (bfly:spawn #'count-down
                                                :name :up-counter)
                                    (bfly:spawn #'count-up
                                                :name :down-counter)))))
  
  (defun tst1 (&optional (ct 1000000))
    ;; read-only transactions
    ;; only one thread for no-contention timings
    (setf *ct* ct)
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec)))
      (loop repeat *ct* do (check-invariant)) ;; (count-down)
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))

  (defun tst1rw (&optional (ct 1000000))
    ;; read/write transactions
    ;; only one thread for no-contention timings
    (setf *ct* ct)
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec)))
      (count-down)
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))
  
  (defun tst2 (&optional (ct 1000000))
    (setf *ct* ct)
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter))
          (up   (bfly:spawn-link #'count-up
                                 :name :up-counter)))
      (loop until (= 2 ct)
            do
            (bfly:recv msg
              ((list :Exit-Message pid _ _)
               :when (or (eq pid down)
                         (eq pid up))
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))

  (defun tst3 (&optional (ct 1000000))
    (setf *ct* ct)
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter))
          (up   (bfly:spawn-link #'count-up
                                 :name :up-counter))
          (down2 (bfly:spawn-link #'count-down
                                  :name :down-counter2)))
      (loop until (= 3 ct)
            do
            (bfly:recv msg
              ((list :Exit-Message pid _ _)
               :when (or (eq pid down)
                         (eq pid up)
                         (eq pid down2))
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))

  (defun tst16 (&optional (ct 100000))
    (setf *ct* ct)
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (procs (loop repeat 16 collect
                       (bfly:spawn-link #'count-down))))
      (loop until (= 16 ct)
            do
            (bfly:recv msg
              ((list :Exit-Message pid _ _)
               :when (member pid procs)
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))
  
  ;; -------------------------------------------

  (defun tst4 ()
    ;; only one thread for no-contention timings
    (setf *a* (make-tvar 0)
          *b* (make-tvar 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter)))
      (loop until (= 1 ct)
            do
            (bfly:recv msg
              ((list* :exit-message pid _)
               :when (eq pid down)
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))
  
  ) ;; progn

(defstruct my-cache
  (modification-count 0)
  a
  b)
 
;; modifier code
(defun tst ()
  (let ((cache (make-my-cache)))
  (sys:with-modification-change
   (my-cache-modification-count cache)
   (setf (my-cache-a cache) 15
         (my-cache-b cache) 16))
 
  ;; reading code
  (loop
   (sys:with-modification-check-macro
       my-cache-did-not-change-p (my-cache-modification-count cache)
     (let ((a (my-cache-a cache))
           (b (my-cache-b cache)))
       (when (my-cache-did-not-change-p)
         (return (values a b )))
       )))))
|#

                 
                    

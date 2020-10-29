;; rps.lisp
;; --------------------------------------------------------------------------------------
;; Reactive Programmng Style -- General purpose reactive programming through
;; explicit notifications and by implicit mutation notification.
;;
;; Copyright (C) 2008 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/RAL  08/08
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
(in-package :rps)
;; --------------------------------------------------------------------------------------

(defclass signal ()
  ((lock    :accessor signal-lock       :initform (mpcompat:make-lock))
   (deps    :accessor signal-dependents :initform nil)))

(defclass event (signal)
  ())

(defclass behavior (signal)
  ((cur-val :accessor behavior-cur-val :initarg :cur-val)
   (cmp-fn  :accessor behavior-cmp-fn  :initarg :cmp  :initform 'eql)
   ))

(defclass dependent-behavior (behavior)
  ((fn      :accessor dependent-behavior-function   :initarg :fn)
   (args    :accessor dependent-behavior-args       :initarg :args)
   (trigd   :accessor dependent-behavior-triggered  :initform nil)
   ))

;; ----------------------------------------------------------------------
;; nonsignaling-envelope -- a wrapper class for arguments that should not signal

(defclass nonsignaling-envelope ()
  ((arg     :accessor underlying-behavior :initarg :arg)))

(defmethod underlying-behavior (x)
  x)

;; ----------------------------------------------------------------------

(defmethod cur-val (x)
  x)

(defmethod cur-val ((beh behavior))
  (mpcompat:with-spinlock ((signal-lock beh))
    (behavior-cur-val beh)))

(defmethod cur-val ((arg nonsignaling-envelope))
  (cur-val (underlying-behavior arg)))

;; ----------------------------------------------------------------------

(defun signal? (x)
  (typep x 'signal))

(defun behavior? (x)
  (typep x 'behavior))

;; ----------------------------------------------------------------------

(defun make-behavior (initial-value &key (cmp 'eql))
  (make-instance 'behavior
                 :cur-val initial-value
                 :cmp     cmp))

;; ----------------------------------------------------------------------

(defmethod apply-behavior (beh)
  beh)

(defmethod apply-behavior ((beh dependent-behavior))
  (funcall (dependent-behavior-function beh) beh))

(defmethod apply-behavior ((beh function))
  (funcall beh))

;; ----------------------------------------------------------------------

(defclass rps-monitor ()
  ((queue      :accessor rps-monitor-queue :initform (priq:make-priq))
   (proc       :accessor rps-monitor-proc)
   ))

(defun rps-monitor-loop (queue)
  (catch 'rps-monitor-top
    (loop
     (ignore-errors
       (apply-behavior (priq:popq queue)))
     )))

(defmethod initialize-instance :after ((rpsmon rps-monitor) &key &allow-other-keys)
  (setf (rps-monitor-proc rpsmon) (mpcompat:process-run-function
                                   (symbol-name (gensym "RPS Monitor "))
                                   '()
                                   'rps-monitor-loop
                                   (rps-monitor-queue rpsmon)
                                   )))

(defvar *rps-monitor* nil)

;; ----------------------------------------------------------------------

(defstruct rps-queue-key
  prio time micros)

(defmethod ord:compare ((a rps-queue-key) (b rps-queue-key))
  (let ((dp (ord:compare (rps-queue-key-prio a) (rps-queue-key-prio b))))
    (if (zerop dp)
        (let ((dt (ord:compare (rps-queue-key-time a) (rps-queue-key-time b))))
          (if (zerop dt)
              (ord:compare (rps-queue-key-micros a) (rps-queue-key-micros b))
            dt))
      dp)))
                 
(defvar *micro-count* 0)
(defvar *last-time*   0)

(defun timestamp ()
  (let ((tv (get-universal-time)))
    (unless (= tv *last-time*)
      (setf *micro-count* -1
            *last-time*   tv))
    (values tv (incf *micro-count*))
    ))

(defun new-rps-key (val)
  (multiple-value-bind (seconds micros)
      (timestamp)
    (make-rps-queue-key
     :prio   val
     :time   seconds
     :micros micros)))

;; ----------------------------------------------------------------------

(defun enqueue-fore (item priq)
  (priq:addq priq (new-rps-key 0) item))

(defun enqueue-aft (item priq)
  (priq:addq priq (new-rps-key most-positive-fixnum) item))

;; ----------------------------------------------------------------------

(defmethod enqueue-update ((beh dependent-behavior))
  (ensure-rps)
  (mpcompat:with-spinlock ((signal-lock beh))
    (unless (shiftf (dependent-behavior-triggered beh) t)
      (enqueue-fore beh (rps-monitor-queue *rps-monitor*)) )))

(defmethod enqueue-update (fn)
  (ensure-rps)
  (enqueue-fore fn (rps-monitor-queue *rps-monitor*)))

(defmacro enqueue (&body body)
  `(enqueue-update (lambda () ,@body)))

;; ----------------------------------------------------------------------

(defun !beh (beh val)
  ;; ask the RPS monitor to make the change for us
  (enqueue
   (mpcompat:with-spinlock ((signal-lock beh))
     (update-behavior beh val))))

;; ----------------------------------------------------------------------

(defun kill-rps ()
  (enqueue
    ;; throw must be eval in rps thread
    (throw 'rps-monitor-top nil)) )

(defun restart-rps ()
  (when *rps-monitor*
    (kill-rps))
  (setf *rps-monitor* (make-instance 'rps-monitor)))

(defun ensure-rps ()
  (unless *rps-monitor*
    (setf *rps-monitor* (make-instance 'rps-monitor))))

;; ----------------------------------------------------------------------

(defun do-enqueue-after (fn)
  (ensure-rps)
  (enqueue-aft fn (rps-monitor-queue *rps-monitor*)))

(defmacro enqueue-after (&body body)
  `(do-enqueue-after
    (lambda ()
      ,@body)))

;; ----------------------------------------------------------------------

(defun update-behavior (beh val)
  (unless (funcall (behavior-cmp-fn beh) (behavior-cur-val beh) val)
    (setf (behavior-cur-val beh) val)
    (dolist (dep (signal-dependents beh))
      (enqueue-update dep)) ))

;; ----------------------------------------------------------------------

(defun make-update-fn (fn)
  (lambda (beh)
    (mpcompat:with-spinlock ((signal-lock beh))
      (setf (dependent-behavior-triggered beh) nil)
      (update-behavior beh (apply fn (mapcar 'cur-val (dependent-behavior-args beh)))))
    ))

(defun make-dependent-behavior (fn args &key (cmp 'eql))
  (let* ((initial-value (apply fn (mapcar 'cur-val args)))
         (beh (make-instance 'dependent-behavior
                             :fn      (make-update-fn fn)
                             :args    (mapcar 'underlying-behavior args)
                             :cur-val initial-value
                             :cmp     cmp)))
    (dolist (arg args)
      (if (signal? arg)
          (mpcompat:with-spinlock ((signal-lock arg))
            (push beh (signal-dependents arg)) )))
    initial-value))

;; ----------------------------------------------------------------------

(defun ns (arg)
  ;; wrap an argument in a nonsignaling-envelope
  ;; to prevent adding a dependent behavior to its list of dependencies
  (make-instance 'nonsignaling-envelope
                 :arg arg))

(defun oper (fn cmp &rest args)
  (if (some 'behavior? args)
      (make-dependent-behavior fn args :cmp (or cmp 'eql))
    (apply fn (mapcar 'cur-val args))))

;; ----------------------------------------------------------------------

#|
(defvar *dbg*
  #+:LISPWORKS (debug-stream:make-debug-stream)
  #-:LISPWORKS *standard-output*)

(defparameter x (make-behavior 0))
(defparameter y (make-behavior 1))
(oper #'(lambda (a b)
	  #+:LISPWORKS
          (debug-stream:pr *dbg* (format nil "~&Value = ~A" (+ a b)))
	  #-:LISPWORKS
	  (format t "~&Value = ~A" (+ a b)))
      nil
      x (ns y))

(!beh x 15)
(!beh y 32)
|#

;; --------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------
;; Environment maps from objects to dep-cells that point to those objects
;; Dep-cells are constructed on demand when a dependent or an observer is
;; declared against the object.

(defclass env-deps ()
  ((lock   :accessor env-deps-lock  :initform (mpcompat:make-lock))
   (table  :accessor env-deps-table :initform
           #+:LISPWORKS
           (make-hash-table :test 'eq :weak-kind :key)
           #+:clozure
           (make-hash-table :test 'eq :weak :key)
           #+:SBCL
           (make-hash-table :test 'eq :weakness :key)
           #+:allegro
           (make-hash-table :test 'eq))
   (notifications :accessor env-deps-notifications :initform (make-hash-table :test 'equalp))
   ))
           
(defparameter *environment* (make-instance 'env-deps))

(defmacro with-rps-global-lock (&body body)
  `(mpcompat:with-spinlock ((env-deps-lock *environment*))
     ,@body))

(defmethod get-dependents (obj)
  (with-rps-global-lock
    (gethash obj (env-deps-table *environment*))))

(defmethod set-dependents (obj deps)
  (with-rps-global-lock
    (setf (gethash obj (env-deps-table *environment*)) deps)))

(defmethod remove-dependents (obj)
  (with-rps-global-lock
    (remhash obj (env-deps-table *environment*))))

;; --------------------------------------------------------------------------------------

(defgeneric value-at  (obj &key &allow-other-keys))
(defgeneric basic-set-value (new-val place &key &allow-other-keys))

;; --------------------------------------------------------------------------------------

(defun value (x &rest args &key at)
  (if (null at)
      (value-of x)
    (apply 'value-at (value-of x) args)))

;; ------------------------------

(defmethod value-of (x)
  x)

(defmethod value-of ((sym symbol))
  (symbol-value sym))

;; ------------------------------

(defmethod value-at (x &key at)
  (error "can't dereference object: ~S at: ~S" x at))

(defmethod value-at ((ht hash-table) &key at default)
  (gethash at ht default))

(defmethod value-at ((seq sequence) &key at)
  (elt seq at))

(defmethod value-at ((vec vector) &key at)
  (aref vec at))

(defmethod value-at ((arr array) &key at)
  (cond ((consp at) (apply 'aref arr at))
        (t          (row-major-aref arr at))
        ))
         
(defmethod value-at ((obj standard-object) &key at)
  (slot-value obj at))

#+(OR :LISPWORKS :CLOZURE :SBCL)
(defmethod value-at ((obj structure-object) &key at)
  (slot-value obj at))

;; --------------------------------------------------------------------------------------

(defmethod set-value (new-val place &rest args)
  (enqueue
    (um:if-let (deps (get-dependents place))
        (apply 'set-value-with-dependencies new-val place deps args)
      (apply 'basic-set-value new-val place args))))

;; --------------------------------------------------------------------------------------
;; Use (SETF (VALUE ... ) ...) to effect changes that permit dependent clauses
;; to notice the changes.

;; (defsetf value (place &rest args) (new-value)
;;    `(set-value ,new-value ,place ,@args))

(defun (setf value) (new-value place &rest args)
   (apply 'set-value new-value place args))

;; E.g., for slotted objects like strutures and class instances:
;; (SETF (VALUE obj :AT 'slot-name) new-val)
;;
;; For sequences (strings, vectors, lists)
;; (SETF (VALUE obj :AT index) new-val)
;;
;; For arrays
;; (SETF (VALUE obj :AT '(list-of-indices)) new-val)
;;
;; For symbols
;; (SETF (VALUE 'symbol) new-val)

;; --------------------------------------------------------------------------------------
;; Noticed-Mutable-Objects are faster than plain object because they hold their own
;; dependency lists. Plain objects require a hash-table lookup.

(defclass noticed-mutable-object (behavior)
  ())

(defun make-noticed-mutable-object (val &key (cmp 'eql))
  (make-instance 'noticed-mutable-object
                 :cur-val val
                 :cmp     cmp))

(defmethod value-of ((obj noticed-mutable-object))
  (cur-val obj))

(defmethod basic-set-value (new-val (obj noticed-mutable-object)
                                    &rest args &key &allow-other-keys)
  (declare (ignore args))
  (update-behavior obj new-val))
  
(defmethod get-dependents ((obj noticed-mutable-object))
  (mpcompat:with-spinlock ((signal-lock obj))
    (signal-dependents obj)))

(defmethod set-dependents ((obj noticed-mutable-object) new-deps)
  (mpcompat:with-spinlock ((signal-lock obj))
    (setf (signal-dependents obj) new-deps)))

(defmethod remove-dependents ((obj noticed-mutable-object))
  (set-dependents obj nil))

;; --------------------------------------------------------------------------------------
;; WITH-NOTICED-MUTATIONS -- used when we need to use setf subseq, replace, etc.
;; Any mutations outside of (setf (value ...))

(defmacro with-noticed-mutations ((obj &key at) &body body)
  `(do-with-noticed-mutations ,obj ,at (lambda () ,@body)))

(defun do-with-noticed-mutations (obj at fn)
  (enqueue
    (let ((old-val (value obj :at at)))
      (funcall fn)
      (enqueue-dependents obj at (get-dependents obj)
                          old-val (value obj :at at))
      )))

;; --------------------------------------------------------------------------------------

(defun set-value-with-dependencies (new-val place deps &key at)
  ;; changing some slot of object referred to by cell
  ;; should be performed in rps-monitor
  (let ((old-val (value place :at at)))
    (basic-set-value new-val place :at at)
    (enqueue-dependents place at deps old-val new-val)
    ))

(defun enqueue-dependents (place at deps old-val new-val)
  (labels ((enqueue-these (alst at-sel)
             (um:when-let (atfns (assoc at-sel alst))
               (dolist (dfn-entry (cdr atfns))
                 (um:bind* ((:values (test dfn) (if (consp dfn-entry)
                                                    (um:bind* (((test . dfn) dfn-entry))
                                                      (values test dfn))
                                                  (values 'eql dfn-entry))))
                   (unless (funcall test old-val new-val)
                     (progn ;; enqueue
                       (funcall dfn place at old-val new-val))
                     ))
                 )))
           (enqueue-all ()
             (when at
               (enqueue-these deps at))  ;; dependents of cell object at slot
             (enqueue-these deps nil)    ;; dependents of cell or cell object regardless of slot
             
             (let ((observers (get (type-of place) 'observers)))
               (when at
                 (enqueue-these observers at))   ;; observers of type at slot
               (enqueue-these observers nil))    ;; observers of type regardless of slot
             ))
    (enqueue-all)
    ))

;; --------------------------------------------------------------------------------------

(defmethod basic-set-value (new-val (place symbol) &key &allow-other-keys)
  (setf (symbol-value place) new-val))

(defmethod basic-set-value (new-val (place hash-table) &key at &allow-other-keys)
  (setf (gethash at place) new-val))

(defmethod basic-set-value (new-val (place sequence) &key at &allow-other-keys)
  (setf (elt place at) new-val))

(defmethod basic-set-value (new-val (place vector) &key at &allow-other-keys)
  (setf (aref place at) new-val))

(defmethod basic-set-value (new-val (place array) &key at &allow-other-keys)
  (cond ((consp at) (setf (apply #'aref place at) new-val))
        (t          (setf (row-major-aref place at) new-val))
        ))
  
(defmethod basic-set-value (new-val (place standard-object) &key at &allow-other-keys)
  (setf (slot-value place at) new-val))

#+(OR :LISPWORKS :CLOZURE :SBCL)
(defmethod basic-set-value (new-val (place structure-object) &key at &allow-other-keys)
  (setf (slot-value place at) new-val))

;; --------------------------------------------------------------------------------------
;; ephemeral cells have quiescent val = nil
;; updates to cell persist only until all notifications
;; have been sent, then the cell is reset to nil.
;; No notifications are sent on the reset.

(defclass ephemeral-cell (noticed-mutable-object)
  ())

(defun make-ephemeral-cell ()
  (make-instance 'ephemeral-cell))

(defmethod basic-set-value (new-val (cell ephemeral-cell) &key)
  (setf (behavior-cur-val cell) new-val)
  (enqueue-after
    (setf (behavior-cur-val cell) nil)))

;; --------------------------------------------------------------------------------------
;; Generic dependents list maintenance
;; Dependency lists are associations between the slot-name (or nil) and a list of
;; dependent function entries. A dependent function entry is either a function designator
;; (which gets called unless old-val is EQL to new-val), or else it is a dotted pair whose
;; car is the test to be applied to old-val and new-val, and the cdr is a function designator
;; to be called when old-val and new-val don't agree according to the test function.

(defun trim-atfns (atfns fn)
  (delete fn (cdr atfns) :key #'(lambda (entry)
                                  (if (consp entry)
                                      (cdr entry)
                                    entry))
          ))

(defun make-atfns-entry (fn test)
  (if test
      (cons test fn)
    fn))

(defun save-trimmed-alst (alst atfns setter remover)
  (um:if-let (trimmed-list (delete atfns alst))
      (funcall setter trimmed-list)
    (funcall remover)))

;; -----------------------

(defun add-dep (alst fn at test setter)
  (um:if-let (atfns (assoc at alst))
      (setf (cdr atfns) (cons (make-atfns-entry fn test)
                              (trim-atfns atfns fn)))
    (funcall setter (acons at (list (make-atfns-entry fn test)) alst))
    ))

(defun rem-dep (alst fn at setter remover)
  (um:when-let (atfns (assoc at alst))
    (um:if-let (trimmed-fns (trim-atfns atfns fn))
        (setf (cdr atfns) trimmed-fns)
      (save-trimmed-alst alst atfns setter remover)
      )))

(defun clr-dep (alst at setter remover)
  (um:when-let (atfns (assoc at alst))
    (save-trimmed-alst alst atfns setter remover)))

;; ------------------------------------------------------------------------------------
;; Dependents on specific objects and possibly on some slot of the object

(defun add-dependent (fn obj &key at test)
  (with-rps-global-lock
   (add-dep (get-dependents obj) fn at test
            (um:curry 'set-dependents obj))))

(defun remove-dependent (fn obj &key at)
  (with-rps-global-lock
   (rem-dep (get-dependents obj) fn at
            (um:curry 'set-dependents obj)
            (um:curry 'remove-dependents obj))))

(defun clear-dependents (obj &key at)
  (with-rps-global-lock
   (clr-dep (get-dependents obj) at
            (um:curry 'set-dependents obj)
            (um:curry 'remove-dependents obj))))

(defun clear-all-dependents (obj)
  (remove-dependents obj))

;; --------------------------------------------------------------------------------------
;; Observers against types (types denoted by symbols, as from type-of)

(defun get-observers (type-symbol)
   (get type-symbol 'observers))

(defun set-observers (type-symbol obs-list)
  (setf (get type-symbol 'observers) obs-list))

(defun remove-observers (type-symbol)
  (remprop type-symbol 'observers))

;; ----------------------------

(defun add-observer (fn type-symbol &key at test)
  (with-rps-global-lock
   (add-dep (get-observers type-symbol) fn at test
            (um:curry 'set-observers type-symbol))))

(defun remove-observer (fn type-symbol &key at)
  (with-rps-global-lock
   (rem-dep (get-observers type-symbol) fn at
            (um:curry 'set-observers type-symbol)
            (um:curry 'remove-observers type-symbol))))

(defun clear-observers (type-symbol &key at)
  (with-rps-global-lock
   (clr-dep (get-observers type-symbol) at
            (um:curry 'set-observers type-symbol)
            (um:curry 'remove-observers type-symbol))))

(defun clear-all-observers (type-symbol)
  (remove-observers type-symbol))

;; --------------------------------------------------------------------------------------
;; Notifications by name (including other parameters)

(defun register-notification-action (name fn)
  (with-rps-global-lock
   (let* ((tbl (env-deps-notifications *environment*))
          (lst (gethash name tbl)))
     (setf (gethash name tbl) (adjoin fn lst)))))

(defun remove-notification-action (name fn)
  (with-rps-global-lock
   (let* ((tbl (env-deps-notifications *environment*))
          (lst (gethash name tbl)))
     (setf (gethash name tbl) (delete fn lst)))))
  
(defun clear-notification-actions (name)
  (with-rps-global-lock
   (remhash name (env-deps-notifications *environment*))))

(defun get-notifications (name)
  (with-rps-global-lock
   (gethash name (env-deps-notifications *environment*))))

(defun notify (name &rest notification)
  (enqueue
    (dolist (fn (get-notifications name))
      (apply fn name notification))))

;; --------------------------------------------------------------------------------------
#|

(defvar *dbg*
  #+:LISPWORKS (debug-stream:make-debug-stream)
  #-:LISPWORKS *standard-output*)

(defstruct motor
  status fuel-pump temp)

(defparameter *motor1* (make-motor
                        :status :on
                        :fuel-pump :open
                        :temp  0))

(defun tpr (str old-val new-val)
  #+:LISPWORKS
  (debug-stream:pr *dbg* (format nil "~&~A changing from ~S to ~S" str old-val new-val))
  #-:LISPWORKS
  (format t "~&~A changing from ~S to ~S" str old-val new-val))

(defun monitor-status (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor status" old-val new-val)
  (setf (value obj :at 'fuel-pump) (case new-val
                                     (:on  :open)
                                     (:off :closed))))

(defun monitor-fuel-pump (obj slot old-val new-val)
  (declare (ignore obj slot))
  (tpr "motor fuel-pump" old-val new-val))
  
(defun monitor-temp (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor temperature" old-val new-val)
  (setf (value obj :at 'status)
        (if (< (* (round new-val 0.05) 0.05) 100)
            :on :off)))

(add-dependent 'monitor-status    *motor1* :at 'status    :test 'eq)
(add-dependent 'monitor-fuel-pump *motor1* :at 'fuel-pump :test 'eq)
(add-dependent 'monitor-temp      *motor1* :at 'temp)

;; the following uses implicit RPS -- engine is instantiated as needed.
;; Any series of consecutive changes will not be deferred,
;; so we must use enqueue-after to be sure that all intervening events
;; resulting from each change get accommodated before we make the next change.

(progn
  (setf (value *motor1* :at 'temp) 0)
  (dotimes (x 2)
    (dotimes (y 10)
      (enqueue-after
        (let ((newtemp (+ 98 x (random 0.07) -.04))) 
          (setf (value *motor1* :at 'temp) newtemp)
          )))))
  
|#

;; ------------------------------------------------------------------------
;; RPS-METACLASS and NOTICED-SLOTS-OBJECT
;; Can use normal setf accessor or setf slot in addition to setf value, as above

;; ------------------------------------------------------------------------
;; NOTICED-SLOTS-OBJECT -- root class of objects whose slots are automatically
;; monitored for changes using normal setf slot or setf accessor, in addition to set value.
;;
;; Such objects have their own local dependency list. We make it into a vector of one element
;; which element is the dependency list. That way we don't ever directly modify the dep slot
;; and so we don't trigger monitoring of that particular slot.
;;
;; Normal slots of defined subclasses will be monitored. Be sure to use (:optimize-slot-access nil)
;; in all subclasses. Otherwise, only setf slot will be montiored, not setf accesssor.

(defclass noticed-slots-metalevel-class (standard-class)
  ())

(defmethod clos:validate-superclass ((class noticed-slots-metalevel-class)
                                     (super standard-class))
  t)

(defclass noticed-slots-root-class ()
  ((|%ns-dependents|  :accessor noticed-slots-dependents :initform (vector nil))
   (|%ns-lock|        :accessor noticed-slots-lock       :initform (mpcompat:make-lock)))
  ;; (:optimize-slot-access nil)
  (:metaclass noticed-slots-metalevel-class))

(defmethod get-dependents ((obj noticed-slots-root-class))
  (mpcompat:with-spinlock ((noticed-slots-lock obj))
    (aref (noticed-slots-dependents obj) 0)))

(defmethod set-dependents ((obj noticed-slots-root-class) new-dependents)
  (mpcompat:with-spinlock ((noticed-slots-lock obj))
    (setf (aref (noticed-slots-dependents obj) 0) new-dependents)))

(defmethod remove-dependents ((obj noticed-slots-root-class))
  (set-dependents obj nil))

(defmethod set-value (new-val (place noticed-slots-root-class) &rest args)
  (apply 'basic-set-value new-val place args))

;; ----------------------------------------------------------------------------------------

#+(OR :LISPWORKS :ALLEGRO)
(defmethod (setf clos:slot-value-using-class) :around
           (new-val (class noticed-slots-metalevel-class) object slot-name)
  ;; (format t "~&setf slot: ~A~%" slot-name) ;; for debugging
  (cond ((or (eq slot-name '|%ns-lock|)        ;; names which are unlikely to be used
             (eq slot-name '|%ns-dependents|)) ;;   for other purposes
         (call-next-method))

        (t 
         (enqueue
           (let ((old-val (and (slot-boundp object slot-name)
                               (slot-value object slot-name))))
             (call-next-method)
             (enqueue-dependents object slot-name
                                 (get-dependents object)
                                 old-val new-val) )))
        ))
  
#+(OR :CLOZURE :SBCL)
(defmethod (setf clos:slot-value-using-class) :around
           (new-val (class noticed-slots-metalevel-class) object slot-def)
  ;; (format t "~&setf slot: ~A~%" slot-def) ;; for debugging
  (let ((slot-name (clos:slot-definition-name slot-def)))
    
    (cond ((or (eq slot-name '|%ns-lock|)        ;; names which are unlikely to be used
               (eq slot-name '|%ns-dependents|)) ;;   for other purposes
           (call-next-method))
          
          (t 
           (enqueue
             (let ((old-val (and (slot-boundp object slot-name)
                                 (slot-value object slot-name))))
               (call-next-method)
               (enqueue-dependents object slot-name
                                   (get-dependents object)
                                   old-val new-val) )))
          )))
  
;; ----------------------------------------------------------------------------------------

(defun ensure-persistent-root-superclass (class direct-superclasses)
  ;; ensure that any persistent classes
  ;; - as implied by using metaclass persistent-metalevel-class -
  ;; gets the persistent root class as one of its superclasses.
  
  (let ((root-class (find-class 'noticed-slots-root-class))
        (pobjs-mc   (find-class 'noticed-slots-metalevel-class)))
    (if (or (eq class root-class)
            (some (lambda (super)
                    (eq pobjs-mc (class-of super)))
                  direct-superclasses))
        direct-superclasses
      (append direct-superclasses (list root-class)))))

(defmethod initialize-instance :around ((class noticed-slots-metalevel-class) &rest all-keys
                                        &key direct-superclasses)
  (let ((new-direct-superclasses (ensure-persistent-root-superclass class direct-superclasses)))
    (apply #'call-next-method
           class
           :direct-superclasses new-direct-superclasses
           #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
           all-keys)))

(defmethod reinitialize-instance :around ((class noticed-slots-metalevel-class) &rest all-keys
                                          &key direct-superclasses)
  (let ((new-direct-superclasses (ensure-persistent-root-superclass class direct-superclasses)))
    (apply #'call-next-method
           class
           :direct-superclasses new-direct-superclasses
           #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
           all-keys)))

;; ----------------------------------------------------------------------------------------

(defmacro define-monitored-class (name superclass slots)
  `(defclass ,name ,(or superclass '(noticed-slots-root-class)) ,slots
     ;; (:optimize-slot-access nil)
     (:metaclass noticed-slots-metalevel-class)))

#+:LISPWORKS
(editor:setup-indent "define-monitored-class" 2 2)

;; ----------------------------------------------------------------------------------------
#| ;; test it out...

(defvar *dbg*
  #+:LISPWORKS (debug-stream:make-debug-stream)
  #-:LISPWORKS *standard-output*)

(define-monitored-class cmotor ()
  ((status    :accessor cmotor-status    :initform nil :initarg :status)
   (fuel-pump :accessor cmotor-fuel-pump :initform nil :initarg :fuel-pump)
   (temp      :accessor cmotor-temp      :initform nil :initarg :temp)))

;; alternatively, with explicit metaclass
(defclass cmotor ()
  ((status    :accessor cmotor-status    :initform nil :initarg :status)
   (fuel-pump :accessor cmotor-fuel-pump :initform nil :initarg :fuel-pump)
   (temp      :accessor cmotor-temp      :initform nil :initarg :temp))
  ;; (:optimize-slot-access nil)
  (:metaclass noticed-slots-metalevel-class))

(defparameter *motor2* (make-instance 'cmotor
                                      :status :on
                                      :fuel-pump :open
                                      :temp  0))

(defun ctpr (str old-val new-val)
  #+:LISPWORKS
  (debug-stream:pr *dbg* (format nil "~&~A changing from ~S to ~S" str old-val new-val))
  #-:LISPWORKS
  (format t "~&~A changing from ~S to ~S" str old-val new-val))

(defun cmonitor-status (obj slot old-val new-val)
  (declare (ignore slot))
  (ctpr "motor status" old-val new-val)
  (setf (cmotor-fuel-pump obj) (case new-val
                                 (:on  :open)
                                 (:off :closed))))

(defun cmonitor-fuel-pump (obj slot old-val new-val)
  (declare (ignore obj slot))
  (ctpr "motor fuel-pump" old-val new-val))
  
(defun cmonitor-temp (obj slot old-val new-val)
  (declare (ignore slot))
  (ctpr "motor temperature" old-val new-val)
  (setf (cmotor-status obj)
        (if (< (* (round new-val 0.05) 0.05) 100)
            :on :off)))

(add-dependent 'cmonitor-status    *motor2* :at 'status    :test 'eq)
(add-dependent 'cmonitor-fuel-pump *motor2* :at 'fuel-pump :test 'eq)
(add-dependent 'cmonitor-temp      *motor2* :at 'temp)

;; the following uses implicit RPS -- engine is instantiated as needed.
;; Any series of consecutive changes will not be deferred,
;; so we must use enqueue-after to be sure that all intervening events
;; resulting from each change get accommodated before we make the next change.

(progn
  (setf (value *motor2* :at 'temp)
        ;; (cmotor-temp *motor2*)
        0)
  (dotimes (x 2)
    (dotimes (y 10)
      (enqueue-after
        (let ((newtemp (+ 98 x (random 0.07) -.04))) 
          (setf ;; (value *motor2* :at 'temp)
                (cmotor-temp *motor2*) newtemp)
          )))))
  
|#
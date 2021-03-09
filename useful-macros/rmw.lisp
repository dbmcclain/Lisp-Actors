;; rmw.lisp -- Read/Modify/Write for general places
;;
;; Lock-free, ABA hazard-free, guaranteed mutation in SMP
;;
;; DM/RAL  11/20
;; -----------------------------------------------------------
(in-package :um)
;; ----------------------------------------------------------
;; To overcome the RMW ABA-Problem...
;;
;; We need to guard against ABA hazards during RMW. So we perform RMW
;; with a 2-phase protocol: First acquire its value, while marking
;; place as in-progress for update. The marking is done with a
;; descriptor that enables any thread to help toward completion if we
;; get interrupted. Then carry out the computation of the new value
;; and perform the update, if it hasn't already been done by anther
;; thread on our behalf. This protocol requires two CAS operations.
;;
;; We can no longer simply read nor write the value in a shared
;; location.  It might be in a state that is being updated by another
;; thread. If so, we nudge it along to final resolution before
;; acquiring/setting its value.
;;
;; As long as all modofiations to place are performed with WR or RMW
;; then we can be assured that there will be no ABA hazards. Reading
;; of place should be performed with RD, which helps push along any
;; update in progress before returning its stable value.
;;
;; Mutations performed with WR and RMW force all pending reads/writes
;; to be completed, and ensures cache coherency with other cores by
;; invalidating their cache lines for mutated place.
;;
;; ----------------------------------------------------------------
;; Assured lock-free, ABA hazard immune, mutation
;;
;; We need two primitives for every class of object:
;;  (BASIC-VAL obj) - returns the value contained in obj at this moment.
;;  (BASIC-CAS obj old new) - accomplishes a CAS on obj, returning T/F.
;;
;; We can't really know what value is held in obj for any length of
;; time after our mutation of it.  Another thread could come along and
;; mutate right after we did.  So we break precedent with SETF and
;; don't bother returning what we just set it to. When you need to
;; know what value is held in obj, perform a RD on it to get the value
;; it had at the time of the RD call.

(defgeneric basic-val (obj)
  (:method ((obj symbol))
   (symbol-value obj))
  (:method ((obj cons))
   (car obj))
  (:method ((obj simple-vector))
   (svref obj 0)))

#+:LISPWORKS
(defgeneric basic-cas (obj old new)
  ;; BASIC-CAS serves as an atomic sync point. All pending loads and
  ;; stores are forced at this point. And matching cache lines are
  ;; invalidated on other cores.
  (:method ((obj symbol) old new)
   (sys:compare-and-swap (symbol-value obj) old new))
  (:method ((obj cons) old new)
   (sys:compare-and-swap (car obj) old new))
  (:method ((obj simple-vector) old new)
   (sys:compare-and-swap (svref obj 0) old new)))

;; --------------------------------------------------------

(defstruct rmw-desc
  ;; contains captured old value, and mutator function
  old new-fn post-fn)

(defun rmw-help (obj desc)
  ;; Here it is known that obj contained desc, but by now it might not
  ;; still contain desc.
  ;;
  ;; NOTE: new-fn can be called repeatedly and from arbitrary threads,
  ;; so it should be idempotent, and don't let it fail...
  ;;
  (with-slots (old new-fn post-fn) desc
    (let ((new (funcall new-fn old)))
      ;; the following CAS could fail if another thread already
      ;; performed this task. That's okay.
      (when (basic-cas obj desc new)
        (funcall post-fn))
    )))

(defun #1=rd (obj)
  (tagbody
   again
   (let ((v (basic-val obj)))
     (cond ((rmw-desc-p v)
            ;; RMW in progress, nudge it along
            (rmw-help obj v)
            (go again))
            
            (t  (return-from #1# v))
            ))))
           
(defmethod rmw (obj new-fn &optional (post-fn #'lw:do-nothing))
  ;; NOTE: RMW does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  (let ((desc (make-rmw-desc
               :new-fn  new-fn
               :post-fn post-fn)))
    (declare (dynamic-extent desc))
    (tagbody
     again
     (let ((old (rd obj)))
       ;; <-- ABA could happen here
       (setf (rmw-desc-old desc) old)
       ;; <-- ABA could happen here
       (if (basic-cas obj old desc)
           ;; At this point we know that some thread will accomplish
           ;; our task if we get preempted. And we know that no
           ;; further ABA hazard can happen to the container contents
           ;; that held captured old val.
           (rmw-help obj desc)
         ;; else - try again
         (go again)))
     )))

;; -----------------------------------------------------

#+:LISPWORKS
(defgeneric basic-atomic-exch (obj val)
  ;; BASIC-ATOMIC-EXCH serves as an atomic sync point. All pending
  ;; loads and stores are forced at this point. And matching cache
  ;; lines are invalidated on other cores.
  (:method ((obj symbol) val)
   (sys:atomic-exchange (symbol-value obj) val))
  (:method ((obj cons) val)
   (sys:atomic-exchange (car obj) val))
  (:method ((obj simple-vector) val)
   (sys:atomic-exchange (svref obj 0) val)))

(defmethod wr (obj new)
  ;; NOTE: WR does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  ;;
  ;; Since RMW uses an idempotent mutator function, that function
  ;; should be side-effect free. Hence, you won't mind if we never
  ;; happen to call it... (this might overwrite an open descriptor)
  (basic-atomic-exch obj new)
  (values))

;; ----------------------------------------------

;; -- end of usefull_macros.lisp -- ;;
#|
;; test speed of spinlocking...
(defun tst (n)
  (let ((x (list nil)))
    (labels ((grab (n)
               (rmw x (lambda (old)
                        (declare (ignore old))
                        n)))
             (iter (nn)
               (loop for ix from nn to n by 4 do
                     (grab ix))))
      (time
       (par
         (iter 0)
         (iter 1)
         (iter 2)
         (iter 3))
       ))))
                     
(defun tst (n)
  (let ((x    (list nil))
        (lock (mp:make-lock)))
    (labels ((grab (n)
               (mp:with-lock (lock)
                 (setf (car x) (constantly n))))
             (iter (nn)
               (loop for ix from nn to n by 4 do
                     (grab ix))))
      (time
       (par
         (iter 0)
         (iter 1)
         (iter 2)
         (iter 3))
       ))))    

#+:LISPWORKS
(defun tst (n)
  (let ((x    (list nil))
        (lock (mp:make-lock)))
    (declare (dynamic-extent x lock))
    (labels ((grab (n)
               (loop for old = (car x)
                     until (and ;; (eq old (car x))
                                (sys:compare-and-swap (car x) old n))))
             (iter (nn)
               (loop for ix from nn to n by 4 do
                     (grab ix))))
      (declare (dynamic-extent #'grab #'iter))
      (time
       (par
         (iter 0)
         (iter 1)
         (iter 2)
         (iter 3))
       ))))

(defun tst (n)
  (let ((x    (list nil))
        (lock (mp:make-lock)))
    (declare (dynamic-extent x lock))
    (labels ((grab (n)
               (mp:with-lock (lock)
                 (setf (car x) n)))
             (iter (nn)
               (loop for ix from nn to n by 4 do
                     (grab ix))))
      (declare (dynamic-extent #'grab #'iter))
      (time
       (par
         (iter 0)
         (iter 1)
         (iter 2)
         (iter 3))
       ))))
|#


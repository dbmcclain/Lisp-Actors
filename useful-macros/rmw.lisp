;; rmw.lisp -- Read/Modify/Write for general places
;;
;; Lock-free, ABA hazard-free, guaranteed mutation in SMP
;;
;; DM/RAL  11/20
;; -----------------------------------------------------------
(in-package :um)
;; ----------------------------------------------------------
;; To overcome the CAS ABA-Problem...
;;
;; We can no longer simply read nor write the value in a shared
;; location.  It might be in a state that is being updated by another
;; thread. If so we nudge it along to final resolution before
;; acquiring/setting its value.
;;
;; We also need to guard against ABA hazards during RMW. So we perform
;; RMW with a 2-phase protocol. First acquire the value, then mark its
;; place as in-progress in such a way that other threads could help us
;; complete if we get interrupted.
;;
;; ----------------------------------------------------------------
;; Assured lock-free, ABA hazard immune, mutation
;;
;;  We need two primitives for every class of object:
;;  (VAL obj) - returns the value contained in obj at this moment.
;;  (CAS obj old new) - this primitve accomplishes a CAS on obj, returning T/F.
;;
;; But after mutation, we can't really know what value is held in obj.
;; Another thread could come along and mutate right after we did.  So
;; we break precedent with SETF and don't bother returning what we
;; just set it to.

(defgeneric basic-val (obj)
  (:method ((obj symbol))
   (symbol-value obj))
  (:method ((obj cons))
   (car obj))
  (:method ((obj simple-vector))
   (svref obj 0)))

#+:LISPWORKS
(defgeneric basic-cas (obj old new)
  (:method ((obj symbol) old new)
   (sys:compare-and-swap (symbol-value obj) old new))
  (:method ((obj cons) old new)
   (sys:compare-and-swap (car obj) old new))
  (:method ((obj simple-vector) old new)
   (sys:compare-and-swap (svref obj 0) old new)))

;; --------------------------------------------------------

(defstruct rmw-desc
  ;; contains captured old value, and mutator function
  old new-fn)

(defun rmw-help (obj desc)
  ;; Here it is known that obj contained desc, but by now it might not
  ;; still contain desc.
  ;;
  ;; NOTE: new-fn can be called repeatedly and from arbitrary threads,
  ;; so it should be idempotent, and don't let it fail...
  ;;
  (with-slots (old new-fn) desc
    (let ((new (funcall new-fn old)))
      ;; the following CAS could fail if another thread already
      ;; performed this task. That's okay.
      (basic-cas obj desc new))
    ))

(defun rd (obj)
  (um:nlet-tail iter ()
    (let ((v (basic-val obj)))
      (cond ((rmw-desc-p v)
             (rmw-help obj v)
             (iter))
            
            (t  v)
            ))))
           
(defmethod rmw (obj new-fn)
  ;; NOTE: RMW does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  (let ((desc (make-rmw-desc
               :new-fn new-fn)))
    (um:nlet-tail iter ()
      (let ((old (rd obj)))
        (setf (rmw-desc-old desc) old)
        (if (basic-cas obj old desc)
            ;; At this point we know that some thread will accomplish
            ;; our task if we get interrupted. And we know that no ABA
            ;; hazard can happen to captured old val.
            (rmw-help obj desc)
          ;; else - try again
          (iter))
        ))
    ))

(defmethod wr (obj new)
  ;; NOTE: WR does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  (rmw obj (constantly new)))

;; -----------------------------------------------------

#+:LISPWORKS
(defgeneric basic-atomic-exch (obj val)
  ;; exch-fn serves as an atomic sync point
  ;; all loads and stores are forced at this point
  (:method ((obj symbol) val)
   (sys:atomic-exchange (symbol-value obj) val))
  (:method ((obj cons) val)
   (sys:atomic-exchange (car obj) val))
  (:method ((obj simple-vector) val)
   (sys:atomic-exchange (svref obj 0) val)))

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


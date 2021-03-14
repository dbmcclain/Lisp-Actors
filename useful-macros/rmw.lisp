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

#|
;; This version is simple and direct... more or less...

;; In practice, I find it to be consistently faster and use fewer CPU
;; cycles than the more correct version below.
;;
;; However, it can theoretically require an unbounded number of
;; attempts from any thread, in trying to establish its intended new
;; value. If a thread is unlucky enough to have every CAS thwarted, it
;; can be stalled indefinitely.
 
(declaim (inline rd))

(defun rd (obj)
  (basic-val obj))

(defmethod rmw (obj newfn)
  (prog ()
    again
    (let ((old (basic-val obj)))
      (unless (basic-cas obj old (funcall newfn old))
        (go again)))
    ))
|#

#| |#
;; This version uses a 2-phase approach, where competing threads help
;; complete an attempt in progress before launching their own.
;;
;; It has a theoretically bounded number of CAS attempts (= N+1) for N
;; competing threads. If a thread cannot complete, another thread will
;; do so for it.
(defstruct rmw-desc
  ;; contains captured old value, and mutator function
  ;;
  ;; WARNING! Mutator function can be called on any thread, and called
  ;; concurrently. It must be side-effect free, and idempotent.
  old new-fn)

(defun rmw-help (obj desc)
  ;; Here it is known that obj did contain desc, but by now it might
  ;; not still contain desc.
  ;;
  ;; NOTE: new-fn can be called repeatedly and from arbitrary threads,
  ;; so it should be idempotent, and don't let it fail...
  ;;
  (with-slots (old new-fn) desc
    ;; the following CAS could fail if another thread already
    ;; performed this task. That's okay.
    (basic-cas obj desc (funcall new-fn old))))

(defun rd (obj)
  (prog ()
    again
    (let ((v (basic-val obj)))
      (cond ((rmw-desc-p v)
             ;; RMW in progress, nudge it along
             (rmw-help obj v)
             (go again))
            
            (t  (return v))
            ))))
           
(defmethod rmw (obj new-fn)
  ;; NOTE: RMW does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  (prog ((desc (make-rmw-desc
                :new-fn  new-fn)))
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
    ))
#||#

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
  (rmw obj (constantly new)))

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
#|
(defun tst-rmw ()
  (time
   (let* ((lst (list (list nil nil nil nil nil)))
          (n     1000000)
          (ctr   (list 0)))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmw lst (lambda (lst)
                                     (sys:atomic-incf (car ctr))
                                     (destructuring-bind (a b c d e) lst
                                       (list (cons ix a) b c d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmw lst (lambda (lst)
                                     (sys:atomic-incf (car ctr))
                                     (destructuring-bind (a b c d e) lst
                                       (list a (cons ix b) c d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmw lst (lambda (lst)
                                     (sys:atomic-incf (car ctr))
                                     (destructuring-bind (a b c d e) lst
                                       (list a b (cons ix c) d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmw lst (lambda (lst)
                                     (sys:atomic-incf (car ctr))
                                     (destructuring-bind (a b c d e) lst
                                       (list a b c (cons ix d) e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmw lst (lambda (lst)
                                     (sys:atomic-incf (car ctr))
                                     (destructuring-bind (a b c d e) lst
                                       (list a b c d (cons ix e))))))))
     (sleep 5)
     (destructuring-bind ((a b c d e)) lst
       (assert (= n
                  (length a)
                  (length b)
                  (length c)
                  (length d)
                  (length e))))
     ;; (inspect lst)
     (list :count (car ctr))
     )))
#|
(tst-rmw)
|#

(defun rmwx (obj newfn)
  (prog ()
    again
    (let ((old (basic-val obj)))
      (unless (basic-cas obj old (funcall newfn old))
        (go again)))
    ))

(defun tst-rmwx ()
  (time
   (let* ((lst (list (list nil nil nil nil nil)))
          (n  1000000)
          (ctr (list 0)))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmwx lst (lambda (lst)
                                      (sys:atomic-incf (car ctr))
                                      (destructuring-bind (a b c d e) lst
                                        (list (cons ix a) b c d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmwx lst (lambda (lst)
                                      (sys:atomic-incf (car ctr))
                                      (destructuring-bind (a b c d e) lst
                                        (list a (cons ix b) c d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmwx lst (lambda (lst)
                                      (sys:atomic-incf (car ctr))
                                      (destructuring-bind (a b c d e) lst
                                        (list a b (cons ix c) d e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmwx lst (lambda (lst)
                                      (sys:atomic-incf (car ctr))
                                      (destructuring-bind (a b c d e) lst
                                        (list a b c (cons ix d) e)))))))
     (ac:spawn-worker (lambda ()
                        (dotimes (ix n)
                          (rmwx lst (lambda (lst)
                                      (sys:atomic-incf (car ctr))
                                      (destructuring-bind (a b c d e) lst
                                        (list a b c d (cons ix e))))))))
     (sleep 5)
     (destructuring-bind ((a b c d e)) lst
       (assert (= n
                  (length a)
                  (length b)
                  (length c)
                  (length d)
                  (length e))))
     ;; (inspect lst)
     (list :count (car ctr))
     )))
#|
(tst-rmw)
(tst-rmwx)
|#

|#
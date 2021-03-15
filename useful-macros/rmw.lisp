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

(defgeneric set-basic-val (obj new)
  (:method ((obj symbol) new)
   (setf (sys:globally-accessible (symbol-value obj)) new))
  (:method ((obj cons) new)
   (setf (sys:globally-accessible (car obj)) new))
  (:method ((obj simple-vector) new)
   (setf (sys:globally-accessible (svref obj 0)) new)))

(defsetf basic-val set-basic-val)

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

#|
  ;; --- See below for compiler optimized forms ---
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
|#

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

#|
(defmethod wr (obj new)
  ;; NOTE: WR does *NOT* return new val. It could be wrong to assume
  ;; that new val corresponds to what is currently stored in obj.
  ;; Remember we are in a dynamic SMP environment.
  ;;
  ;; Since RMW uses an idempotent mutator function. That function
  ;; should be side-effect free. Hence, you won't mind if we never
  ;; happen to call it... (this function might overwrite an open
  ;; descriptor)
  (basic-atomic-exch obj new)
  new)
|#
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
;; ---------------------------------------------

(defgeneric rd-object  (obj))
(defgeneric wr-object  (obj new))
(defgeneric rmw-object (obj new-fn))

(defmacro gen-rmw-funcs (place)
  (flet ((gen-name (kind &optional (placer (car place)))
           (intern (format nil "~A-~A"
                           (string kind)
                           (string placer))
                   )))
    (lw:with-unique-names (obj ix v new new-fn desc old again)
      (let* (type
             (deftype     'defun)
             rd-name
             (rd-args     `(,obj))
             wr-name
             (wr-args     `(,obj ,new))
             rmw-name
             (rmw-args    `(,obj ,new-fn))
             accessor
             (cas-fn      'sys:compare-and-swap)
             cas-accessor
             (exch-fn     'sys:atomic-exchange)
             extra-defs)
        
        (ecase (car place)
          ((object)
           (setf accessor     `(basic-val ,obj)
                 cas-fn       'basic-cas
                 cas-accessor obj
                 exch-fn      `basic-atomic-exch
                 deftype      'defmethod))
          ((car)
           (setf type         'cons
                 accessor     `(car ,obj)))
          ((cdr)
           (setf type         'cons
                 accessor     `(cdr ,obj)))
          ((symbol-value)
           (setf type         'symbol
                 accessor     `(symbol-value ,obj)))
          ((svref)
           (setf type         'simple-vector
                 rd-args      `(,obj ,ix)
                 wr-args      `(,obj ,ix ,new)
                 rmw-args     `(,obj ,ix ,new-fn)
                 accessor     `(svref ,obj ,ix)))
          ((struct)
           (destructuring-bind (_ place-name struct-name accessor-fn) place
             (declare (ignore _))
             (setf type       struct-name
                   rd-name    (gen-name :rd  place-name)
                   wr-name    (gen-name :wr  place-name)
                   rmw-name   (gen-name :rmw place-name)
                   accessor   `(,accessor-fn ,obj)
                   extra-defs `((defmethod rd-object ((,obj ,struct-name))
                                  (,rd-name ,obj))
                                (defmethod wr-object ((,obj ,struct-name) ,new)
                                  (,wr-name ,obj ,new))
                                (defmethod rmw-object ((,obj ,struct-name) ,new-fn)
                                  (,rmw-name ,obj ,new-fn))))
             )))
        
        (macrolet ((set-default (sym form)
                     `(unless ,sym
                        (setf ,sym ,form))))
          (set-default rd-name      (gen-name :rd))
          (set-default wr-name      (gen-name :wr))
          (set-default rmw-name     (gen-name :rmw))
          (set-default cas-accessor accessor))
        
        ;; Same basic structure to all versions, so define the logic
        ;; in just one place...
        `(progn
           (export '(,rd-name ,wr-name ,rmw-name))
           (,deftype ,rd-name ,rd-args
             #F
             ,@(when type
                 `((declare (,type ,obj))))
             (prog ()
               ,again
               (let ((,v ,accessor))
                 (cond ((rmw-desc-p ,v)
                        (let ((,new (funcall (rmw-desc-new-fn ,v) (rmw-desc-old ,v))))
                          (if (,cas-fn ,cas-accessor ,v ,new)
                              (return ,new)
                            (go ,again))
                          ))
                       (t
                        (return ,v))
                       ))))
           
           (,deftype ,wr-name ,wr-args
             #F
             (declare (,type ,obj))
             ;; this gives a compiler error for unused result
             ;;   (,exch-fn ,cas-accessor ,new)
             ;;   ,new
             #|
             (or (and (,exch-fn ,cas-accessor ,new)
                      ,new)
                 ,new)
             |#
             (setf (sys:globally-accessible ,accessor) ,new))
           
           (,deftype ,rmw-name ,rmw-args
             #F
             (declare (,type ,obj))
             (let ((,desc (make-rmw-desc
                           :new-fn ,new-fn)))
               (prog ()
                 ,again
                 (let ((,old (,rd-name ,@rd-args)))
                   (setf (rmw-desc-old ,desc) ,old)
                   (if (,cas-fn ,cas-accessor ,old ,desc)
                       (,cas-fn ,cas-accessor ,desc (funcall ,new-fn ,old))
                     (go ,again))
                   ))))
           ,@extra-defs)
        ))))

(defmacro gen-struct-rmw-funcs (place-name (struct-name accessor-fn))
  `(gen-rmw-funcs (struct ,place-name ,struct-name ,accessor-fn)))

(progn
  (gen-rmw-funcs (object))
  (gen-rmw-funcs (car obj))
  (gen-rmw-funcs (cdr obj))
  (gen-rmw-funcs (symbol-value obj))
  (gen-rmw-funcs (svref obj ix)))

(defmacro rd (place)
  (cond ((consp place)
         (destructuring-bind (placer obj &optional arg) place
           (case placer
             ((car)
              `(rd-car ,obj))
             ((cdr)
              `(rd-cdr ,obj))
             ((symbol-value)
              `(rd-symbol-value ,obj))
             ((svref)
              `(rd-svref ,obj ,arg))
             (t
              `(rd-object ,place))
             )))
        (t
         `(rd-object ,place))
        ))

(defmacro wr (place new)
  (cond ((consp place)
         (destructuring-bind (placer obj &optional arg) place
           (case placer
             ((car)
              `(wr-car ,obj ,new))
             ((cdr)
              `(wr-cdr ,obj ,new))
             ((symbol-value)
              `(wr-symbol-value ,obj ,new))
             ((svref)
              `(wr-svref ,obj ,arg ,new))
             (t
              `(wr-object ,place ,new))
             )))
        (t
         `(wr-object ,place ,new))
        ))

(defmacro rmw (place new-fn)
  (cond ((consp place)
         (destructuring-bind (placer obj &optional arg) place
           (case placer
             ((car)
              `(rmw-car ,obj ,new-fn))
             ((cdr)
              `(rmw-cdr ,obj ,new-fn))
             ((symbol-value)
              `(rmw-symbol-value ,obj ,new-fn))
             ((svref)
              `(rmw-svref ,obj ,arg ,new-fn))
             (t
              `(rmw-object ,place ,new-fn))
             )))
        (t
         `(rmw-object ,place ,new-fn))
        ))

(defgeneric cas (obj old new)
  (:method (obj old new)
   (basic-cas obj old new)))

(defgeneric atomic-exch (obj new)
  (:method (obj new)
   (basic-atomic-exch obj new)))

(defgeneric atomic-incf (obj))
(defgeneric atomic-decf (obj))


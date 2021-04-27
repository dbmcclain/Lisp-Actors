
(in-package :actors)

;; --------------------------------------------------
;; Agents - thread exclusive code with local state
;; No thread switching involved, so dispatch CALL is a direct call/return
;; Runs in the process of the caller.
;; --------------------------------------------------

#|
(defmacro make-agent (bindings &rest body)
  (um:with-gensyms (lock this msg)
      (let ((g!self-call (intern (string 'self-call))))
        `(let ((,lock  (mp:make-lock))
               (,this  nil)
               ,@bindings)
           (setf ,this
                 (lambda (&rest ,msg)
                   (mp:with-lock (,lock)
                     (let ((*current-actor* ,this))
                       (labels ((,g!self-call (&rest ,msg)
                                  (dcase ,msg
                                    (actor-internal-message:continuation (fn &rest args)
                                                                         (apply fn args))
                                    ,@body)))
                         (apply #',g!self-call ,msg)))
                     ))
                 ))
        )))

(defmacro make-shared-agent (bindings &key shared excl)
  (um:with-gensyms (lock this msg)
      (let ((g!self-call (intern (string 'self-call)))
            (kexcl       (cons 'actor-internal-message:continuation
                               (mapcar 'first excl))))
        `(let ((,lock  (mp:make-lock :sharing t))
               (,this  nil)
               ,@bindings)
           (setf ,this
                 (lambda (&rest ,msg)
                   (labels ((,g!self-call (&rest ,msg)
                              ;; excl routines can call any other routine
                              (if (member (car ,msg) ',kexcl)
                                  (mp:with-exclusive-lock (,lock)
                                    (let ((*current-actor* ,this))
                                      (dcase ,msg
                                        (actor-internal-message:continuation (fn &rest args)
                                                                             (apply fn args))
                                        ,@excl)))
                                (labels ((,g!self-call (&rest ,msg)
                                           ;; shared routines can only call other shared routines
                                           (mp:with-sharing-lock (,lock)
                                             (dcase ,msg
                                               ,@shared))
                                           ))
                                  (apply #',g!self-call ,msg)))))
                     (apply #',g!self-call ,msg)))
                 ))
        )))
|#

(defun make-agent (fn)
  (let ((lock (mp:make-lock))
        (this nil))
    (setf this
          (lambda (&rest msg)
            (mp:with-lock (lock)
              (let ((*current-actor* this))
                (cond
                 ((and (consp msg)
                       (eq (car msg) 'actor-internal-message:continuation))
                  (apply (cadr msg) (cddr msg)))
                 
                 (t
                  (apply fn msg))
                 ))))
          )))

(defmethod actor-user-fn ((fn function))
  fn)

(defun make-shared-agent (&key excl-keys excl shared)
  (let ((lock  (mp:make-lock :sharing t))
        (this  nil)
        (exclk (cons 'actor-internal-message:continuation excl-keys)))
    (setf this
          (lambda (&rest msg)
            (cond ((and (consp msg)
                        (member (car msg) exclk))
                   (mp:with-exclusive-lock (lock)
                     (let ((*current-actor* this))
                       (cond ((eq (car msg) 'actor-internal-message:continuation)
                              (apply (cadr msg) (cddr msg)))
                             (t
                              (apply excl msg))
                             ))))
                             
                  (t
                   (mp:with-sharing-lock (lock)
                     (let ((*current-actor* this))
                       (apply shared msg))))
                  ))
          )))

(defun call (agent &rest msg)
  ;; alternative to send for agents, since it is a blocking call
  (apply agent msg))

#|
(defun =cont (fn)
  (let ((self *current-agent*))
    (if self
        (lambda (&rest vals)
          (if (eq self *current-agent*)
              (apply fn vals)
            (apply self 'actor-internal-message:continuation fn vals)))
      fn)))
|#

;; --------------------------------------------------

(defvar *async-mbox* (mp:make-mailbox))
(defvar *async-proc* nil)
  
(defun runner ()
  (loop
   (let ((msg (mp:mailbox-read *async-mbox*)))
     (ignore-errors
       (apply (car msg) (cdr msg)))
     )))

(defun dispatch (&rest msg)
  ;; async call to agent via runner thread
  (unless *async-proc*
    (setf *async-proc*
          (mp:process-run-function "async-runner" ()
                                   'runner)))
  (mp:mailbox-send *async-mbox* msg))


;; --------------------------------------------------

#|
(defvar *test-agent*
  (make-shared-agent :excl-keys '(:excl-print :excl-to-shared-print)
                     :shared
                     (dlambda*
                       (:shared-print (x)
                        (print x))
                       (:shared-to-excl-print (x)
                        ;; this should fail... can't grab exclusive
                        ;; lock if I'm holding a shared lock...
                        ;;
                        ;; -- shared routines can't call any excl
                        ;; routines, excl not visible
                        (self-call :excl-print x)) )
                       
                     :excl
                     (dlambda*
                       (:excl-print (x)
                        (print x))
                       (:excl-to-shared-print (x)
                        (self-call :shared-print x)) )
                     ))

(call *test-agent* :shared-print :hello)
(call *test-agent* :excl-print :hello-x)
(call *test-agent* :shared-to-excl-print :hello-sx)
(call *test-agent* :excl-to-shared-print :hello-xs)
(dispatch *test-agent* :excl-to-shared-print :hello-d)

(defmonitor
 (def-excl excl-print (x)
   (print x))
 (def-excl excl-to-shared-print (x)
   (shared-print x))
 
 (um:def-shared shared-print (x)
   (print x))
 (um:def-shared shared-to-excl-print (x)
   (excl-print x)))

(shared-print :hello-s)
(excl-print :hello-x)
(shared-to-excl-print :hello-sx) ;; should fail
(excl-to-shared-print :hello-xs)

(defvar *test-agent-rt*
  (make-agent
   (dlambda
     (:add (x y)
      (+ x y)))))

(defvar *test-actor-rt*
  (make-actor
   (dlambda*
     (:add (x y)
      (+ x y)))))

(defun test-agent-rt (&optional (n 1000))
  (time
   (loop repeat n do
         (call *test-agent-rt* :add 5 7))))

(defun test-actor-rt (&optional (n 1000))
  (time
   (loop repeat n do
         (ask *test-actor-rt* :add 5 7))))

 |#


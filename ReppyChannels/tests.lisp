
(in-package :rch)

(defun tst ()
  ;; randomly rendezvous with one of two threads, sending an abort to
  ;; the other thread
  (let* ((ch12  (make-channel))
         (ch12a (make-channel)) ;; abort chan
         (ch13  (make-channel))
         (ch13a (make-channel))) ;; abort chan
    (ac:pr "----------------------------------")
    (ac:spawn-worker
     (lambda ()
       (select (wrap (recvEvt ch12)
                     (lambda (ans)
                       (ac:pr (format nil "t2 got ~A" ans))))
               (wrap (recvEvt ch12a)
                     (lambda (_)
                       (ac:pr :t2-fail))))
       ))
    (ac:spawn-worker
     (lambda ()
       (select (wrap (recvEvt ch13)
                     (lambda (ans)
                       (ac:pr (format nil "t3 got ~A" ans))))
               (wrap (recvEvt ch13a)
                     (lambda (_)
                       (ac:pr :t3-fail)))
               )))
    (sleep 0.5)
    (select
     (wrap-abort (wrap (sendEvt ch12 :one-two)
                       (lambda (_)
                         (ac:pr :sent12)))
                 (lambda ()
                   (poke ch12a t)))
     (wrap-abort (wrap (sendEvt ch13 :one-three)
                       (lambda (_)
                         (ac:pr :sent13)))
                 (lambda ()
                   (poke ch13a t))))
    (values)))
    
(tst)

;; --------------------------------------------------------------------------

;; Share Queue but only one writer and one reader.
;; Need a real queue manager which can read/write and manage the queue itself
;; Need a writer Actor that permits only writing to the queue
;; Need a reader Actor that permits only reading from the queue
;; Need a resource manager that permits only one writer and one reader to acquire the corresponding
;; Actor addresses

(defun make-queue-manager ()
  ;; manager never blocks waiting
  (let ((q  (make-fifo))
        (chw nil)
        (chr nil))
    (make-actor
     (dlambda
       (:acquire-reader (ch)
        (unless chr
          (setf chr ch)
          (poke ch t)))
       (:release-reader (ch)
        (unless (eq ch chr)
          (setf chr nil)
          (poke ch t)))
       (:acquire-writer (ch)
        (unless chw
          (setf chw ch)
          (poke ch t)))
       (:release-writer (ch)
        (unless (eq ch chw)
          (setf chw nil)
          (poke ch t)))
       (:put (ch item)
        (when (eq ch chw)
          (addq q item)
          (poke ch t)))
       (:get (ch)
        (when (eq ch chr)
          (unless (emptyq q)
            (poke ch (popq q)))))
       ))))

;; ---------------------------------------          
;; from System
(setf *q-manager* (make-q-manager))

;; --------------------------------------
;; from reader
(let ((ch  (make-channel)))
  (send *q-manager* :acquire-reader ch)
  (ac:with-timeout 1
    ;; timeout indicates non-available
    (setf reader (recv ch)))
  ...
  (loop do
        (ac:send *q-manager* :get ch)
        (let ((fragment nil))
          (loop while (null fragment) do
                (handler-case
                    (ac:with-timeout 1
                      (setf fragment (rch:recv ch)))
                  (timeout ()
                    ;; maybe wait and retry later?
                    (sleep 10) ;; ??
                    )))
          (do-something-with fragment))
  ...
  ;; when finished
  (send *q-manager* :release-reader ch))

;; -------------------------------------------
;; from writer
(let ((ch  (make-channel)))
  (send *q-manager* :acquire-writer ch)
  (ac:with-timeout 1
    ;; timeout indicates non-available
    (setf writer (recv ch)))
  ...
  (loop do
        (let ((fragment (gather-fragment)))
          (send *q-manager* :put ch fragment)
        ))
  ...
  ;; when finished
  (send *q-manager* :release-writer ch))

;; -------------------------------------------------------
;; with-nack

(progn
  (defparameter *tos*  0)
  (defparameter *dbg* (ac:make-actor
                       (let ((dbg  (debug-stream:make-debug-stream)))
                         (um:dlambda
                           (:cls ()
                            (debug-stream:cls dbg))
                           (:pr (obj)
                            (debug-stream:pr dbg obj))
                           ))))
  (defun cls ()
    (ac:send *dbg* :cls))
  (defun pr (obj)
    (ac:send *dbg* :pr obj)))

(defun tst ()
  (flet ((run (ch ix)
           (send ch t)
           ;; (sleep (random 1.0))
           (ac:with-timeout 1
             (handler-case
                 (sync (with-nack ch (wrap (recvEvt ch)
                                           (lambda (ans)
                                             (ac:pr (format nil "thr ~A got ~A" ix ans))))
                                  (lambda ()
                                    (ac:pr (format nil "thr ~A fail" ix)))
                                  ))
               (timeout ()
                 (sys:atomic-incf *tos*))
               ))))
  
    (let* ((ch12 (make-channel))
           (ch13 (make-channel))
           (ch14 (make-channel))
           (t2   (spawn-worker #'run ch12 2))
           (t3   (spawn-worker #'run ch13 3))
           (t4   (spawn-worker #'run ch14 4)))
      (recv ch12)
      (recv ch13)
      (recv ch14)
      (let ((*timeout* 1))
        (handler-case
            (select*
             (wrap (sendEvt ch12 :one-two)
                   (lambda (_)
                     (ac:pr :sent12)))
             (wrap (sendEvt ch14 :one-four)
                   (lambda (_)
                     (ac:pr :sent14)))
             (wrap (sendEvt ch13 :one-three)
                   (lambda (_)
                     (ac:pr :sent13))))
          (timeout ()
            (sys:atomic-incf *tos*))
          ))
      )))


(defun tst ()
  (flet ((run (ch ix)
           (send ch t)
           ;; (sleep (random 1.0))
           (ac:with-timeout 1
             (handler-case
                 (sync (with-nack ch (sendEvt ch ix)
                                  (lambda ()
                                    (pr (format nil "thr ~A fail" ix)))
                                  ))
               (timeout ()
                 (sys:atomic-incf *tos*))
               ))))
  
    (let* ((ch12 (make-channel))
           (ch13 (make-channel))
           (ch14 (make-channel))
           (t2   (spawn-worker #'run ch12 2))
           (t3   (spawn-worker #'run ch13 3))
           (t4   (spawn-worker #'run ch14 4)))
      (recv ch12)
      (recv ch13)
      (recv ch14)
      (let ((*timeout* 1))
        (pr (format nil "Got ~A"
                       (handler-case
                           (select (recvEvt ch12)
                                   (recvEvt ch13)
                                   (recvEvt ch14))
                         (timeout ()
                           (sys:atomic-incf *tos*)
                           :timeout))
                       )))
      )))

(defun tst ()
  (flet ((run (ch ix)
           (send ch t)
           ;; (sleep (random 1.0))
           (ac:with-timeout 1
             (handler-case
                 (select (wrap-abort (sendEvt ch ix)
                                     (lambda ()
                                       (pr (format nil "thr ~A fail" ix))))
                         (recvEvt ch))
               (timeout ()
                 (sys:atomic-incf *tos*))
               ))))
  
    (let* ((ch12 (make-channel))
           (ch13 (make-channel))
           (ch14 (make-channel))
           (t2   (spawn-worker #'run ch12 2))
           (t3   (spawn-worker #'run ch13 3))
           (t4   (spawn-worker #'run ch14 4)))
      (recv ch12)
      (recv ch13)
      (recv ch14)
      (let ((*timeout* 1))
        (pr (format nil "Got ~A"
                       (handler-case
                           (select (wrap-abort (recvEvt ch12)
                                               (lambda ()
                                                 (send ch12 t)))
                                   (wrap-abort (recvEvt ch13)
                                               (lambda ()
                                                 (send ch13 t)))
                                   (wrap-abort (recvEvt ch14)
                                               (lambda ()
                                                 (send ch14 t))))
                                   
                         (timeout ()
                           (sys:atomic-incf *tos*)
                           :timeout))
                       )))
      )))

(time (progn
        (cls)
        (setf *tos* 0)
        (loop repeat 1000 do (tst))
        (pr (format nil "Accum Timeouts = ~A" *tos*))))

;; -------------------------------------------------------
;; try rolling our own with wrap-abort

(defun tst ()
  (let ((ch  (make-channel))
        (cha (make-channel)))
    (ac:spawn-worker (lambda ()
                       (sync
                        (with-nack
                         cha
                         (sendEvt ch :something))
                         (lambda ()
                           (pr "Worker send aborted"))
                        )))
    (sync
     (with-nack
      cha
      (recvEvt ch)
      (lambda ()
        (pr "Client recv aborted")))
    ))

(defun tst ()
  (let ((ch  (make-channel))
        (ch2 (make-channel))
        (cha (make-channel)))
    (ac:spawn-worker (lambda ()
                       (sync
                        (choose
                         (with-nack
                          ch
                          (sendEvt ch :something)
                          (lambda ()
                            (pr "Worker send aborted")))
                         (wrap (recvEvt ch2)
                               #'pr))
                        )))
    (sync
     (sendEvt ch2 :side-event)
     ;; (sendEvt cha cha)
     )
    ))

(tst)

(let ((ch (make-channel)))
  (ac:pr (list :writer (sync
                        (wrap-abort
                         (wrap (sendEvt ch 15 :async t)
                               (lambda (ans)
                                 (ac:pr (list :poke-wrap ans))))
                         (lambda ()
                           (ac:pr :poke-wrap-abort))))))
  (let ((ac:*timeout* 1))
    (ac:pr (list :reader (sync
                          (wrap-abort
                           (wrap
                            (recvEvt ch :async t)
                            (lambda (ans)
                              (ac:pr (list :peek-wrap ans))
                              ans))
                           (lambda ()
                             (ac:pr :peek-wrap-abort))))
                 )))
  (values))


(let ((ch (make-channel)))
  (ac:spawn-worker (lambda ()
                     (let ((ac:*timeout* 5))
                       (ac:pr (list :reader (sync
                                             (wrapping
                                              (recvEvt ch :async t)
                                              :on-rendezvous
                                              (lambda (ans)
                                                (ac:pr (list :peek-wrap ans))
                                                ans)
                                              :on-abort
                                              (lambda ()
                                                (ac:pr :peek-wrap-abort))))
                                    )))))
  ;; (sleep 1)
  (ac:pr (list :writer (sync
                        (wrapping
                         (sendEvt ch 15 :async t)
                         :on-rendezvous
                         (lambda (ans)
                           (ac:pr (list :poke-wrap ans))
                           ans)
                         :on-abort
                         (lambda ()
                           (ac:pr :poke-wrap-abort))))))
  (values))

(sync
 (wrap-handler
  (wrap
   (wrap (alwaysevt 15)
         (lambda (ans)
           (ac:pr (list :wrap-inner ans))
           (1+ ans)))
   (lambda (ans)
     (ac:pr (list :wrap-outer ans))
     (error "Hey!")
     (1+ ans)))
  (lambda (fn)
    (handler-case
        (funcall fn)
      (error (e)
        (ac:pr :what?))))
  ))
 
(defun tst (x y &key thing)
  (declare (optimize (speed 3) (safety 0) (float 0)))
  (list x y thing))


(ac:recv)

(defun recvList (chans)
  (if chans
      (destructuring-bind (ch . rest) chans
        (choose* (wrap-abort (wrap (recvEvt ch)
                                   (lambda (ans)
                                     (list ch ans)))
                             (lambda ()
                               (poke ch *no-rendezvous-token*)))
                 (recvList rest)))
    (neverEvt)))

;; ---------------------------------------------------------

(defun tst ()
  ;; about 5% misses
  (let* ((ctr (list 0))
         (misses 0)
         (proc (ac:spawn-worker (lambda ()
                                  (loop
                                   (sys:atomic-incf (car ctr)))))))
    (unwind-protect
        (loop for trial from 0
              for old = (car ctr) do
              (unless (sys:compare-and-swap (car ctr) old old) 
                (incf misses)
                (format t "~D misses after ~D trials (~,1F%)~%"
                        misses
                        trial
                        (/ misses trial 0.01))))
      (ac:terminate-actor proc))))

;; -------------------------------------------------------

(let* ((arr (make-array 32
                        :element-type 'fixnum
                        :initial-element 0)))
  (loop repeat 10000 do
        (let ((val (lw:mt-random #.(ash 1 32))))
          (dotimes (ix 32)
            (when (logbitp ix val)
              (incf (aref arr ix))))))
  (plt:plot 'histo arr :clear t))

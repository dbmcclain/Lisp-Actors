
(in-package :ac)

(defun tst (&optional (niter #N100_000))
  (time
   (dotimes (ix niter)
     (with-futures (&rest args)
         (nil nil nil nil nil
          nil nil nil nil nil)
       args))))

(defun tst2 (&optional (niter #N1_000_000))
  (time
   (dotimes (ix niter)
     (print (sin ix)))))

(defmacro nest (&rest r)
  (reduce (lambda (o i)
            `(,@o ,i))
          r
          :from-end t))

(nest
 (let ((x 15)))
 (let ((y 32)))
 (progn
   (doit)
   (didit 12)))

(trivia:match 15
  (x (print x)))

(lw:push-end-new 15 lst)

(defun doit (x)
  (print (+ x 3)))

(lw:defadvice (doit doit-1 :around)
    (x)
  (lw:call-next-advice (+ x 2)))

;; ----------------------------------------------

(sys:call-system-showing-output "sysctl -n hw.logicalcpu")
(with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
  (parse-integer (read-line s nil nil)))


(let* ((lock (mp:make-lock))
       (ch   (rch:make-channel))
       (proc (spawn-worker (lambda ()
                             (mp:with-lock (lock)
                               (rch:send ch :ok)
                               (sleep 60))))))
  (rch:recv ch)
  (unwind-protect
      (mp:process-lock lock nil 0)
    (terminate-actor proc)))

;; =========================================================

(let* ((acts (coerce
              (loop for ix from 0 below 5 collect
                    (make-actor
                     (let ((my-ix ix))
                       (lambda (n)
                         (pr (list my-ix n))
                         ;;(sleep 1)
                         )
                       )))
              'vector)))
  (loop for ix from 0 below 150 do
        (send (aref acts (mod ix 5)) ix))
  )

;;--------------------------------------------
;; experiment with continuatons


(hoare-monitor diddly
  (let ((a 1)
        (b 2))
    (defun do-k1 (x)
      (1+ x))
    (defun do-k2 (x)
      (1- x))))
  
(ask diddly #'do-k1 5)
(ask diddly 'do-k1 5)


;; -------------------------------

(loop repeat 3 do (spawn (lambda ()
                           (loop repeat 100 do (format t "Actor ~A" (current-actor)))
                           )))
;; -----------------------------------------

(labels
    ((doit-1 (%self &rest args)
       (with-slots (a b c) %self
         (apply (lambda (actual-args)
                  body)
                args))))

  )

(defclass diddly ()
  (a b))

(defclass diddlyx (diddly)
  (c))


(setf x (make-instance 'diddlyx))


;; ==-------------------------------------


(defstruct tcd
  busy
  exec-mbox
  data-mbox
  data-queue)

(defmacro define-tcd-method (name ((obj class) &rest args) &body body)
  `(labels
       ((,exec-fn ()
          ,@body))
     (apply 'send ,obj 'tcd-internal-message:exec #',exec-fn ,args)))

(defmacro define-actor-method (function-specifier &body body)
  (multiple-value-bind (name combo argspec)
      (parse-function-specifier function-specifier)
    (destructuring-bind ((actor class) &rest args) argspec
      `(defmethod ,name ,@comdo ((,actor ,class) ,@args)
         (labels
             ((,exec-fn ()
                ,@body))
           (send ,actor 'actor-internal-message:exec #',exec-fn)))
      )))
          
                   
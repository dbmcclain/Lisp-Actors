
(in-package #:actors-user)

(kill-executives)
(get-actors)

(make-actor () ((x 15))
  (sleep 0.1)
  (pr (format nil "~&Self (1) = ~A" self))
  (wait (a b c)
      (list "ayy" "bee" "see") ;; <-- performed in foreign thead
    (pr "After WAIT"
        (format nil "~&Self (2) = ~A" self)
        (list a b c))
    (send self 32)
    (next-message (msg)
      (pr (format nil "~&Self (3) = ~A" self)
          msg
          'done))
    ))

(defun tst ()
  (make-actor () ()
    (pr self)
    (wait (a b c)
        (list "ayy" "bee" "see") ;; <-- performed in foreign thead
      (pr self
          (list a b c))
      (send self 32)
      (next-message (msg)
        (pr msg 'done))
      )))
(compile 'tst)
(tst)



(defun tst (n)
  ;; Ans: about 10 usec/ spawn-exit elapsed time
  (loop repeat n do
        (spawn (lambda ()))))


(let ()
  (dotimes (ix 1000)
    (spawn (lambda (ix) (pr ix)) ix)))

;; --------------------------------------

(make-actor (&rest msg)
    (shared-state)
  (do-something)
  (wait (&rest msg)
        (wait-form)
     (do-more)))

=>
(make-actor (&rest msg)
    (shared-state)
  (do-something)
  (spawn (lambda ()
           (send self (wait-form))))
  (become (behav (&rest msg)
              ((old-me #'me))
            (do-more)
            (become old-me))))

(lw:function-lambda-list (lambda (a b &key c &rest x)
                           (list a b c x)))

(defun tst (a b &rest x &key c &allow-other-keys)
  (list a b c x))

;; ---------------------------------------------

(defun do-become (self behav)
  (setf (actor-behavior self) behav))

(defmacro become (args state &body body)
  (let ((a!self (anaphor 'self)))
    `(do-become (behav ,args ,state ,@body))
    ))

(defun re-enqueue (actor msg-lst)
  (map nil (lambda (msg)
             (apply #'send actor msg))
       (nreverse msg-lst)))

(defmacro wait (args wait-form &body body)
  (let ((a!self   (anaphor 'self))
        (g!unique (gensym-like :msg-))
        (g!msg    (gensym-like :msg-))
        (g!args   (gensym-like :args-))
        (g!queue  (gensym-like :queue-))
        (g!old-me (gensym-like :old-me-)))
    `(progn
       (spawn (lambda ()
                (send ,a!self ,g!unique ,wait-form)))
       (become (&rest ,g!msg) ((,g!old-me #',a!me))
               (um:dcase ,g!msg
                 (,g!unique ,args
                            ,@body
                            (re-enqueue ,a!self ,g!queue)
                            (become ,g!old-me))
                 (_ (&rest ,g!args)
                    (push ,g!args ,g!queue))
                 )))
    ))

(progn
  (doit-toit)
  (become (&rest msg)
          ()
          (didit-already)))

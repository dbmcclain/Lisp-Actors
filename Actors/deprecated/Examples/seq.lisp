
(in-package :ac)

(defun make-ignore-beh ()
  (lambda* _))

(defvar *bit-pit* (make-actor (make-ignore-beh)))

(defun make-thunk-beh (client fn)
  (lambda* _
    (send client (funcall fn))))

(defun make-expr-beh (client fn)
  (lambda (&rest args)
    (send client (apply fn args))))

(defun seq-launch (client &rest fns)
  (um:nlet iter ((fns    (reverse fns))
                 (client client))
    (if (endp fns)
        (send client)
      (let ((actor (make-actor (make-thunk-beh client (car fns)))))
        (go-iter (cdr fns) actor)))
    ))

(defmacro $seq (client &rest clauses)
  (when clauses
    `(seq-launch ,client ,@(mapcar #`(lambda () ,a1) clauses))
    ))

(defun make-join-beh (client ct)
  (lambda* _
    (when (zerop (decf ct))
      (send client))))

(defun par-launch (client &rest fns)
  (let ((join (make-actor (make-join-beh client (length fns)))))
    (dolist (fn fns)
      (send (make-actor (make-thunk-beh join fn))))))

(defmacro $par (client &rest clauses)
  (when clauses
    `(par-launch ,client ,@(mapcar #`(lambda () ,a1) clauses))))

(defun make-alt-beh (client tfn ffn)
  (lambda (val)
    (send client (funcall (if val tfn ffn)))))
#|
($seq client (a) (b) (c))
($par client (a) (b) (c))

($seq *bit-pit*
 (print :A)
 (print :B)
 (print :C)
 (print :D)
 (print :E)
 (print :F)
 (print :G)
 (print :H)
 (print :I))

($par *bit-pit*
 (print :A)
 (print :B)
 (print :C)
 (print :D)
 (print :E)
 (print :F)
 (print :G)
 (print :H)
 (print :I))

(let ((actor (make-actor (lambda () (print :hello))))) (send actor))
 |#

(defun make-ignore-beh ()
  (lambda* _))

(defvar *bit-pit* (make-actor (make-ignore-beh)))

(defun make-thunk-beh (fn)
  (lambda* _
    (send client (funcall fn))))

(defun make-expr-beh (fn)
  (lambda (client &rest args)
    (send client (apply fn args))))

(defun make-emtpy-seq-beh ()
  (um:dlambda
    (:attach (actor)
     (let ((next (make-actor self-beh)))
       (become (make-seq-beh actor next))))
    
(defun seq-launch (client &rest fns)
  (um:nlet iter ((fns (reverse fns)))
                 (client client))
    (if (endp fns)
        (send client)
      (let ((actor (make-actor (make-thunk-beh (car fns)))))
        (go-iter (cdr fns) actor)))
    ))

(defmacro $seq (client &rest clauses)
  (when clauses
    `(seq-launch ,client ,@(mapcar #`(lambda () ,a1) clauses))
    ))

(defun make-join-beh (client ct)
  (lambda* _
    (when (zerop (decf ct))
      (send client))))

(defun par-launch (client &rest fns)
  (let ((join (make-actor (make-join-beh client (length fns)))))
    (dolist (fn fns)
      (send (make-actor (make-thunk-beh join fn))))))

(defmacro $par (client &rest clauses)
  (when clauses
    `(par-launch ,client ,@(mapcar #`(lambda () ,a1) clauses))))

(defun make-alt-beh (client tfn ffn)
  (lambda (val)
    (send client (funcall (if val tfn ffn)))))

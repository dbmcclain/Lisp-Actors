;; tlet.liap - Thread-local Let-Over-Lambda
;;
;; DM/RAL  2026/07/27T12:21:52U
;; ----------------------------------

(defpackage #:tlet
  (:use #:common-lisp)
  (:import-from #:mpc
   #:tlet
   #:tlet*
   #:with-tls)
  (:export
   #:tlet
   #:tlet*
   #:with-tls
   ))

(in-package #:tlet)

;; ----------------------------------
;; Declare a TLS version of LOL

(defun tletter (let-kind bindings body)
  (let* ((names  (mapcar (lambda (binding)
                           (if (consp binding)
                               (car binding)
                             binding))
                         bindings))
         (vals   (mapcar (lambda (binding)
                           (and (consp binding)
                                (cadr binding)))
                         bindings))
         (gnames (mapcar (lambda (name)
                           (gensym (string name)))
                         names)))
    `(,let-kind ,(mapcar #'list gnames vals)
                (symbol-macrolet ,(mapcar (lambda (name gname)
                                            `(,name (tlsval ',gname ,gname)))
                                          names gnames)
                  ,@body))
    ))

(defmacro tlet (bindings &body body)
  (tletter 'let bindings body))

(defmacro tlet* (bindings &body body)
  (tletter 'let* bindings body))

;; --------------------------------------------
;; Fetching/Creating the Thread-Local Store

(defvar *tlstbl* nil)

(defmacro with-tls (&body body)
  ;; Cache the table. For use when you know you are going to
  ;; repeatedly refer to TLS vars.
  `(let ((*tlstbl*  (tlstbl nil))) ;; ensure fresh
     ,@body))

#+:LISPWORKS
(let (st-tlstbl)
  (defun tlstbl (&optional (tbl *tlstbl*))
    (or tbl
        (if mp:*current-process* ;; SMP Running?
            (or (mp:process-private-property 'tlsvars)
                (setf (mp:process-private-property 'tlsvars)
                      (make-hash-table :single-thread t)))
          ;; else
          (or st-tlstbl
              (setf st-tlstbl
                    (make-hash-table :single-thread t)))
          ))))
      
#+:SBCL
(let ((tlstbls   (make-hash-table ;; indexed by process
                  :weakness :key))
      (tbls-lock (mpc:make-lock)))
  (defun tlstbl (&optional (tbl *tlstbl*))
    (or tbl
        (let ((key (mpc:get-current-process)))
          (flet ((doit ()
                   (or (gethash key tlstbls)
                       (setf (gethash key tlstbls) (make-hash-table)))
                   ))
            (if key
                (mpc:with-lock (tbls-lock)
                  (doit))
              (doit))
            )))))

;; --------------------------------------------
;; TLS Var Access

(defun tlsval (name &optional init)
  (gethash name (tlstbl) init))

(defun set-tlsval (name new-val)
  (setf (gethash name (tlstbl)) new-val))

(defsetf tlsval (name &optional init) (new-val)
  (declare (ignore init))
  `(set-tlsval ,name ,new-val))

;; --------------------------------------------
#|
(tlet (pos)
  (lambda (x)
    (print (list pos x))))

(let ((tst (tlet* ((sav  15)
                   (bak  32))
             (lambda (&rest args)
               (print (cons sav (cons bak args)))))
           ))
  (funcall tst 'a 'b))
|#

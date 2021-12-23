;; transactional-db.lisp -- transactional database processing in Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------------------

(in-package :ac)
  
(defun trans-gate-beh (tag-commit tag-rollback db)
  (flet ((try (cust target args)
           (send* target db tag-commit tag-rollback cust target args)))
    (alambda 
     ((cust :req target . args)
      (try cust target args))
     
     ((atag db-old db-new cust retry-target . args) when (eql atag tag-commit)
      (cond ((eql db-old db)
             (become (trans-gate-beh tag-commit tag-rollback db-new))
             (send cust :ok))
            (t
             (try cust retry-target args))
            ))
     
     ((atag cust retry-target . args) when (eql atag tag-rollback)
      (try cust retry-target args))
     )))
  
(defun make-trans-gate (db)
  (actors ((trans        (trans-gate-beh tag-commit tag-rollback db))
           (tag-commit   (tag-beh trans))
           (tag-rollback (tag-beh trans)))
    trans))

(defvar *db* (make-trans-gate (maps:empty)))

(defun add-rec (cust key val)
  (send *db* cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (send* commit db (maps:add db key val) retry-info))
        ))

(defun remove-rec (cust key)
  (send *db* cust :req
        (actor (db commit rollback &rest retry-info)
          (declare (ignore rollback))
          (send* commit db (maps:remove db key) retry-info))
        ))

(defun show-db ()
  (send *db* nil :req
        (actor (db &rest ignored)
          (declare (ignore ignored))
          (sets:view-set db))))

#|
(dotimes (ix 5)
  (add-rec println ix ix))
(add-rec println :dave :chara)
(show-db)
(dotimes (ix 10)
  (remove-rec println ix))

(let ((m (maps:empty)))
  (setf m (maps:add m :dave :dog))
  (eql m (maps:add m :dave :dog)))
(let ((m (sets:empty)))
  (setf m (sets:add m :dave))
  (eql m (sets:add m :dave)))
|#               

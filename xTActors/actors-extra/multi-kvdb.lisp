
(defpackage #:com.ral.actors.multi-kvdb
  (:use #:common-lisp #:ac #:kvdb)
  (:export
   #:multi-req
   ))

(in-package #:com.ral.actors.multi-kvdb)

(defun init-bank-bal ()
  (let+ ((:β (kvdb1) (racurry kvdb-maker :make-kvdb "diddly1.db"))
         (:β (kvdb2) (racurry kvdb-maker :make-kvdb "diddly2.db"))
         (:β _       (racurry kvdb1 :add :bank-bal 10))
         (:β _       (racurry kvdb2 :add :bank-bal 0)))
    (check-bals)))

(defun check-bals ()
  (let+ ((:β (kvdb1) (racurry kvdb-maker :make-kvdb "diddly1.db"))
         (:β (kvdb2) (racurry kvdb-maker :make-kvdb "diddly2.db"))
         (:β (bal1)  (racurry kvdb1 :find :bank-bal))
         (:β (bal2)  (racurry kvdb2 :find :bank-bal)))
    (send fmt-println "Bal1 = ~A" bal1)
    (send fmt-println "Bal2 = ~A" bal2)))
    
#|
(init-bank-bal)
(check-bals)
|#

(defun transfer (cust kvdb-from kvdb-to amt &optional (count 0))
  (with-actors
    (let ((me self))
      (β (db-from db-to)
          (send (fork (racurry kvdb-from :req-excl me 1)
                      (racurry kvdb-to   :req-excl me 1))
                β)
        (cond

         ((member :fail (list db-from db-to))
          (send kvdb-from sink :abort me)
          (send kvdb-to   sink :abort me)
          (let ((new-count (1+ count)))
            (if (< new-count 5)
                ;; delay by random amount, try again
                (β _
                    (send-after (random 1.0) β)
                  (transfer cust kvdb-from kvdb-to amt new-count))
              ;; else - give up
              (send cust :fail))
            ))
         
         (t
          (let* ((bal-from      (db-find db-from :bank-bal 0))
                 (bal-to        (db-find db-to   :bank-bal 0))
                 (new-bal-from  (- bal-from amt))
                 (new-bal-to    (+ bal-to   amt))
                 (new-db-from   (db-add db-from :bank-bal new-bal-from))
                 (new-db-to     (db-add db-to   :bank-bal new-bal-to)))
            (send kvdb-from `(,sink . ,me) :commit db-from new-db-from)
            (send kvdb-to   `(,sink . ,me) :commit db-to   new-db-to)
            (send cust :ok)
            ))
         )))))

#|
  ;; tranfer 10 from diddly1 to diddly2
(let+ ((:β (kvdb-from) (racurry kvdb-maker :make-kvdb "diddly1.db"))
       (:β (kvdb-to)   (racurry kvdb-maker :make-kvdb "diddly2.db")))
  (transfer println kvdb-from kvdb-to 10))

  ;; tranfer 10 from diddly2 to diddly1
(let+ ((:β (kvdb-from) (racurry kvdb-maker :make-kvdb "diddly2.db"))
       (:β (kvdb-to)   (racurry kvdb-maker :make-kvdb "diddly1.db")))
  (transfer println kvdb-from kvdb-to 10))
|#


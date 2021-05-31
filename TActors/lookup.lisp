;; lookup.lisp -- Examining where to place the dividing line between Actors and data
;;
;; DM/RAL 05/21
;; ----------------------------------------------------------------------

(in-package :ac)

(defvar $not-found #())

;; ---------------------------------------------------------
;; Spread-Actor Lookup Tables - really only supports single-level keying.
;; Logically atomic add/remove/replace. One element per Actor.
;; This 1-level table is FP.

(defun empty-beh ()
  (alambda
   ((cust :find _)
    (send cust $not-found))
   
   ((cust :add key val)
    ;; insert new entry ahead of myself
    (become (entry-beh key val (make-actor self-beh)))
    (send cust))

   ((cust :replace key val)
    ;; insert new entry ahead of myself
    (become (entry-beh key val (make-actor self-beh)))
    (send cust))

   ((prev :purge)
    (send prev :purged self-beh))

   ((cust :is-empty?)
    (send cust t))
   ))

(defun purged-beh (next)
  (alambda
   ((:purged beh)
    (become beh))

   ( msg
      (send* next msg))
   ))

(defun entry-beh (key val next)
  (alambda
   ((cust :find akey) when (eql key akey)
    (send cust val))

   ((cust :add akey aval) when (eql key akey)
    ;; insert (possibly duplicate key) entry ahead of myself
    (let ((new-next (make-actor self-beh)))
      (become (entry-beh akey aval new-next))
      (send cust)))
   
   ((cust :remove akey) when (eql key akey)
    (become (purged-beh next))
    (send next self :purge)
    (send cust))

   ((cust :replace akey aval) when (eql key akey)
    (become (entry-beh akey aval next))
    (send cust))

   ((prev :purge)
    (send prev :purged self-beh))

   ((cust :is-empty?)
    (send cust nil))
   
   ( msg
     (send* next msg))
   ))

(defun make-lookup-table ()
  (make-actor (empty-beh)))

;; ----------------------------------------------
;; N-level keying for Spread-Actor tables
;; Use a gateway to protect under concurrency.
;; Not quite as clean as a purely functional system.
;; But still logically atomic for addn/removen/replacen
;; Slower, but supports higher degree of concurrency than FP all-in-one solution.

(defun n-level-beh (tbl)
  (alambda
   ((cust :findn key . keys)
    (findn cust tbl key keys))

   ((cust :addn val key . keys)
    (become-update-beh tbl cust (list* :addn val key keys)))

   ((cust :removen key . keys)
    (become-update-beh tbl cust (list* :removen key keys)))

   ((cust :replacen val key . keys)
    (become-update-beh tbl cust (list* :replacen val key keys)))
   ))

;; -----------------------

(defun become-update-beh (tbl cust cmd)
  (let ((writer (make-actor (writer-beh tbl))))
    (send* writer self cmd)
    (become (n-level-upd-beh tbl cust writer nil))))

(defun findn (cust tbl key keys)
  (actor-nlet iter ((tbl  tbl)
                    (key  key)
                    (keys keys))
    (if keys
        (beta (sub)
            (send tbl beta :find key)
          (if (eq sub $not-found)
              (send cust $not-found)
            (send iter sub (car keys) (cdr keys))
            ))
      (send tbl cust :find key))
    ))

;; -------------------------------------------
;; A cheap FP Banker's queue - empty queue is NIL

(defun qnorm (q)
  ;; on entry q is never NIL
  ;; if queue is empty we return NIL
  ;; otherwise, something is left in (CAR Q)
  (if (car q)
      q
    (when (cdr q)
      (list (reverse (cdr q)))
      )))

(defun addq (q item)
  ;; add item, return new queue
  (if q
      (cons (car q) (cons item (cdr q)))
    (list (list item))))

(defun popq (q)
  (when q
    ;; return next item, and new queue
    (let ((item (caar q))
          (newq (qnorm (cons (cdar q) (cdr q)))))
      (values item newq t)
      )))

;; -----------------------

(defun n-level-upd-beh (tbl wr-cust writer pend)
  (alambda
   ((cust :findn key . keys)
    (findn cust tbl key keys))

   ((cust :addn val key . keys)
    (become (n-level-upd-beh tbl wr-cust writer
                             (addq pend (list* cust :addn val key keys)))
            ))

   ((cust :removen key . keys)
    (become (n-level-upd-beh tbl wr-cust writer
                             (addq pend (list* cust :removen key keys)))
            ))

   ((cust :replacen val key . keys)
    (become (n-level-upd-beh tbl wr-cust writer
                             (addq pend (list* cust :replacen val key keys)))
            ))

   ((wrt :update) when (eq writer wrt)
    (send wr-cust)
    (if pend
        (multiple-value-bind (cmd new-pend) (popq pend)
          (let ((new-writer (make-actor (writer-beh tbl))))
            (send* new-writer self (cdr cmd))
            (become (n-level-upd-beh tbl (car cmd) new-writer new-pend))
            ))
      (become (n-level-beh tbl))
      ))
   ))

;; -----------------------

(defun writer-beh (tbl)
  (alambda
   ((cust :addn val key . keys)
    (let ((me self))
      (actor-nlet iter ((tbl  tbl)
                        (key  key)
                        (keys keys))
        (if keys
            (beta (sub)
                (send tbl beta :find key)
              (if (eq sub $not-found)
                  (let ((new-sub (make-lookup-table)))
                    (beta _
                        (send tbl beta :add key new-sub)
                      (send iter new-sub (car keys) (cdr keys))
                      ))
                (send iter sub (car keys) (cdr keys))
                ))
          (beta _
              (send tbl beta :add key val)
            (send cust me :update))
          ))))

   ((cust :removen key . keys)
    (let ((me self))
      (actor-nlet iter ((tbl  tbl)
                        (par  nil)
                        (key  key)
                        (keys keys))
        (flet ((remove-entry ()
                 (beta _
                     (send tbl beta :remove key)
                   (if par
                       (send tbl par :is-empty?)
                     (send cust me :update))
                   )))
          (if keys
              (beta (sub)
                  (send tbl beta :find key)
                (if (eq sub $not-found)
                    (send cust me :update)
                  (beta (is-empty-sub?)
                      (send iter sub beta (car keys) (cdr keys))
                    (if is-empty-sub?
                        (remove-entry)
                      (send cust me :update)))
                  ))
            (remove-entry)
            )))))

   ((cust :replacen val key . keys)
    (let ((me self))
      (actor-nlet iter ((tbl  tbl)
                        (key  key)
                        (keys keys))
        (if keys
            (beta (sub)
                (send tbl beta :find key)
              (if (eq sub $not-found)
                  (let ((new-sub (make-lookup-table)))
                    (beta _
                        (send tbl beta :add key new-sub)
                      (send iter new-sub (car keys) (cdr keys))
                      ))
                (send iter sub (car keys) (cdr keys))
                ))
          (beta _
              (send tbl beta :replace key val)
            (send cust me :update))
          ))))
   ))

;; -----------------------

(defun make-n-level-lookup ()
  (make-actor (n-level-beh (make-lookup-table))))

;; =======================================================

;; --------------------------------------------------
;; N-level nested ALists (purely functional)
;; ALists permit addition with duplicate keys. Add prepends new entry.

(defun findn-alist (tbl key &rest keys)
  (let ((pair (assoc key tbl)))
    (if pair
        (if keys
            (apply #'findn-alist (cdr pair) keys)
          (cdr pair))
      $not-found)))

(defun addn-alist (tbl val key &rest keys)
  (let ((pair (assoc key tbl)))
    (if keys
        (let ((new-sub (apply #'addn-alist (cdr pair) val keys)))
          (acons key new-sub (if pair (remove pair tbl) tbl)))
      (acons key val tbl))
    ))

(defun removen-alist (tbl key &rest keys)
  (let ((pair (assoc key tbl)))
    (if pair
        (if keys
            (let ((new-sub (apply #'removen-alist (cdr pair) keys)))
              (if new-sub
                  (acons key new-sub (remove pair tbl))
                (remove pair tbl)))
          (remove pair tbl))
      tbl)))

(defun replacen-alist (tbl val key &rest keys)
  (let ((pair (assoc key tbl)))
    (if keys
        (let ((new-sub (apply #'replacen-alist (cdr pair) val keys)))
          (acons key new-sub (if pair (remove pair tbl) tbl)))
      (acons key val (if pair (remove pair tbl) tbl)))
    ))

;; --------------------------------------------------
;; N-level nested Maps (purely functional)
;; Maps permit only one entry for each key. Add is replacement.

(defun findn-map (tbl key &rest keys)
  (let ((item (maps:find tbl key $not-found)))
    (if (eq item $not-found)
        $not-found
      (if keys
          (apply #'findn-map item keys)
        item))
    ))

(defun addn-map (tbl val key &rest keys)
  ;; addn-map has replacement semantics
  (let ((item (maps:find tbl key $not-found)))
    (if keys
        (let ((new-sub (apply #'addn-map
                              (if (eq item $not-found)
                                  (maps:empty)
                                item)
                              val keys)))
          (maps:add tbl key new-sub))
      (maps:add tbl key val))))

(defun removen-map (tbl key &rest keys)
  (let ((item (maps:find tbl key $not-found)))
    (if (eq item $not-found)
        tbl
      (if keys
          (let ((new-sub (apply #'removen-map item keys)))
            (if (sets:is-empty new-sub)
                (maps:remove tbl key)
              (maps:add tbl key new-sub)))
        (maps:remove tbl key))
      )))

(defun replacen-map (tbl val key &rest keys)
  (apply #'addn-map tbl val key keys))

;; -----------------------------------------------------
;; FP ALIST Multi-level lookup - faster, lower concurrency
;; All actions are logically atomic.

(defun shared-alist-beh (&optional tbl)
  (ensure-par-safe-behavior
   (unshared-alist-beh 'shared-alist-beh tbl)))

(defun unshared-alist-beh (&optional (atype 'unshared-alist-beh) tbl)
  (alambda
   ((cust :state?)
    (send cust atype tbl))

   ((cust :set-state! new-type . new-state)
    (become (apply new-type new-state))
    (send cust))

   ((cust :find key)
    (let ((pair (assoc key tbl)))
      (if pair
          (send cust (cdr pair))
        (send cust $not-found))))

   ((cust :add key val)
    (um:aconsf tbl key val)
    (send cust))

   ((cust :remove key)
    (um:removef tbl key 
                :key #'car)
    (send cust))

   ((cust :replace key val)
    (setf tbl (acons key val (remove key tbl :key #'car)))
    (send cust))

   ((cust :findn key . keys)
    (send cust (apply #'findn-alist tbl key keys)))

   ((cust :addn val key . keys)
    (setf tbl (apply #'addn-alist tbl val key keys))
    (send cust))

   ((cust :removen key . keys)
    (setf tbl (apply #'removen-alist tbl key keys))
    (send cust))

   ((cust :replacen val key . keys)
    (setf tbl (apply #'replacen-alist tbl val key keys))
    (send cust))
   ))


(defun make-unshared-alist ()
  (make-actor (unshared-alist-beh)))

(defun make-shared-alist ()
  (make-actor (shared-alist-beh)))

;; --------------------------------------------------------
;; FP MAP Multi-level lookup - faster, lower concurrency
;; All actions are logically atomic.

(defun shared-map-beh (&optional (tbl (maps:empty)))
  (ensure-par-safe-behavior
   (unshared-map-beh 'shared-map-beh tbl)))

(defun unshared-map-beh (&optional (atype 'unshared-map-beh) (tbl (maps:empty)))
  (alambda
   ((cust :state?)
    (send cust atype tbl))

   ((cust :set-state! new-type . new-state)
    (become (apply new-type new-state))
    (send cust))

   ((cust :find key)
    (send cust (maps:find tbl key $not-found)))

   ((cust :add key val)
    (maps:addf tbl key val)
    (send cust))

   ((cust :remove key)
    (maps:removef tbl key)
    (send cust))

   ((cust :replace key val)
    (maps:addf tbl key val)
    (send cust))

   ((cust :findn key . keys)
    (send cust (apply #'findn-map tbl key keys)))

   ((cust :addn val key . keys)
    (setf tbl (apply #'addn-map tbl val key keys))
    (send cust))

   ((cust :removen key . keys)
    (setf tbl (apply #'removen-map tbl key keys))
    (send cust))

   ((cust :replacen val key . keys)
    (setf tbl (apply #'replacen-map tbl val key keys))
    (send cust))
   ))


(defun make-unshared-map ()
  (make-actor (unshared-map-beh)))

(defun make-shared-map ()
  (make-actor (shared-map-beh)))

;; -----------------------------------------------
#|
(defun make-n-actors-tst (quads)
  (actor (cust niter)
      (let ((tbl (make-n-level-lookup) ;; 10 us
                 ;; (make-unshared-alist) ;;  1.8 us
                 ;; (make-unshared-map)   ;;  5.5 us
                 ))
        (actor-nlet iter ((n  niter))
          (if (plusp n)
              (beta _
                  (actor-nlet inner ((quads quads))
                    (if quads
                        (beta _
                            (send* tbl beta :replacen (car quads))
                          (send inner (cdr quads)))
                      (send beta)))
                (send iter (1- n)))
            (send cust)))
        )))

(defparameter *quads*
  (loop for ix from 0 below 1000 collect
        (map 'list #'round (vm:unoise 4 10))))

(let* ((quads *quads*)
       (niter 2)
       (npts  1000)
       (dut   (simple-collector npts (* 1000 niter)
                                (med3
                                 (timing
                                  (make-n-actors-tst quads)
                                  ))))
       (act (actor (cust)
              (beta (arr)
                  (send dut beta niter)
                (send cust)
                (send (histogram) arr)
                (send (statistics) println arr)
                ))))
  (send (timing act) println))
|#

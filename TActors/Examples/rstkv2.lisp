;; remote-tkv-server.lisp -- a Key-Value service aimed at minimizing
;; network traffic
;;
;; DM/RAL 11/17 - original version
;; DM/RAL 09/20 - Updated to use modern Actors
;; DM/RAL 05/21 - Really radical update to Classical Actors
;;                Design inspired by Dale Schumacher (http://www.dalnefre.com)
;;
;; --------------------------------------------------------------------------------

#|

The latest design keeps only LZW-compressed, loenc-encoded, data
vectors in the database proper.

All keys are normalized to loenc-encodings of whatever you want to use
for keying. (Sadly, for now, this breaks lexical sort order among some
numbers and strings. Maybe we fix that later... LOENC was designed
before the idea of using it for key normalization was throught up.)

There is no need to form deep copies as they are implicit in the final
client decoding of the encoded data vectors.

So even a local server, in the same Lisp image, need not worry about
clients mutating the interior contents of database entry values. Each
client gets their own local copy on reconstruction from the original
loenc-encoding vector held in the database.

Clients assume responsibility for presenting the server with
loenc-encoded key and data vectors. But client code decodes the
contents received from the database to present normal Lisp objects to
the user. These all become unique deep copies of the database data.

Nothing can change in the database itself until clients commit their
additions / changes / deletions.

For transmission across a network, these LZW-compressed loenc-encoded
vectors will become again encoded, but the overhead of re-encoding a
byte vector is minimal. Just enough to provide a type code and length
indication.

The database on disk stores all data in a loenc-encoded LZW compressed
form. That data being an loenc-encoded FPL tree which holds the
mapping between key and data. So it becomes quite space efficient for
storage and network transmission.

|#

(defpackage #:rstkv-server
  (:use #:common-lisp #:actors)
  (:export
   #:*service-id*
   #:*writeback-delay*
   #:make-stkv-server
   #:query
   #:update
   #:find-k
   #:find-ks
   #:add-kv
   #:add-kvs
   #:rem-k
   #:rem-ks
   ))

(in-package #:rstkv-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(um:dcase
            um:magic-word
            )))

;; ---------------------------------------------------------------
(defconstant +STKV-Signature+ "{972F35AC-B87E-11E7-913F-985AEBDA9C2A}")
;; ---------------------------------------------------------------

(defun new-ver ()
  (uuid:make-v1-uuid))

(defstruct kv-state
  path
  (ver  (uuid:make-null-uuid))
  (map  (maps:empty)))

(defun copy-state-with (state &key path ver map)
  (make-kv-state
   :path (or path (kv-state-path state))
   :ver  (or ver  (kv-state-ver  state))
   :map  (or map  (kv-state-map  state))
   ))

;; ---------------------------------------------------

(defun make-kv-database-beh (state sync)
  (with-accessors ((kv-map  kv-state-map)) state
    (alambda

     ((cust :read queryfn)
      (with-worker
        (send cust (funcall queryfn kv-map))))

     ((cust :write updatefn)
      (let ((writer (make-writer cust updatefn kv-map )))
        (send writer self)
        (become (make-locked-db-beh writer state sync nil))))
     )))

;; ---------------------------------------------------

(defun make-locked-db-beh (writer state sync pend-wr)
  (with-accessors ((kv-map  kv-state-map)) state
    (alambda
     
     ((cust :read queryfn)
      (with-worker
        (send cust (funcall queryfn kv-map) )))
      
     ((cust :write updatefn)
      (become (make-locked-db-beh writer state sync
                                  (bankers-queue:q-add pend-wr
                                                       (cons cust updatefn) ))))
      
      ((cust :update new-map wr-cust) when (eq cust writer)
         (send wr-cust self (not (eq kv-map new-map)))
         (let ((new-state
                (cond ((eq new-map kv-map)
                       state)
                      (t
                       (let ((new-state (copy-state-with state
                                                         :map new-map
                                                         :ver (new-ver))))
                         (send sync self :update new-state)
                         new-state))
                      )))
           (cond (pend-wr
                  (multiple-value-bind (pair new-queue)
                      (bankers-queue:q-pop pend-wr)
                    (let ((new-writer (make-writer (car pair) (cdr pair) new-map)))
                      (send new-writer self)
                      (become (make-locked-db-beh new-writer new-state sync new-queue))
                      )))
                 (t
                  (become (make-kv-database-beh new-state sync)))
                 )))
      )))

;; ---------------------------------------------

(defun make-writer (cust updatefn map)
  (make-actor
   (lambda (db)
     ;; We need to return a map to release the locked db.
     ;; If anything goes wrong, just return the original.
     ;; Implement a 1 sec timeout.
     (let ((gate  (once db)))
       (send-after 1 gate self :update map cust)
       (let ((ans  (or (handler-case
                           (let ((new-map (funcall updatefn map)))
                             (maps:find new-map #()) ;; will err if new-map isn't a MAP
                             new-map)
                         (error ()
                           nil))
                       map)))
         (send gate self :update ans cust)
         )))))

;; ----------------------------------------

(defvar *writeback-delay* 10)

(defun make-sync-beh (server last-state tag)
  (alambda

   ((cust :update state) when (eq cust server)
    (unless (eq state last-state)
      (let ((tag  (tag self)))
        (send-after *writeback-delay* tag :write state)
        (become (make-sync-beh server state tag))
        )))
   
   ((a-tag :write state) when (eq a-tag tag)
    (save-database sink state))
   ))

;; ---------------------------------------------------

(defun enc-key (k)
  (loenc:encode k))

(defun dec-key (k)
  (loenc:decode k))

(defun enc-val (v)
  (lzw:compress v))

(defun dec-val (v)
  (lzw:decompress v))

(defun add-kv (map k v)
  (maps:add map (enc-key k) (enc-val v)))

(defun add-kvs (map kvs)
  (dolist (pair kvs)
    (setf map (add-kv map (car pair) (cdr pair))))
  map)

(defun rem-k (map k)
  (maps:remove map (enc-key k)))

(defun rem-ks (map ks)
  (dolist (k ks)
    (setf map (rem-k map k)))
  map)

(defun find-k (map k)
  (let ((val (maps:find map (enc-key k))))
    (when val
      (values (lzw:decompress val) t))))

(defun find-ks (map ks)
  (mapcan (lambda (k)
            (multiple-value-bind (val found)
                (find-k map k)
              (when found
                `((,k . ,val)))
              ))
          ks))

;; ----------------------------
;; QUERY & UPDATE -- The two fundamental ways to operate with the KV
;; store.

(defun query (cust kv-serv query-fn)
  (send kv-serv cust :read query-fn))

(defun update (cust kv-serv update-fn)
  (send kv-serv cust :write update-fn))

;; -----------------------------

(defun read-database (cust path)
  (let ((doit (actor (cust)
                (if (probe-file path)
                    (with-open-file (f path
                                       :direction :input
                                       :element-type '(unsigned-byte 8))
                      
                      (optima:match (loenc:deserialize f
                                                       :use-magic (magic-word "STKV"))
                        ((list signature _ new-ver new-table) when (string= +stkv-signature+ signature)
                         (log-info :system-log
                                   (format nil "Loaded STKV Store ~A~%Created: ~A"
                                           path (uuid:when-created new-ver)))
                         (send cust (make-kv-state
                                     :path (namestring (truename path))
                                     :map  (lzw:decompress new-table)
                                     :ver  new-ver)))
                        
                        (_
                         (error "Not an STKV Persistent Store: ~A" path))
                        ))
                  ;; else - no persistent copy, just reset to initial state
                  (let ((tmp-state (make-kv-state
                                    :path  path)))
                    (beta _
                        (save-database beta tmp-state)
                      (send cust (make-kv-state
                                  :path  (namestring (truename path))))
                      ))
                  ))))
    (send (io doit) cust)
    ))

;; ---------------------------------

(defun save-database (cust state)
  (declare (kv-state state))
  (with-accessors ((map    kv-state-map)
                   (path   kv-state-path)
                   (ver    kv-state-ver)) state
    (let ((doit (actor (cust)
                  (ensure-directories-exist path)
                  (with-open-file (f path
                                     :direction :output
                                     :if-exists :rename
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
                    (loenc:serialize
                     (list +stkv-signature+
                           (format nil
                                   " --- This is an STKV-SERVER Persistent Store, Version: ~A, Created: ~A --- "
                                   ver (uuid:when-created ver))
                           ver
                           (lzw:compress map))
                     f
                     :use-magic (magic-word "STKV")))
                  (log-info :system-log
                            (format nil "Saved STKV Store ~A~%Created: ~A"
                                    path (uuid:when-created ver)))
                  (send cust)
                  )))
      (send (io doit) cust)
      )))

;; ---------------------------------------------------------------
;; bare minimum services offered - keeps comm traffic to a minimum
;; across network. Puts burden on clients to do most of the work
;; locally.
;;
;; Old version was based on a hash-table read-only master table, and
;; private COW hash-tables. Tried to keep communication costs low by
;; transferring only changed pairs. But it suffered from SMP safe
;; opening and commits, where an entire hash-table needs to be
;; constructed, copying every element from two older tables on commit.
;; Open and commit were lengthy operations.
;;
;; This version is now based on purely functional balanced binary
;; trees (a bit slower - O(Log(N)) ) but offers the possibility that,
;; once a transaction has begun, it will be with forever consistent
;; data. Open and commit are extremely quick, except for network
;; traffic in any event.
;;
;; On successful commit, the transaction ID is updated so you can keep
;; rolling forward with the same transaction object.  On a rollback
;; exception, you need to restart with a call to ROLLBACK to get a new
;; transaction object.
;;
;; Only on commit do we find out if we have been outdated. But before
;; signalling a rollback exception, the server updates the transaction
;; to the latest version. (... that doesn't help a remote
;; connection) The transaction object continues to be valid going
;; forward, and does not need to be reconstructed anew.
;;
;; However, each client :open/:commit causes a full table copy over
;; the network.
;;
;; You don't ever have to :commit. You can just use your own private
;; copy of the table for whatever purposes, and simply discard it at
;; the end without updating the master table. But none of your changes
;; will persist unless you :commit.
;;
;; This version may be a bit less efficient on read/write/open/commit
;; in one sense, but it is far more efficient with regard to SMP
;; sharing as a result of truly functonal data structures in use.
;;
;; In this case the Actor based handler isn't really needed so much
;; for serializing requests, except for commits. But it is the sole
;; keeper of the master table.  Once opened for transactions, the
;; service is never called again until a commit.
;;
;; NOTE: Even though the tables are safely shared, the value objects
;; stored in the table must be treated as read-only and should never
;; be mutated. A fresh object should always be constructed for updates
;; through SET-KEY. Of course, over a remote connection you do have
;; your own copies. In the same running Lisp image, you could perform
;; your own deep copy in order to sidestep this issue.
;;
;; Another advantage of the functional map for tables is that items
;; are kept in some kind of sorted order.

;; ---------------------------------------------------------------

(defun default-database-pathname ()
  (merge-pathnames "STKV-Persistent-Store.dat"
                   #-:LINUX
                   (sys:get-folder-path :documents)
                   #+:LINUX
                   #P"~/Documents/"))

(defvar *service-id*    :RSTKV)
(defvar *stkv-servers*  (maps:empty))

(defun make-stkv-server (cust
                         &key
                        (path (default-database-pathname))
                        (registration *service-id*))
  (let* ((make-new-server
          (actor (cust)
            (beta (state)
                (read-database beta path)
              (actors ((server (make-kv-database-beh state sync))
                       (sync   (make-sync-beh server nil nil)))
                (maps:addf *stkv-servers* (kv-state-path state) server)
                (send cust server)
                ))))
         (prober
          (actor (cust)
            (send cust
                  (when (probe-file path)
                    ;; file exists, so we can get its true name
                    (truename path)))))
         (get-new-or-existing
          (actor (cust)
            (beta (true-path)
                (send (io prober) beta)
              (cond (true-path
                     (let* ((key    (namestring true-path))
                            (server (maps:find *stkv-servers* key)))
                       (if server
                           (send cust server)
                         (send make-new-server cust))))
                    (t
                     (send make-new-server cust))
                    )))))
    (beta (server)
        (send get-new-or-existing beta)
      (when registration
        (register-actor registration server))
      (send cust server))))
#|
(make-stkv-server sink)
(make-stkv-server sink :path "dumstkv" :registration :dummy)
(update println :dummy
        (lambda (tbl)
          (add-kv tbl :diddly :doright)))

(beta (act)
    (find-actor beta :rstkv)
  (inspect act))

(query println :rstkv 
      (lambda (tbl)
        (maps:fold tbl (lambda (k v acc)
                         (cons (cons (prin1-to-string (dec-key k))
                                     (prin1-to-string (dec-val v)))
                               acc))
                   nil)))
(update println :rstkv
        (lambda (tbl)
          (add-kv tbl "Pussy" "Galore")))
(update println :rstkv
        (lambda (tbl)
          (rem-k tbl "Pussy")))

(query println :rstkv
       (lambda (tbl)
         (find-ks tbl '(:dog :cat :pi :diddly))))

|#




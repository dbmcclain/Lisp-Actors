7;; remote-tkv-server.lisp -- a Key-Value service aimed at minimizing
;; network traffic
;;
;; DM/RAL 11/17 - original version
;; DM/RAL 09/20 - Updated to use modern Actors
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

#|
(defpackage #:remote-tkv-server
  (:use #:common-lisp)
  (:nicknames #:rstkv)
  (:import-from #:bfly
   #:!
   #:!?
   #:make-service
   #:unregister-service
   #:log-info)
  (:export
   #:make-remote-stkv-service
   #:rollback
   #:commit
   #:get-key
   #:get-keys
   #:set-key
   #:delete-key
   #:map-locally
   #:save
   #:revert
   #:rollback-exception
   ))
|#

(in-package :rstkv-client)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            rstkv-server:*service-id*

            actors:send
            actors:ask

            timeout:with-timeout
            )))

;; ---------------------------------------------------------------
;; Client Side - make client do all of the work, except for commits,
;; to cut down on comms

(defmacro with-server ((server) &body body)
  `(let ((*service-id* ,server))
     ,@body))

(defvar *rstkv-timeout*  5)

(defstruct trans
  (db     *service-id*)
  ver
  (cache  (maps:empty))
  (cow    (maps:empty))
  (nfnd   (sets:empty))
  (dels   (sets:empty)))

(defmethod get-server (trans)
  trans)

(defmethod get-server ((trans null))
  *service-id*)

(defmethod get-server ((trans trans))
  (trans-db trans))

(defun !kv (trans &rest msg)
  (apply 'send (get-server trans) msg))

(defun !?kv (trans &rest msg)
  (with-timeout *rstkv-timeout*
    (apply 'ask (get-server trans) msg)))

;; ---------------------------------------------

(defmethod open-trans (server)
  (!?kv server :open))

(defmethod get-database-key (server ver key)
  (!?kv server :get-key ver key))

(defmethod get-database-keys (server ver keys)
  (!?kv server :get-keys ver keys))

(defmethod get-all-database-keys (server ver)
  (!?kv server :get-all-keys ver))

(defmethod commit-trans (server ver adds dels)
  (!?kv server :commit ver adds dels))

(defmethod save-database (server)
  (!kv server :save))

(defmethod revert-database (server)
  (!kv server :revert))

(defmethod shutdown-server (server)
  (!kv server :shutdown))

(defmethod quit-server (server)
  (!kv server :quit))

;; ---------------------------------------------

(defstruct normalized-key
  repr)

(defmethod normalized-key ((key normalized-key))
  key)

(defmethod normalized-key (key)
  ;; we need to ensure that keys are comparable and orderable, since
  ;; we are using sets and maps. If we used only hash tables then we
  ;; would still need to ensure that keys are comparable.
  ;;
  ;; We would like to allow arbitrary data as keys, so to normalize we
  ;; will use their printed representation
  (make-normalized-key
   :repr (loenc:encode key)))

(defmethod recover-key ((nkey-repr vector))
  (loenc:decode nkey-repr))


;; ---------------------------------------------

(defun rollback (&optional trans (*service-id* *service-id* server-provided-p))
  ;; Rollback as often as you like, but especially after a rollback
  ;; exceeption. Resets our view to the last committed state.  Returns
  ;; a new transaction object for use in get-key, set-key, map-table,
  ;; and commit.
  (let* ((server  (if server-provided-p
                      *service-id*
                    (if trans
                        (trans-db trans)
                      *service-id*)))
         (new-ver (open-trans server)))
    (cond (trans
           (with-accessors ((db    trans-db)
                            (ver   trans-ver)
                            (cow   trans-cow)
                            (dels  trans-dels)
                            (nfnd  trans-nfnd)
                            (cache trans-cache)) trans
             (setf db    (get-server server)
                   cache (maps:empty)
                   cow   (maps:empty)
                   dels  (sets:empty)
                   nfnd  (sets:empty)
                   ver   new-ver)
             trans))
          
          (t
           (make-trans
            :ver  new-ver))
          )))

(defun commit (trans)
  (with-accessors ((ver   trans-ver)
                   (cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (unless (and (maps:is-empty cow)
                 (sets:is-empty dels))
      (setf ver   (commit-trans trans ver (maps:map cow 'lzw:compress) dels)
            cache (maps:union cow cache)
            nfnd  (sets:union dels nfnd)
            cow   (maps:empty)
            dels  (sets:empty)))
    trans))

(defmacro with-normalized-key ((name repr-name) key &body body)
  `(let* ((,name      (normalized-key ,key))
          (,repr-name (normalized-key-repr ,name)))
     ,@body))

(defun get-locally (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (if (or (sets:mem dels nkey-repr)
              (sets:mem nfnd nkey-repr))
          (values nil nil t) ;; return default, not-found, and known missing
        (multiple-value-bind (val found)
            (maps:find cow nkey-repr)
          (if found
              (values val t)
            (maps:find cache nkey-repr)))
        ))))
  
(defun get-key (trans key &optional default)
  (with-accessors ((ver   trans-ver)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (multiple-value-bind (val found del)
          (get-locally trans nkey)
        (cond (del
               (values default nil))
              (found
               (values val t))
              (t
               (multiple-value-bind (val found)
                   (get-database-key trans ver nkey-repr)
                 (cond (found
                        (let ((rval (lzw:decompress val)))
                          (setf cache (maps:add cache nkey-repr rval))
                          (values rval t)))
                       
                       (t
                        (setf nfnd (sets:add nfnd nkey-repr))
                        (values default nil))
                       )))
              )) )))

(defun get-keys (trans keys &optional default)
  (with-accessors ((ver   trans-ver)
                   (cache trans-cache)
                   (nfnd  trans-nfnd)) trans
    (let (local
          not-local)
      (dolist (key keys)
        (with-normalized-key (nkey nkey-repr) key
          (multiple-value-bind (val found del)
              (get-locally trans nkey)
            (cond (del
                   (push `(,key ,default nil) local))
                  (found
                   (push `(,key ,val t) local))
                  (t
                   (push nkey-repr not-local))
                  ))))
      (when not-local
        (dolist (triple (get-database-keys trans ver not-local))
          (destructuring-bind (key val found) triple
            (let ((rkey (recover-key key))
                  (rval (lzw:decompress val)))
              (cond (found
                     (setf cache (maps:add cache key rval))
                     (push `(,rkey ,rval t) local))
                    
                    (t
                     (setf nfnd (sets:add nfnd key))
                     (push `(,rkey ,default nil) local))
                    )))))
      local)))

(defun get-all-keys (trans)
  (with-accessors ((ver   trans-ver)
                   (dels  trans-dels)
                   (cow   trans-cow)) trans

    (flet ((list->set (lst &optional (init (sets:empty)))
             (reduce (lambda (ans k)
                       (sets:add ans k))
                     lst
                     :initial-value init))
           
           (map->set (map &optional (init (sets:empty)))
             (maps:fold map
                        (lambda (k v acc)
                          (declare (ignore v))
                          (sets:add acc k))
                        init))
           
           (map-set-to-list (fn set)
             (um:accum acc
               (sets:iter set
                          (lambda (k)
                            (acc (funcall fn k)))
                          ))))
      
      (let* ((rkeys (get-all-database-keys trans ver))
             ;; less the deleted keys on this end
             (rset  (sets:diff (list->set rkeys) dels))
             ;; plus any new keys on this end
             (keys  (map->set cow rset)))
        (map-set-to-list 'recover-key keys)
        ))))


(defun set-key (trans key val)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (setf cow   (maps:add cow nkey-repr val)
            nfnd  (sets:remove nfnd nkey-repr)
            dels  (sets:remove dels nkey-repr)
            cache (maps:remove cache nkey-repr))
      val)))

(defun delete-key (trans key)
  (with-accessors ((cow   trans-cow)
                   (dels  trans-dels)
                   (nfnd  trans-nfnd)
                   (cache trans-cache)) trans
    (with-normalized-key (nkey nkey-repr) key
      (setf dels  (sets:add dels nkey-repr)
            nfnd  (sets:remove nfnd nkey-repr)
            cache (maps:remove cache nkey-repr)
            cow   (maps:remove cow nkey-repr))
      key)))

(defun map-locally (trans fn)
  ;; only maps over those entries that we have either looked up,
  ;; added, or changed. But not the deletes and not-found.
  (with-accessors ((cow   trans-cow)
                   (cache trans-cache)) trans
    (maps:iter (sets:union cache cow)
               (lambda (k v)
                 (funcall fn (recover-key k) v))
               )))
               
(defsetf get-key set-key)

(defun save (trans)
  (save-database trans))

(defun revert (trans)
  (revert-database trans))

#| ;; example
(with-server ("rstkv@rincon.local")
  (let ((trans (rollback)))
    (get-key trans :pi)))

(ac:make-remote-actor "rstkv@rincon.local"
                      :register :RSTKV)
(let ((trans (rollback)))
  (get-key trans :pi))



(rstkv-server:make-stkv-server)
(ac:become-remote :rstkv "rstkv@rincon.local")
(ac:become-local  :rstkv)

(let ((trans (rollback)))
  (get-key trans :pi))

(defvar srv (ac:find-actor :RSTKV))
(ac:watch srv)
(ac:unwatch srv)

(with-server (srv)
  (let ((trans (rollback)))
    (get-key trans :pi)))

(with-server ("rstkv@arroyo.local")
  (let ((trans (rollback)))
    (get-key trans :pi)))

(with-server ("rstkv@10.0.0.142") ;; i.e. RAMBO on Win/7
  (let ((trans (rollback)))
    (get-key trans :e)))

(let ((server (ac:make-proxy
               :addr "rstkv@rincon.local")))
  (with-server (server)
    (let ((trans (rollback)))
      (get-key trans :pi))))

(with-server ("rstkv@dachshund.local")
  (let ((trans (rollback)))
    (get-key trans :pi)))

(with-server (:RSTKV)
  (let ((trans (rollback)))
    (get-key trans :e)))

(with-server (:RSTKV)
  (let ((trans (rollback)))
    (setf (get-key trans :e) (exp 1.0))
    (commit trans)))

(rstkv-server:make-stkv-server)
|#
;; -----------------------------------------------------

(defgeneric make-readable-entry (entry)
  (:method ((entry vector))
   (recover-key entry))
  (:method ((entry maps:map-cell))
   (maps:map-cell
    (recover-key (maps:map-cell-key entry))
    (maps:map-cell-val entry))))

(defun view-set (set)
  (sets:view-set set
                 :key 'make-readable-entry))

#|
(make-stkv-server)
(setf tran (rollback))
(setf (get-key tran :dog)   :cat
      (get-key tran :cat)   :mouse
      (get-key tran :mouse) :man
      (get-key tran :man)   :dog)
(commit tran)
(get-key tran :man)
(get-keys tran '(:cat :mouse :man :dog :bird))
(save)
(um:accum acc
  (map-locally tran (lambda (k v)
                      (acc (cons k v)))))

(bfly:register-service :rstkv
                       (bfly:remote-service
                        "rstkv@Dachshund.local"))

(defvar *trans* (rollback))
(progn
  (set-key *trans* :bf/all-files (car (linda:srdp '(:all-files ?x))))
  (set-key *trans* :bf/all-files-hashes (car (linda:srdp '(:all-files-hashes ?x))))
  (set-key *trans* :bf/all-files-bloom-filter (car (linda:srdp '(:all-files-bloom-filter ?x))))
  (set-key *trans* :bf/full-dir-tree (car (linda:srdp '(:full-dir-tree ?x))))
  (set-key *trans* :bf/full-directory (car (linda:srdp '(:full-directory ?x ? ?))))
  (commit *trans*))

(progn
  (delete-key *trans* :bf/all-files)
  (delete-key *trans* :bf/all-files-hashes)
  (delete-key *trans* :bf/all-files-bloom-filter)
  (delete-key *trans* :bf/full-dir-tree)
  (delete-key *trans* :bf/full-directory)
  (commit *trans*))

(let ((old-keys '(:bf-all-files
                  :bf-all-files-hashes
                  :bf-all-files-bloom-filter
                  :bf-full-dir-tree
                  :bf-full-directory))
      (new-keys '(:bf/all-files
                  :bf/all-files-hashes
                  :bf/all-files-bloom-filter
                  :bf/full-dir-tree
                  :bf/full-directory)))
  (get-keys *trans* old-keys)
  (um:foreach (lambda (k-old k-new)
                (set-key *trans* k-new
                         (get-key *trans* k-old))
                (delete-key *trans* k-old))
              old-keys new-keys)
  (commit *trans*))
|#

#|
(progn
  (mapc (lambda (k)
          (delete-key tran k))
        (get-all-keys tran))
  (mapc (lambda (assoc)
          (setf (get-key tran (first assoc)) (second assoc)))
        '((:CAT (:NAME "Chara" :LIKES :MOUSE))
          (:DOG (:ATTR :MANS-BEST-FRIEND :LIKES :CAT))
          (:MAN :DOG)
          (:MOUSE (:BEHAVIOR :MEEK :AFRAID-OF :CAT))
          (:WOMAN (:NAME "Helene" :MARRIED-TO "David"))))
  (commit tran)
  (rollback tran))

(ac:spawn (lambda () (let ((trans (rollback))) (ac:pr (ac:ask :rstkv :get-all-keys (trans-ver trans))))))
(let ((trans (rollback))) (ac:pr (ac:ask :rstkv :get-all-keys (trans-ver trans))))
|#
#|
(defparameter *tbl* (sets:empty))
(defun get-all-types ()
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (let ((item (loenc:encode (type-of obj))))
       (setf *tbl* (sets:add *tbl* item)))))
  (sets:view-set *tbl*
                 :key 'loenc:decode))

(defun get-all-classes ()
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (let* ((class (class-of obj))
            (item  (class-name class)))
       (setf *tbl* (sets:add *tbl* item)))))
  (sets:view-set *tbl*))

(defmethod screener ((obj stream))
  t)

(defmethod screener ((obj clos::system-object))
  t)

(defmethod screener (obj)
  nil)

(defun get-builtin-classes (type)
  (setf *tbl* (sets:empty))
  (hcl:sweep-all-objects
   (lambda (obj)
     (when (subtypep (type-of obj) type)
       (let* ((class (class-of obj))
              (item  (class-name class)))
         (setf *tbl* (sets:add *tbl* item))))))
  (um:accum acc
    (sets:iter *tbl*
               (lambda (k)
                 (acc k))
               )))

(defun sinc (x)
  (let ((phi (* 2 pi x)))
    (/ (sin phi) phi)))

(defun sinsinc (n x)
  ;; describes leakage in cell j of N-point FFT
  ;; for x = j/N - F/Fs
  (let ((phi (* 2 pi x)))
    (/ (sin (* n phi))
       (* n (sin phi)))))

|#

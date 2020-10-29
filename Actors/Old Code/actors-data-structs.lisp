
(in-package #:actors-data-structs)

;; -------------------------------------------------------
;; Safe shared FIFO queues

(def-factory make-shared-queue (hd tl)
  ;; use send, ask to retrieve
  ()
  (labels ((normalize ()
             (unless hd
               (setf hd (nreverse tl)
                     tl nil))))
    (um:dlambda
      
      (:add (item)
       (push item tl)
       (normalize))
      
      (:pop ()
       (prog1
           (pop hd)
         (normalize)))
      
      (:empty-p ()
       (null hd))
      
      (:pop-all ()
       (nconc (shiftf hd nil) (nreverse (shiftf tl nil))))
      
      (:inspect ()
       (inspect (list hd tl)))
      )))

;; -------------------------------------------------------
;; Safe shared LIFO stacks

(def-factory make-shared-stack (items)
  ;; use send, ask to retrieve
  ()
  (um:dlambda
    
    (:add (item)
     (push item items))
    
    (:pop ()
     (pop items))
    
    (:empty-p ()
     (null items))
    
    (:pop-all ()
     (nreverse (shiftf items nil)))
    
    (:inspect ()
     (inspect items))
    ))

;; -----------------------------------------
;; Safe shared functional maps

(def-factory make-shared-map ((map (sets:empty)))
  ()
  (um:dlambda
    
    (:add (key val)
     (setf map (maps:add key val map)))
    
    (:find (key)
     (maps:find key map))
    
    (:fold (fn accu)
     (maps:fold fn map accu))
      
    (:is-empty ()
     (maps:is-empty map))
      
    (:remove (key)
     (setf map (sets:remove key map)))
      
    (:map (fn)
     (maps:map fn map))
      
    (:iter (fn)
     (maps:iter fn map))
      
    (:get ()
     (shiftf map (maps:empty)))
      
    (:setf (new-map)
     (setf map new-map))
      
    (:empty ()
     (setf map (sets:empty)))
      
    (:cardinal ()
     (sets:cardinal map))
      
    (:mapi (fn)
     (maps:mapi fn map))
    ))

#|
(let ((mm (make-shared-map)))
  (send mm :add :dog :cat)
  (send mm :add :cat :mouse)
  (send mm :add :mouse :man)
  (send mm :add :man :dog)
  (send mm :iter (lambda (k v) (pr (cons k v))))
  (prog1
      (pr
       (ask mm :fold  (lambda (k v a)
                        (cons (list k v) a))
            nil))
    (pr (ask mm :cardinal))
    ))
 |#

;; -----------------------------------------
;; Safe shared functional sets

(def-factory make-shared-set ((set (sets:empty)))
  ()
  (um:dlambda
      
    (:add (key)
     (setf set (sets:add key set)))
      
    (:mem (key)
     (sets:mem key set))
      
    (:fold (fn accu)
     (sets:fold fn set accu))
      
    (:is-empty ()
     (sets:is-empty set))
      
    (:remove (key)
     (setf set (sets:remove key set)))
      
    (:iter (fn)
     (sets:iter fn set))
      
    (:get ()
     (shiftf set (sets:empty)))
      
    (:setf (new-set)
     (setf set new-set))
      
    (:empty ()
     (setf set (sets:empty)))
      
    (:cardinal ()
     (sets:cardinal set))
      
    (:some (pred)
     (sets:some pred set))
      
    (:!union (arg-set)
     (setf set (sets:union set arg-set)))
      
    (:@union (arg-set)
     (sets:union set arg-set))
      
    (:!intersection (arg-set)
     (setf set (sets:intersection set arg-set)))
      
    (:@intersection (arg-set)
     (sets:intersection set arg-set))
      
    (:!s-x (arg-set)
     (setf set (sets:diff set arg-set)))
      
    (:!x-s (arg-set)
     (setf set (sets:diff arg-set set)))
      
    (:@s-x (arg-set)
     (sets:diff set arg-set))
      
    (:@x-s (arg-set)
     (sets:diff arg-set set))
      
    (:partition (pred)
     (sets:partition pred set))
      
    (:elements ()
     (sets:elements set))
    ))

;; -----------------------------------------------

(def-factory make-shared-hash-table ((table (make-hash-table
                                             :single-thread t)))
  ()
  (um:dlambda
      
    (:clear ()
     (clrhash table))
      
    (:add (key val)
     ;; this simply overwrites any existing entry
     (setf (gethash key table) val))
      
    (:remove (key)
     (remhash key table))
      
    (:get-all ()
     (um:accum acc
       (maphash (um:compose #'acc #'cons) table)))
      
    (:find (name)
     (gethash name table))
      
    (:find-or-add (name val)
     (or (gethash name table)
         (setf (gethash name table) val)))
    ))

  
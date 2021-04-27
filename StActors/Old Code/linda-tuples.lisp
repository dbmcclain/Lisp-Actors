;; linda-tuples.lisp -- Augmented tuple spaces with Actors restricted
;; to tuples
;;
;; DM/RAL 11/17
;; ---------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


;; ------------------------------------------------------------------------------------

(in-package #:linda)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------------------------------------

#|
(bfly::make-actor-service linda:*linda* :register :LINDA)
|#

(defstruct (queue
            (:constructor %make-queue))
  hd tl)

(defun make-queue (&optional data)
  (let ((lst (cons nil data)))
    (%make-queue
     :hd lst
     :tl (last lst))))

(defun contents (queue)
  (cdr (queue-hd queue)))

(defun addq (queue item)
  (declare (queue queue))
  (setf (queue-tl queue)
        (setf (cdr (the cons (queue-tl queue)))
              (list item))))

(defun find-if-removing (queue pred-fn)
  (declare (queue queue))
  (let ((hd (queue-hd queue)))
    (declare (cons hd))
    (nlet-tail iter ((hd       (cdr hd))
                     (follower hd))
      (when hd
        (if (funcall pred-fn (car (the cons hd)))
            (progn
              (unless (setf (cdr (the cons follower)) (cdr (the cons hd)))
                (setf (queue-tl queue) follower))
              t)
          ;; else
          (iter (cdr (the cons hd)) (cdr (the cons follower)))))
      )))

(defun scan-if-removing-all (queue pred-fn)
  (declare (queue queue))
  (let ((hd (queue-hd queue)))
    (declare (cons hd))
    (nlet-tail iter ((hd       (cdr hd))
                     (follower hd))
      (if hd
          (if (funcall pred-fn (car (the cons hd)))
              (let ((next (cdr (the cons hd))))
                (setf (cdr (the cons follower)) next)
                (iter next follower))
            ;; else
            (iter (cdr (the cons hd)) (cdr (the cons follower))))
        ;; else
        (setf (queue-tl queue) follower)))
    ))
  
;; ---------------------------------------------------------

(defvar *bindings*)

(defun match (pat tuple)
  (declare (cons pat tuple))
  ;; we already match first elements on entry
  (and (= (length pat) (length tuple))
       (let ((*bindings* nil))
         (and (every #'match-element
                     (cdr pat) (cdr tuple))
              (values t *bindings*))
         )))

(defmethod match-element ((pat-elt symbol) tuple-elt)
  (cond ((string= "?" (symbol-name pat-elt)))
        ((char= #\? (char (symbol-name pat-elt) 0))
         (let ((pair (assoc pat-elt *bindings*))) ;; have we seen this one before?
           (cond (pair
                  ;; '(?x ?x) - make sure previous binding matches tuple-elt
                  (match-element (cdr (the cons pair)) tuple-elt))
                 (t
                  ;; fresh binding
                  (setf *bindings*
                        (acons pat-elt tuple-elt *bindings*))))
           ))
        ((and (symbolp tuple-elt)
              ;; two symbols can match, regardless of their packages
              (string= (symbol-name pat-elt)
                       (symbol-name (the symbol tuple-elt)))))
        ))

(defmethod match-element ((pat-elt cons) (tuple-elt cons))
  ;; allow for some destructuring matches
  (and (match-element (car pat-elt) (car tuple-elt))
       (match-element (cdr pat-elt) (cdr tuple-elt))))

(defmethod match-element ((pat-elt string) (tuple-elt string))
  ;; case sensitive
  (string= pat-elt tuple-elt))

(defmethod match-element (pat-elt tuple-elt)
  ;; vectors, numbers, complex, etc.
  (equalp pat-elt tuple-elt))

;; ----------------------------------------------------------

(defun kw (sym)
  (intern (symbol-name sym) :keyword))

(defun wild? (sym)
  (and (symbolp sym)
       (char= #\? (char (symbol-name sym) 0))))

(defun collect-wild-args (pat)
  (delete-duplicates
   (mapcan (lambda (pat-elt)
             (cond ((wild? pat-elt)
                    (unless (string= pat-elt "?")
                      (list pat-elt)))
                   ((consp pat-elt)
                    (collect-wild-args pat-elt))))
           pat)))

(defun chk-tuple (tup)
  (let ((key (car tup)))
    (when (or (not (symbolp key))
              (wild? key))
      (error "Invalid tuple key: ~A" tup))
    (cons (kw key) (cdr tup))))

(defun chk-keys (keys)
  (unless (every #'symbolp keys)
    (error "Invalid key(s) in list: ~A" keys))
  (mapcar #'kw keys))

(defun chk-plist (plist)
  (let* ((bindings (group plist 2))
         (keys     (chk-keys (mapcar #'car bindings)))
         (vals     (mapcar #'cadr bindings)))
    (pairlis keys vals)))

;; ---------------------------------------------------------

(defun make-ts (&key bindings tuples)
  (declare (list bindings tuples))
  (check-type bindings list)  ;; initial bindings should be a plist
  (check-type tuples list)    ;; initial data is a simple list
  (make-actor
   (let ((readers         (make-hash-table
                           :test 'eq
                           :single-thread t))
         (extractors      (make-hash-table
                           :test 'eq
                           :single-thread t))
         (binding-readers (make-queue))
         (tuple-space     (make-hash-table
                           :test 'eq
                           :single-thread t))
         (binding-space   (make-hash-table
                           :test 'eq
                           :single-thread t)))
     (declare (hash-table readers extractors
                          tuple-space binding-space)
              (queue binding-readers))
     
     (labels ((collect-bindings (keys)
                (block nil
                  (mapcar (lambda (key)
                            (let ((val (gethash key binding-space binding-space)))
                              (if (eq val binding-space)
                                  (return nil)
                                val)))
                          keys)))

              (bulk-load-tuples (tuples)
                (foreach (lambda (tuple)
                           (let* ((tuple (chk-tuple tuple))
                                  (key   (car tuple))
                                  (queue (find-or-add-queue key tuple-space)))
                             (addq queue tuple)))
                         tuples))

              (bulk-load-bindings (bindings)
                (foreach (lambda (binding)
                           (setf (gethash (car bindings) binding-space) (cdr binding)))
                         (chk-plist bindings)))

              (find-or-add-queue (key table)
                (or (gethash key table)
                    (setf (gethash key table) (make-queue))))

              (get-tree-of-table (table)
                (let ((tree (maps:empty)))
                  (maphash (lambda (k v)
                             (setf tree (maps:add k v tree)))
                           table)
                  tree))
              
              (bindings-as-list ()
                (let ((tree (get-tree-of-table binding-space)))
                  (um:accum acc
                    (maps:iter (lambda (k v)
                                 (acc (list k v)))
                               tree))))

              (table-as-list (table)
                (let ((tree (get-tree-of-table table)))
                  (um:accum acc
                    (maps:iter (lambda (k v)
                                 (declare (ignore k))
                                 (foreach #'acc
                                          (contents v)))
                               tree))))

              (match-rdr-tup (tuple pat fn)
                (multiple-value-bind (ok bindings)
                    (match pat tuple)
                  (when ok
                    (apply #'spawn fn 
                           (mapcar #'cdr bindings))
                    t)))
                
              (match-readers (tuple)
                (lambda (pair)
                  (destructuring-bind (pat fn) pair
                    (match-rdr-tup tuple pat fn))))
              
              (match-tuples (pat fn)
                (lambda (tuple)
                  (match-rdr-tup tuple pat fn))))
              
       (bulk-load-tuples   tuples)
       (bulk-load-bindings bindings)
       
       (dlambda*
         ;; ----------------------------------------------------
         (:reset (&key bindings tuples)
          (:remove-all-bindings)
          (:remove-all-tuples)
          (bulk-load-tuples   tuples)
          (bulk-load-bindings bindings))
         
           ;; ----------------------------------------------------
           (:out (tuple)
            ;; find, but don't remove, a datum that satisfies the
            ;; pattern, calling the match extractor with the datum, else
            ;; enqueue the reader for later
            (let* ((key   (car tuple)))
              (when-let (queue (gethash key readers))
                (scan-if-removing-all
                 queue
                 (match-readers tuple)))
              
              (let ((queue (gethash key extractors)))
                (unless (and queue
                             (find-if-removing
                              queue
                              (match-readers tuple)))
                  (addq (find-or-add-queue key tuple-space) tuple)
                  ))))

           ;; ----------------------------------------------------
           (:rd (pat fn pred)
            ;; find, but don't remove, a datum that satisfies the
            ;; pattern, calling the match extractor with the datum, else
            ;; enqueue the reader for later
            ;;
            ;; If pred is non-nil, this becomes a non-blocking
            ;; predicate call, with pred being the function to invoke
            ;; if there are no matching tuples.
            ;;
            (let* ((key   (car pat))
                   (queue (gethash key tuple-space)))
              (unless (and queue
                           (some (match-tuples pat fn)
                                 (contents queue)))
                (if pred
                    (spawn pred)
                  (addq (find-or-add-queue key readers) (list pat fn)))
                )))
           
           ;; ----------------------------------------------------
           (:in (pat fn pred)
            ;; find, but don't remove, a datum that satisfies the
            ;; pattern, calling the match extractor with the datum, else
            ;; enqueue the reader for later
            (let* ((key   (car pat))
                   (queue (gethash key tuple-space)))
              (unless (and queue
                           (find-if-removing
                            queue
                            (match-tuples pat fn)))
                (if pred
                    (spawn pred)
                  (addq (find-or-add-queue key extractors) (list pat fn)))
                )))
           
           ;; ----------------------------------------------------
           (:remove-all-tuples ()
            (clrhash tuple-space)
            (clrhash readers)
            (clrhash extractors))
           
           ;; ----------------------------------------------------
           (:outb (bindings)
            ;; bindings is an alist
            (foreach (lambda (binding)
                       (setf (gethash (car binding) binding-space) (cdr binding)))
                     bindings)
            (scan-if-removing-all
             binding-readers
             (lambda (triple)
               (destructuring-bind (keys fn) triple
                 (when-let (vals (collect-bindings keys))
                   (apply #'spawn fn vals)
                   t )))))
           
           ;; ----------------------------------------------------
           (:rdb (keys fn pred)
            (if-let (vals (collect-bindings keys))
                (apply #'spawn fn vals)
              (if pred
                  (spawn pred)
                (addq binding-readers (list keys fn)))
              ))

           ;; ----------------------------------------------------
           (:remove-bindings (keys)
            (foreach (lambda (key)
                       (remhash key binding-space))
                     keys))
           
           ;; ----------------------------------------------------
           (:remove-all-bindings ()
            (clrhash binding-space)
            (setf binding-readers (make-queue)))
           
           ;; ----------------------------------------------------
           (:introspect ()
            (inspect `(:bindings   ,(bindings-as-list)
                       :data       ,(table-as-list tuple-space)
                       :extractors ,(table-as-list extractors)
                       :readers    ,(table-as-list readers)
                       :breaders   ,(contents binding-readers)
                       )))
           )))))

;; ---------------------------------------------------------

(defvar *linda* (make-ts))

;; ---------------------------------------------------------
;; Tuples

(defun get-ts (ts)
  (case ts
    ((t nil) *linda*)
    (t       ts)
    ))

(defun send-ts (ts &rest args)
  (apply #'send (get-ts ts) args))

;; ----------------------------------

(defun out (tuple &optional ts)
  (check-type tuple cons)
  (send-ts ts :out (chk-tuple tuple)))

(defun rd (pat fn &optional ts)
  ;; non-destrutive read
  (check-type pat cons)
  (check-type fn function)
  (send-ts ts :rd (chk-tuple pat) (=cont fn) nil))

(defun rdp (pat fn &optional ts (fail-fn #'lw:do-nothing))
  ;; non-destrutive read
  (check-type pat cons)
  (check-type fn function)
  (check-type fail-fn function)
  (send-ts ts :rd (chk-tuple pat) (=cont fn) (=cont fail-fn)))

(defmacro on-rd ((pat &optional ts) &body body)
  `(rd ',pat (lambda ,(collect-wild-args pat) ,@body) ,ts))

(defmacro on-rdp ((pat &optional ts) t-form &optional f-form)
  `(rdp ',pat (lambda ,(collect-wild-args pat) ,t-form) ,ts (lambda nil ,f-form)))

(defun in (pat fn &optional ts)
  ;; destructive read
  (check-type pat cons)
  (check-type fn function)
  (send-ts ts :in (chk-tuple pat) (=cont fn) nil))

(defun inp (pat fn &optional ts (fail-fn #'lw:do-nothing))
  ;; destructive read
  (check-type pat cons)
  (check-type fn function)
  (check-type fail-fn function)
  (send-ts ts :in (chk-tuple pat) (=cont fn) (=cont fail-fn)))

(defmacro on-in ((pat &optional ts) &body body)
  `(in ',pat (lambda ,(collect-wild-args pat) ,@body) ,ts))

(defmacro on-inp ((pat &optional ts) t-form &optional f-form)
  `(inp ',pat (lambda ,(collect-wild-args pat) ,t-form) ,ts (lambda nil ,f-form)))

;; ---------------------------------------------------------
;; Bindings

(defun outb (plist &optional ts)
  (check-type plist cons)
  (send-ts ts :outb (chk-plist plist)))

(defun rdb (keys fn &optional ts)
  (check-type keys cons)
  (check-type fn function)
  (send-ts ts :rdb (chk-keys keys) (=cont fn) nil))

(defun rdbp (keys fn &optional ts (fail-fn #'lw:do-nothing))
  (check-type keys cons)
  (check-type fn function)
  (check-type fail-fn function)
  (send-ts ts :rdb (chk-keys keys) (=cont fn) (=cont fail-fn)))

(defmacro on-rdb ((keys &optional ts) &body body)
  `(rdb ',keys (lambda ,keys ,@body) ,ts))

(defmacro on-rdbp ((keys &optional ts) t-form &optional f-form)
  `(rdbp ',keys (lambda ,keys ,t-form) ,ts (lambda nil ,f-form)))

(defun remove-bindings (keys &optional ts)
  (check-type keys cons)
  (send-ts ts :remove-bindings (chk-keys keys)))

;; ------------------------------------------------------------------------------------
;; Synchronous operations, bridging Linda world and imperative Lisp

(defun sop (fn pat ts)
  (let ((mbox (mp:make-mailbox)))
    (without-actor-status
      ;; we need this to avoid deadlock
      ;;
      ;; If this were called within an Actor, then the coming =CONT
      ;; used in preparing the callback functions will have those
      ;; continuations delivered to the Actor's mailbox. Meanwhile,
      ;; the Actor will be tied up waiting on this local mailbox,
      ;; never seeing those messages, and so would become permanently
      ;; deadlocked.
      ;;
      ;; The use of WITHOUT-ACTOR-STATUS makes the =CONT become an
      ;; identity operation.
      ;;
      (funcall fn pat
               (lambda (&rest args)
                 (mp:mailbox-send mbox args))
               ts
               (lambda ()
                 (mp:mailbox-send mbox nil))))
    (mp:mailbox-read mbox)))

(defun srdp (pat &optional ts)
  (sop #'rdp pat ts))

(defun sinp (pat &optional ts)
  (sop #'inp pat ts))

(defun srdbp (keys &optional ts)
  (sop #'rdbp keys ts))


(defun remove-tuples (pat &optional ts)
  (loop while (sinp pat ts)))

;; ---------------------------------------------------------
;; Maintenance

(defun reset (&optional ts)
  (send-ts ts :reset))

(defun remove-all-tuples (&optional ts)
  (send-ts ts :remove-all-tuples))

(defun remove-all-bindings (&optional ts)
  (send-ts ts :remove-all-bindings))

(editor:setup-indent "on-rd"   1)
(editor:setup-indent "on-in"   1)
(editor:setup-indent "on-rdb"  1)
(editor:setup-indent "on-rdp"  1)
(editor:setup-indent "on-inp"  1)
(editor:setup-indent "on-rdbp" 1)

;; ---------------------------------------------------------

#|
(outb '(x 15 y 32))
(foreach #'out '((:one 1)
                 (:two 2)
                 (:three 3)
                 (:x  15)
                 (:easy2 32)
                 (:list  (a b c))))

(progn
  (on-rdb ((x y z))
    (pr (format nil "Bindings: ~A" (list :x x :y y :z z))))
  (on-in ((:one ?one))
    (pr ?one))
  (on-in ((:two ?two))
    (pr ?two))
  (on-in ((:three ?three))
    (pr ?three))
  (on-in ((:x  ?x))
    (pr ?x))
  (on-in ((:easy2 ?x))
    (pr ?x))
  (on-in ((:list ?list))
    (pr ?list))
  (outb '(z 0.3))
  )

(send *linda* :introspect)
(reset)
(setf *linda* (make-ts))


(defun tst-spd (&key (nthr 1) (niter #N100_000))
  (reset)
  (let ((time-info (SYSTEM::MAKE-AND-INITIALIZE-TIME-INFO
                              '(LOOP REPEAT N DO (DIDDLE *LINDA*))
                              NIL 'SYSTEM::NOT-GIVEN T)))
    (labels ((doit ()
               (on-in ((:test-on ?n))
                 (cond ((plusp ?n)
                        (out `(:test-on ,(- ?n nthr)))
                        (doit))
                       ((zerop ?n)
                        (out '(:test-done)))
                       ))))
      
      (loop repeat nthr
            for n from niter by -1
            do
            (out `(:test-on ,n))
            (spawn #'doit))

      (on-in ((:test-done))
        (SYSTEM::TIME-BODY-POST time-info))
      )))

(plt:plot 'timings
          '(1  2  3  4  8  16 100)
          '(67 42 31 29 27 27 27)
          :clear t
          :title "Test Timings vs #Threads"
          :xtitle "Nbr Threads"
          :ytitle "Execution Time/1M Iterations"
          :symbol :circle
          :plot-joined t
          :xlog t
          :ylog nil)

;; --------------------------------------------------------

(defvar *done* nil)

(labels ((monitor-temp ()
           (on-inp ((temp ?temp))
             (pr (format nil "Temp: ~A" ?temp))
             (outp `(status
                     ,(if (< (* (round ?temp 0.05) 0.05) 100)
                          :on :off)))
             (unless *done*
               (monitor-temp))))

         (monitor-status ()
           (on-inp ((status ?on/off))
             (pr (format nil "Status: ~A" ?on/off))
             (outp `(fuel-pump ,(case ?on/off
                                  (:on  :open)
                                  (:off :closed))))
             (unless *done*
               (monitor-status))))

         (monitor-fuel-pump ()
           (on-inp ((fuel-pump ?open/closed))
             (pr (format nil "Fuel pump now ~A" ?open/closed))
             (unless *done*
               (monitor-fuel-pump)))))
  
  (setf *done* nil)
  (monitor-temp)
  (monitor-status)
  (monitor-fuel-pump))

(let ()
  (outp '(temp 0))
  (dotimes (x 2)
    (dotimes (y 10)
      (sleep 1)
      (let ((temp (+ 98 x (random 0.07) 1.04)))
        (outp `(temp ,temp)))
      )))
(progn
  (setf *done* t)
  (outp '(temp 0)))

(bfly:! :linda :introspect)
(send *linda* :introspect)
(on-inp (())
  (val 
   (pr val)))
(send-item 15)                   
|#


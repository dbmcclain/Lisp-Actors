;; linda.lisp -- Augmented tuple spaces with Actors
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


(defpackage #:linda
  (:use #:common-lisp #:fac)
  (:import-from #:um
   #:if-let
   #:when-let
   #:foreach
   #:nlet
   #:nlet-tail
   #:group
   #:dlambda)
  (:import-from #:optima.extra
   #:lambda-match)
  (:export
   #:*linda*
   #:make-ts
   #:send-item
   #:send-items
   #:get-data
   #:on-data
   #:peek-data
   #:send-bindings
   #:remove-bindings
   #:on-bindings
   #:with-on-data
   #:with-get-data
   ))

;; ------------------------------------------------------------------------------------

(in-package #:linda)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; ------------------------------------------------------------------------------------

#|
(bfly::make-actor-service linda:*linda* :register :LINDA)
|#

(defvar +unbound+ (load-time-value (gensym) t))

(defun make-ts (&key bindings data)
  (declare (list bindings data))
  (check-type bindings list)  ;; initial bindings should be a plist
  (check-type data list)      ;; initial data is a simple list
  (make-actor
   (labels ((make-queue (&optional data)
              (let ((lst (cons nil (copy-list data))))
                (cons lst (last lst))))
     
            (addq (queue item)
              (declare (cons queue))
              (setf (cdr queue) (setf (cddr queue) (list item))))

            (send-continuation (actor cont-fn &rest args)
              (if actor
                  (apply #'send actor
                         :continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A}
                         cont-fn args)
                (apply #'spawn cont-fn args)))
            
            (collect-bindings (keys)
              (block nil
                (mapcar (lambda (key)
                          (let ((val (getf bindings key +unbound+)))
                            (if (eq val +unbound+)
                                (return nil)
                              val)))
                        keys))))

     (let ((readers         (make-queue))
           (extractors      (make-queue))
           (binding-readers (make-queue)))
       (declare (cons readers extractors binding-readers))
       
       (setf bindings (copy-list bindings)
             data     (make-queue data))
       (locally
         (declare (cons data))
         
         (dlambda
           ;; ----------------------------------------------------
           (:reset (&key new-bindings new-data)
            (setf bindings  new-bindings
              data            (make-queue new-data)
              readers         (make-queue)
              extractors      (make-queue)
              binding-readers (make-queue)))
           
           ;; ----------------------------------------------------
           (:on-bindings (actor keys cont-fn)
            (if-let (binds (collect-bindings keys))
                (apply #'send-continuation actor cont-fn binds)
              (addq binding-readers (list actor cont-fn keys))))
           
           ;; ----------------------------------------------------
           (:send-bindings (binds)
            ;; binds should be a plist
            (foreach (lambda (binding)
                       (declare (cons binding))
                       (destructuring-bind (key val) binding
                         (setf (getf bindings key) val)))
                     (group binds 2))
            (let ((hd (car binding-readers)))
              (declare (cons hd))
              (nlet-tail iter ((leader   (cdr hd))
                               (follower hd))
                (declare (list leader)
                         (cons follower))
                (if (null leader)
                    (setf (cdr binding-readers) follower)
                  (destructuring-bind (actor cont-fn keys) (car leader)
                    (if-let (binds (collect-bindings keys))
                        (progn
                          (apply #'send-continuation actor cont-fn binds)
                          (let ((new-tl (cdr leader)))
                            (setf (cdr follower) new-tl)
                            (iter new-tl follower)))
                      ;; else
                      (iter (cdr leader) (cdr follower))))
                  ))))
           
           ;; ----------------------------------------------------
           (:remove-bindings (keys)
            (foreach (lambda (key)
                       (remf bindings key))
                     keys))
           
           ;; ----------------------------------------------------
           (:get-data (actor pat-fn)
            ;; find and remove a datum that satisfies the patten,
            ;; calling the match extractor with the datum, else enqueue
            ;; the extractor for later
            (let ((hd (car data)))
              (declare (cons hd))
              (nlet-tail iter ((leader   (cdr hd))
                               (follower hd))
                (declare (list leader)
                         (cons follower))
                (if (null leader)
                    (addq extractors (cons actor pat-fn))
                  (if-let (extractor-fn (funcall pat-fn (car leader)))
                      (progn
                        (send-continuation actor extractor-fn)
                        (unless (setf (cdr follower) (cdr leader))
                          (setf (cdr data) follower)))
                    ;; else
                    (iter (cdr leader) (cdr follower)))
                  ))))

           ;; ----------------------------------------------------
           (:on-data (actor pat-fn)
            ;; find, but don't remove, a datum that satisfies the
            ;; pattern, calling the match extractor with the datum, else
            ;; enqueue the reader for later
            (unless (funcall (current-actor) :peek-data actor pat-fn)
              (addq readers (cons actor pat-fn))))
           
           ;; ----------------------------------------------------
           (:peek-data (actor pat-fn)
            ;; find, but don't remove, a datum that satisfies the
            ;; pattern, calling the match extractor with the datum, else
            ;; enqueue the reader for later
            (some (lambda (item)
                    (when-let (reader-fn (funcall pat-fn item))
                      (send-continuation actor reader-fn)
                      t))
                  (cdar data)))
           
           ;; ----------------------------------------------------
           (:send-item (datum)
            ;; remove and enable all pending matching readers with the
            ;; datum
            (let ((hd (car readers)))
              (declare (cons hd))
              (nlet-tail iter ((leader   (cdr hd))
                               (follower hd))
                (declare (list leader)
                         (cons follower))
                (if (null leader)
                    (setf (cdr readers) follower)
                  (destructuring-bind (actor . pat-fn) (car leader)
                    (if-let (reader-fn (funcall pat-fn datum))
                        (progn
                          (send-continuation actor reader-fn)
                          (let ((new-tl (cdr leader)))
                            (setf (cdr follower) new-tl)
                            (iter new-tl follower)))
                      ;; else
                      (iter (cdr leader) (cdr follower)))
                    ))))
            ;; find and remove one extractor that matches the datum,
            ;; calling the match extractor with the data, or else enqueue
            ;; the datum for later
            (let ((hd (car extractors)))
              (declare (cons hd))
              (nlet-tail iter ((leader   (cdr hd))
                               (follower hd))
                (declare (list leader)
                         (cons follower))
                (if (null leader)
                    (addq data datum)
                  (destructuring-bind (actor . pat-fn) (car leader)
                    (if-let (extractor-fn (funcall pat-fn datum))
                        (progn
                          (send-continuation actor extractor-fn)
                          (unless (setf (cdr follower) (cdr leader))
                            (setf (cdr extractors) follower)))
                      ;; else
                      (iter (cdr leader) (cdr follower)))
                    )))))
           
           ;; ----------------------------------------------------
           (:send-items (data)
            (let ((self (current-actor)))
              (dolist (datum data)
                (funcall self :send-item datum))))
           
           ;; ----------------------------------------------------
           (:introspect ()
            (inspect `(:bindings   ,bindings
                       :data       ,data
                       :extractors ,extractors
                       :readers    ,readers
                       :breaders   ,binding-readers)))
           ))))))

(defvar *linda* (make-ts))

(defun send-item (datum &optional (ts *linda*))
  (send ts :send-item datum))

(defun send-items (data &optional (ts *linda*))
  ;; data should be a list of datum
  (check-type data list)
  (send ts :send-items data))

(defun peek-data (patfn &optional (ts *linda*))
  ;; non-destructive, non-blocking read
  (send ts :peek-data (current-actor) patfn))

(defun get-data (patfn &optional (ts *linda*))
  ;; destructive read
  (send ts :get-data (current-actor) patfn))

(defun on-data (patfn &optional (ts *linda*))
  ;; non-destructive read
  (send ts :on-data (current-actor) patfn))

(defun send-bindings (bindings &optional (ts *linda*))
  ;; bindings should be a plist
  (check-type bindings list)
  (send ts :send-bindings bindings))

(defun remove-bindings (keys &optional (ts *linda*))
  (check-type keys list)
  (send ts :remove-bindings keys))

(defun on-bindings (keys cont-fn &optional (ts *linda*))
  ;; non-destructive read
  (check-type keys list)
  (send ts :on-bindings (current-actor) keys cont-fn))

(defmacro with-get-data ((&optional (ts *linda*)) &rest clauses)
  (let ((clauses-fn (fac::parse-pattern-clauses clauses)))
    `(get-data ,clauses-fn ,ts)))

(defmacro with-on-data ((&optional (ts *linda*)) &rest clauses)
  (let ((clauses-fn (fac::parse-pattern-clauses clauses)))
    `(on-data ,clauses-fn ,ts)))

(editor:setup-indent "with-get-data" 1)
(editor:setup-indent "with-on-data"  1)

#|
(send-bindings '(:x 15 :y 32))
(send-items '(:one :two :three 15 32 (a b c)))

(progn
  
  (on-bindings '(:x :y :z)
                 (lambda (x y z)
                   (pr (format nil "Bindings: ~A" (list :x x :y y :z z)))))
  
  (spawn (lambda ()
           (nlet doit ()
             (with-get-data ()
               (sym when (keywordp sym)
                    (pr (current-actor) sym)
                    (doit))
               ))))
  
  (spawn (lambda ()
           (nlet doit ()
             (with-get-data ()
               (n when (realp n)
                  (pr (current-actor) n)
                  (doit))
               ))))
   
  (spawn (lambda ()
           (nlet doit ()
             (with-get-data ()
                 (lst when (listp lst)
                      (pr (current-actor) lst)
                      (doit))
               ))))
  
  (spawn (lambda ()
           (with-on-data ()
             (lst when (listp lst)
                  (pr (format nil "Reader: ~A" lst)))
             )))
  
  (send-bindings '(:z 0.3)))

(send *linda* :introspect)
(send *linda* :reset)

(defun tst-spd (&optional (n #N100_000))
  ;; (setf *linda* (make-ts))
  (let ((time-info (SYSTEM::MAKE-AND-INITIALIZE-TIME-INFO
                              '(LOOP REPEAT N DO (DIDDLE *LINDA*))
                              NIL 'SYSTEM::NOT-GIVEN T)))
    (spawn (lambda (n)
             (nlet iter ((n n))
               (if (zerop n)
                   (send-item :done)
                 (progn
                   (send-item :test-on)
                   (with-get-data ()
                     (:test-off
                      (iter (1- n)))
                     )))))
           n)
    
    (spawn (lambda ()
             (nlet iter ()
               (with-get-data ()
                 (:test-on
                  (send-item :test-off)
                  (iter)))
               )))
    
    (with-get-data ()
      (:done
       (SYSTEM::TIME-BODY-POST time-info))
      )))

(defun tst-spd (&optional (n #N100_000))
  ;; (setf *linda* (make-ts))
  (let ((time-info (SYSTEM::MAKE-AND-INITIALIZE-TIME-INFO
                              '(LOOP REPEAT N DO (DIDDLE *LINDA*))
                              NIL 'SYSTEM::NOT-GIVEN T)))
    (nlet iter ((n n))
      (if (zerop n)
          (send-item :done)
        (progn
          (send-item :test-on)
          (with-get-data ()
            (:test-off
             (iter (1- n)))
            ))))
    
    (nlet iter ()
      (with-get-data ()
        (:test-on
         (send-item :test-off)
         (iter))))
    
    (with-get-data ()
      (:done
       (SYSTEM::TIME-BODY-POST time-info))
      )))

;; --------------------------------------------------------

(defvar *done* nil)

(labels ((monitor-temp ()
           (with-get-data ()
             ((list :temp temp)
              (pr (format nil "Temp: ~A" temp))
              (send-item `(:status
                           ,(if (< (* (round temp 0.05) 0.05) 100)
                                :on :off)))
              (unless *done*
                (monitor-temp)))
             ))
         (monitor-status ()
           (with-get-data ()
             ((list :status on/off)
              (pr (format nil "Status: ~A" on/off))
              (send-item `(:fuel-pump ,(case on/off
                                         (:on  :open)
                                         (:off :closed))))
              (unless *done*
                (monitor-status)))
             ))
         (monitor-fuel-pump ()
           (with-get-data ()
             ((list :fuel-pump open/closed)
              (pr (format nil "Fuel pump now ~A" open/closed))
              (unless *done*
                (monitor-fuel-pump)))
             )))
  (setf *done* nil)
  (monitor-temp)
  (monitor-status)
  (monitor-fuel-pump))

(let ()
  (send-item '(:temp 0))
  (dotimes (x 2)
    (dotimes (y 10)
      (sleep 1)
      (let ((temp (+ 98 x (random 0.07) 1.04)))
        (send-item `(:temp ,temp))
        ))))
(progn
  (setf *done* t)
  (send-item '(:temp 0)))

(bfly:! :linda :introspect)
(send *linda* :introspect)
(with-get-data ()
  (val 
   (pr val)))
(send-item 15)                   
|#


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


(defstruct triple-entry
  key
  (q (priq:make-unsafe-fifo)))

(defun s-add-triple (k1 k2 k3 data dir)
  (labels ((make-new-entry ()
             (let ((ent (make-triple-entry
                         :key (list k1 k2 k3))))
               (priq:addq (triple-entry-q ent) data)
               ent)))
                         
    (let ((map2 (maps:find k1 dir)))
      (if map2
          (let ((map3 (maps:find k2 map2)))
            (if map3
                (let ((ent (maps:find k3 map3)))
                  (if ent
                      (progn
                        (priq:addq (triple-entry-q ent) data)
                        (values dir ent))
                    
                    (let ((ent (make-new-entry)))
                      (setf map3 (maps:add k3 ent map3)
                            map2 (maps:add k2 map3 map2)
                            dir  (maps:add k1 map2 dir))
                      (values dir ent))))

            (let* ((ent  (make-new-entry))
                   (map3 (maps:add k3 ent (maps:empty))))
              (setf map2 (maps:add k2 map3 map2)
                    dir  (maps:add k1 map2 dir))
              (values dir ent))))
        
      (let* ((ent (make-new-entry))
             (map3 (maps:add k3 ent (maps:empty)))
             (map2 (maps:add k2 map3 (maps:empty))))
        (setf dir (maps:add k1 map2 dir))
        (values dir ent))
      ))))

(defun s-sync-triple (k1 k2 k3 data dir)
  (let ((map2 (maps:find k1 dir)))
    (if map2
        (let ((map3 (maps:find k2 map2)))
          (if map3
              (unless (maps:find k3 map3)
                (setf map3 (maps:add k3 data map3)
                      map2 (maps:add k2 map3 map2)
                      dir  (maps:add k1 map2 dir)))
            
            (setf map3 (maps:add k3 data (maps:empty))
                  map2 (maps:add k2 map3 map2)
                  dir  (maps:add k1 map2 dir))))

      (let* ((map3 (maps:add k3 data (maps:empty)))
             (map2 (maps:add k2 map3 (maps:empty))))
        (setf dir (maps:add k1 map2 dir))))
    dir))

(defun s-find-triple (k1 k2 k3 dir default nrot)
  (labels ((gen-ans (k1 k2 k3 data)
             (case nrot
               (0   (list k1 k2 k3 data))
               (1   (list k3 k1 k2 data))
               (2   (list k2 k3 k1 data))))
           
           (find-level3 (k1 k2 map3)
              (if (wild? k3)
                  (let* ((cell (sets:min-elt map3))
                         (ref  (maps:map-cell-val cell))
                         (k3   (maps:map-cell-key cell)))
                    (gen-ans k1 k2 k3 (priq:lastq (triple-entry-q ref))))
                (let ((ref (maps:find k3 map3)))
                  (if ref
                      (gen-ans k1 k2 k3 (priq:lastq (triple-entry-q ref)))
                    default)))))
    
    (let ((map2 (maps:find k1 dir)))
      (if map2
          (if (wild? k2)
              (let* ((cell (sets:min-elt map2))
                     (k2   (maps:map-cell-key cell))
                     (map3 (maps:map-cell-val cell)))
                (if (sets:is-empty map3)
                    default
                  (find-level3 k1 k2 map3)))
            (let ((map3 (maps:find k2 map2)))
              (if map3
                  (find-level3 k1 k2 map3)
                default)))
        default))
    ))

(defun s-remove-triple (k1 k2 k3 dir pop)
  (let* ((map2 (maps:find k1 dir))
         (map3 (maps:find k2 map2))
         (ref  (maps:find k3 map3)))
    (when pop
      (priq:popq (triple-entry-q ref)))
    (when (priq:emptyq-p (triple-entry-q ref))
      (setf map3 (maps:remove k3 map3)
            map2 (maps:add k2 map3 map2)
            dir  (maps:add k1 map2 dir)))
    dir))

(defun wild? (sym)
  (string= "_" sym))

(defun make-triple-store ()
  (make-actor
   (let ((dir1 (maps:empty))
         (dir2 (maps:empty))
         (dir3 (maps:empty)))
     (dlambda
       
       (:add-triple (k1 k2 k3 data)
        (multiple-value-bind (new-dir1 ent)
          (s-add-triple k1 k2 k3 data dir1)
          (setf dir1 new-dir1
                dir2 (s-sync-triple k2 k3 k1 ent dir2)
                dir3 (s-sync-triple k3 k1 k2 ent dir3))))
       
       (:find-triple (k1 k2 k3 &optional default)
        (if (sets:is-empty dir1)
            default
          (if (wild? k1)
              (if (wild? k2)
                  (if (wild? k3)
                      (let* ((cell1 (sets:min-elt dir1))
                             (k1    (maps:map-cell-key cell1))
                             (map2  (maps:map-cell-val cell1)))
                        (if (sets:is-empty map2)
                            default
                          (let* ((cell2 (sets:min-elt map2))
                                 (k2    (maps:map-cell-key cell2))
                                 (map3  (maps:map-cell-val cell2)))
                            (if (sets:is-empty map3)
                                default
                              (let* ((cell3 (sets:min-elt map3))
                                     (k3    (maps:map-cell-key cell3))
                                     (ref   (maps:map-cell-val cell3)))
                                (list k1 k2 k3 (priq:lastq (triple-entry-q ref))))
                              ))))
                    (s-find-triple k3 k1 k2 dir3 default 2))
                (s-find-triple k2 k3 k1 dir2 default 1))
            (s-find-triple k1 k2 k3 dir1 default 0))))
        
       (:get-triple (k1 k2 k3 &optional default)
        (let ((ans (funcall (current-actor) :find-triple k1 k2 k3 default)))
          (cond ((eql ans default)  ans)
                (t (prog1
                       ans
                     (multiple-value-bind (k1 k2 k3) (values-list ans)
                       (setf dir1 (s-remove-triple k1 k2 k3 dir1 t)
                             dir2 (s-remove-triple k2 k3 k1 dir2 nil)
                             dir3 (s-remove-triple k3 k1 k2 dir3 nil)))))
                )))
        
       (:introspect ()
        (inspect (list :dir1 dir1
                       :dir2 dir2
                       :dir3 dir3)))
       ))))

(defvar *triple-store* (make-triple-store))

(defun add-triple (k1 k2 k3 data &optional (ts *triple-store*))
  (send ts :add-triple k1 k2 k3 data))

(defun find-triple (k1 k2 k3 &optional default (ts *triple-store*))
  (ask ts :find-triple k1 k2 k3 default))

(defun get-triple (k1 k2 k3 &optional default (ts *triple-store*))
  (ask ts :get-triple k1 k2 k3 default))

#|
(send *triple-store* :introspect)
|#
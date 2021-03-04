;; parsing.lisp -- convert expression trees into minimal 3-address code
;; -- common subexpression reduction
;; -- automatic register allocation
;;
;; DM/Acudora  06/12
;; augmented DM/RAL 06/15
;; ----------------------------------------
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

(in-package :ecc-crypto-b571)

#|
(defparameter *affine-add-tree*
  '(list
    (setf s    (/ (+ pt1.y pt2.y)
                                  (+ pt2.x pt1.x)))
    (setf ans.x (1+
                      (+ (+ (* s
                               s)
                            s)
                         (+ pt1.x pt2.x))))
         
         (setf ans.y (+ pt1.y
                        (+ ans.x
                           (* s
                              (+ pt1.x ans.x)))))
         ))
|#

(defparameter *affine-add-tree*
  (let* ((s  `(/ (+ pt1.y pt2.y) (+ pt1.x pt2.x))))
    `(list (setf ans.x (+ (^ ,s 2) ,s pt1.x pt2.x gEcc571_a))
           (setf ans.y (+ (* ,s (+ pt1.x ans.x)) ans.x pt1.y))
           )))

(defparameter *affine-double-tree*
  (let* ((s  `(+ pt.x (/ pt.y pt.x))))
    `(list
      (setf ans.x (+ (^ ,s 2) ,s gEcc571_a))
      (setf ans.y (+ (^ pt.x 2) (* (1+ ,s) ans.x)))
      )))

(defparameter *proj-jacobi-double-tree*
  (let* ((ta  `(+ (* pt.y pt.z) (^ pt.x 2))))
    `(list
      (setf ans.z   (* pt.x (^ pt.z 2)))
      (setf ans.x   (+ (^ ,ta 2)
                       (* ans.z (+ (* gEcc571_a ans.z)
                                   ,ta))))
      (setf ans.y   (+ (* ans.z (+ ans.x (^ pt.x 4)))
                       (* ,ta ans.x)))
      )))

(defparameter *proj-jacobi-add-tree*
  (let* ((ta  `(+ (* pt1.x (^ pt2.z 2))
                  (* pt2.x (^ pt1.z 2))))
         (tb  `(+ (* pt1.y (^ pt2.z 3))
                  (* pt2.y (^ pt1.z 3))))
         (tc  `(* pt2.z ,ta)))
    `(list
      (setf ans.z (* pt1.z pt2.z ,ta))
      (setf ans.x (+ (^ ,ta 3)
                     (* gEcc571_a (^ ans.z 2))
                     (* ,tb ans.z)
                     (^ ,tb 2)))
      (setf ans.y (+ (* ,tc
                        (+ (* pt1.z ans.x)
                           (* pt1.y (^ ,tc 2))))
                     (* ,tb
                        (+ ans.x
                           (* pt1.x (^ ,tc 2))))))
      )))

#|
  ;; these projective routines from the handbook are no good for randomized projective coords
  ;; DM/RAL  06/15
(defparameter *projective-double-tree*
  '(list
    (setf ans.z   (* (* pt.x pt.x)
                     (* pt.z pt.z)))
    
    (setf ans.x   (+ (* (* pt.x pt.x)
                        (* pt.x pt.x))
                     (* gEcc571_b (* (* pt.z pt.z)
                                 (* pt.z pt.z)))))

    (setf ans.y   (+ (* ans.x
                        (+ (* gEcc571_a ans.z)
                           (+ (* gEcc571_b
                                 (* (* pt.z pt.z)
                                    (* pt.z pt.z)))
                              (* pt.y pt.y))))
                     (* (* gEcc571_b
                           (* (* pt.z pt.z)
                              (* pt.z pt.z)))
                        ans.z)))
    ))

(defparameter *projective-add-tree*
  '(list
    (setf ans1.x (+ PT1.X (* PT1.Z PT2.X)))

    (setf ans1.y (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y)))

    (setf ans.z  (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X))) (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))))

    (setf ans.x  (+ (+ (* (* (+ PT1.X (* PT1.Z PT2.X)) (+ PT1.X (* PT1.Z PT2.X)))
                          (+ (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                             (* GECC571_A (* PT1.Z PT1.Z))))
                       (* (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y))
                          (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y))))
                    (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                       (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y)))))
    
    (setf ans.y  (+ (* (+ (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                             (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y)))
                          (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                             (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))))
                       (+ (* PT2.X
                             (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                                (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))))
                          (+ (+ (* (* (+ PT1.X (* PT1.Z PT2.X)) (+ PT1.X (* PT1.Z PT2.X)))
                                   (+ (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                                      (* GECC571_A (* PT1.Z PT1.Z))))
                                (* (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y))
                                   (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y))))
                             (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                                (+ PT1.Y (* (* PT1.Z PT1.Z) PT2.Y))))))
                    (* (* (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                             (* PT1.Z (+ PT1.X (* PT1.Z PT2.X))))
                          (* (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))
                             (* PT1.Z (+ PT1.X (* PT1.Z PT2.X)))))
                       (+ PT2.X PT2.Y))))
    ))
|#
#|
;; -------------------------------
;; -------------------------------
;; -------------------------------
;; ans.x part-1

(subst '(* pt1.z pt2.x) 't1
  (subst '(* pt1.z pt1.z) 't2
         '(+ pt1.x t1)))
;; ------------------------------------
;; ans.y part-1
(subst '(* pt1.z pt2.x) 't1
  (subst '(* pt1.z pt1.z) 't2
  (subst '(+ pt1.x t1) 'ans1.x
  (subst '(* pt1.z ans1.x) 't1
  (subst '(* t2 pt2.y) 't3
  '(+ pt1.y t3))))))

;; ---------------------------------
;; ans.z part-2
(subst '(* pt1.z pt2.x) 't1
  (subst '(* pt1.z pt1.z) 't2
  (subst '(+ pt1.x t1) 'ans1.x
  (subst '(* pt1.z ans1.x) 't1
  (subst '(* t2 pt2.y) 't3
  (subst '(+ pt1.y t3) 'ans1.y
         '(* t1 t1)))))))
;; -------------------------------------
;; ans.x part-2
(subst '(* pt1.z pt2.x) 't1
  (subst '(* pt1.z pt1.z) 't2
  (subst '(+ pt1.x t1) 'ans1.x
  (subst '(* pt1.z ans1.x) 't1
  (subst '(* t2 pt2.y) 't3
  (subst '(+ pt1.y t3) 'ans1.y

      (subst '(* t1 t1) 'ans.z
      
      (subst '(* t1 ans1.y) 't3
             (subst '(* gEcc571_a t2) 't9
      (subst '(+ t1 t9) 't1
      
      (subst '(* ans1.x ans1.x) 't2
      (subst '(* t2 t1) 'ans.x
      (subst '(* ans1.y ans1.y) 't2
      
      (subst '(+ ans.x t2) 'ans.x
      '(+ ans.x t3)
      ))))))))))))))
;; ---------------------------------------------
;; ans.y part-2
(subst '(* pt1.z pt2.x) 't1
  (subst '(* pt1.z pt1.z) 't2
  (subst '(+ pt1.x t1) 'ans1.x
  (subst '(* pt1.z ans1.x) 't1
  (subst '(* t2 pt2.y) 't3
  (subst '(+ pt1.y t3) 'ans1.y

      (subst '(* t1 t1) 'ans.z
      
      (subst '(* t1 ans1.y) 't3
             (subst '(* gEcc571_a t2) 't9
      (subst '(+ t1 t9) 't1
      
      (subst '(* ans1.x ans1.x) 't2
      (subst '(* t2 t1) 'ans.x
      (subst '(* ans1.y ans1.y) 't2
      
      (subst '(+ ans.x t2) 'ans.x
      (subst '(+ ans.x t3) 'ans.x
      (subst '(* pt2.x ans.z) 't2
      
      (subst '(+ t2 ans.x) 't2
      (subst '(* ans.z ans.z) 't1
      (subst '(+ t3 ans.z) 't3
      
      (subst '(* t3 t2) 'ans.y
      (subst '(+ pt2.x pt2.y) 't2
      (subst '(* t1 t2) 't3
      
      '(+ ans.y t3)
      ))))))))))))))))))))))
|#

(defparameter *nodes* nil)
(defparameter *assoc* nil)

(defstruct node
  name expr refc tmp visited)

(defmethod print-object ((node node) stream)
  (format stream "N~A:~A:~A"
          (node-name node)
          (opnd node)
          (mapcar #'opnd (node-expr node))))

(defun scan-tree (tree)
  (setf *assoc* nil
        *nodes* (make-array 0
                            :adjustable t
                            :fill-pointer 0))

  (labels ((search (node)
             (find node *nodes*
                   :test #'tree-equal
                   :key  #'node-expr))
           
           (add-node (node)
             (let* ((pos (fill-pointer *nodes*))
                    (n   (make-node
                          :name  pos
                          :tmp   nil
                          :refc  0
                          :expr  node
                          :visited nil)))
               (vector-push-extend n *nodes*)
               n)))

    (um:nlet iter ((tree tree))
      (um:match tree
        
        (('list &rest args)
         (let ((n `(list ,@(mapcar #'iter args))))
           (or (search n)
               (add-node n))))
        
        (('setf name expr)
         (let ((e (iter expr)))
           (push (cons name e) *assoc*)
           e))
        
        ((op a)
         (let* ((aa (iter a))
                (n  `(,op ,aa)))
           (or (search n)
               (add-node n))))

        (('^ a n)
         (cond ((zerop n) (iter 1))
               ((= n 1)   (iter a))
               ((= n 2)   (iter `(* ,a ,a)))
               (t         (iter `(* ,a (^ ,a ,(1- n)))))
               ))
         
        ((op a b)
         (let* ((aa (iter a))
                (bb (iter b))
                (n  `(,op ,aa ,bb)))
           (case op
             ((+ *) (or (search n)
                        (search `(,op ,bb ,aa))
                        (add-node n)))

             (t (or (search n)
                    (add-node n)))
             )))

        ((op a b . rest)
         (iter `(,op ,a (,op ,b ,@rest))))
        
        (_ (or (cdr (assoc tree *assoc*))
               tree))
        )) ))

(defun longest (lst)
  (um:nlet iter ((lst lst)
                 (ans nil))
    (if lst
        (iter (cdr lst) (if (> (length (car lst)) (length ans))
                            (car lst)
                          ans))
      ans)))

(defun collect-unit-chain (node)
  (labels ((get-unit-chains (node)
             (when (node-p node)
               (if (and (null (node-tmp node))
                        (= 1 (node-refc node)))
                   (cons node (collect-unit-chain node))
                 (collect-unit-chain node)))))
    (longest (mapcar #'get-unit-chains (cdr (node-expr node))))))
      

(defparameter *regs*
  (make-array 10
              :adjustable t
              :fill-pointer 0))

(defun alloc-reg ()
  (let ((reg
         (let ((reg (position t *regs*)))
           (if reg
               (progn
                 (setf (aref *regs* reg) nil)
                 reg)
             ;; else
             (let ((reg (fill-pointer *regs*)))
               (vector-push-extend nil *regs*)
               reg)))))
    reg))

(defun free-reg (reg)
  (setf (aref *regs* reg) t))
      
(defun assign-regs (top)
  (setf (fill-pointer *regs*) 0)

  (um:nlet iter ((node top))
    (when (node-p node)
      (incf (node-refc node))
      (unless (node-visited node)
        (setf (node-visited node) t)
        (mapcar #'iter (cdr (node-expr node))))))
  
  (loop for node across *nodes* do
        (setf (node-visited node) nil))

  ;; preallocate the registers declared results
  ;; propagate to the longest lower chain of unit connections
  (loop for (name . node) in (reverse *assoc*) do
        (setf (node-tmp node) name)
        (dolist (n (collect-unit-chain node))
          (setf (node-tmp n) name)))

  (labels ((purge (node)
             (when (node-p node)
               (decf (node-refc node))
               (when (and (zerop (node-refc node))
                          (numberp (node-tmp node)))
                 (free-reg (node-tmp node))) )))
           
    (um:nlet iter ((node top))
      (when (node-p node)
        ;; (print node)
        (assert (plusp (node-refc node)))
        (unless (node-visited node)
          (setf (node-visited node) t)
          (mapcar #'iter  (node-expr node))
          (mapcar #'purge (node-expr node))
          (unless (node-tmp node)
            (setf (node-tmp node) (alloc-reg))) )))
    ))

(defun opnd (node)
  (cond ((node-p node)
         (let ((tmp (node-tmp node)))
           (cond ((numberp tmp) (format nil "tmp~A" (1+ tmp)))
                 (t (string-downcase tmp)))))
        (t (string-downcase node))))

(defun linearize-tree (tree)
  (show-graph tree)
  (let* ((top (scan-tree tree)))
    (assign-regs top)
    (loop for item across *nodes* do
          (um:match (node-expr item)

            (('list &rest args)
             )
            
            ((op a b)
             (format t "~&gf571_~A(~A, ~A, ~A);"
                     (case op
                       (+ "add")
                       (* "mul")
                       (/ "div"))
                     (opnd a)
                     (opnd b)
                     (opnd item)))

            ((op a)
             (case op
               (1+ (format t "~&gf571_oneplus(~A, ~A);"
                           (opnd a)
                           (opnd item)))
               (sqr (format t "~&gf571_mul(~A, ~A, ~A);"
                            (opnd a)
                            (opnd a)
                            (opnd item)))))

            (_ (error "Unknwn node type"))
            ))
    ))

(defun show-graph (tree)
  (let* ((top (scan-tree tree)))
    (assign-regs top)
    
    (labels ((node-children (node)
               (um:nlet iter ((lst (node-expr node))
                              (ch  nil))
                 (if lst
                     (let ((item (car lst)))
                       (if (node-p item)
                           (iter (cdr lst) (cons (aref *nodes* (node-name item)) ch))
                         (iter (cdr lst) ch)))
                   (nreverse ch)))))
      (capi:contain
       (make-instance 'capi:graph-pane
                      :roots (list top)
                      :children-function #'node-children
                      :layout-function :top-down
                      )
       :best-width  600
       :best-height 450) )))

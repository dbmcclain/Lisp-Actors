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

(in-package :tolstoy-aont)


(defun view-simple-tree (tree)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list tree)

                  :children-function (lambda (tree)
                                       (cond ((null tree) nil)
                                             (t (destructuring-bind (l r) tree
                                                  (cond ((and (consp l) (consp r))
                                                         (list l r))
                                                        ((consp l) (list l))
                                                        ((consp r) (list r))
                                                        (t nil)) ))))
                  
                  :print-function (lambda (node)
                                    (cond ((null node) "")
                                          (t (destructuring-bind (l r) node
                                               (cond ((and (consp l) (consp r)) "")
                                                     ((consp l)
                                                      (format nil "=/~A" r))
                                                     ((consp r)
                                                      (format nil "~A/=" l))
                                                     (t
                                                      (format nil "~A/~A" l r)))
                                               ))))
                  )))


(defun assemble-huffman-tree (alst &optional viz)
  ;; Sort by use count
  #F
  (let ((salst (sort alst '< :key 'car)))
    (destructuring-bind ((ct (str . strs)) . tl) salst
      
      (cond
       
       (strs
        (let* ((ct2      (+ ct ct))
               (dst      (assoc ct2 tl))
               (dst-strs (when dst
                           (setf tl (remove dst tl))
                           (cadr dst))))
          
          (if (oddp (length strs))
              (push str strs)
            ;; else
            (push `(,ct (,str)) tl))
          
          (push `(,ct2 (,@dst-strs ,@(um:group strs 2))) tl)
          (assemble-huffman-tree tl viz)
          ))
       
       
       (tl
        (destructuring-bind ((ct2 (str2 . strs)) . tl) tl
          
          (let* ((ct-sum (+ ct ct2))
                 (dst      (assoc ct-sum tl))
                 (dst-strs (when dst
                             (setf tl (remove dst tl))
                             (cadr dst))))
            
            (when strs
              (push `(,ct2 ,strs) tl))
            
            (push `(,ct-sum (,@dst-strs (,str2 ,str))) tl)
            (assemble-huffman-tree tl viz)
            )))
       
       (t
        (when viz
          (view-simple-tree str))
        str)
       
       ))))

;; ------------------------------------------------------------------

(defun kill-text (str start-delim end-delim &optional nested)
  (let ((start (position start-delim str)))
    (if start
        (let ((hd (subseq str 0 start)))
          (when (and nested
                     (every 'digit-char-p hd))
            (setf hd ""))
          (let ((end (position end-delim str :start (1+ start))))
            (if end
                (concatenate 'string
                             hd
                             (kill-text (subseq str (1+ end)) start-delim end-delim t))
              ;; else
              hd)))
      ;; else
      str)))

(defun kill-html (str)
  (kill-text
   (kill-text str #\< #\>)
   #\& #\;))

(defun get-wp-lines ()
  (let* ((xs (hcl:file-string "Crypto/war-and-peace-edit.txt")))
    (remove-if (lambda (line)
                 (or (string= "" line)
                     (every 'lw:whitespace-char-p line)
                     (search "CHAPTER" line
                             :test 'string=)))
               (mapcar 'kill-html 
                       (um:split-string xs :delims '(#\linefeed))))
    ))

;; ------------------------------------------------------------------

(defun get-word-usage-tree ()
  ;; Generate a R-B Tree, keyed by distinct words found in the
  ;; document. The tree nodes carry a count of usage and a subtree of
  ;; what words it followed.
  ;;
  ;; The subtree similarly carries a use count and another subtree of
  ;; what words that word-pair followed.
  ;;
  ;; That 2nd subtree also carries a subtree with use counts for the
  ;; word-triple.
  ;;
  ;; Gives us a 3-level Markov-chain system.
  ;;
  (let* ((tree  (maps:empty))
         (prev1 nil)
         (prev2 nil))
        
    (dolist (line (get-wp-lines))
      (let* ((wds (um:tokens line
                             :test-not 'lw:whitespace-char-p)))
        (dolist (wd wds)
          (unless (string= wd "")
            (let* ((p2    (shiftf prev2 prev1 wd))
                   (p1    prev2)
                   (found (maps:find tree wd))
                   (node  (or found
                              (list 0
                                    (maps:empty)) )))
              (incf (car node))
              (unless found
                (setf tree (maps:add tree wd node)))
              
              (when p1
                (let* ((ent   (maps:find tree p1))
                       (found (maps:find (second ent) wd))
                       (node  (or found
                                  (list 0
                                        (maps:empty)))))
                  (incf (car node))
                  (unless found
                    (setf (second ent) (maps:add (second ent) wd node)))
                  ))
              
              (when p2
                (let* ((ent1  (maps:find tree p2))
                       (ent2  (maps:find (second ent1) p1))
                       (found (maps:find (second ent2) wd))
                       (node  (or found
                                  (list 0))))
                  (incf (car node))
                  (unless found
                    (setf (second ent2) (maps:add (second ent2) wd node)))
                  ))
              )))
        ))
    tree))


(defun melt-tree (rb-tree)
  ;; convert rb-tree mapping tree to pure cons form for speed of
  ;; access
  (unless (maps:is-empty rb-tree)
    (maps:with-node-bindings (l vn r _) rb-tree
      (let ((k  (maps::map-cell-key vn))
            (v  (maps::map-cell-val vn)))
        `(,(melt-tree l)
          (,k ,v)
          ,(melt-tree r))))
    ))

(defun make-code-table (tree)
  (let ((tbl (maps:empty)))
    (um:nlet iter ((top   tree)
                   (nbits 0)
                   (code  0))
      (let* ((new-nbits (1+ nbits))
             (new-code  (ash code 1)))
        
        (if (stringp (car top))
            (setf tbl (maps:add tbl (car top) (list new-nbits new-code)))
          ;; else
          (iter (car top) new-nbits new-code))
        
        (if (stringp (cadr top))
            (setf tbl (maps:add tbl (cadr top) (list new-nbits (1+ new-code))))
          ;; else
          (iter (cadr top) new-nbits (1+ new-code)))) )
    (melt-tree tbl)))

(defun make-hufftree (tree)
  ;; ctree is a RB Tree keyed by use count, which contains the list of
  ;; words with that usage.
  ;;
  ;; clst is an A-List of use count and word list derived from ctree.
  (let* ((ctree   (maps:fold tree
                             (lambda (k v accum)
                               (let* ((found  (maps:find accum (car v)))
                                      (node   (or found
                                                  (list nil))))
                                 (setf (car node) (cons k (car node)))
                                 (if found
                                     accum
                                   (maps:add accum (car v) node))
                                 ))
                             (maps:empty)))
         (clst    (maps:fold ctree
                             (lambda (ct v accum)
                               (cons `(,ct ,@v) accum))
                             nil))
         (htree   (assemble-huffman-tree clst))
         (hcodes  (make-code-table htree)))
    
    (list htree hcodes)))

(defun make-3rd-order-hufftree (tree)
  (melt-tree
   (maps:fold tree
              (lambda (k v accum)
                (if (< (maps:cardinal (second v)) 8)
                    accum
                  (maps:add accum k (list (make-hufftree (second v))))))
              (maps:empty))))

(defun make-2nd-order-hufftree (tree)
  (melt-tree
   (maps:fold tree
              (lambda (k v accum)
                (if (< (maps:cardinal (second v)) 64)
                    accum
                  (let* ((ent (list (make-hufftree (second v))
                                    (make-3rd-order-hufftree (second v)))))
                    (maps:add accum k ent))))
              (maps:empty))))

(defun make-huffman-trees ()
  (let ((tree (get-word-usage-tree)))
    (list (make-hufftree tree)
          (make-2nd-order-hufftree tree))
    ))

(defun dump-wp-map-data (trees)
  (ensure-directories-exist *wp-data-file*)
  (hcl:with-output-to-fasl-file (f *wp-data-file*
                                   :overwrite t
                                   :delete-on-error t
                                   :dump-standard-objects nil)
    (hcl:dump-form trees f)))





#|
(dump-wp-map-data (make-huffman-trees))
|#

;; ----------------------------------------------------------------------------

#|
(defparameter *my-xptrees*  *xptrees*)
#|
(setf *my-xptrees* (make-huffman-trees))
(setf *xptrees* (make-huffman-trees))
|#
(let* ((htree  (caar *my-xptrees*))
       (ftbl   (cadr *my-xptrees*))
       (prev1  nil)
       (prev2  nil))
  
  (labels ((next-tbl (wd)
             (princ wd)
             (princ #\space)
             (shiftf prev2 prev1 wd)
             (caar
              (or (let ((ent (tree-find prev2 ftbl)))
                    (and ent
                         (tree-find prev1 (second ent))))
                  (tree-find prev1 ftbl)
                  *my-xptrees*))
             ))
    (terpri)
    (um:nlet iter ((nwds 100)
                   (top  htree))
      (when (plusp nwds)
        (case (lw:mt-random 2)
          (0 (if (consp (car top))
                 (go-iter nwds (car top))
               (go-iter (1- nwds) (next-tbl (car top)))))
          
          (1 (if (consp (cadr top))
                 (go-iter nwds (cadr top))
               (go-iter (1- nwds) (next-tbl (cadr top)))))
          ) ))))

|#
  
#|
;; Collect useful statistics on Tolstoy's "War and Peace"
(let* ((xlines (get-wp-lines))
       (terms  (reduce (um:curry 'concatenate 'string)
                       (um:flatten
                        (mapcar
                         (lambda (line)
                           (remove-if-not (lambda (c)
                                            (member c '(#\. #\? #\! #\, #\' #\-)))
                                          line))
                         xlines))))
       (nparas (length xlines))
       (xparas (mapcar (lambda (line)
                         (um:split-string (string-trim '(#\" #\-) line)
                                          :delims '(#\. #\? #\! #\: #\;)))
                       xlines))
       (paras  (mapcar 'length xparas))
       (xsents (mapcar (lambda (wds)
                         (remove "" wds :test 'equal))
                       (mapcar 'um:tokens
                               (mapcar (lambda (sent)
                                         (substitute-if-not #\space (lambda (ch)
                                                                      (or (alpha-char-p ch)
                                                                          (digit-char-p ch)
                                                                          (char= #\' ch)
                                                                          (char= #\- ch)))
                                                            sent))
                                       (um:flatten xparas)))))
       (sents  (mapcar 'length xsents))
       (xwds   (um:flatten xsents)))
  
  ;; #\? and #\! are about 1 in 10, compared to #\.
  (plt:histogram 'hterms (map 'vector 'char-code terms)
                 :clear t
                 :yrange '(0 4)
                 :xrange '(30 70)
                 :ylog  nil)
  (plt:draw-text 'hterms "Exclaim"
                 `(,(char-code #\!) 0.5))
  (plt:draw-text 'hterms "Comma"
                 `(,(char-code #\,) 3.1))
  (plt:draw-text 'hterms "Hyphen"
                 `(,(char-code #\-) 0.5))
  (plt:draw-text 'hterms "Period"
                 `(,(char-code #\.) 2.4))
  (plt:draw-text 'hterms "Question"
                 `(,(char-code #\?) 0.4))
  (plt:draw-text 'hterms "Apostrophe"
                 `(,(char-code #\') 0.7))
  
    (plt:histogram 'hparas paras
                 :title "Sentences per Paragraph"
                 :xtitle "N Sentences"
                 :ytitle "PSD"
                 :cum   t
                 :ylog nil
                 :clear t)
  (plt:paramplot 'hparas '(0.01 0.99)
                 (lambda (y)
                   (floor (max 2 (* -4 (log (- 1 y))))))
                 'identity
                 :color :red)
  ;; Sentences per paragraph:
  ;;   n = floor(max(2, -4*ln(1-C))) ; C uniform random (0,1)
  
  (plt:histogram 'hsents sents
                 :title "Words per Sentence"
                 :xtitle "N Words"
                 :ytitle "PSD"
                 :cum t
                 :ylog nil
                 :clear t)
  (plt:paramplot 'hsents '(0.01 0.99)
                 (lambda (y)
                   (ceiling (max 1 (* -13 (log (- 1 y))))))
                 'identity
                 :color :red)
  ;; Words per sentence
  ;;   n = ceiling(max(1, -13*ln(1-C))) ; C uniform random (0,1)
  
  ;; (inspect terms)
  (list :nparas nparas
        :pmn    (float (vm:mean paras))
        :psd    (vm:stdev paras)
        :smn    (float (vm:mean sents))
        :ssd    (vm:stdev sents)))

|#

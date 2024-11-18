;; plotter.lsp -- Plotting support for Lisp
;; DM 07/95

;; -------------------------------------------------------
(defmacro if-let ((var val) t-clause &optional f-clause)
  `(let ((,var ,val))
     (if ,var
         ,t-clause
       ,f-clause)))

(defun rcurry (f &rest suf-args)
  #'(lambda (&rest pref-args)
      (apply f (append pref-args suf-args))))

(defun foldr (fn seq init)
  (reduce fn seq :from-end t :initial-value init))

(defun compose (&rest fns)
  (cond ((null fns)   #'identity)
        ((single fns) (car fns))
        (t (let ((fn1 (last1 fns))
                 (fns (butlast fns)))
             #'(lambda (&rest args)
                 (foldr #'funcall fns (apply fn1 args)))
             ))
        ))

(defun single (arg)
  (and (consp arg)
       (null (cdr (the cons arg)))))

(defun last1 (lst)
  (car (the cons (last lst))))

;; -------------------------------------------------------
;;
(defclass <collector> ()
  ((hd :accessor collector-hd)
   (tl :accessor collector-tl)))

(defun collector-discard-contents (c)
  (let ((v (list nil)))
    (setf (collector-hd c) v
          (collector-tl c) v)))

(defmethod initialize-instance ((c <collector>) &key &allow-other-keys)
  (collector-discard-contents c))

(defun collector-contents (c &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  (let ((lst (cdr (the cons (collector-hd c)))))
    (if discard
        (progn
          (collector-discard-contents c)
          lst)
      (copy-seq lst))
    ))
    
(defun collector-ncontents (c)
  (length (cdr (the cons (collector-hd c)))))

(defun collector-empty-p (c)
  (zerop (collector-ncontents c)))

(defun collector-append-item (c item)
  (setf (collector-tl c)
        (cdr (the cons (rplacd (the cons (collector-tl c)) (list item))))
        ))

(defun collector-push-item (c item)
  (setf (collector-hd c)
        (cons nil (the cons (rplaca (the cons (collector-hd c)) item)))
        ))

(defun collector-pop (c)
  (let* ((lst (collector-contents c))
         (v   (car lst)))
    (unless (endp lst)
      (setf (collector-hd c) lst))
    v))

(defun make-collector ()
  (make-instance '<collector>))

;; ------------------------------------------
(defun tokens (str &key (test 'constituent) test-not (start 0) end key)
  (let ((test (if test-not
                  (complement test-not)
                test)))
    (loop for p1 = (position-if test str :start start :end end :key key)
          while p1
          do (setf start (position-if-not test str :start p1 :end end :key key))
          collect (subseq str p1 (or start end))
          while start
          )))

(defun tokens-if (test str &rest args)
  (apply 'tokens str :test test args))

(defun tokens-if-not (test-not str &rest args)
  (apply 'tokens str :test-not test-not args))

(defun split-string (str &key delims (start 0) end key)
  (if delims
      (tokens-if-not (rcurry #'find #|c|# delims) str
                     :start start :end end :key key)
    (tokens str :start start :end end :key key)
    ))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) 
      (princ a s))))


;; ------------------------------------------
(defclass <plotter-window> (capi:output-pane)
  ((xlog   :accessor plotter-xlog)
   (xmin   :accessor plotter-xmin)
   (xmax   :accessor plotter-xmax)

   (ylog   :accessor plotter-ylog)
   (ymin   :accessor plotter-ymin)
   (ymax   :accessor plotter-ymax)
   
   (box    :accessor plotter-box)
   (xform  :accessor plotter-xform)
   (dlist  :accessor plotter-display-list :initform (make-collector))

   ;; stuff for paramplot
   (trange   :accessor plotter-trange)
   (xsf      :accessor plotter-xsf)
   (ysf      :accessor plotter-ysf)
   (tprepfns :accessor plotter-tprepfns)
   (xprepfns :accessor plotter-xprepfns)
   (yprepfns :accessor plotter-yprepfns)
   ))

;; ------------------------------------------
(defmacro with-current-plotting-window ((name &optional pane) &body body)
  `(let ((,name ,(if pane
                     `(or ,pane
                          (current-plotting-window))
                   `(current-plotting-window))
                ))
     (capi:apply-in-pane-process ,name
                                 (lambda ()
                                   ,@body))))


(defun log10 (x)
  (log x 10.0d0))

(defun pow10 (x)
  (expt 10.0d0 x))

(defun vmin (vect)
  (reduce #'min vect))

(defun vmax (vect)
  (reduce #'max vect))

;; ------------------------------------------
(defun inset-box-sides (box dxleft dytop 
                            &optional (dxright dxleft)
                                      (dybottom dytop))
  (list (+ (gp:rectangle-left box) dxleft)
        (+ (gp:rectangle-top box) dytop)
        (- (gp:rectangle-right box) dxright)
        (- (gp:rectangle-bottom box) dybottom)))

(defmacro box-left (box)
  `(gp:rectangle-left ,box))

(defmacro box-top (box)
  `(gp:rectangle-top ,box))

(defmacro box-right (box)
  `(gp:rectangle-right ,box))

(defmacro box-bottom (box)
  `(gp:rectangle-bottom ,box))

(defmacro box-width (box)
  `(gp:rectangle-width ,box))

(defmacro box-height (box)
  `(gp:rectangle-height ,box))

(defmacro box-top-left (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-left ,gbx) (box-top ,gbx)))))

(defmacro box-top-right (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-right ,gbx) (box-top ,gbx)))))

(defmacro box-bottom-left (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-left ,gbx) (box-bottom ,gbx)))))

(defmacro box-bottom-right (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-right ,gbx) (box-bottom ,gbx)))))

(defun qrange (rng &optional (default 0.1))
  (if (zerop rng)
      default
    rng))

(defun qdiv (a b &optional (default 0.1))
  (/ a (qrange b default)))

;; ------------------------------------------
(defmethod pw-init-xv-yv ((cpw <plotter-window>) xv yv
                          xrange yrange inbox
                          &key xlog ylog aspect)
  (let ((_box (inset-box-sides inbox 30 20 10 30))
        (_xmin (if xrange
                   (first xrange)
                 (vmin xv)))
        (_xmax (if xrange
                   (second xrange)
                 (vmax xv)))
        (_ymin (if yrange
                   (first yrange)
                 (vmin yv)))
        (_ymax (if yrange
                   (second yrange)
                 (vmax yv))))

    (if xlog
        (setf _xmin (log10 _xmin)
              _xmax (log10 _xmax)))
    (if ylog
        (setf _ymin (log10 _ymin)
              _ymax (log10 _ymax)))
    
    (unless yrange
      (let ((dy (/ (qrange (- _ymax _ymin)) 18)))
        (decf _ymin dy)
        (incf _ymax dy)))

    (unless xrange
      (let ((dx (/ (qrange (- _xmax _xmin)) 18)))
        (decf _xmin dx)
        (incf _xmax dx)))
    
    (setf (plotter-box  cpw) _box
          (plotter-xmin cpw) _xmin
          (plotter-xmax cpw) _xmax
          (plotter-ymin cpw) _ymin
          (plotter-ymax cpw) _ymax
          (plotter-xlog cpw) xlog
          (plotter-ylog cpw) ylog)
    
    (let ((xscale (qdiv (- (box-right _box) (box-left _box))
                        (- _xmax _xmin)))
          (yscale (qdiv (- (box-bottom _box) (box-top _box))
                        (- _ymax _ymin))))
      
      (if (and (numberp aspect)
               (plusp aspect))
          
          (let* ((x-squeeze (<= aspect 1))
                 (scale     (if x-squeeze
                                (min xscale yscale)
                              (max xscale yscale))))
            (setf xscale (if x-squeeze
                             (* aspect scale)
                           scale)
                  yscale (if x-squeeze
                             scale
                           (/ scale aspect)))
            ))
      
      (let ((xform (gp:make-transform)))
        (gp:apply-translation xform (- _xmin) (- _ymin))
        (gp:apply-scale xform xscale (- yscale))
        (gp:apply-translation xform (box-left _box) (box-bottom _box))
        (setf (plotter-xform cpw) xform))
      )))
  
;; ------------------------------------------
(defun draw-path (pane &rest positions)
  (gp:draw-polygon pane
                   (mapcan #'append positions)))

(defun bounds-overlap-p (bounds1 bounds2)
  (or (<= (first bounds1) (first bounds2)  (second bounds1))
      (<= (first bounds1) (second bounds2) (second bounds1))
      (<= (first bounds2) (first bounds1)  (second bounds2))
      ))

(defun expand-bounds (bounds dx)
  (list (- (first bounds) dx) (+ (second bounds) dx)))

;; ------------------------------------------
(defun draw-string-x-y (pane string x y
                             &key 
                             (x-alignment :left) 
                             (y-alignment :baseline)
                             prev-bounds
                             font
                             (transparent t)
                             (color :black)
                             &allow-other-keys)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent pane string font)
    (let* ((dx (ecase x-alignment
                 (:left     0)
                 (:right    (- left right))
                 (:center   (floor (- left right) 2))
                 ))
           (dy (ecase y-alignment
                 (:top      (- top))
                 (:bottom   0)
                 (:center   (- (floor (- top bottom) 2) top))
                 (:baseline 0)))
           (new-bounds (list (+ x left dx) (+ x right dx))))
      
      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds 2) new-bounds))
          prev-bounds
        
        (gp:with-graphics-state
            (pane :foreground color)
          (gp:draw-string pane string (+ x dx) (+ y dy)
                          :font font :block (not transparent))
          new-bounds)
        ))))

;; ------------------------------------------
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :baseline)
                                  (y-alignment :left)
                                  font
                                  prev-bounds
                                  (color :black)
                                  (transparent t))
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)
    (let* ((wd (- rt lf -1))
           (ht (- bt tp -1))
           (dx (ecase x-alignment
                 (:top      0)
                 (:bottom   (- ht))
                 (:baseline tp)
                 (:center   (floor tp 2))
                 ))
           (dy (ecase y-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dy) (+ y rt dy))))
      
      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds 2) new-bounds))

          prev-bounds
        
        (gp:with-pixmap-graphics-port (ph pane wd ht
                                          :clear t)
          (gp:with-graphics-state
              (ph :foreground color)
            (gp:draw-string ph string
                            0 (- tp)
                            :font font
                            :block (not transparent)))
          
          (let* ((h-image (gp:make-image-from-port ph))
                 (v-image (gp:make-image pane ht wd))
                 (ha (gp:make-image-access ph h-image))
                 (va (gp:make-image-access pane v-image)))
            (loop for ix from 0 below wd do
                  (loop for iy from 0 below ht do
                        (setf (gp:image-access-pixel va iy (- wd ix 1))
                              (gp:image-access-pixel ha ix iy))
                        ))
            (gp:free-image-access ha)
            (gp:free-image-access va)
            (gp:free-image ph h-image)
            
            (gp:draw-image pane v-image (+ x dx) (+ y dy))
            (gp:free-image pane v-image)
            
            new-bounds
            )))
      )))

;; ------------------------------------------
(defmethod pw-plot-xv-yv ((cpw <plotter-window>) xvector yvector 
                          &key
                          (color #.(color:make-rgb 0.0 0.5 0.0))
                          thick
                          (linewidth (or thick 1))
                          linedashing
                          symbol
                          plot-joined
                          &allow-other-keys)
  (let ((xlog (plotter-xlog cpw))
        (ylog (plotter-ylog cpw)))
    (labels ((qxlog (x)
               (if xlog (log10 x) x))
             (qylog (y)
               (if ylog (log10 y) y)))
    
      (gp:with-graphics-state (cpw
                               :thickness  linewidth
                               :dashed     (not (null linedashing))
                               :dash       linedashing
                               :foreground color
                               :mask       (plotter-box cpw))
      
        (if (eql symbol :steps)
            (gp:with-graphics-transform (cpw (plotter-xform cpw))
              (loop for ix from 1 below (length xvector)
                    for jx from 1 below (length yvector)
                    for (x xprev) = (list (qxlog (aref xvector 1)) (qxlog (aref xvector 0)))
                    then (list (qxlog (aref xvector ix)) x)
                    for (y yprev) = (list (qylog (aref yvector 1)) (qylog (aref yvector 0)))
                    then (list (qylog (aref yvector jx)) y)
                    do
                    (let ((xmid (* 0.5 (+ x xprev))))
                      (gp:draw-line cpw
                                    xprev yprev
                                    xmid yprev)
                      (gp:draw-line cpw
                                    xmid yprev
                                    xmid y)
                      (gp:draw-line cpw
                                    xmid y
                                    x    y))
                    ))
        
          (progn
            (if (or (not symbol)
                    plot-joined)
                (gp:with-graphics-transform (cpw (plotter-xform cpw))
                  (loop for ix from 1 below (length xvector)
                        for jx from 1 below (length yvector)
                        for (x xprev) = (list (qxlog (aref xvector 1)) (qxlog (aref xvector 0)))
                        then (list (qxlog (aref xvector ix)) x)
                        for (y yprev) = (list (qylog (aref yvector 1)) (qylog (aref yvector 0)))
                        then (list (qylog (aref yvector jx)) y)
                        do
                        (gp:draw-line cpw xprev yprev x y))
                  ))
          
            (if symbol
                (let ((plotfn (ecase symbol
                                (:cross
                                 (lambda (x y)
                                   (gp:draw-line cpw (- x 3) y (+ x 3) y)
                                   (gp:draw-line cpw x (- y 3) x (+ y 3))
                                   ))
                                
                                (:circle
                                 (lambda (x y)
                                   (labels ((draw-circle (&optional filled)
                                              (gp:draw-circle cpw x (1- y) 3
                                                              :filled filled)))
                                     (gp:with-graphics-state
                                         (cpw
                                          :foreground #.(color:make-gray 1.0 0.25))
                                       (draw-circle t))
                                     (draw-circle))
                                   ))
                              
                                (:filled-circle
                                 (lambda (x y)
                                   (labels ((draw-circle (&optional filled)
                                              (gp:draw-circle cpw
                                                              (if filled (1+ x) x)
                                                              (1- y) 3
                                                              :filled filled)))
                                     (draw-circle t)
                                     (draw-circle))
                                   ))
                              
                                ((:box :square)
                                 (lambda (x y)
                                   (labels ((draw-rectangle (&optional filled)
                                              (gp:draw-rectangle cpw (- x 3) (- y 3) 6 6
                                                                 :filled filled)))
                                     (gp:with-graphics-state
                                         (cpw
                                          :foreground #.(color:make-gray 1.0 0.25))
                                       (draw-rectangle t))
                                     (draw-rectangle))
                                   ))
                              
                                ((:filled-box :filled-square)
                                 (lambda (x y)
                                   (labels ((draw-rectangle (&optional filled)
                                              (gp:draw-rectangle cpw (- x 3) (- y 3) 6 6
                                                                 :filled filled)))
                                     (draw-rectangle t)
                                     (draw-rectangle))
                                   ))
                              
                                (:triangle
                                 (lambda (x y)
                                   (labels ((draw-triangle (&optional filled)
                                              (gp:draw-polygon cpw
                                                               (list (- x 3) (+ y 3)
                                                                     x (- y 4)
                                                                     (+ x 3) (+ y 3))
                                                               :closed t
                                                               :filled filled)))
                                     (gp:with-graphics-state
                                         (cpw
                                          :foreground #.(color:make-gray 1.0 0.25))
                                       (draw-triangle t))
                                     (draw-triangle))
                                   ))
                              
                                (:filled-triangle
                                 (lambda (x y)
                                   (labels ((draw-triangle (&optional filled)
                                              (gp:draw-polygon cpw
                                                               (list (- x 3) (+ y 3)
                                                                     x (- y 4)
                                                                     (+ x 3) (+ y 3))
                                                               :closed t
                                                               :filled filled)))
                                     (draw-triangle t)
                                     (draw-triangle))
                                   ))
                              
                                (:dot
                                 (lambda (x y)
                                   (gp:draw-circle cpw x (1- y) 0.5)
                                   ))
                              
                                )))
                
                  (loop for xval across xvector
                        for yval across yvector
                        for xform = (plotter-xform cpw)
                        do
                        (multiple-value-bind (x y)
                            (gp:transform-point xform (qxlog xval) (qylog yval))
                          (funcall plotfn x y)
                          ))
                  )))
          )))
    ))

;; ------------------------------------------
(defun calc-start-delta (vmin vmax)
  (destructuring-bind (sf c)
      (loop for sf = (/ (pow10
                         (ceiling (log10 (max (abs vmin)
                                              (abs vmax))
                                         ))
                         ))
            then (* 10.0d0 sf)
            do
            (let* ((a    (* sf vmin))
                   (b    (* sf vmax))
                   (diff (abs (- b a)))
                   (c    (* 10.0d0 (ceiling (min a b) 10.0d0))))
              (if (and (> diff 1.0d0)
                       (<= (abs (- c a)) diff))
                  (return (list sf c)))
              ))
    (loop for sf2 = 1.0d0 then (* 0.1d0 sf2)
          do
          (let* ((a   (* sf sf2 vmin))
                 (b   (* sf sf2 vmax))
                 (c   (* sf2 c))
                 (rng (abs (- b a))))
            
            (if (<= rng 10.0d0)
                (let* ((dv  (cond ((> rng 5.0d0) 1.0d0)
                                  ((< rng 2.0d0) 0.2d0)
                                  (t               0.5d0)))
                       (nl  (floor (abs (- c a)) dv))
                       (nu  (floor (abs (- b c)) dv))
                       (v0  (if (not (plusp (* a b)))
                                0.0d0
                              (/ c sf sf2)))
                       (dv  (/ dv sf sf2)))
                  (return (list v0 dv nl nu)))
              ))
          )))

;; ------------------------------------------
(let ((ext-logo (ignore-errors
                  (gp:read-external-image
                   "/usr/local/lib/Logo75-Alpha25x.pdf"))))
  (defun stamp-logo (pane)
    (when ext-logo
      (let ((logo  (gp:convert-external-image pane ext-logo)))
        (gp:draw-image pane logo 40 25)
        (gp:free-image pane logo))
      )))

(defun watermark (pane)
  (let ((wmark "R A L")
        (cright1 "Copyright (c) 2006-2007 by Refined Audiometrics Laboratory, LLC")
        (cright2 "All rights reserved.")
        (font1   (gp:find-best-font pane
                                    (gp:make-font-description
                                     :name "Zapfino"
                                     :size 70)))
        (font2   (gp:find-best-font pane
                                    (gp:make-font-description
                                     :name "Times"
                                     :size 10)))
        (color1 #.(color:make-rgb 0.9 0.9 0.95))
        (color2 #.(color:make-gray 0.7)))
    
    (draw-string-x-y pane wmark
                     (floor (gp:port-width pane)  2)
                     (+ 20 (floor (gp:port-height pane) 2))
                     :x-alignment :center
                     :y-alignment :center
                     :color color1
                     :font  font1)
    
    (draw-string-x-y pane cright1
                     40 (- (gp:port-height pane) 60)
                     :x-alignment :left
                     :y-alignment :top
                     :font  font2
                     :color color2)
    (draw-string-x-y pane cright2
                     40 (- (gp:port-height pane) 48)
                     :x-alignment :left
                     :y-alignment :top
                     :font  font2
                     :color color2)

    (stamp-logo pane)
    ))

;; ------------------------------------------
(let ((log-subdivs
       (mapcar #'log10
               '(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
                     2 3 4 5 6 7 8 9))))
  (defmethod pw-axes ((cpw <plotter-window>)
                      &key
                      (fullgrid t)
                      (xtitle "X")
                      (ytitle "Y")
                      (title  "Plot")
                      (watermarkfn #'watermark)
                      &allow-other-keys)
    (let ((_box  (plotter-box cpw))
          (font  (gp:find-best-font
                  cpw
                  (gp:make-font-description
                   :name "Times"
                   :size 12)))
          (xlog  (plotter-xlog cpw))
          (ylog  (plotter-ylog cpw)))
    
      (labels
          ((qxlog (x)
             (if xlog (log10 x) x))
           (qylog (y)
             (if ylog (log10 y) y))
           (iqxlog (x)
             (if xlog (pow10 x) x))
           (iqylog (y)
             (if ylog (pow10 y) y))
           (trim-mantissa (v)
             (string-right-trim
              "."
              (string-right-trim
               "0" v)))
           (plabel (val)
             (if (or (zerop val)
                     (and (<= 0.01 (abs val))
                          (< (abs val) 10000)))
                 (trim-mantissa (format nil "~,2F" (float val 1.0)))
               (destructuring-bind (mant expon)
                   (split-string (format nil "~,2E" (float val 1.0))
                                    :delims "E")
                 (mkstr (trim-mantissa mant) "E" expon))
               )))
      
        (graphics-ports:clear-graphics-port cpw)
        (if watermarkfn
            (funcall watermarkfn cpw))

        (when title
          (draw-string-x-y cpw title
                           (floor (+ (box-left _box) (box-right _box)) 2)
                           0
                           :x-alignment :center
                           :y-alignment :top
                           :font (gp:find-best-font
                                  cpw
                                  (gp:make-font-description
                                   :name "Times"
                                   :size 16))))

        (draw-path cpw
                   (box-top-left     _box)
                   (box-bottom-left  _box)
                   (box-bottom-right _box))

        (pw-plot-xv-yv cpw
                       (vector (iqxlog (plotter-xmin cpw)) (iqxlog (plotter-xmax cpw)))
                       (vector (iqylog 0) (iqylog 0))
                       :color #.(color:make-gray 0.5))
      
        (pw-plot-xv-yv cpw
                       (vector (iqxlog 0) (iqxlog 0))
                       (vector (iqylog (plotter-ymin cpw)) (iqylog (plotter-ymax cpw)))
                       :color #.(color:make-gray 0.5))

        (when xtitle
          (draw-string-x-y cpw xtitle
                           (floor (+ (box-left _box) (box-right _box)) 2)
                           (- (gp:port-height cpw) 3)
                           :font font
                           :x-alignment :center
                           :y-alignment :bottom)
        
          (let* ((_xmin (plotter-xmin cpw))
                 (_xmax (plotter-xmax cpw))
                 (_xlast nil)
                 (_xstart nil))
            (destructuring-bind (x0 dx nl nu) (calc-start-delta _xmin _xmax)
              (declare (ignore nl nu))
              (if xlog
                  (setf dx 1))
              (labels ((xwork (xval xprev)
                         (let* ((xpos  (gp:transform-point (plotter-xform cpw) xval 0))
                                (xlast (draw-string-x-y
                                        cpw (plabel (iqxlog xval))
                                        xpos (+ 4 (box-bottom _box))
                                        :prev-bounds xprev
                                        :x-alignment :center
                                        :y-alignment :top
                                        :font font)))
                         
                           (when fullgrid
                             (when xlog
                               (gp:with-graphics-state
                                   (cpw :foreground #.(color:make-gray 0.75))
                                 (let ((xscale (first (plotter-xform cpw))))
                                   (loop for ix in log-subdivs do
                                         (let ((x (+ xpos (* xscale ix))))
                                           (if (< (box-left _box) x (box-right _box))
                                               (gp:draw-line cpw
                                                             x (box-top _box)
                                                             x (box-bottom _box))
                                             )))
                                   )))
                             (unless (zerop xval)
                               (gp:with-graphics-state
                                   (cpw 
                                    :foreground
                                    (if (vectorp fullgrid)
                                        fullgrid
                                      (color:make-gray
                                       (if xlog 0.5 0.75))))
                                 (gp:draw-line cpw
                                               xpos (box-top _box)
                                               xpos (box-bottom _box))
                                 )))
                         
                           (gp:draw-line cpw
                                         xpos (- (box-bottom _box) 2)
                                         xpos (+ (box-bottom _box) 3))

                           xlast)))
              
                (loop for xval = x0 then (- xval dx)
                      until (< xval (if (> _xmax _xmin) _xmin _xmax))
                      do
                      (setf _xlast (xwork xval _xlast))
                      (unless _xstart
                        (setf _xstart _xlast)))
              
                (setf _xlast _xstart)
              
                (loop for xval = (+ x0 dx) then (+ xval dx)
                      until (> xval (if (< _xmin _xmax) _xmax _xmin))
                      do
                      (setf _xlast (xwork xval _xlast)))
                ))))
      
        (when ytitle
          (draw-vert-string-x-y cpw ytitle
                                0 (floor (+ (box-top _box) (box-bottom _box)) 2)
                                :font  font
                                :x-alignment :top
                                :y-alignment :center)
        
          (let* ((_ymin (plotter-ymin cpw))
                 (_ymax (plotter-ymax cpw))
                 (_ylast  nil)
                 (_ystart nil))
            (destructuring-bind (y0 dy nl nu) (calc-start-delta _ymin _ymax)
              (declare (ignore nl nu))
              (if ylog
                  (setf dy 1))
              (labels ((ywork (yval yprev)
                         (multiple-value-bind (xpos ypos)
                             (gp:transform-point (plotter-xform cpw) 0 yval)
                           (declare (ignore xpos))

                           (let ((ylast (draw-vert-string-x-y cpw (plabel (iqylog yval))
                                                              (box-left _box) ypos
                                                              :prev-bounds yprev
                                                              :x-alignment :bottom
                                                              :y-alignment :center
                                                              :font font)))
                           
                             (when fullgrid
                               (when ylog
                                 (gp:with-graphics-state
                                     (cpw
                                      :foreground #.(color:make-gray 0.75))
                                   (let ((yscale (fourth (plotter-xform cpw))))
                                     (loop for ix in log-subdivs do
                                           (let ((y (+ ypos (* yscale ix))))
                                             (if (> (box-bottom _box) y (box-top _box))
                                                 (gp:draw-line cpw
                                                               (box-left _box) y
                                                               (box-right _box) y)
                                               ))))
                                   ))
                               (unless (zerop yval)
                                 (gp:with-graphics-state
                                     (cpw 
                                      :foreground
                                      (if (vectorp fullgrid)
                                          fullgrid
                                        (color:make-gray
                                         (if ylog 0.5 0.75))))
                                   (gp:draw-line cpw
                                                 (box-left _box)  ypos
                                                 (box-right _box) ypos)
                                   )))
                           
                             (gp:draw-line cpw
                                           (- (box-left _box) 2) ypos
                                           (+ (box-left _box) 3) ypos)
                             ylast))))
              
                (loop for yval = y0 then (- yval dy)
                      until (< yval (if (> _ymax _ymin) _ymin _ymax))
                      do
                      (setf _ylast (ywork yval _ylast))
                      (unless _ystart
                        (setf _ystart _ylast)))

                (setf _ylast _ystart)
              
                (loop for yval = (+ y0 dy) then (+ yval dy)
                      until (> yval (if (< _ymin _ymax) _ymax _ymin))
                      do
                      (setf _ylast (ywork yval _ylast)))
                ))))
        ))))


;; ------------------------------------------
(defvar *pwins* nil)

(defclass <window-rep> ()
  ((name :accessor window-rep-name
         :initarg :name)
   (intf :accessor window-rep-intf
         :initarg :intf)
   (pane :accessor window-rep-pane
         :initarg :pane)))

(defun current-plotting-window ()
  (if *pwins*
      (window-rep-pane (car *pwins*))
    (window 0)))

(defun find-named-window-rep (name)
  (find name *pwins* :key 'window-rep-name))

#|
(defun filter-nans (x y)
  (let ((vals 
         (loop for xv across x and
               yv across y keep-unless
               (or (nanp xv) (nanp yv)))))
    (values (apply 'vector (mapcar 'first  vals))
            (apply 'vector (mapcar 'second vals)))
    ))
|#

(defun do-plot (cpw xvector yvector
                    &rest args
                    &key 
                    box
                    xrange
                    yrange
                    xlog
                    ylog
                    aspect
                    &allow-other-keys)
  (let ((pbox (or box 
                  (list 0 0 
                        (gp:port-width cpw) 
                        (gp:port-height cpw)))))
    (pw-init-xv-yv cpw xvector yvector xrange yrange pbox
                   :xlog xlog :ylog ylog :aspect aspect)
    (pw-axes cpw)
    ;;
    ;; Now plot the data points
    ;; 
    (apply #'pw-plot-xv-yv cpw xvector yvector args)))

(defun oplot2 (xvector yvector 
                      &rest args
                      &key
                      draw-axes
                      (color (if draw-axes #.(color:make-rgb 0.0 0.5 0.0) :red))
                      thick
                      (linewidth (or thick 1))
                      pane
                      (fullgrid t)
                      &allow-other-keys)
  (let* ((yv (coerce yvector 'vector))
         (xv (coerce xvector 'vector)))
    (with-current-plotting-window (cpw pane)
      (if (or draw-axes
              (collector-empty-p (plotter-display-list cpw)))
          (progn
            (collector-discard-contents
             (plotter-display-list cpw))
            (collector-append-item
             (plotter-display-list cpw)
             #'(lambda (pane x y width height)
                 (declare (ignore x y width height))
                 (apply #'do-plot pane xv yv
                        :color     color
                        :linewidth linewidth
                        :fullgrid  fullgrid
                        args))))
        (collector-append-item
         (plotter-display-list cpw)
         #'(lambda (pane x y width height)
             (declare (ignore x y width height))
             (apply #'pw-plot-xv-yv pane xv yv 
                    :color color
                    args))))
      (gp:invalidate-rectangle cpw)
      )))

;; ------------------------------------------
(defun have-only-one-array? (args)
  (or (null args)
      (keywordp (first args))))

(defun plot (arr &rest args)
  (let* ((only1 (have-only-one-array? args))
         (ys    (if only1 arr (first args)))
         (xs    (if only1 nil arr))
         (parms (if only1 args (rest args))))
    (apply #'oplot2 xs ys :draw-axes t parms)))

(defun oplot (arr &rest args)
  (let* ((only1 (have-only-one-array? args))
         (ys    (if only1 arr (first args)))
         (xs    (if only1 nil arr))
         (parms (if only1 args (rest args))))
    (apply #'oplot2 xs ys parms)))

;; ------------------------------------------
(defun axes (&rest args
                   &key xrange yrange pane
                   &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (pw-init-xv-yv cpw nil nil xrange yrange
                   (list 0 0
                         (gp:port-width  cpw)
                         (gp:port-height cpw)))
    (collector-discard-contents (plotter-display-list cpw))
    (collector-append-item
     (plotter-display-list cpw)
     #'(lambda (pane x y width height)
         (declare (ignore x y width height))
         (apply #'pw-axes pane args)))
    (graphics-ports:invalidate-rectangle cpw)
    ))
                   
;; ------------------------------------------
(defun display-callback (pane x y width height)
  (gp:clear-graphics-port-state pane)
  (gp:clear-graphics-port pane)
  (dolist (item (collector-contents
                 (plotter-display-list pane)
                 :discard nil))
    (funcall item pane x y width height)))

;; ------------------------------------------
(defun window (name &rest args &key
                    (title      (format nil "Plotter:~A" name))
                    (background #.(color:make-gray 1))
                    (foreground #.(color:make-gray 0))
                    (xsize      400)
                    (ysize      300)
                    xpos
                    ypos)
  (let ((rep (find-named-window-rep name)))
    (if (and rep
             (null args))
        (window-rep-pane rep)
      (let* ((pane (make-instance 
                    '<plotter-window>
                    :foreground       foreground
                    :background       background
                    :display-callback 'display-callback
                    ))
             (intf (make-instance 
                    'capi:interface
                    :title title
                    :best-width          xsize
                    :best-height         ysize
                    :visible-min-width   200
                    :visible-min-height  150
                    :visible-max-width   800
                    :visible-max-height  600
                    :best-x              xpos
                    :best-y              ypos
                    :window-styles       '(:internal-borderless)
                    :layout (make-instance 'capi:column-layout
                                           :description (list pane))
                    :destroy-callback
                    #'(lambda (intf)
                        (setf *pwins* (remove intf *pwins*
                                              :key #'window-rep-intf)))
                    )))
        (wclose name)
        (push (make-instance 
               '<window-rep>
               :name name
               :intf intf
               :pane pane)
              *pwins*)
        (capi:display intf)
        pane
        ))
    ))

;; ------------------------------------------
(defun wset (name)
  ;; if window exists then raise it to the top and make it the current plotting window
  ;; if window does not exist then create it with default parameters
  (if-let (rep (find-named-window-rep name))
             (let ((intf (window-rep-intf rep)))
               (setf *pwins* (cons rep (remove rep *pwins*)))
               (capi:execute-with-interface intf
                                            (lambda ()
                                              (capi:raise-interface intf))
                                            ))
             (window name)))

;; ------------------------------------------
(defun wshow (name)
  ;; if window exists then raise it to the top, but don't make it the current plotting window
  ;; if window does not exist then create it with default parameters
  (if-let (rep (find-named-window-rep name))
             (let ((intf (window-rep-intf rep)))
               (capi:execute-with-interface intf
                                            (lambda ()
                                              (capi:raise-interface intf))
                                            ))
             (window name)))

;; ------------------------------------------
(defun wclose (name)
  ;; if window exists then ask it to commit suicide and disappear
  (lw:when-let (rep (find-named-window-rep name))
    (let ((intf (window-rep-intf rep)))
      ;; *pwins* is trimmed in the destroy-callback
      (capi:execute-with-interface intf
                                   (lambda ()
                                     (capi:destroy intf))
                                   ))
    ))

;; ------------------------------------------
(defun werase (&key pane)
  (with-current-plotting-window (cpw pane)
    (collector-discard-contents (plotter-display-list cpw))
    (gp:invalidate-rectangle cpw)))

;; ------------------------------------------
(defun outsxy (x y str
                  &rest args
                  &key
                  (font-size 12)
                  (font (gp:make-font-description
                         :name "Times"
                         :size font-size))
                  anchor
                  (align :w)
                  pane
                  &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (collector-append-item
     (plotter-display-list cpw)
     #'(lambda (pane xarg yarg width height)
         (declare (ignore xarg yarg width height))
         (let ((x (if (consp x)
                      (ecase (first x)
                        (:frac  (round (* (second x) (gp:port-width pane))))
                        (:data  (gp:transform-point (plotter-xform pane) (second x)))
                        (:pixel (second x)))
                    x))
               (y (if (consp y)
                      (ecase (first y)
                        (:frac  (round (* (- 1.0 (second y)) (gp:port-height pane))))
                        (:data  (multiple-value-bind (xx yx)
                                    (gp:transform-point (plotter-xform pane)
                                                        0 (second y))
                                  (declare (ignore xx))
                                  yx))
                        (:pixel (second y)))
                    y))
               (font (gp:find-best-font pane
                                        (if (stringp font)
                                            (gp:make-font-description
                                             :name font
                                             :size font-size)
                                          font)))
               (x-align (ecase (or anchor align)
                          ((:nw :w :sw) :left)
                          ((:n :s :ctr) :center)
                          ((:ne :e :se) :right)))
               (y-align (ecase (or anchor align)
                          ((:nw :n :ne) :top)
                          ((:w :ctr :e) :center)
                          ((:sw :s :se) :baseline))))
           (apply #'draw-string-x-y pane str x y
                  :font font
                  :x-alignment x-align
                  :y-alignment y-align
                  args))))
    (gp:invalidate-rectangle cpw)
    ))

(defun draw-text (str x y &rest args)
  (apply #'outsxy x y str args))

;; ------------------------------------------
(defmacro with-delayed-update (&body body)
  `(progn
     ,@body))

;; ------------------------------------------
;; -----------------------------------------------------------------
;; Functional plotting with adaptive gridding
;; DM/RAL 12/06
;; ----------------------------------------------------------------------------------------
;; Parametric Plotting with adaptive gridding
;;
(defun do-param-plotting (plotfn cpw xfn yfn args)
  (destructuring-bind (tmin tmax) (plotter-trange cpw)
    (destructuring-bind (tprepfn itprepfn) (plotter-tprepfns cpw)
      (declare (ignore tprepfn))
      (destructuring-bind (xprepfn ixprepfn) (plotter-xprepfns cpw)
        (destructuring-bind (yprepfn iyprepfn) (plotter-yprepfns cpw)
          (let* ((xsf (plotter-xsf cpw))
                 (ysf (plotter-ysf cpw))
                 (ts  (loop for ix from 0 to 16 collect
                            (+ tmin (* ix 0.0625 (- tmax tmin)))))
                 (xfn (compose xprepfn xfn itprepfn))
                 (yfn (compose yprepfn yfn itprepfn))
                 (xs  (mapcar xfn ts))
                 (ys  (mapcar yfn ts)))
            
            (labels ((split-interval (lvl t0 t1 x0 x1 y0 y1 new-xs new-ys)
                       (if (> lvl 9)
                           
                           (list (cons x0 new-xs)
                                 (cons y0 new-ys))
                         
                         (let* ((tmid (* 0.5 (+ t0 t1)))
                                (xmid (* 0.5 (+ x0 x1)))
                                (ymid (* 0.5 (+ y0 y1)))
                                (xmv  (funcall xfn tmid))
                                (ymv  (funcall yfn tmid)))
                           
                           (if (or (> (abs (* xsf (- xmv xmid))) 0.5)
                                   (> (abs (* ysf (- ymv ymid))) 0.5))
                               
                               (destructuring-bind (new-xs new-ys)
                                   (split-interval (1+ lvl)
                                                   t0 tmid x0 xmv y0 ymv
                                                   new-xs new-ys)
                                 
                                 (split-interval (1+ lvl)
                                                 tmid t1 xmv x1 ymv y1
                                                 new-xs new-ys))
                             
                             (list (cons x0 new-xs)
                                   (cons y0 new-ys))))
                         ))
                     
                     (iter-points (ts xs ys new-xs new-ys)
                       (if (endp (rest ts))
                           
                           (list (cons (first xs) new-xs)
                                 (cons (first ys) new-ys))
                         
                         (destructuring-bind (t0 t1 &rest _) ts
                           (declare (ignore _))
                           (destructuring-bind (x0 x1 &rest _) xs
                             (declare (ignore _))
                             (destructuring-bind (y0 y1 &rest _) ys
                               (declare (ignore _))
                               
                               (destructuring-bind (new-xs new-ys)
                                   (split-interval 0 t0 t1 x0 x1 y0 y1 new-xs new-ys)
                                 
                                 (iter-points (rest ts) (rest xs) (rest ys) new-xs new-ys)
                                 ))))
                         )))
              
              (destructuring-bind (xs ys) (iter-points ts xs ys nil nil)
                (let ((xs (if ixprepfn (mapcar ixprepfn xs) xs))
                      (ys (if iyprepfn (mapcar iyprepfn ys) ys)))
                  (apply plotfn xs ys args)
                  (values (length xs) xs ys)
                  )))
            )))
      )))

;; ------------------------------------------
(defun paramplot (domain xfn yfn &rest args
                         &key over tlog xlog ylog xrange yrange pane
                         &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (labels ((get-prepfns (islog)
               (if islog
                   (list #'log10 #'pow10)
                 (list #'identity #'identity)))
             (get-minmax (est-minmax req-minmax prepfn)
               (destructuring-bind (est-min est-max) est-minmax
                 (destructuring-bind (req-min req-max) req-minmax
                   (if (= req-min req-max)
                       (if (= est-min est-max)
                           (let* ((vmin (funcall prepfn est-min))
                                  (vmax (if (zerop vmin) 0.1 (* 1.1 vmin))))
                             (list vmin vmax))
                         (list (funcall prepfn est-min)
                               (funcall prepfn est-max)))
                     (list (funcall prepfn req-min)
                           (funcall prepfn req-max)))
                   ))))
      (destructuring-bind (tprepfn itprepfn) (get-prepfns tlog)
        (destructuring-bind (xprepfn ixprepfn) (get-prepfns xlog)
          (destructuring-bind (yprepfn iyprepfn) (get-prepfns ylog)
            (destructuring-bind (tmin tmax) (get-minmax '(0 0) domain tprepfn)
              (let* ((ts (loop for ix from 0 to 16 collect
                               (+ tmin (* ix 0.0625 (- tmax tmin)))))
                     (xs (mapcar (compose xfn itprepfn) ts))
                     (ys (mapcar (compose yfn itprepfn) ts)))
                (destructuring-bind (xmin xmax) (get-minmax
                                                 (list (reduce #'min xs)
                                                       (reduce #'max xs))
                                                 (or xrange '(0 0)) xprepfn)
                  (destructuring-bind (ymin ymax) (get-minmax
                                                   (list (reduce #'min ys)
                                                         (reduce #'max ys))
                                                   (or yrange '(0 0)) yprepfn)
                    (setf (plotter-trange cpw)   (list tmin tmax)
                          (plotter-xsf cpw)      (qdiv (gp:port-width  cpw) (- xmax xmin))
                          (plotter-ysf cpw)      (qdiv (gp:port-height cpw) (- ymax ymin))
                          (plotter-tprepfns cpw) (list tprepfn itprepfn)
                          (plotter-xprepfns cpw) (list xprepfn (and xlog ixprepfn))
                          (plotter-yprepfns cpw) (list yprepfn (and ylog iyprepfn)))
                    (do-param-plotting (if over #'oplot #'plot) cpw xfn yfn args)
                    )))
              )))
        ))))

(defun oparamplot (xfn yfn &rest args
                       &key pane &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (do-param-plotting #'oplot cpw xfn yfn args)))

(defun fplot (domain fn &rest args)
  (apply #'paramplot domain #'identity fn args))

(defun ofplot (fn &rest args)
  (apply #'oparamplot #'identity fn args))

;; ------------------------------------------
#| Test code...

(let (x y)
  (defun ramp (min max npts)
    (let ((val (make-array npts))
          (rate (/ (- max min) npts)))
      (dotimes (ix npts val)
        (setf (aref val ix) (+ min (* ix rate))))
      ))
  
  (setf x (ramp -10 10 100))
  (defun sinc (val)
    (if (zerop val)
        1.0
      (/ (sin val) val)))
  (setf y (map 'vector 'sinc x))
  
  (window 0 :xsize 400 :ysize 300)
  (plot x y 
        :color (color:make-rgb 1.0 0.0 0.0 0.25) ;;:red
        :thick 2
        :title "Sinc(x)"
        :xtitle "X Values"
        :ytitle "Y Values")

;;  (window 1 :background :black :foreground :yellow :xsize 400 :ysize 300)
;;  (plot x y 
;;        :color (color:make-rgb 1.0 0.0 1.0 0.25) ;;:magenta
;;        :linewidth 2
;;        :fullgrid (color:make-gray 0.25)
;;        :title "Sinc(x)"
;;        :xtitle "X Values"
;;        :ytitle "Y Values")
  )


(let ((s "Hello from LispWorks! 0123456789")
      (font "Times-Roman")
      (x    200)
      (y    40))
  (with-current-plotting-window (cpw)
    (multiple-value-bind (lf tp rt bt)
        (graphics-ports:get-string-extent cpw s font)
      (print (list lf tp rt bt))
      (let ((wd (- rt lf -1))
            (ht (- bt tp -1)))
        
        (gp:with-pixmap-graphics-port (ph cpw wd ht
                                          :clear t)
          (gp:draw-string ph s
                          0 (- tp)
                          :font font)
          
          (let* ((h-image (gp:make-image-from-port ph))
                 (v-image (gp:make-image cpw ht wd))
                 (ha (gp:make-image-access ph h-image))
                 (va (gp:make-image-access cpw v-image)))
            (loop for ix from 0 below wd do
                  (loop for iy from 0 below ht do
                        (setf (gp:image-access-pixel va iy (- wd ix 1))
                              (gp:image-access-pixel ha ix iy))
                        ))
            (gp:free-image-access ha)
            (gp:free-image-access va)
            (gp:free-image ph h-image)
            
            (gp:draw-image cpw v-image x y
                           :global-alpha 0.5)
            
            (gp:free-image cpw v-image)
            ))
        ))
    ))

(progn
  (setf xf (gp:make-transform))
  (gp:apply-translation xf (- -10) (- -0.23))
  (gp:apply-scale xf (/ 380 20) (- (/ 280 1.23)))
  (gp:apply-translation xf 20 280)
  )

|#


;; *eof* ;;

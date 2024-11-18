;; plotter.lsp -- Plotting support for Lisp
;; DM 02/07

(in-package "PLOTTER")

(defclass <plotter-pane> (capi:output-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ((xlog      :accessor plotter-xlog           :initform nil)
   (xmin      :accessor plotter-xmin           :initform 0.0d0)
   (xmax      :accessor plotter-xmax           :initform 1.0d0)

   (ylog      :accessor plotter-ylog           :initform nil)
   (ymin      :accessor plotter-ymin           :initform 0.0d0)
   (ymax      :accessor plotter-ymax           :initform 1.0d0)
   
   (box       :accessor plotter-box)
   (xform     :accessor plotter-xform          :initform '(1 0 0 1 0 0))
   (inv-xform :accessor plotter-inv-xform      :initform '(1 0 0 1 0 0))
   (dlist     :accessor plotter-display-list   :initform (um:make-mpsafe-collector))
   (dirty     :accessor plotter-dirty          :initform nil)
   (timer     :accessor plotter-resize-timer   :initform nil)
   
   ;; stuff for paramplot
   (trange    :accessor plotter-trange)
   (xsf       :accessor plotter-xsf)
   (ysf       :accessor plotter-ysf)
   (tprepfns  :accessor plotter-tprepfns)
   (xprepfns  :accessor plotter-xprepfns)
   (yprepfns  :accessor plotter-yprepfns)

   ;; info for nice looking zooming
   (title     :accessor plotter-base-title     :initarg :base-title     :initform "Plot")
   (def-wd    :accessor plotter-nominal-width  :initarg :nominal-width  :initform nil)
   (def-ht    :accessor plotter-nominal-height :initarg :nominal-height :initform nil)

   (sf        :accessor plotter-sf    :initform 1)
   (magn      :accessor plotter-magn  :initform 1)

   (legend    :accessor plotter-legend-info        :initform (um:make-collector))
   (legend-x  :accessor plotter-legend-x           :initform '(:frac 0.75))
   (legend-y  :accessor plotter-legend-y           :initform '(:frac 0.9))

   (x-ro-hook :accessor plotter-x-readout-hook     :initform #'identity)
   (y-ro-hook :accessor plotter-y-readout-hook     :initform #'identity)

   (delay-backing  :accessor plotter-delay-backing :initform nil)
   (backing        :accessor plotter-backing       :initform nil)

   (capi::destroy-callback
                   :initform   'discard-backing-image
                   :allocation :class)
   ))

;; ------------------------------------------
(defstruct legend-info
  color thick linedashing symbol plot-joined text)

;; ------------------------------------------
(defconstant $tiny-times-font-size
  #+:COCOA 10
  #+:WIN32  8)

(defconstant $normal-times-font-size
  #+:COCOA 12
  #+:WIN32  9)

(defconstant $big-times-font-size
  #+:COCOA 14
  #+:WIN32 10)
  
;; ------------------------------------------
#+:WIN32
(defun adjust-linewidth (wd)
  (max 1 (round wd)))

#+:COCOA
(defun adjust-linewidth (wd)
  wd)

#+:WIN32
(defun adjust-color (pane color alpha)
  (let* ((bg (color:get-color-spec (gp:graphics-state-background
                                    (gp:get-graphics-state pane))
                                   ))
         (c  (color:get-color-spec color))
         (alpha (or alpha
                    (color:color-alpha c)))
         (1malpha (- 1.0 alpha)))
    (labels ((mix (fn)
               (+ (* 1malpha (funcall fn bg)) (* alpha (funcall fn c)))))
      (color:make-rgb
       (mix #'color:color-red)
       (mix #'color:color-green)
       (mix #'color:color-blue))
      )))
      
#+:COCOA
(defun adjust-color (pane color alpha)
  (declare (ignore pane))
  (let* ((c (color:get-color-spec color))
         (alpha (or alpha
                    (color:color-alpha c))))
    (color:make-rgb
     (color:color-red   c)
     (color:color-green c)
     (color:color-blue  c)
     alpha)
    ))

#+:WIN32
(defun adjust-box (box)
  (mapcar #'round box))

#+:COCOA
(defun adjust-box (box)
  box)

;; ------------------------------------------
(defvar *pwins*      nil)
(defvar *pwins-lock* (mp:make-lock :name "PWins Lock"))

(defmacro with-pwins-lock (&body body)
  `(mp:with-lock (*pwins-lock*)
     ,@body))

(defclass <window-rep> ()
  ((name :accessor window-rep-name  :initarg :name)
   (intf :accessor window-rep-intf  :initarg :intf)
   (pane :accessor window-rep-pane  :initarg :pane)))

(defun current-plotting-window ()
  (um:if-let (first-window (with-pwins-lock
                             (car *pwins*)))
             (window-rep-pane first-window)
             (drawing-area (window 0))
             ))

(defun find-named-window-rep (name)
  (with-pwins-lock
   (find name *pwins* :key 'window-rep-name)))

;; ------------------------------------------

(defun do-with-current-plotting-window (fn &optional pane)
  (funcall fn (or pane
                  (current-plotting-window))
           ))

(defmacro with-current-plotting-window ((name &optional pane) &body body)
  `(do-with-current-plotting-window
    (lambda (,name)
      ,@body)
    ,pane))

(defmacro with-image ((port (image imgexpr)) &body body)
  `(let ((,image ,imgexpr))
     ,@body
     (gp:free-image ,port ,image)))

(defmacro with-image-access ((acc access-expr) &body body)
  `(let ((,acc ,access-expr))
     ,@body
     (gp:free-image-access ,acc)))

;; ------------------------------------------
(defparameter *update-delay* 0)

(defun sync-with-capi-interface (intf fn &rest args)
  (let ((mbox (mp:make-mailbox)))
    (capi:execute-with-interface intf
                                 (lambda ()
                                   (apply fn args)
                                   (mp:mailbox-send mbox :done)))
    (mp:mailbox-read mbox)))

(defun sync-with-capi-pane (pane fn &rest args)
  (let ((mbox (mp:make-mailbox)))
    (capi:apply-in-pane-process pane
                                (lambda ()
                                  (apply fn args)
                                  (mp:mailbox-send mbox :done)))
    (mp:mailbox-read mbox)))

(defun wdraw ()
  (when (zerop *update-delay*)
    (let ((reps (with-pwins-lock
                  (copy-seq *pwins*))))
      (dolist (rep reps)
        (let ((pane (window-rep-pane rep)))
          (when (plotter-dirty pane)
            (sync-with-capi-pane pane #'gp:invalidate-rectangle pane)
            (setf (plotter-dirty pane) nil)
            ))
        ))
    ))

(defun mark-dirty (pane)
  (setf (plotter-dirty pane) t)
  (discard-backing-image pane)
  (wdraw))

;; ------------------------------------------
(defun do-with-delayed-update (fn)
  (let ((*update-delay* (1+ *update-delay*)))
    (funcall fn))
  (wdraw))

(defmacro with-delayed-update (&body body)
  `(do-with-delayed-update
    (lambda ()
      ,@body)))

;; ------------------------------------------

(defun log10 (x)
  (if (not (plusp x))
      -300
    (log x 10.0d0)))

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

;; ------------------------------------------
(defun qrange (rng &optional (default 0.1))
  (if (zerop rng)
      default
    rng))

(defun qdiv (a b &optional (default 0.1))
  (/ a (qrange b default)))

;; ------------------------------------------
(defun get-range (range v islog)
  (or range
      (and (plusp (length v))
           (list (vmin v) (vmax v)))
      (list (if islog 0.1 0) 1)))

(defmethod pw-init-xv-yv ((cpw <plotter-pane>) xv yv
                          xrange yrange inbox
                          &key xlog ylog aspect
                          &allow-other-keys)
  (let* ((_box (inset-box-sides inbox 30 20 10 30)))
    (destructuring-bind (_xmin _xmax) (get-range xrange xv xlog)
      (destructuring-bind (_ymin _ymax) (get-range yrange yv ylog)
        
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
          
          (let ((xform     (gp:make-transform))
                (inv-xform (gp:make-transform)))
            (gp:apply-translation xform (- _xmin) (- _ymin))
            (gp:apply-scale xform xscale (- yscale))
            (gp:apply-translation xform (box-left _box) (box-bottom _box))
            (gp:invert-transform xform inv-xform)
            (setf (plotter-xform     cpw) xform
                  (plotter-inv-xform cpw) inv-xform)
            )))
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
                             (margin 2)
                             (transparent t)
                             (color :black)
                             clip
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
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))
          prev-bounds
        
        (gp:with-graphics-state
            (pane :foreground color
                  :mask       (and clip
                                   (adjust-box (plotter-box pane))))
          (gp:draw-string pane string (+ x dx) (+ y dy)
                          :font font
                          :block (not transparent))
          new-bounds)
        ))))

;; ------------------------------------------
#+:COCOA
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :left)
                                  (y-alignment :baseline)
                                  font
                                  prev-bounds
                                  (margin 2)
                                  (color :black)
                                  ;;(transparent t)
                                  )
  ;;
  ;; draw vertical string by appealing directly to Cocoa
  ;;
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)
    (declare (ignore bt tp))

    (let* ((wd (- rt lf -1))
           (dx (ecase x-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dx) (+ y rt dx)))
           (font-attrs (gp:font-description-attributes (gp:font-description font)))
           (font-size  (getf font-attrs :size))
           (font-name  (getf font-attrs :name)))

      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))

          prev-bounds

        (progn
          (add-label pane string x  y
                     :font      font-name
                     :font-size font-size
                     :color     color
                     :alpha     1.0
                     :x-alignment x-alignment
                     :y-alignment y-alignment
                     :angle     90.0)
          new-bounds)
        ))))

#+:WIN32
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :left)
                                  (y-alignment :baseline)
                                  font
                                  prev-bounds
                                  (margin 2)
                                  (color :black)
                                  (transparent t))
  ;;
  ;; draw vertical string by rotating bitmap of horizontal string
  ;;
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)

    (let* ((wd (- rt lf -1))
           (ht (- bt tp -1))
           (dy (ecase y-alignment
                 (:top      0)
                 (:bottom   (- ht))
                 (:baseline tp)
                 (:center   (floor tp 2))
                 ))
           (dx (ecase x-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dx) (+ y rt dx))))
      
      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))

          prev-bounds
        
        (gp:with-pixmap-graphics-port (ph pane wd ht
                                          :clear t)
          (gp:with-graphics-state
              (ph :foreground color)
            (gp:draw-string ph string
                            0 (- tp)
                            :font font
                            :block (not transparent)))

          (with-image (pane (v-image #+:COCOA (gp:make-image pane ht wd)
                                     #+:WIN32 (gp:make-image pane ht wd :alpha nil)
                                     ))
             (with-image (ph (h-image (gp:make-image-from-port ph)))
                 (with-image-access (ha (gp:make-image-access ph h-image))
                    (with-image-access (va (gp:make-image-access pane v-image))
                      (gp:image-access-transfer-from-image ha)
                      (loop for ix from 0 below wd do
                            (loop for iy from 0 below ht do
                                  (setf (gp:image-access-pixel va iy (- wd ix 1))
                                        (gp:image-access-pixel ha ix iy))
                                  ))
                      (gp:image-access-transfer-to-image va)
                      )))
             (gp:draw-image pane v-image (+ x dy) (+ y dx)))
          new-bounds
          ))
      )))

;; ------------------------------------------
(defun zip (&rest seqs)
  (apply #'map 'list #'list seqs))

(defun staircase (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev yprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y xmid yprev) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun make-bars (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun interleave (&rest seqs)
  (mapcan #'nconc (apply #'zip seqs)))


(defun get-symbol-plotfn (cpw symbol)
  (labels ((translucent+frame (fn)
             (gp:with-graphics-state
                 (cpw
                  :foreground #.(color:make-gray 1.0 0.25))
               ;; translucent interior
               (funcall fn t))
             ;; solid frame
             (funcall fn))
           
           (solid+frame (fn)
             (funcall fn t)
             (funcall fn)))
    
    (ecase symbol
      (:cross     (lambda (x y)
                    (gp:draw-line cpw (- x 3) y (+ x 3) y)
                    (gp:draw-line cpw x (- y 3) x (+ y 3))
                    ))
      
      (:circle    (lambda (x y)
                    (labels ((draw-circle (&optional filled)
                               (gp:draw-circle cpw
                                               x 
                                               #+:COCOA (- y 0.5)
                                               #+:WIN32 y
                                               3
                                               :filled filled)))
                      (translucent+frame #'draw-circle)
                      )))
      
      (:filled-circle (lambda (x y)
                        (labels ((draw-circle (&optional filled)
                                   (gp:draw-circle cpw
                                                   (if filled (1+ x) x)
                                                   (1- y) 3
                                                   :filled filled)))
                          (solid+frame #'draw-circle)
                          )))
      
      ((:box :square) (lambda (x y)
                        (labels ((draw-rectangle (&optional filled)
                                   (gp:draw-rectangle cpw (- x 3) (- y 3) 6 6
                                                      :filled filled)))
                          (translucent+frame #'draw-rectangle)
                          )))
      
      ((:filled-box :filled-square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle cpw (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (solid+frame #'draw-rectangle)
           )))
      
      (:triangle    (lambda (x y)
                      (labels ((draw-triangle (&optional filled)
                                 (gp:draw-polygon cpw
                                                  (list (- x 3) (+ y 3)
                                                        x (- y 4)
                                                        (+ x 3) (+ y 3))
                                                  :closed t
                                                  :filled filled)))
                        (translucent+frame #'draw-triangle)
                        )))
      
      (:filled-triangle  (lambda (x y)
                           (labels ((draw-triangle (&optional filled)
                                      (gp:draw-polygon cpw
                                                       (list (- x 3) (+ y 3)
                                                             x (- y 4)
                                                             (+ x 3) (+ y 3))
                                                       :closed t
                                                       :filled filled)))
                             (solid+frame #'draw-triangle)
                             )))
      
      (:dot            (lambda (x y)
                         (gp:draw-circle cpw x (1- y) 0.5)
                         ))
      )))

(defmethod pw-plot-xv-yv ((cpw <plotter-pane>) xvector yvector 
                          &key
                          (color #.(color:make-rgb 0.0 0.5 0.0))
                          alpha
                          thick
                          (linewidth (or thick 1))
                          linedashing
                          symbol
                          plot-joined
                          legend
                          legend-x
                          legend-y
                          &allow-other-keys)
  ;; this is the base plotting routine
  ;; called only from within the pane process
  (let* ((xv        (if (plotter-xlog cpw)
                        (map 'vector #'log10 xvector)
                      xvector))
         (yv        (if (plotter-ylog cpw)
                        (map 'vector #'log10 yvector)
                      yvector))
         (sf        (plotter-sf  cpw))
         (box       (let ((box (plotter-box cpw)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform cpw))
         (color     (adjust-color cpw color alpha))
         (linewidth (adjust-linewidth (* sf linewidth))))

    (when legend
      (um:collector-append-item (plotter-legend-info cpw)
                                (make-legend-info
                                 :color       color
                                 :thick       linewidth
                                 :linedashing linedashing
                                 :symbol      symbol
                                 :plot-joined plot-joined
                                 :text        legend)))

    (when legend-x
      (setf (plotter-legend-x cpw) legend-x))
    (when legend-y
      (setf (plotter-legend-y cpw) legend-y))
    
    (gp:with-graphics-state (cpw
                             :thickness  linewidth
                             :dashed     (not (null linedashing))
                             :dash       (mapcar (um:curry #'* sf) linedashing)
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)

      (gp:with-graphics-scale (cpw sf sf)
        
        (case symbol
          (:steps
           (gp:with-graphics-transform (cpw xform)
             (gp:draw-polygon cpw
                              (staircase xv yv)
                              :closed nil)))
          
          (:vbars
           (gp:with-graphics-transform (cpw xform)
             (labels ((draw-bar (x1 y1 x2 y2)
                        (declare (ignore y1))
                        (gp:draw-rectangle cpw
                                           x2 0
                                           (- x1 x2) y2
                                           :filled t))
                      (draw-bars (bars)
                        (when (cddr bars)
                          (destructuring-bind (x1 y1 x2 y2 &rest _) bars
                            (declare (ignore _))
                            (draw-bar x1 y1 x2 y2)
                            (draw-bars (cddr bars)))
                          )))
               (draw-bars (make-bars xv yv))
               )))

          (:hbars
           (gp:with-graphics-transform (cpw xform)
             (labels ((draw-bar (x1 y1 x2 y2)
                        (declare (ignore y1))
                        (gp:draw-rectangle cpw
                                           0 x2
                                           y2 (- x1 x2)
                                           :filled t))
                      (draw-bars (bars)
                        (when (cddr bars)
                          (destructuring-bind (x1 y1 x2 y2 &rest _) bars
                            (declare (ignore _))
                            (draw-bar x1 y1 x2 y2)
                            (draw-bars (cddr bars)))
                          )))
               (draw-bars (make-bars yv xv))
               )))

          (t
           (when (or (not symbol)
                     plot-joined)
             (gp:with-graphics-transform (cpw xform)
               (gp:draw-polygon cpw
                                (interleave xv yv)
                                :closed nil)
               ))
           
           (when symbol
             (let ((plotfn (get-symbol-plotfn cpw symbol)))
               (map nil (lambda (x y)
                          (apply plotfn
                                 (multiple-value-list
                                  (gp:transform-point xform x y))
                                 ))
                    xv yv)
               ))
           ))
        ))
    ))


(defun plt-draw-shape (pane shape x0 y0 x1 y1
                          &key
                          color alpha filled
                          border-thick border-color border-alpha
                          start-angle sweep-angle)
  ;; for rectangles: shape = :rect, (x0,y0) and (x1,y1) are opposite corners
  ;; for ellipses:   shape = :ellipse, (x0,y0) is ctr (x1,y1) are radii
  (let* ((x0        (if (plotter-xlog pane)
                        (log10 x0)
                      x0))
         (x1        (if (plotter-xlog pane)
                        (log10 x1)
                      x1))
         (y0        (if (plotter-ylog pane)
                        (log10 y0)
                      y0))
         (y1        (if (plotter-ylog pane)
                        (log10 y1)
                      y1))
         (wd        (- x1 x0))
         (ht        (- y1 y0))
         (sf        (plotter-sf  pane))
         (box       (let ((box (plotter-box pane)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform pane))
         (color     (adjust-color pane color alpha))
         (bcolor    (adjust-color pane border-color border-alpha))
         (linewidth (adjust-linewidth (* sf (or border-thick 0)))))
    
    (gp:with-graphics-state (pane
                             :thickness  linewidth
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)

      (gp:with-graphics-scale (pane sf sf)

        (gp:with-graphics-transform (pane xform)
          (when filled
            (ecase shape
              (:rect
               (gp:draw-rectangle pane
                                  x0 y0 wd ht
                                  :filled t))
              (:ellipse
               (gp:draw-ellipse pane
                                x0 y0 x1 y1
                                :filled t))
              
              (:arc
               (gp:draw-arc pane
                            x0 y0 wd ht
                            start-angle sweep-angle
                            :filled t))
              ))
          
          (when border-thick
            (gp:with-graphics-state (pane
                                     :foreground bcolor)
              (case shape
                (:rect
                 (gp:draw-rectangle pane
                                    x0 y0 wd ht
                                    :filled nil))
                (:ellipse
                 (gp:draw-ellipse pane
                                  x0 y0 x1 y1
                                  :filled nil))

                (:arc
                 (gp:draw-arc pane
                              x0 y0 wd ht
                              start-angle sweep-angle
                              :filled nil))
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
(defparameter *ext-logo*
  (ignore-errors
    (gp:read-external-image
     ;;"/usr/local/lib/Logo75-Alpha25.pdf"
     #+:COCOA "/usr/local/lib/Logo75Img-Alpha25y.pdf"
     #+:WIN32 "c:/projects/lib/Logo75Img-Alpha25y.bmp")))

(defun stamp-logo (pane)
  (when *ext-logo*
    (with-image (pane (logo (gp:convert-external-image pane *ext-logo*)))
      (let ((sf (plotter-sf pane)))
        (gp:with-graphics-scale (pane sf sf)
          (gp:draw-image pane logo 40 50))
        ))
    ))

(defun watermark (pane)
  (let ((cright1 "Copyright (c) 2006-2007 by Refined Audiometrics Laboratory, LLC")
        (cright2 "All rights reserved.")
        (font2   (gp:find-best-font pane
                                    (gp:make-font-description
                                     :family "Times"
                                     :size (* $tiny-times-font-size
                                              (plotter-sf pane)))))
        (color2 #.(color:make-gray 0.7))
        (sf  (plotter-sf pane)))

    (stamp-logo pane)

    (draw-string-x-y pane cright1
                     (* sf 40)
                     (* sf (- (plotter-nominal-height pane) 60))
                     :x-alignment :left
                     :y-alignment :top
                     :font  font2
                     :color color2)
    (draw-string-x-y pane cright2
                     (* sf 40)
                     (* sf (- (plotter-nominal-height pane) 48))
                     :x-alignment :left
                     :y-alignment :top
                     :font  font2
                     :color color2)
    ))

;; ------------------------------------------
(defparameter *log-subdivs*
  (mapcar #'log10
          '(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
                2 3 4 5 6 7 8 9)))

(defmethod pw-axes ((cpw <plotter-pane>)
                    &key
                    (fullgrid t)
                    (xtitle "X")
                    (ytitle "Y")
                    (title  "Plot")
                    (watermarkfn #'watermark)
                    &allow-other-keys)
  (let* ((box   (plotter-box cpw))
         (sf    (plotter-sf cpw))
         (font  (gp:find-best-font
                 cpw
                 (gp:make-font-description
                  :family "Times"
                  :size (* $normal-times-font-size
                           (plotter-sf cpw)))))
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
               (trim-mantissa (format nil "~,3F" (float val 1.0)))
             (destructuring-bind (mant expon)
                 (um:split-string (format nil "~,2E" (float val 1.0))
                                  :delims "E")
               (um:mkstr (trim-mantissa mant) "e" (remove #\+ expon)))
             )))

      (gp:clear-graphics-port cpw)
      (if watermarkfn
          (funcall watermarkfn cpw))

      (when title
        (draw-string-x-y cpw title
                         (floor (* sf (+ (box-left box) (box-right box))) 2)
                         0
                         :x-alignment :center
                         :y-alignment :top
                         :font (gp:find-best-font
                                cpw
                                (gp:make-font-description
                                 :family "Times"
                                 :size (* $big-times-font-size
                                          (plotter-sf cpw)))
                                )))

      (gp:with-graphics-scale (cpw sf sf)
        (gp:with-graphics-state (cpw :scale-thickness t)
          (draw-path cpw
                     (box-top-left     box)
                     (box-bottom-left  box)
                     (box-bottom-right box)
                     )))
        
      (pw-plot-xv-yv cpw
                     (vector (iqxlog (plotter-xmin cpw))
                             (iqxlog (plotter-xmax cpw)))
                     (vector (iqylog 0) (iqylog 0))
                     :color #.(color:make-gray 0.5))
        
      (pw-plot-xv-yv cpw
                     (vector (iqxlog 0) (iqxlog 0))
                     (vector (iqylog (plotter-ymin cpw))
                             (iqylog (plotter-ymax cpw)))
                     :color #.(color:make-gray 0.5))

      (when xtitle
        (draw-string-x-y cpw xtitle
                         (floor (* sf (+ (box-left box) (box-right box))) 2)
                         (* sf (+ (box-bottom box) 26))
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
                       (let* ((xpos  (gp:transform-point (plotter-xform cpw)
                                                         xval 0))
                              (xlast (draw-string-x-y
                                      cpw (plabel (iqxlog xval))
                                      (* sf xpos)
                                      (* sf (+ 4 (box-bottom box)))
                                      :prev-bounds xprev
                                      :margin (* 2 sf)
                                      :x-alignment :center
                                      :y-alignment :top
                                      :font font)))
                           
                         (gp:with-graphics-scale (cpw sf sf)
                           (gp:with-graphics-state
                               (cpw
                                :scale-thickness t)
                             (when fullgrid
                               (when xlog
                                 (gp:with-graphics-state
                                     (cpw :foreground #.(color:make-gray 0.75))
                                   (let ((xscale (first (plotter-xform cpw))))
                                     (loop for ix in *log-subdivs* do
                                           (let ((x (+ xpos (* xscale ix))))
                                             (if (< (box-left box) x
                                                    (box-right box))
                                                 (gp:draw-line
                                                  cpw
                                                  x (box-top box)
                                                  x (box-bottom box))
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
                                                 xpos (box-top box)
                                                 xpos (box-bottom box))
                                   )))
                               
                             (gp:draw-line cpw
                                           xpos (- (box-bottom box) 2)
                                           xpos (+ (box-bottom box) 3))))
                           
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
                              #+:WIN32 0 #+:COCOA (* sf 3)
                              (floor (* sf (+ (box-top box)
                                              (box-bottom box))) 2)
                              :font  font
                              :x-alignment :center
                              :y-alignment :top)
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
                           
                         (let ((ylast (draw-vert-string-x-y
                                       cpw
                                       (plabel (iqylog yval))
                                       (* sf (- (box-left box) #+:WIN32 1 #+:COCOA 3))
                                       (* sf ypos)
                                       :prev-bounds yprev
                                       :margin (* 2 sf)
                                       :x-alignment :center
                                       :y-alignment :bottom
                                       :font font)))
                             
                           (gp:with-graphics-scale (cpw sf sf)
                             (gp:with-graphics-state
                                 (cpw :scale-thickness t)
                               (when fullgrid
                                 (when ylog
                                   (gp:with-graphics-state
                                       (cpw
                                        :foreground #.(color:make-gray 0.75))
                                     (let ((yscale (fourth (plotter-xform cpw))))
                                       (loop for ix in *log-subdivs* do
                                             (let ((y (+ ypos (* yscale ix))))
                                               (if (> (box-bottom box) y
                                                      (box-top box))
                                                   (gp:draw-line
                                                    cpw
                                                    (1+ (box-left box)) y
                                                    (box-right box) y)
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
                                                   (1+ (box-left box))  ypos
                                                   (box-right box) ypos)
                                     )))
                                 
                               (gp:draw-line cpw
                                             (- (box-left box) 2) ypos
                                             (+ (box-left box) 3) ypos)))
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
              )))
        ))
    ))

;; ----------------------------------------------------------------
(defun parse-location (sym)
  ;; BEWARE:: '1.2d+3p means (:pixel 1200) NOT (:data 1.2 +3)
  ;; be sure to disambiguate the "d" as in '1.2data+3p
  (let* ((s    (if (stringp sym)
                   sym
                 (symbol-name sym)))
         (slen (length s)))
    
    (labels ((iter (state ix ans)
               (ecase state
                 (:initial
                  (cond ((>= ix slen)
                         ;; have been all number constituents
                         ;;assume we have pixels specified
                         (list :pixel (read-from-string s) 0))
                        
                        ((digit-char-p (char s ix))
                         ;; still have number constituent
                         (iter :initial (1+ ix) nil))
                        
                        ((char= #\. (char s ix))
                         ;; still have number constituent
                         (iter :initial (1+ ix) nil))
                        
                        ((char-equal #\d (char s ix))
                         ;; might be part of an exponential notation
                         ;; or it might be a frac or data specifier
                         (iter :check-for-exponent (1+ ix) nil))
                        
                        (t
                         (iter :get-units ix (read-from-string s t :eof :end ix)))
                        ))
                 
                 (:check-for-exponent
                  (cond ((>= ix slen)
                         ;; we had a D so it must have been (:data nn.nnn)
                         (list :data
                               (read-from-string s t :eof :end (1- ix))
                               ))
                        
                        ((alpha-char-p (char s ix))
                         ;; can't be a +/- sign, so must have been (:data nn.nn)
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :data
                                     (read-from-string s t :eof :end (1- ix))
                                     )))
                        
                        (t ;; assume we have a +/- sign and continue with number scan
                           (iter :initial (1+ ix) nil))
                        ))
                  
                 (:get-units
                  (ecase (char-upcase (char s ix))
                    (#\P  (iter :scan-to-plus-or-minus (1+ ix) (list :pixel ans)))
                    (#\D  (iter :scan-to-plus-or-minus (1+ ix) (list :data  ans)))
                    (#\F  (iter :scan-to-plus-or-minus (1+ ix) (list :frac  ans)))))
                 
                 (:scan-to-plus-or-minus
                  (cond ((>= ix slen)
                         ;; no offset, we a finished
                         ans)
                        
                        ((or (char= #\+ (char s ix))
                             (char= #\- (char s ix)))
                         (let ((end (position-if #'alpha-char-p s :start ix)))
                           ;; read the offset and return
                           (list (first ans)  ;; type
                                 (second ans) ;; value
                                 (read-from-string s t :eof :start ix :end end) ;; offset
                                 )))
                        
                        (t ;; keep looking
                           (iter :scan-to-plus-or-minus (1+ ix) ans))
                        ))
                 )))
      
      (iter :initial 0 nil)
      )))
              
(defun get-x-location (pane x)
  (cond
   ((and (consp x)
         (symbolp (first x)))
    (ecase (char-upcase (char (symbol-name (first x)) 0))
      ;; accommodates :DATA :DAT :D :DATUM, :FRAC :F :FRACTION, :PIXEL :P :PIX :PIXELS, etc.
      (#\F  (round (* (second x)
                      (plotter-nominal-width pane)
                      )))
      (#\D  (gp:transform-point (plotter-xform pane)
                                (if (plotter-xlog pane)
                                    (log10 (second x))
                                  (second x))
                                0))
      (#\P (second x))
      ))

   ((numberp x) ;; assume :data
    (list :data x))

   (t ;; else, expect a parsable symbol or string '1.2data+3pix
      (destructuring-bind (xtype x &optional (dx 0)) (parse-location x)
        (+ dx (get-x-location pane (list xtype x)))
        ))
   ))
  
(defun get-y-location (pane y)
  (cond
   ((and (consp y)
         (symbolp (first y)))
    (ecase (char-upcase (char (symbol-name (first y)) 0))
      ;; accommodates :DATA :DAT :D :DATUM, :FRAC :F :FRACTION, :PIXEL :P :PIX :PIXELS, etc.
      (#\F  (round (* (- 1.0 (second y))
                      (plotter-nominal-height pane)
                      )))
      (#\D  (multiple-value-bind (xx yx)
                (gp:transform-point (plotter-xform pane)
                                    0
                                    (if (plotter-ylog pane)
                                        (log10 (second y))
                                      (second y)))
              (declare (ignore xx))
              yx))
      (#\P (second y))
      ))
   
   ((numberp y) ;; assume :data
    (list :data y))

   (t ;; else, expect a parsable symbol or string '1.2data+3pix
      (destructuring-bind (ytype y &optional (dy 0)) (parse-location y)
        (+ dy (get-y-location pane (list ytype y)))
        ))
   ))

(defun draw-legend (pane)
  (let ((items (um:collector-contents (plotter-legend-info pane))))
    (when items
      (let* ((sf   (plotter-sf pane))
             (font (gp:find-best-font pane (gp:make-font-description
                                            :family "Times"
                                            :size   (* sf $tiny-times-font-size))
                                      ))
             (nitems (length items)))
        
        (multiple-value-bind (txtwd txtht txtbase)
            (let ((maxwd   0)
                  (maxht   0)
                  (maxbase 0))
              (loop for item in items do
                    (multiple-value-bind (lf tp rt bt)
                        (gp:get-string-extent pane (legend-info-text item) font)
                      (setf maxwd   (max maxwd (- rt lf))
                            maxht   (max maxht (- bt tp))
                            maxbase (max maxbase tp))))
              (values maxwd maxht maxbase))
          
          (declare (ignore txtbase))
          (let* ((totwd (+ txtwd  (* sf 40)))
                 (totht (* nitems (+ txtht 0)))
                 (effwd (/ totwd sf))
                 (effht (/ totht sf))
                 (effht1 (/ txtht sf))
                 (x     (get-x-location pane (plotter-legend-x pane)))
                 (y     (get-y-location pane (plotter-legend-y pane))))
            
            (gp:with-graphics-scale (pane sf sf)
              
              (gp:with-graphics-state (pane :foreground
                                            (color:make-rgb 1 1 1 0.65))
                (gp:draw-rectangle pane x y effwd effht
                                   :filled t))
              
              (gp:with-graphics-state (pane :thickness (round sf))
                (gp:draw-rectangle  pane x y effwd effht))
              
              (loop for item in items
                    for y from (+ y effht1) by effht1
                    do
                    (gp:with-graphics-state
                        (pane
                         :foreground (legend-info-color item)
                         :thickness  (legend-info-thick item)
                         :dashed     (not (null (legend-info-linedashing item)))
                         :dash       (legend-info-linedashing item))
                      
                      (when (or (null (legend-info-symbol item))
                                (eql  (legend-info-symbol item) :steps)
                                (legend-info-plot-joined item))
                        (let ((y (floor (- y (/ effht1 2)))))
                          (gp:draw-line pane
                                        (+ x 3)  y
                                        (+ x 33) y)
                          ))
                      
                      (when (and (legend-info-symbol item)
                                 (not (member (legend-info-symbol item)
                                              '(:steps :vbars :hbars))))
                        (funcall (get-symbol-plotfn pane (legend-info-symbol item))
                                 (+ x 18) (- y (/ effht1 2))
                                 )))
                    (gp:draw-string pane (legend-info-text item) (+ x 36) (- y 3)
                                    :font font))
              
              )))
        ))
    ))
                 
                 
                 
    
    
;; ----------------------------------------------------------------
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
  (let* ((pbox (or box 
                   (list 0 0 
                         (plotter-nominal-width  cpw)
                         (plotter-nominal-height cpw)
                         ))))
    (pw-init-xv-yv cpw xvector yvector xrange yrange pbox
                   :xlog xlog :ylog ylog :aspect aspect)
    (apply #'pw-axes cpw args)
    ;;
    ;; Now plot the data points
    ;; 
    (apply #'pw-plot-xv-yv cpw xvector yvector args)))

;; -------------------------------------------------------------------

(defmethod coerce-to-vector ((v vector))
  v)

(defmethod coerce-to-vector ((lst list))
  (coerce lst 'vector))

(defmethod coerce-to-vector ((cv c-arrays:<carray>))
  (c-arrays:convert-to-lisp-object cv))

;; -------------------------------------------------------------------

(defun draw-shape (shape x0 y0 x1 y1
                     &key
                     (color :darkgreen)
                     (filled t)
                     (alpha 1)
                     border-thick
                     (border-color :black)
                     (border-alpha 1)
                     pane
                     start-angle  ;; for arc
                     sweep-angle  ;; for arc
                     )
  (with-current-plotting-window (cpw pane)
    (let ((dlist (plotter-display-list cpw)))
      (um:with-locked-collector (dlist)
        (um:collector-append-item dlist
                                  #'(lambda (pane x y width height)
                                      (declare (ignore x y width height))
                                      (plt-draw-shape pane shape
                                                    x0 y0 x1 y1
                                                    :color  color
                                                    :alpha  alpha
                                                    :filled filled
                                                    :border-thick border-thick
                                                    :border-color border-color
                                                    :border-alpha border-alpha
                                                    :start-angle  start-angle
                                                    :sweep-angle  sweep-angle))
                                  )))
    (mark-dirty cpw)))

(defun draw-rect (&rest args)
  (apply #'draw-shape :rect args))

(defun draw-ellipse (&rest args)
  (apply #'draw-shape :ellipse args))

(defun draw-arc (&rest args)
  (apply #'draw-shape :arc args))

(defun oplot2 (xvector yvector 
                      &rest args
                      &key
                      draw-axes
                      alpha
                      (color (if draw-axes
                                 (color:make-rgb 0.0 0.5 0.0 alpha)
                               (color:make-rgb 1.0 0.0 0.0 alpha)))
                      thick
                      (linewidth (or thick 1))
                      pane
                      (fullgrid t)
                      &allow-other-keys)
  (let* ((yv (coerce-to-vector yvector))
         (xv (if xvector
                 (coerce-to-vector xvector)
               (vm:framp (length yv)))))
    (with-current-plotting-window (cpw pane)
      (let ((dlist (plotter-display-list cpw)))
        (um:with-locked-collector (dlist)
          (if (or draw-axes
                  (um:collector-empty-p dlist))
              (progn
                #|
                (setf (plotter-x-readout-hook cpw) #'identity
                      (plotter-y-readout-hook cpw) #'identity)
                |#
                (um:collector-discard-contents dlist)
                (um:collector-append-item dlist
                                          #'(lambda (pane x y width height)
                                              (declare (ignore x y width height))
                                              (apply #'do-plot pane xv yv
                                                     :color     color
                                                     :linewidth linewidth
                                                     :fullgrid  fullgrid
                                                     args))))
            (um:collector-append-item dlist
                                      #'(lambda (pane x y width height)
                                          (declare (ignore x y width height))
                                          (apply #'pw-plot-xv-yv pane xv yv 
                                                 :color color
                                                 args))
                                      ))))
      (mark-dirty cpw)
      )))

;; ------------------------------------------
(defun have-only-one-array? (args)
  (or (null args)
      (keywordp (first args))))

(defun plot (arr &rest args)
  (if (keywordp arr)
      (apply #'oplot2 nil nil :draw-axes t arr args)
    (let* ((only1 (have-only-one-array? args))
           (ys    (if only1 arr (first args)))
           (xs    (if only1 nil arr))
           (parms (if only1 args (rest args))))
      (apply #'oplot2 xs ys :draw-axes t parms))
    ))

(defun oplot (arr &rest args)
  (if (keywordp arr)
      (apply #'oplot2 nil nil arr args)
    (let* ((only1 (have-only-one-array? args))
           (ys    (if only1 arr (first args)))
           (xs    (if only1 nil arr))
           (parms (if only1 args (rest args))))
      (apply #'oplot2 xs ys parms))
    ))

;; ------------------------------------------
(defun axes (&rest args
                   &key xrange yrange pane
                   &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    #|
    (setf (plotter-x-readout-hook cpw) #'identity
          (plotter-y-readout-hook cpw) #'identity)
    |#
    (let ((dlist (plotter-display-list cpw)))
      (um:with-locked-collector (dlist)
        (um:collector-discard-contents dlist)
        (um:collector-append-item dlist
                                  #'(lambda (pane x y width height)
                                      (declare (ignore x y width height))
                                      (apply #'pw-init-xv-yv pane
                                             nil nil
                                             xrange yrange
                                             (list 0 0
                                                   (plotter-nominal-width  pane)
                                                   (plotter-nominal-height pane))
                                             args)
                                      (apply #'pw-axes pane args))
                                  )))
    (mark-dirty cpw)
    ))
                   
;; ------------------------------------------
;; these callbacks are only called from the capi process

(defun save-backing-image (pane)
  (with-accessors ((backing-image  plotter-backing       )
                   (sf             plotter-sf            )
                   (nominal-width  plotter-nominal-width )
                   (nominal-height plotter-nominal-height)) pane
    (setf backing-image
          (gp:make-image-from-port pane
                                 0 0
                                 (* sf nominal-width)
                                 (* sf nominal-height)))
    ))

(defun discard-backing-image (pane)
  (with-accessors ((backing-image  plotter-backing)) pane
    (when backing-image
      (gp:free-image pane backing-image)
      (setf backing-image nil))
    ))
   
(defun display-callback (pane x y width height)
  (with-accessors ((nominal-width   plotter-nominal-width )
                   (nominal-height  plotter-nominal-height)
                   (sf              plotter-sf            )
                   (magn            plotter-magn          )
                   (xform           plotter-xform         )
                   (port-width      gp:port-width         )
                   (port-height     gp:port-height        )
                   (display-list    plotter-display-list  )
                   (backing-image   plotter-backing       )
                   (delay-backing   plotter-delay-backing )) pane
    
    (unless nominal-width
      (setf nominal-width  width
            nominal-height height))
    
    (gp:clear-graphics-port-state pane)
    
    (setf xform '(1 0 0 1 0 0)
          magn  1
          sf    (min (/ port-height nominal-height)
                     (/ port-width  nominal-width)))


    #|
    (print (list x y width height sf
                 nominal-width port-width
                 nominal-height port-height))
    |#

    (if #+:COCOA backing-image
      #+:WIN32 (and backing-image
                    (= (gp:image-width backing-image)
                       (* sf nominal-width))
                    (= (gp:image-height backing-image)
                       (* sf nominal-height)))
      
        (gp:draw-image pane backing-image 0 0
                       :from-width  (gp:image-width  backing-image)
                       :from-height (gp:image-height backing-image)
                       :to-width    (* sf nominal-width)
                       :to-height   (* sf nominal-height))
      (progn
        (dolist (item (um:collector-contents display-list :discard nil))
          (funcall item pane x y width height))
        (draw-legend pane)

        (unless delay-backing
          (save-backing-image pane))
        ))
    ))
    
(defun resize-callback (pane x y width height)
  (declare (ignore x y width height))
  (with-accessors ((resize-timer  plotter-resize-timer)) pane
    (unless resize-timer
      (setf resize-timer
            (mp:make-timer
             (lambda ()
               (discard-backing-image pane)
               (capi:apply-in-pane-process pane
                                           #'gp:invalidate-rectangle
                                           pane)))
            ))
    (mp:schedule-timer-relative-milliseconds resize-timer 100)
    ))

(defun compute-x-y-at-cursor (pane x y)
  (with-accessors  ((sf         plotter-sf                 )
                    (magn       plotter-magn               )
                    (inv-xform  plotter-inv-xform          )
                    (xlog       plotter-xlog               )
                    (ylog       plotter-ylog               )
                    (x-readout-hook  plotter-x-readout-hook)
                    (y-readout-hook  plotter-y-readout-hook)) pane
    (let ((eff-sf (* sf magn)))
      (multiple-value-bind (xx yy)
          (gp:transform-point inv-xform (/ x eff-sf) (/ y eff-sf))
        (list (funcall x-readout-hook
                       (if xlog
                           (pow10 xx)
                         xx))
              (funcall y-readout-hook
                       (if ylog
                           (pow10 yy)
                         yy))
              ))
      )))
  
(defun mouse-move (pane x y &rest args)
  (declare (ignore args))
  (let ((intf (capi:top-level-interface pane)))
    (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
      (setf (capi:interface-title intf)
            (format nil "~A  x = ~,5g  y = ~,5g"
                    (plotter-base-title pane) xx yy))
      )))

(defun show-x-y-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
    (let ((xstr (format nil "~,5g" xx))
          (ystr (format nil "~,5g" yy)))
      (capi:display-tooltip pane
                            :x  (+ x 10)
                            :y  (+ y 10)
                            :text (format nil "(~A, ~A)"
                                          (string-trim " " xstr)
                                          (string-trim " " ystr))
                            ))))

(defun set-x-readout-hook (fn
                           &key pane)
  (with-current-plotting-window (cpw pane)
    (setf (plotter-x-readout-hook cpw) fn)))

(defun set-y-readout-hook (fn
                           &key pane)
  (with-current-plotting-window (cpw pane)
    (setf (plotter-y-readout-hook cpw) fn)))

;; -----------------------------------------------------------
(defun get-nominal-image (pane)
  ;; should only be called by the capi process
  (let* ((xpane (gp:create-pixmap-port pane
                                       (plotter-nominal-width pane)
                                       (plotter-nominal-height pane)))
         (sf (/ (plotter-sf pane))))
    (with-image (pane (img (gp:make-image-from-port pane)))
      (gp:with-graphics-scale (xpane sf sf)
        (gp:draw-image xpane img 0 0)
        ))
    (values xpane (gp:make-image-from-port xpane))))

(defmacro with-nominal-image ((pane img) &body body)
  ;; should only be used by functions called by the capi process
  (um:with-gensyms (xpane)
    `(multiple-value-bind (,xpane ,img)
         (get-nominal-image ,pane)
       ,@body
       (gp:free-image ,xpane ,img)
       (gp:destroy-pixmap-port ,xpane))
    ))

;; ----------------------------------------------------------
;;
(defun delay-backing-store (pane)
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (discard-backing-image pane)
                                (setf (plotter-delay-backing pane) t)
                                (gp:invalidate-rectangle pane))))

(defun undelay-backing-store (pane)
  (setf (plotter-delay-backing pane) nil))

(defmacro without-backing-store ((pane) &body body)
  `(progn
     (delay-backing-store ,pane)
     (unwind-protect
         (progn
           ,@body)
       (undelay-backing-store ,pane))
     ))

(defun save-image (file &key pane &allow-other-keys)
  ;; can be called from anywhere
  (let ((dest (or file
                  (capi:prompt-for-file
                   "Write Image to File"
                   :operation :save
                   :filter #+:COCOA "*.pdf"
                           #+:WIN32 "*.bmp"))))
    (when dest
      (with-current-plotting-window (cpw pane)
        (sync-with-capi-pane cpw
                             #+:COCOA
                             (lambda ()
                               (without-backing-store (cpw)
                                                      (save-pdf-plot cpw (namestring dest))))
                             #+:WIN32
                             (lambda ()
                               (with-nominal-image (cpw img)
                                 (let ((eimg (gp:externalize-image cpw img)))
                                   (gp:write-external-image eimg dest
                                                            :if-exists :supersede)
                                   )))
                             ))
      )))

(defun save-image-from-menu (pane &rest args)
  ;; called only in the pane's process
  (declare (ignore args))
  (let ((dest (capi:prompt-for-file
               "Write Image to File"
               :operation :save
               :filter #+:COCOA "*.pdf"
                       #+:WIN32 "*.bmp")))
    (when dest
      #+:COCOA (save-pdf-plot pane (namestring dest))
      #+:WIN32 (save-image (namestring dest) :pane pane))
    ))

(defun copy-image-to-clipboard (pane &rest args)
  ;; called only as a callback in the capi process
  (declare (ignore args))
  #+:COCOA
  (without-backing-store (pane)
    (copy-pdf-plot pane))
  #+:WIN32
  (with-nominal-image (pane img)
    (capi:set-clipboard pane nil nil (list :image img)))
  )

(defun print-plotter-pane (pane &rest args)
  (declare (ignore args))
  ;; executed in the process of the capi pane
  (capi:simple-print-port pane
                          :interactive t))

(defparameter *cross-cursor*
  (ignore-errors
    (capi:load-cursor
     '((:win32 "c:/projects/lib/crosshair.cur")
       (:cocoa "/usr/local/lib/crosshair.gif"
        :x-hot 7
        :y-hot 7))
     )))

;; ------------------------------------------
(defconstant $extra-width-needed
  #+:WIN32 4
  #+:COCOA 0)

(defconstant $extra-height-needed
  #+:WIN32 4
  #+:COCOA 0)

(capi:define-interface plotter-window ()
  ()
  (:panes (drawing-area <plotter-pane>
                        :display-callback 'display-callback
                        :resize-callback  'resize-callback
                        :input-model      '((:motion mouse-move)
                                            ((:button-1 :press) show-x-y-at-cursor)
                                            ((:gesture-spec "Control-c")
                                             copy-image-to-clipboard)
                                            ((:gesture-spec "Control-p")
                                             print-plotter-pane)
                                            ((:gesture-spec "Control-s")
                                             save-image-from-menu)
                                            )
                        :cursor   (or *cross-cursor*
                                      :crosshair)
                        :accessor drawing-area))
  
  (:layouts (default-layout
             capi:simple-layout
             '(drawing-area)))
  
  (:menus (pane-menu "Pane"
                     (("Copy"
                       :callback      'copy-image-to-clipboard
                       :accelerator   "accelerator-c")
                      ("Save as..."
                       :callback      'save-image-from-menu
                       :accelerator   "accelerator-s")
                      ("Print..."
                       :callback      'capi:simple-print-port
                       :accelerator   "accelerator-p"))
                     :callback-type :data
                     :callback-data  drawing-area))
  
  (:menu-bar pane-menu)
  
  (:default-initargs
   :layout              'default-layout
   :window-styles       '(:internal-borderless)
   :destroy-callback    #'(lambda (intf)
                            (with-pwins-lock
                              (setf *pwins* (remove intf *pwins*
                                                    :key #'window-rep-intf))))
   ))


(defun make-plotter-window (&key
                            (title      "Plot")
                            (fg         :black)
                            (bg         :white)
                            (foreground fg)
                            (background bg)
                            (xsize      400)
                            (ysize      300)
                            xpos
                            ypos
                            (best-width         (+ $extra-width-needed  xsize))
                            (best-height        (+ $extra-height-needed ysize))
                            (best-x             xpos)
                            (best-y             ypos)
                            (visible-min-width  (+ $extra-width-needed
                                                   (/ xsize 2)))
                            (visible-min-height (+ $extra-height-needed
                                                   (/ ysize 2)))
                            (visible-max-width  (+ $extra-width-needed
                                                   (* xsize 2)))
                            (visible-max-height (+ $extra-height-needed
                                                   (* ysize 2))))
  (let* ((intf (make-instance 'plotter-window
                              :title               title
                              :best-width          best-width
                              :best-height         best-height
                              :visible-min-width   visible-min-width
                              :visible-min-height  visible-min-height
                              :visible-max-width   visible-max-width
                              :visible-max-height  visible-max-height
                              :best-x              best-x
                              :best-y              best-y
                              :background          background
                              :foreground          foreground))
         (pane (drawing-area intf)))
    
    (setf (capi:simple-pane-background pane) background
          (capi:simple-pane-foreground pane) foreground
          (plotter-nominal-width  pane)      (- best-width $extra-width-needed)
          (plotter-nominal-height pane)      (- best-height $extra-height-needed)
          (plotter-base-title     pane)      title)

    intf))

;; ------------------------------------------
(defun window (name &rest args &key
                    (title      (format nil "Plotter:~A" name))
                    (background #.(color:make-gray 1))
                    (foreground #.(color:make-gray 0))
                    (xsize      400)
                    (ysize      300)
                    xpos
                    ypos
                    (best-width         (+ $extra-width-needed  xsize))
                    (best-height        (+ $extra-height-needed ysize))
                    (best-x             xpos)
                    (best-y             ypos)
                    (visible-min-width  (+ $extra-width-needed  (/ xsize 2)))
                    (visible-min-height (+ $extra-height-needed (/ ysize 2)))
                    (visible-max-width  (+ $extra-width-needed  (* xsize 2)))
                    (visible-max-height (+ $extra-height-needed (* ysize 2))))

  (when (or args
            (not (find-named-window-rep name)))
    (let* ((intf (make-plotter-window
                  :title               title
                  :best-width          best-width
                  :best-height         best-height
                  :visible-min-width   visible-min-width
                  :visible-min-height  visible-min-height
                  :visible-max-width   visible-max-width
                  :visible-max-height  visible-max-height
                  :best-x              best-x
                  :best-y              best-y
                  :background          background
                  :foreground          foreground)))
      
      (wclose name)
      (with-pwins-lock
        (push (make-instance 
               '<window-rep>
               :name name
               :intf intf
               :pane (drawing-area intf))
              *pwins*))
      (capi:display intf)
      )))

;; ------------------------------------------
(defun wset (name)
  ;; if window exists then make it the current plotting window
  ;; if window does not exist then create it with default parameters
  (um:if-let (rep (find-named-window-rep name))
             (with-pwins-lock
               (setf *pwins* (cons rep (remove rep *pwins*))))
             (window name)))

;; ------------------------------------------
(defun wshow (name)
  ;; if window exists then raise it to the top,
  ;; but don't make it the current plotting window
  ;; if window does not exist then create it with default parameters
  (um:if-let (rep (find-named-window-rep name))
             (let ((intf (window-rep-intf rep)))
               (sync-with-capi-interface intf
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
      (sync-with-capi-interface intf
                                (lambda ()
                                  (capi:destroy intf))
                                ))
    ))

;; ------------------------------------------
(defun werase (&key pane)
  (with-current-plotting-window (cpw pane)
    (um:collector-discard-contents (plotter-display-list cpw))
    (mark-dirty cpw)
    ))

;; ------------------------------------------
(defun outsxy (x y str
                  &rest args
                  &key
                  (font-size 12)
                  (font "Times")
                  anchor
                  (align :w)
                  pane
                  (offset-x 0) ;; pixel offsets
                  (offset-y 0)
                  clip
                  (color :black)
                  alpha
                  &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (um:collector-append-item
     (plotter-display-list cpw)
     #'(lambda (pane xarg yarg width height)
         (declare (ignore xarg yarg width height))
         (let* ((x (+ offset-x (get-x-location pane x)))
                (y (+ offset-y (get-y-location pane y)))
                (font (gp:find-best-font pane
                                         (gp:make-font-description
                                          :family font
                                          :size (* font-size (plotter-sf pane)))
                                         ))
                (x-align (ecase (or anchor align)
                           ((:nw :w :sw) :left)
                           ((:n :s :ctr) :center)
                           ((:ne :e :se) :right)))
                (y-align (ecase (or anchor align)
                           ((:nw :n :ne) :top)
                           ((:w :ctr :e) :center)
                           ((:sw :s :se) :baseline)))
                (sf   (plotter-sf pane))
                (mask (and clip
                           (adjust-box
                            (mapcar (um:curry #'* sf)
                                    (plotter-box pane))
                            )))
                (color (adjust-color pane color alpha)))
           #+:WIN32
           (gp:with-graphics-state (pane
                                    :mask mask)
             (apply #'draw-string-x-y pane str
                    (* sf x) (* sf y)
                    :font font
                    :x-alignment x-align
                    :y-alignment y-align
                    :color       color
                    args))
           #+:COCOA
           (let* ((font-attrs (gp:font-description-attributes (gp:font-description font)))
                  (font-name  (getf font-attrs :name))
                  (font-size  (getf font-attrs :size)))
             (apply #'add-label pane str (* sf x) (* sf y)
                    :font        font-name
                    :font-size   font-size
                    :color       color
                    :x-alignment x-align
                    :y-alignment y-align
                    :box         mask
                    args))
           
           )))
    (mark-dirty cpw)
    ))

;; ---------------------------------------------------------
;; org can be a list of (type xorg yorg), e.g., '(:frac 0.9 0.96)
;; or a pair of typed values ((type xorg) (type yorg)), e.g., '((:frac 0.9) (:data 14.3))
;;
;; convert to a list of typed pairs, e.g., '((:frac 0.9) (:data 14.3))
;;
(defun get-xy-orgs (org)
  (if (= 3 (length org))
      (list (list (first org) (second org))
            (list (first org) (third org)))
    org))

(defun draw-text (str org &rest args)
  (destructuring-bind (xorg yorg) (get-xy-orgs org)
    (apply #'outsxy xorg yorg str args)))

;; ------------------------------------------
(defun plot-histogram (v &rest args
                         &key min max range nbins binwidth
                         ylog cum norm over
                         (symbol :steps)
                         &allow-other-keys)
  (multiple-value-bind (x h)
      (vm:histogram v
                    :min      min
                    :max      max
                    :range    range
                    :nbins    nbins
                    :binwidth binwidth)
    (let (tot minnz)
      (when norm
        (setf tot (vm:total h))
        (loop for v across h
              for ix from 0
              do
              (setf (aref h ix) (/ v tot))
              ))
      (when cum
        (loop for v across h
              for ix from 0
              for sum = v then (+ sum v)
              do
              (setf (aref h ix) sum)
              (unless (or minnz
                          (zerop sum))
                (setf minnz sum))
              ))
      (when ylog
        (let ((zlim (cond (cum  minnz)
                          (norm (/ 0.9 tot))
                          (t     0.9)
                          )))
          (loop for v across h
                for ix from 0
                do
                (when (zerop v)
                  (setf (aref h ix) zlim)))
          ))
      (apply (if over #'oplot #'plot) x h :symbol symbol args)
      )))

;; -----------------------------------------------------------------
;; Functional plotting with adaptive gridding
;; DM/RAL 12/06
;; ----------------------------------------------------------------------------------------
;; Parametric Plotting with adaptive gridding
;;
(defun do-param-plotting (plotfn cpw xfn yfn npts args)
  (destructuring-bind (tmin tmax) (plotter-trange cpw)
    (destructuring-bind (tprepfn itprepfn) (plotter-tprepfns cpw)
      (declare (ignore tprepfn))
      (destructuring-bind (xprepfn ixprepfn) (plotter-xprepfns cpw)
        (destructuring-bind (yprepfn iyprepfn) (plotter-yprepfns cpw)
          (let* ((xsf (plotter-xsf cpw))
                 (ysf (plotter-ysf cpw))
                 (ts  (if npts
                          (loop for ix from 0 to npts collect
                                (+ tmin (* ix (/ (- tmax tmin) npts))))
                        (loop for ix from 0 to 16 collect
                              (+ tmin (* ix 0.0625 (- tmax tmin))))))
                 (xfn (um:compose xprepfn xfn itprepfn))
                 (yfn (um:compose yprepfn yfn itprepfn))
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
                                   (split-interval 0 t0 t1 x0 x1 y0 y1
                                                   new-xs new-ys)
                                 
                                 (iter-points (rest ts) (rest xs) (rest ys)
                                              new-xs new-ys)
                                 ))))
                         )))

              (destructuring-bind (xs ys)
                  (if npts
                      (list xs ys)
                    (iter-points ts xs ys nil nil))
                (let ((xs (if ixprepfn (mapcar ixprepfn xs) xs))
                      (ys (if iyprepfn (mapcar iyprepfn ys) ys)))
                  (apply plotfn xs ys args)
                  (values (length xs) xs ys)
                  ))
              )))
        ))))

;; ------------------------------------------
(defun paramplot (domain xfn yfn &rest args
                         &key over tlog xlog ylog xrange yrange pane npts
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
                     (xs (mapcar (um:compose xfn itprepfn) ts))
                     (ys (mapcar (um:compose yfn itprepfn) ts)))
                (destructuring-bind (xmin xmax) (get-minmax
                                                 (list (reduce #'min xs)
                                                       (reduce #'max xs))
                                                 (or xrange '(0 0)) xprepfn)
                  (destructuring-bind (ymin ymax) (get-minmax
                                                   (list (reduce #'min ys)
                                                         (reduce #'max ys))
                                                   (or yrange '(0 0)) yprepfn)
                    (setf (plotter-trange cpw)   (list tmin tmax)
                          (plotter-xsf cpw)      (qdiv (plotter-nominal-width cpw)
                                                       (- xmax xmin))
                          (plotter-ysf cpw)      (qdiv (plotter-nominal-height cpw)
                                                       (- ymax ymin))
                          (plotter-tprepfns cpw) (list tprepfn itprepfn)
                          (plotter-xprepfns cpw) (list xprepfn (and xlog ixprepfn))
                          (plotter-yprepfns cpw) (list yprepfn (and ylog iyprepfn)))
                    (do-param-plotting (if over #'oplot #'plot) cpw xfn yfn npts args)
                    )))
              )))
        ))))

(defun oparamplot (xfn yfn &rest args
                       &key pane npts &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (do-param-plotting #'oplot cpw xfn yfn npts args)))

(defun fplot (domain fn &rest args)
  (apply #'paramplot domain #'identity fn args))

(defun ofplot (fn &rest args)
  (apply #'oparamplot #'identity fn args))

;; ------------------------------------------
(defparameter *gray-colormap*
  (let ((map (make-array 256)))
    (loop for ix from 0 to 255 do
          (setf (aref map ix) (color:make-gray (/ (float ix) 255.0))
                ))
    map))

(defparameter *heat-colormap*
  (let ((map (make-array 256)))
    (labels ((clip (v)
               (max 0.0 (min 1.0 (/ v 255)))))
      (loop for ix from 0 to 255 do
            (setf (aref map ix)
                  (color:make-rgb
                   (clip (/ (* 255 ix) 176))
                   (clip (/ (* 255 (- ix 120)) 135))
                   (clip (/ (* 255 (- ix 190)) 65)))
                  )))
    map))

(defparameter *current-colormap* *heat-colormap*) ;;*gray-colormap*

(defparameter *tst-img*
  (let ((img (make-array '(64 64))))
    (loop for row from 0 below 64 do
          (loop for col from 0 below 64 do
                (setf (aref img row col) (* row col))
                ))
    img))

(defun tvscl (arr
              &key pane (magn 1)
              &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (um:collector-append-item
     (plotter-display-list cpw)
     #'(lambda (pane x y width height)
         (declare (ignore x y width height))
         (let* ((wd   (array-dimension arr 1))
                (ht   (array-dimension arr 0))
                (varr (make-array (array-total-size arr)
                                  :displaced-to arr))
                (mn   (reduce #'min varr))
                (mx   (reduce #'max varr))
                (sf   (/ 255 (- mx mn))))
           
           (with-image (pane (img #+:COCOA (gp:make-image pane wd ht)
                                  #+:WIN32 (gp:make-image pane wd ht :alpha nil)
                                  ))
             (with-image-access (acc (gp:make-image-access pane img))
               (loop for row from 0 below ht do
                     (loop for col from 0 below wd do
                           (setf (gp:image-access-pixel acc row col)
                                 (color:convert-color
                                  cpw
                                  (aref *current-colormap*
                                        (round (* sf (- (aref arr row col) mn)))))
                                 )))
               (gp:image-access-transfer-to-image acc))
             
             (let ((sf (* magn (plotter-sf pane))))
               (gp:with-graphics-scale (pane sf sf)
                 (gp:draw-image pane img 0 0))
               ))
           (setf (plotter-magn pane) magn)
           )))
    (mark-dirty cpw)
    ))

(defun render-image (ext-img
                     &key
                     pane
                     (magn 1)
                     (to-x 0)
                     (to-y 0)
                     (from-x 0)
                     (from-y 0)
                     to-width
                     to-height
                     from-width
                     from-height
                     transform
                     global-alpha
                     &allow-other-keys)
  (with-current-plotting-window (cpw pane)
    (um:collector-append-item
     (plotter-display-list cpw)
     #'(lambda (pane x y wd ht)
         (declare (ignore x y wd ht))
         (let ((sf (* magn (plotter-sf pane))))
           (with-image (pane (img (gp:convert-external-image pane ext-img)))
             (gp:with-graphics-scale (pane sf sf)
               (gp:draw-image pane img to-x to-y
                              :transform    transform
                              :from-x       from-x
                              :from-y       from-y
                              :to-width     to-width
                              :to-height    to-height
                              :from-width   from-width
                              :from-height  from-height
                              :global-alpha global-alpha)
               ))
           (setf (plotter-magn pane) magn)
           )))
    (mark-dirty cpw)
    ))

(defun read-image (&optional file)
  (gp:read-external-image (or file
                              (capi:prompt-for-file
                               "Select Image File"))
                          ))

(defun dump-hex (arr &key (nlines 10))
  (loop for ix from 0 below (array-total-size arr) by 16
        for line from 0 below nlines
        do
        (format t "~%~4,'0x: ~{~{~2,'0x ~} ~} ~A"
                ix
                (loop for jx from 0 below 16 by 4
                      collect
                      (coerce (subseq arr (+ ix jx) (+ ix jx 4)) 'list))
                (let ((s (make-string 16)))
                  (loop for jx from 0 below 16 do
                        (setf (aref s jx)
                              (let ((v (code-char (aref arr (+ ix jx)))))
                                (if (graphic-char-p v)
                                    v
                                  #\.))
                              ))
                  s))
        ))

(defun sinc (x)
  (/ (sin x) x))

#|
(window 2)
(fplot '(0.001 10) (lambda (x) (/ (sin x) x)))
(tvscl *tst-img* :magn 4)
|#

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
|#


;; *eof* ;;

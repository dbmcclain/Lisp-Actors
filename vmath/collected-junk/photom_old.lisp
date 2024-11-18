;; photom.lisp -- Interactive stellar photometry
;; DM/MCFA  12/99
;;

(defpackage "PHOTOMETRY"
  (:use "USEFUL-MACROS" "COMMON-LISP")
  (:nicknames "PHOT")
  (:export
   "PHOTOM"))

(in-package "PHOTOMETRY")

;; set the colormap to grays
(defun init-colormap ()
  (let ((r (vm:iramp 256)))
    (sg:set-cmap r r r)))

(capi:define-interface interface-1 ()
  ((image  :accessor interface-image :initform nil)
   (killch :accessor killch          :initarg :killch))
  (:panes

   (image-display
    capi:output-pane
    :accessor image-display
    :cursor :crosshair
    :input-model
    '((#\control-\c    copy-to-clipboard)
      (#\left          move-left 1)
      (#\right         move-right 1)
      (#\up            move-up 1)
      (#\down          move-down 1)
      (#\control-left  move-left 10)
      (#\control-right move-right 10)
      (#\control-up    move-up 10)
      (#\control-down  move-down 10)
      (#\z             set-ref-magnitude-from-kbd)
      (:motion         show-position)
      ((:button-3 :press)     set-ref-magnitude)
      ;((:button-1 :motion)   redraw-crosshairs)
      ;((:button-1 :release)  undraw-crosshairs)
      )
    :display-callback 'redisplay-image
    :min-width 640
    :min-height 480)

   (color-bar-display
    capi:output-pane
    :accessor color-bar-display
    :display-callback 'redraw-color-bar
    :min-width 9
    :max-width 9
    :min-height 256
    :max-height 256)
   
   (magn-image-display
    capi:output-pane
    :accessor magn-image-display
    ; :cursor :arrow
    :min-width (* 11 8)
    :max-width (* 11 8)
    :min-height (* 11 8)
    :max-height (* 11 8)
    :display-callback 'draw-magn-frame-fiducials)

   (x-display-title
    capi:title-pane
    :accessor x-display-title
    :text "X Position")
   (x-display
    capi:title-pane
    :accessor x-display
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (y-display-title
    capi:title-pane
    :accessor y-display-title
    :text "Y Position")
   (y-display
    capi:title-pane
    :accessor y-display
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (z-display-title
    capi:title-pane
    :accessor z-display-title
    :text "Z Value")
   (z-display
    capi:title-pane
    :accessor z-display
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (bg-display-title
    capi:title-pane
    :accessor bg-display-title
    :text "BG Median")
   (bg-display
    capi:title-pane
    :accessor bg-display
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (m-display-title
    capi:title-pane
    :accessor m-display-title
    :text "Magnitude")
   (m-display
    capi:title-pane
    :accessor m-display
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (offset-slider
    capi:slider
    :accessor offset-slider
    :start 0
    :end 500
    :slug-start 0
    :min-width 200
    :max-width 200
    :callback 'slider-refresh-image
    :title "Offset")

   (range-slider
    capi:slider
    :accessor range-slider
    :start 0
    :end 500
    :slug-start 500
    :min-width 200
    :max-width 200
    :callback 'slider-refresh-image
    :title "Range")

   (neg-button
    capi:check-button
    :accessor neg-button
    :text "Negative"
    :selection-callback 'refresh-image
    :retract-callback 'refresh-image
    :min-width 80
    :max-width 80
    :enabled t)

   (logscale-button
    capi:check-button
    :accessor logscale-button
    :selection-callback 'refresh-image
    :retract-callback 'refresh-image
    :text "Log Scale"
    :min-width 80
    :max-width 80
    :enabled t)
   )
  (:layouts
   (row-layout-1
    capi:row-layout
    '(image-display
      color-bar-display
      column-layout-1))
   (column-layout-1
    capi:column-layout
    '(row-layout-3   ;; magn image + readouts
      row-layout-4   ;; check boxes
      offset-slider
      range-slider
      ))
   (row-layout-3
    capi:row-layout
    '(magn-image-display
      column-layout-2
      column-layout-3))
   (column-layout-2
    capi:column-layout
    '(x-display-title
      y-display-title
      z-display-title
      bg-display-title
      m-display-title))
   (column-layout-3
    capi:column-layout
    '(x-display
      y-display
      z-display
      bg-display
      m-display))
   (row-layout-4
    capi:row-layout
    '(neg-button
      logscale-button)))

  (:menu-bar menu-1)
  (:menus
   (menu-1
    "File"
    (("Open Image..." :selection-callback 'get-image)
     ("Exit"          :selection-callback 'kill-interface))))

  (:default-initargs
   :layout 'row-layout-1
   :title "Stellar Photometry"
   :destroy-callback #'(lambda (intf)
			 (let ((ch (killch intf)))
			   (if ch
			       (rch:send ch t))))
   ))

(defvar *bullseye-cursor* nil)

(defun init-cursor ()
  (unless *bullseye-cursor*
    (setf *bullseye-cursor*
	  (sg:load-cursor-from-file "bullseye.cur"))))

(defun doittoit (ch)
  (init-colormap)
  (init-cursor)
  (let* ((intf (capi:display
		(make-instance 'interface-1
			       :killch ch)))
         (pane (image-display intf))
	 (repr  (slot-value pane 'capi-internals:representation)))
    (setf (slot-value repr 'capi-win32-lib::cursor) *bullseye-cursor*)
    intf))

(defun doittoit-t ()
  (let ((ch (rch:make-channel)))
    (doittoit ch)
    (rch:recv ch)
    (lw:quit :status 0)))

(defun doittoit-f ()
  (doittoit nil))

(defun photom ()
  (if (mp:list-all-processes)
      (doittoit-f)
      (progn
	(push '("Photom Launcher" nil doittoit-t) mp:*initial-processes*)
	(mp:initialize-multiprocessing))))
    
(defun kill-interface (data intf)
  (declare (ignore data))
  (capi:destroy intf))

(defun copy-to-clipboard (&rest args)
  (declare (ignore args))
  (sg:copy-graphic-to-clipboard)
  (capi:display-message "image captured"))

#|
(defun draw-crosshairs (pane x y)
  (gp:with-graphics-state (pane :operation boole-xor
                                :foreground :white)
    (gp:draw-line pane -16000 y (- x 5) y)
    (gp:draw-line pane (+ x 5) y 16000 y)
    (gp:draw-line pane x -16000 x (- y 5))
    (gp:draw-line pane x (+ y 5) x 16000)
    (gp:draw-circle pane x y 10)))

(defvar *lastx* 0)
(defvar *lasty* 0)
  
(defun undraw-crosshairs (pane x y)
  (declare (ignore x y))
  (when *lastx*
    (draw-crosshairs pane *lastx* *lasty*)
    (setf *lastx* nil *lasty* nil)))

(defun redraw-crosshairs (pane x y)
  (undraw-crosshairs pane *lastx* *lasty*)
  (draw-crosshairs pane x y)
  (setf *lastx* x *lasty* y)
  (show-position pane x y))
|#

(defclass <image-array-x> (img:<image-array>)
  ((tint :accessor integration-time   :initarg  :tint)))

(defun make-image-x (arr tint)
  (if (= (array-rank arr) 2)
      (make-instance '<image-array-x>
		     :arena arr
                     :tint  (or tint 1.0))
    (error "flat 2-D array required")))
                 
(defmethod img:make-similar ((a <image-array-x>) arr)
  (make-instance '<image-array-x>
		 :arena arr
                 :tint  (integration-time a)))

(defun set-window-title (intf str)
  (setf (capi:interface-title intf) str))

(defun set-interface-image (intf img)
  (setf (interface-image intf) img))

(defun set-slider-bounds (intf img)
  (let* ((pcs   (vm:percentiles (img:image-array-arena img)))
         (pc01  (truncate (getf pcs :pc01)))
         (pc99  (max (truncate (getf pcs :pc99))
                     (+ pc01 500)))
         (range (* 2 (- pc99 pc01)))
         (offset-slider (offset-slider intf))
         (range-slider  (range-slider intf)))
    (setf (capi:range-end range-slider)  range
          (capi:range-end offset-slider) pc99
          (capi:range-slug-start offset-slider) pc01
          (capi:range-slug-start range-slider)  range)))

(defmethod float-value (v &optional (default 1.0))
  (declare (ignore v))
  default)

(defmethod float-value ((v number) &optional default)
  (declare (ignore default))
  (float v))

(defmethod float-value ((v string) &optional default)
  (declare (ignore default))
  (float (read-from-string v)))

(defun get-image (data intf)
  (handler-case
      (multiple-value-bind (img attrs fname)
          (scids:getvar :attrs '("exptime"))
        (let* ((tint (float-value (first attrs)))
               (ximg (make-image-x img tint)))
          (set-interface-image intf (img:flipv ximg))
          (set-window-title intf (namestring fname))
          (set-slider-bounds intf ximg)
          (refresh-image data intf)))
    (scids:USER-CANCEL ())))

(defun make-color-bar (&key reversed)
  (let ((cdata (make-array '(256 9)
                           :element-type 'integer)))
    (if reversed
        (dotimes (iy 256)
          (dotimes (ix 9)
            (setf (aref cdata iy ix) (- 255 iy))))
      (dotimes (iy 256)
        (dotimes (ix 9)
          (setf (aref cdata iy ix) iy))))
    cdata))

(defun redraw-color-bar (pane &rest args)
  (declare (ignore args))
  (sg:direct-redraw pane))

(defun draw-color-bar (intf)
  (let ((wpane (color-bar-display intf)))
    (sg:wset wpane)
    (let ((neg-img (capi:button-selected (neg-button intf))))
      (if neg-img
          (sg:tvscl (make-color-bar :reversed t))
        (sg:tvscl (make-color-bar))))))

(defvar *magnification*  2)

(defun draw-image-display (intf img)
  (let ((wpane           (image-display intf))
        (offs-pane       (offset-slider intf))
        (range-pane      (range-slider intf))
        (negate-button   (neg-button intf))
        (logscale-button (logscale-button intf)))
    (sg:wset wpane)
    (let* ((display-min (capi:range-slug-start offs-pane))
           (display-max (+ display-min
                           (capi:range-slug-start range-pane)))
           (neg-img     (capi:button-selected negate-button))
           (use-log     (capi:button-selected logscale-button)))
      (if use-log
          (show-log-stretch img
                            display-min
                            display-max
                            neg-img)
        (show-linear-stretch img
                             display-min
                             display-max
                             neg-img)))
    ))

(defun show-log-stretch (img minv maxv neg)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (img:tvscl (img:pixelwise (img)
			    (log (max 1 (- img minv))))
	     :range (list 0 (log (max (- maxv minv) 1)))
	     :magn  *magnification*
	     :neg   neg))

(defun show-linear-stretch (img minv maxv neg)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (img:tvscl img
	     :range (list minv maxv)
	     :magn  *magnification*
	     :neg   neg))

(defun slider-refresh-image (slider val action)
  (when (eq action :move)
    (refresh-image val (slot-value slider 'capi:interface))))

(defun refresh-image (data intf)
  (declare (ignore data))
  (let ((img (interface-image intf)))
    (when img
      (draw-color-bar intf)
      (draw-image-display intf img))))

(defun redisplay-image (pane &rest args)
  (declare (ignore args))
  (sg:direct-redraw pane))

(defun show-magnified-selection (intf img x y med)
  (let* ((pane  (magn-image-display intf))
         (ximg  (img:subimage-centered img (list y x) (list 11 11)))
	 ;;(negate-button   (neg-button intf))
	 ;;(neg-img     (capi:button-selected negate-button))
         (arr   (img:image-array-arena ximg)))
    (sg:wset pane)
    (let ((v (aref arr 5 5)))
      (img:tvscl ximg
                 :magn 8
                 :range (list (reduce #'min (vm:vector-of arr))
                              (max v (+ med 100)))
		 :neg nil) ;; neg-img)
      )))

(defun draw-magn-frame-fiducials (pane x y width height)
  (declare (ignore x y width height))
  (sg:direct-redraw pane)
  (gp:with-graphics-state (pane :operation boole-1
                                :foreground :red)
    (gp:draw-line pane 0 43 38 43)
    (gp:draw-line pane 48 43 87 43)
    (gp:draw-line pane 43 0 43 38)
    (gp:draw-line pane 43 48 43 87)
    (gp:draw-rectangle pane 23 23 40 40)
    (gp:draw-circle pane 43 43 10)))

(defparameter *wmask*
  (um:where #'(lambda (v)
                (> v 19))
            (vm:shifth (vm:dist 41 41))))

(defvar *ref-magn*  (+ 14.31 -0.84 0.89 -1.83 0.12))

(defun set-bgmed-display (intf med)
  (setf (capi:title-pane-text (bg-display intf))
        (format nil "~D" (round med))))

(defun compute-magnitude (x y img intf)
  (let* ((ximg (img:subimage-centered img (list y x) (list 5 5)))
         (simg (img:subimage-centered img (list y x)
                                  (list 41 41)))
         (moat (um:subselect (img:image-array-arena simg) *wmask*))
         (med  (vm:median moat))
         (tot  (img:foldl #'(lambda (rslt v)
                          (+ rslt (- v med)))
                      0.0 (vm:vector-of (img:image-array-arena ximg)))))
    (show-magnified-selection intf img x y med)
    (set-bgmed-display intf med)
    (if (plusp tot)
        (+ (* -2.5 (- (log tot 10)
                      (log (integration-time img) 10)))
           *ref-magn*)
      99)))

(defparameter *lastx* 0)
(defparameter *lasty* 0)

(defun set-ref-magnitude-from-kbd (pane &rest args)
  (declare (ignore args))
  (set-ref-magnitude pane *lastx* *lasty*))

(defun cvt-display-to-image-coords (img x y)
  (let* ((a    (img:image-array-arena img))
         (ylim (1- (array-dimension a 0))))
    (values (truncate x *magnification*)
            (- ylim (truncate y *magnification*)))
    ))
  
(defun set-ref-magnitude (pane x y)
  (setf *lastx* x
        *lasty* y)
  (let* ((intf (capi:element-interface pane))
         (img  (interface-image intf)))
    (when img
      (multiple-value-bind (x y) (cvt-display-to-image-coords img x y)
        (let* ((magn      (compute-magnitude x y img intf))
               (user-magn (capi:prompt-for-string
                           "Enter reference magnitude"
                           :initial-value (format nil "~,2F" magn))))
          (when user-magn
            (setf *ref-magn* (+ *ref-magn*
                                (- (float-value user-magn) magn))))
          )))))

(defun set-x-display (intf x)
  (setf (capi:title-pane-text (x-display intf))
        (format nil "~D" x)))

(defun set-y-display (intf y)
  (setf (capi:title-pane-text (y-display intf))
        (format nil "~D" y)))

(defun set-z-display (intf z)
  (setf (capi:title-pane-text (z-display intf))
        (format nil "~D" (round z))))

(defun set-m-display (intf m)
  (setf (capi:title-pane-text (m-display intf))
        (format nil "~,2F" m)))

(defun clear-z-display (intf)
  (setf (capi:title-pane-text (z-display intf)) ""))

(defun clear-m-display (intf)
  (setf (capi:title-pane-text (m-display intf)) ""))

(defun show-position (pane x y)
  (setf *lastx* x
        *lasty* y)
  (let* ((intf (capi:element-interface pane))
         (img  (interface-image intf)))
    (when img
      (let* ((a     (img:image-array-arena img))
             (xlim  (1- (array-dimension a 1)))
             (ylim  (1- (array-dimension a 0))))
        (multiple-value-bind (x y) (cvt-display-to-image-coords img x y)
          (set-x-display intf x)
          (set-y-display intf y)
          (if (and (>= x 0)
                   (<= x xlim)
                   (>= y 0)
                   (<= y ylim))
              (progn
                (set-z-display intf (aref a y x))
                (set-m-display intf (compute-magnitude x y img intf)))
            (progn
              (clear-z-display intf)
              (clear-m-display intf)))
          )))))

(defun adjust-cursor (dx dy)
  (fli:with-dynamic-foreign-objects ()
    (let ((pos (fli:allocate-dynamic-foreign-object
                :type :long
                :nelems 2)))
      (win32:get-cursor-pos pos)
      (win32:set-cursor-pos (+ dx (fli:dereference pos :index 0))
                            (+ dy (fli:dereference pos :index 1)))
      )))

(labels
    ((move-cursor (pane x y dx dy)
                  (let ((dx (* dx *magnification*))
                        (dy (* dy *magnification*)))
                    (show-position pane (+ x dx) (+ y dy))
                    (adjust-cursor dx dy))))
#|  
(defun move-cursor (pane x y dx dy)
  (let ((dx (* dx *magnification*))
        (dy (* dy *magnification*)))
    (show-position pane (+ x dx) (+ y dy))
    (adjust-cursor dx dy)))
|#
  
  (defun move-up (pane x y key data)
    (declare (ignore key))
    (move-cursor pane x y 0 (- data)))
  
  (defun move-down (pane x y key data)
    (declare (ignore key))
    (move-cursor pane x y 0 data))
  
  (defun move-left (pane x y key data)
    (declare (ignore key))
    (move-cursor pane x y (- data) 0))
  
  (defun move-right (pane x y key data)
    (declare (ignore key))
    (move-cursor pane x y data 0)))

  
;; ----------------------------------------------------------

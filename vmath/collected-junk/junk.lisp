;;
;; test attached windows with SciGraphics
;;

(let ((r (vm:iramp 256)))
  (sg:set-cmap r r r))

(capi:define-interface interface-1 ()
  ((image :accessor interface-image :initform nil))
  (:panes

   (image-display
    capi:output-pane
    :cursor :crosshair
    :input-model
    '((#\control-\c copy-to-clipboard)
      (:motion  show-position)
      ((:button-3 :press)     set-ref-magnitude)
      ;((:button-1 :motion)   redraw-crosshairs)
      ;((:button-1 :release)  undraw-crosshairs)
      )
    :display-callback 'redisplay-image
    :min-width 640
    :min-height 480)

   (color-bar-display
    capi:output-pane
    :display-callback 'redraw-color-bar
    :min-width 9
    :max-width 9
    :min-height 256
    :max-height 256)
   
   (magn-image-display
    capi:output-pane
    :cursor :arrow
    :min-width (* 11 8)
    :max-width (* 11 8)
    :min-height (* 11 8)
    :max-height (* 11 8)
    :display-callback 'draw-magn-frame-fiducials)
#|
   (get-image-button
    capi:push-button
    :text "Get Image"
    :selection-callback 'get-image
    :min-width 80
    :max-width 80)
|#
   (refresh-button
    capi:push-button
    :text "Refresh"
    :selection-callback 'refresh-image
    :min-width 80
    :max-width 80
    :enabled nil)
   
   (x-display-title
    capi:title-pane
    :text "X Position")
   (y-display-title
    capi:title-pane
    :text "Y Position")
   (z-display-title
    capi:title-pane
    :text "Z Value")
   (bg-display-title
    capi:title-pane
    :text "BG Median")
   (m-display-title
    capi:title-pane
    :text "Magnitude")

   (x-display
    capi:title-pane
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)
   (y-display
    capi:title-pane
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)
   (z-display
    capi:title-pane
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)
   (bg-display
    capi:title-pane
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)
   (m-display
    capi:title-pane
    :foreground :white
    :background :gray50
    :min-width 50
    :max-width 50)

   (offset-slider
    capi:slider
    :start 0
    :end 500
    :slug-start 0
    :min-width 200
    :max-width 200
    :scroll-callback 'refresh-image
    :title "Offset")

   (range-slider
    capi:slider
    :start 0
    :end 500
    :slug-start 500
    :min-width 200
    :max-width 200
    :scroll-callback 'refresh-image
    :title "Range")

   (neg-button
    capi:check-button
    :text "Negative"
    :selection-callback 'refresh-image
    :min-width 80
    :max-width 80
    :enabled t)

   (logscale-button
    capi:check-button
    :text "Log Scale"
    :min-width 80
    :max-width 80
    :enabled t)
#|
   (close-button
    capi:push-button
    :text "Close"
    :min-width 80
    :max-width 80
    :selection-callback 'close-viewer
    :enabled t)
|#
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
      refresh-button
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
    ("Open Image...")
     :selection-callback 'get-image))

  (:default-initargs
   :layout 'row-layout-1
   :title "NML in CAPI"))

(defvar *bullseye-cursor*
  (sg:load-cursor-from-file "bullseye.cur"))

(defun image-viewer ()
  (let* ((intf (capi:display
                (make-instance 'interface-1)))
         (pane  (slot-value intf 'image-display))
         (repr  (slot-value pane 'capi-internals:representation)))
    (setf (slot-value repr 'capi-win32-lib::cursor) *bullseye-cursor*)
    intf))

#|
(defun close-viewer (data intf)
  (declare (ignore data))
  (sg:wdetach (slot-value intf 'image-display))
  (sg:wdetach (slot-value intf 'magn-image-display))
  (sg:wdetach (slot-value intf 'color-bar-display))
  (capi:destroy intf))
|#

(defun copy-to-clipboard (&rest args)
  (declare (ignore args))
  (sg:copy-graphic-to-clipboard)
  (capi:display-message "image captured"))

(defun draw-crosshairs (pane x y)
  (gp:with-graphics-state (pane :operation boole-xor
                                :foreground :white)
    (gp:draw-line pane -16000 y (- x 5) y)
    (gp:draw-line pane (+ x 5) y 16000 y)
    (gp:draw-line pane x -16000 x (- y 5))
    (gp:draw-line pane x (+ y 5) x 16000)
    (gp:draw-circle pane x y 10)))

(let (lastx lasty)
  
  (defun undraw-crosshairs (pane x y)
    (declare (ignore x y))
    (when lastx
        (draw-crosshairs pane lastx lasty)
        (setf lastx nil lasty nil)))

  (defun redraw-crosshairs (pane x y)
    (undraw-crosshairs pane lastx lasty)
    (draw-crosshairs pane x y)
    (setf lastx x lasty y)
    (show-position pane x y)))


(defclass <image-array-x> (<image-array>)
  ((tint :accessor integration-time   :initarg  :tint)))

(defun make-image-x (arr tint)
  (if (= (array-rank arr) 2)
      (make-instance '<image-array-x>
		     :arena arr
                     :tint  (or tint 1.0))
    (error "flat 2-D array required")))
                 
(defmethod make-similar ((a <image-array-x>) arr)
  (make-instance '<image-array-x>
		 :arena arr
                 :tint  (integration-time a)))

(defun set-window-title (intf str)
  (setf (capi:interface-title intf) str))

(defun set-interface-image (intf img)
  (setf (interface-image intf) img))

(defun enable-refresh-button (intf)
  (setf (capi:button-enabled (slot-value intf 'refresh-button)) t))

(defun set-slider-bounds (intf img)
  (let* ((pcs   (vm:percentiles (image-array-arena img)))
         (pc01  (truncate (getf pcs :pc01)))
         (pc99  (max (truncate (getf pcs :pc99))
                     (+ pc01 500)))
         (range (* 2 (- pc99 pc01)))
         (offset-slider (slot-value intf 'offset-slider))
         (range-slider  (slot-value intf 'range-slider)))
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
        (let* ((tint (float-value (car attrs)))
               (ximg (make-image-x img tint)))
          (set-interface-image intf (flipv ximg))
          (enable-refresh-button intf)
          (set-window-title intf (namestring fname))
          (set-slider-bounds intf ximg)
          (refresh-image data intf)))
    (error ())
    (condition (cx) (error cx))))

(defun make-color-bar ()
  (let ((cdata (make-array '(256 9)
                           :element-type 'integer)))
    (dotimes (iy 256)
      (dotimes (ix 9)
        (setf (aref cdata iy ix) iy)))
    cdata))

(defun redraw-color-bar (pane &rest args)
  (declare (ignore args))
  (sg:direct-redraw pane))

(defun draw-color-bar (intf)
  (let ((wpane (slot-value intf 'color-bar-display)))
    (sg:wset wpane)
    (sg:tvscl (make-color-bar))))

(defvar *magnification*  2)

(defun draw-image-display (intf img)
  (let ((wpane           (slot-value intf 'image-display))
        (offs-pane       (slot-value intf 'offset-slider))
        (range-pane      (slot-value intf 'range-slider))
        (negate-button   (slot-value intf 'neg-button))
        (logscale-button (slot-value intf 'logscale-button)))
    (sg:wset wpane)
    ;; the following wsave forces the scigraphics to resize
    ;; its backing store (if necessary, e.g., after window
    ;; is resized)
    ;;(sg:wsave)
    ;;(sg:werase sg:$black)
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
  (if neg
      (tvscl (pixelwise (img)
                        (- (log (max 1 (- img minv)))))
             :range (list (- (log (max (- maxv minv) 1))) 0)
             :magn *magnification*)
    (tvscl (pixelwise (img)
                      (log (max 1 (- img minv))))
           :range (list 0 (log (max (- maxv minv) 1)))
           :magn  *magnification*)
    ))

(defun show-linear-stretch (img minv maxv neg)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if neg
      (tvscl (pixelwise (img)
                        (- img))
             :range (list (- maxv) (- minv))
             :magn *magnification*)
    (tvscl img
           :range (list minv maxv)
           :magn  *magnification*)
    ))

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
  (let* ((pane  (slot-value intf 'magn-image-display))
         (ximg  (subimage-centered img (list y x) (list 11 11)))
         (arr   (image-array-arena ximg)))
    (sg:wset pane)
    (let ((v (aref arr 5 5)))
      (tvscl ximg
             :magn 8
             :range (list (reduce #'min (vm:vector-of arr))
                          (max v (+ med 100))))
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
  (setf (capi:title-pane-text (slot-value intf 'bg-display))
        (format nil "~A" med)))

(defun compute-magnitude (x y img intf)
  (let* ((ximg (subimage-centered img (list y x) (list 5 5)))
         (simg (subimage-centered img (list y x)
                                  (list 41 41)))
         (moat (um:subselect (image-array-arena simg) *wmask*))
         (med  (vm:median moat))
         (tot  (foldl #'(lambda (rslt v)
                          (+ rslt (- v med)))
                      0.0 (vm:vector-of (image-array-arena ximg)))))
    (show-magnified-selection intf img x y med)
    (set-bgmed-display intf med)
    (if (plusp tot)
        (+ (* -2.5 (- (log tot 10)
                      (log (integration-time img) 10)))
           *ref-magn*)
      99)))

(defun set-ref-magnitude (pane x y)
  (let* ((intf (capi:element-interface pane))
         (img  (interface-image intf)))
    (when img
      (let* ((a     (image-array-arena img))
             (ylim  (array-dimension a 0))
             (x     (truncate x 2))
             (y     (- ylim (truncate y 2) 1))
             (magn  (compute-magnitude x y img intf))
             (user-magn (capi:prompt-for-string
                         "Enter reference magnitude"
                         :initial-value (format nil "~,2F" magn))))
        (when user-magn
          (setf *ref-magn* (+ *ref-magn*
                              (- (float (read-from-string user-magn))
                                 magn)))
          ))
      )))

(defun set-x-display (intf x)
  (setf (capi:title-pane-text (slot-value intf 'x-display))
        (format nil "~A" x)))

(defun set-y-display (intf y)
  (setf (capi:title-pane-text (slot-value intf 'y-display))
        (format nil "~A" y)))

(defun set-z-display (intf z)
  (setf (capi:title-pane-text (slot-value intf 'z-display))
        (format nil "~A" z)))

(defun set-m-display (intf m)
  (setf (capi:title-pane-text (slot-value intf 'm-display))
        (format nil "~,2F" m)))

(defun clear-z-display (intf)
  (set-z-display intf ""))

(defun clear-m-display (intf)
  (setf (capi:title-pane-text (slot-value intf 'm-display)) ""))

(defun show-position (pane x y)
  (let* ((intf (capi:element-interface pane))
         (img  (interface-image intf)))
    (when img
      (let* ((a     (image-array-arena img))
             (xlim  (array-dimension a 1))
             (ylim  (array-dimension a 0))
             (x     (truncate x *magnification*))
             (y     (- ylim (truncate y *magnification*) 1)))
        (set-x-display intf x)
        (set-y-display intf y)
        (if (and (>= x 0)
                 (< x xlim)
                 (>= y 0)
                 (< y ylim))
            (progn
              (set-z-display intf (aref a y x))
              (set-m-display intf (compute-magnitude x y img intf)))
          (progn
            (clear-z-display intf)
            (clear-m-display intf)))
        ))))

;(image-viewer)

;; ----------------------------------------------------------
#|
(defun draw-a-circle (pane)
  (capi:beep-pane pane)
  (gp:draw-circle pane 100 100 50))

(progn
  (setf pane
        (capi:contain
         (make-instance 'capi:output-pane
                        ;; :display-callback 'kill-crosshairs
                        :cursor :crosshair
                        :input-model
                        '((#\control-\c copy-to-clipboard)
                          ;;((:button-1 :press)   draw-crosshairs)
                          ((:button-1 :motion)  redraw-crosshairs)
                          ((:button-1 :release) undraw-crosshairs)))
         :best-width 400
         :best-height 300
         :title "NML in CAPI"))

  (sg:wattach pane)
  )

(setf img (make-image (scids:getvar)))
(setf simg (subimage-centered img '(43 108) '(21 21)))

(plot-surface simg :colorfn #'surf:lamps-color-fn)
(plot-surface simg :colorfn #'surf:gray-color-fn)
(plot-surface simg :colorfn #'surf:red-color-fn)

(lego-plot simg)

(plot-surface (shifth (dist 32))
              :colorfn #'surf:lamps-color-fn)
(plot-surface (dist 32)
              :colorfn #'surf:lamps-color-fn)
(lego-plot (dist 32))
(lego-plot (shifth (dist 32)))

(sg:window 0)
(sg:wset pane)
|#

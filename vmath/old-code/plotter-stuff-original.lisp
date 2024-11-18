;; Plotter-stuff.lisp -- additional routines to:
;;
;;    1. Permit text output at arbitrary angles
;;    2. Copy graphics pane to clipboard as a PDF image
;;    3. Save graphics pane as a PDF file
;;
;; DM/RAL  05/07
;; ----------------------------------------------------------

(in-package plotter)

;; -----------------------------------------------------------------------

(defparameter *pltstuff-lib*
  (translate-logical-pathname "PROJECTS:DYLIB;libLispPlotterStuff.dylib"))

(fli:register-module *pltstuff-lib*)
#|
;; for reload and retry...
(fli:disconnect-module *pltstuff-lib*)
|#

(fli:define-c-struct (color-struct (:foreign-name "colorStruct"))
  (red      :float)
  (green    :float)
  (blue     :float)
  (alpha    :float))
                                   
(fli:define-foreign-function (_add-label "pltstuff_add_label" :source)
    ((view    :pointer)
     (text   (:reference-pass
              (:ef-mb-string :limit 256)))
     (x       :float)
     (y       :float)
     (font-name  (:reference-pass
                  (:ef-mb-string :limit 256)))
     (font-size   :float)
     (color      (:pointer color-struct))
     (just        :long)
     (angle       :float)
     (transparent :long)
     (bgcolor     (:pointer color-struct)))
  :language    :ansi-c
  :result-type :void)

(defun sfloat (v)
  (float v 1e0))

(defun fill-color-spec (port color-struct color &optional alpha)
  (let ((rgb  (color:get-color-spec (color:unconvert-color port color))))
    (labels ((fill (component fn)
               (setf (fli:foreign-slot-value color-struct component)
                     (sfloat (funcall fn rgb)))
               ))
      (fill 'red   #'color:color-red)
      (fill 'green #'color:color-green)
      (fill 'blue  #'color:color-blue)
      (fill 'alpha (if alpha
                       (constantly alpha)
                     #'color:color-alpha))
      )))
                                      
(defmethod ns-view ((pane capi:output-pane))
  (objc:objc-object-pointer
   (capi-internals:representation pane)))

(defun add-label (pane text x y
                       &key
                       (font "Times-Roman")
                       (font-size 12)
                       (color :black)
                       ;;(alpha 1.0)
                       alpha
                       (x-alignment :left)
                       (y-alignment :baseline)
                       (angle 0.0)
                       box
                       (transparent t)
                       (bgcolor     (gp:graphics-state-background
                                     (gp:get-graphics-state pane)))
                       &allow-other-keys)
  (fli:with-dynamic-foreign-objects ()
    (let ((color-struct    (fli:allocate-dynamic-foreign-object
                            :type 'color-struct))
          (bg-color-struct (fli:allocate-dynamic-foreign-object
                            :type 'color-struct)))
      
      (fill-color-spec pane color-struct color alpha)
      (fill-color-spec pane bg-color-struct bgcolor)
      (gp:with-graphics-state (pane
                               :mask (or box
                                         (list 0 0 (gp:port-width pane)
                                               (gp:port-height pane))))
        (gp:draw-line pane -1 -1 -1 -1) ;; force the cliprect to take effect
        (_add-label (ns-view pane)
                    text
                    (sfloat x)
                    
                    ;; NSView y-coords are inverted from pane y-coords
                    (sfloat (- (gp:port-height pane) y))
                    
                    font
                    (sfloat font-size)
                    color-struct
                    (+ (* 4 (ecase y-alignment
                              (:center   0)
                              (:baseline 1)
                              (:bottom   2)
                              (:top      4)))
                       (ecase x-alignment
                         (:left   0)
                         (:center 1)
                         (:right  2)))
                    (sfloat angle)
                    (if transparent 1 0)
                    bg-color-struct)
        ))))


;; ---------------------------------------------------------
;; Copy pane as PDF image
;;
(fli:define-foreign-function (_copy-pdf-plot "pltstuff_copy_pdf_plot" :source)
    ((view :pointer))
  :language :ansi-c
  :result-type :void)

(defun copy-pdf-plot (pane)
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (_copy-pdf-plot (ns-view pane)))
                              ))

;; -------------------------------------------------------
;; Save pane as PDF file
;;
(fli:define-foreign-function (_save-pdf-plot "pltstuff_save_pdf_plot" :source)
    ((view :pointer)
     (filename (:reference-pass
                (:ef-mb-string :limit 256))))
  :language :ansi-c
  :result-type :void)

(defun save-pdf-plot (pane filename)
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (_save-pdf-plot (ns-view pane) filename))
                              ))

;; -------------------------------------------------------------
;; Attempt to provide graphics compositing

#|
(fli:define-foreign-function (_set-compositing "pltstuff_set_compositing" :source)
    ((mode :long))
  :language :ansi-c
  :result-type :void)

(fli:define-foreign-function (_restore-context "pltstuff_restore_context" :source)
    ()
  :language :ansi-c
  :result-type :void)

(defmacro with-compositing ((mode) &body body)
  `(progn
     (_set-compositing ,mode)
     (unwind-protect
	 (progn
	   ,@body)
       (_restore-context))))

(fli:define-foreign-function (_draw-xor-line "pltstuff_draw_xor_line" :source)
    ((view  :pointer)
     (x0    :float)
     (y0    :float)
     (x1    :float)
     (y1    :float)
     (color (:pointer color-struct)))
  :language :ansi-c
  :result-type :void)

(defun draw-xor-line (pane x0 y0 x1 y1 &key (color :red))
  (fli:with-dynamic-foreign-objects ()
    (let* ((color-struct    (fli:allocate-dynamic-foreign-object
                             :type 'color-struct))
           (ns-view (slot-value (capi-internals:representation pane)
                                'capi-cocoa-library::main-view))
           (gstate  (gp:get-graphics-state pane))
           (bgcolor (gp:graphics-state-background gstate)))

      (fill-color-spec color-struct (or color
                                        bgcolor))
      #|
      (destructuring-bind (xx0 yy0 xx1 yy1)
          (gp:transform-points
           (gp:graphics-state-transform gstate)
           (list x0 y0 x1 y1))
        |#

      (_draw-xor-line ns-view
                      (float x0 1.0)
                      ;; NSView y-coords are inverted from pane y-coords
                      (float (- (gp:port-height pane) y0) 1.0)
                      (float x1 1.0)
                      ;; NSView y-coords are inverted from pane y-coords
                      (float (- (gp:port-height pane) y1) 1.0)
                      color-struct)
        )))

|#

        
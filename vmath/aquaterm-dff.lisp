#| DATE           : 25 Jun 2004 
 | USER           : davidmcclain 
 | PROCESSED FILE : /Users/davidmcclain/Projects/AquaTerm1.0.a2/include/aquaterm.h
 |#

(in-package #:com.ral.aquaterm)

;;; Derived from file : "/Users/davidmcclain/Projects/AquaTerm1.0.a2/include/aquaterm.h"

(defconstant Butt-Line-Cap-Style    0)
(defconstant Round-Line-Cap-Style   1)
(defconstant Square-Line-Cap-Style  2)

(defconstant Align-Left     #x00)
(defconstant Align-Center   #x01)
(defconstant Align-Right    #x02)

(defconstant Align-Middle   #x00)
(defconstant Align-Baseline #x04)
(defconstant Align-Bottom   #x08)
(defconstant Align-Top      #x10)

;; --------------------------------------------------------------------
;; AquaTerm Client uses Cocoa, and hence, all calls must occur
;; on the same thread as the init call...
;;
;; AQT-PROXY -- a background thread through which all AQT interface 
;; calls are routed, so that one AQT Mapper connection
;; can service all subsequent LispWorks threads.
;;

(defvar *aqt-process* nil)
(defvar *aqt-channel* nil)
  
(fli:define-foreign-function (_aqtinit "aqtInit" :source)
    nil
  :result-type :int
  :language    :ansi-c)

(defun aqt-proxy-thread-fn ()
  (_aqtinit)
  (loop
   (destructuring-bind (replyCh fn)
       (rch:recv *aqt-channel*)
     (rch:send replyCh
               (multiple-value-list
                (ignore-errors
                  (multiple-value-list
                   (funcall fn)))))
     )))

(defun proxy-request (fn)
  (unless *aqt-process*
    (fli:register-module (translate-logical-pathname "PROJECTS:DYLIB;libaquaterm.dylib"))
    (setf *aqt-channel* (rch:make-channel)
          *aqt-process* (mp:process-run-function
                         "AQT Proxy Thread"
                         `(:priority ,(1- (mp:process-priority mp:*current-process*)))
                         #'aqt-proxy-thread-fn))
    (set-accepting-events 1))
  (let ((replyCh (rch:make-channel)))
    (rch:send *aqt-channel* (list replyCh fn))
    (destructuring-bind (ans &optional cond)
        (rch:recv replyCh)
      (if cond
          (error cond)
        (apply #'values ans))
      )))

(def-proxy-fli-function (aqtterminate "aqtTerminate" :source)
                        nil
                        :result-type :void
                        :language    :ansi-c)

(defun terminate ()
  (when *aqt-process*
    (aqtterminate)
    (mp:process-kill *aqt-process*)
    (um:nilf *aqt-channel* *aqt-process*)))

;; ------------------------------------------------------------------
;;
(def-proxy-fli-function (aqtseteventhandler
                              "aqtSetEventHandler"
                              :source)
                             ((func
                               (:pointer
                                (:function
                                 (:int (:pointer (:const :char)))
                                 :void))))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (open-plot "aqtOpenPlot" :source)
                             ((refnum :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (select-plot "aqtSelectPlot" :source)
                             ((refnum :int))
                             :result-type :int
                             :language    :ansi-c)
(def-proxy-fli-function (set-plot-size "aqtSetPlotSize" :source)
                             ((width  :float)
			      (height :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-plot-title
                              "aqtSetPlotTitle"
                              :source)
                             ((title (:reference-pass
				      (:ef-mb-string :limit 256))))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (save-context
                              "aqtSaveContext"
                              :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (restore-context
                              "aqtRestoreContext"
                              :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-cliprect "aqtSetClipRect" :source)
                             ((originx :float)
                              (originy :float)
                              (width   :float)
                              (height  :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-affine-transform
                         "aqtSetAffineTransform"
                         :source)
                        ((scalex   :float)
                         (scaley   :float)
                         (offx     :float)
                         (offy     :float)
                         (angDeg   :float))
                        :result-type :void
                        :language    :ansi-c)

(def-proxy-fli-function (render-plot "aqtRenderPlot" :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (clear-plot "aqtClearPlot" :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (close-plot "aqtClosePlot" :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (save-plot "aqtSavePlot" :source)
                        ((filename (:reference-pass
                                    (:ef-mb-string :limit 256))))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-accepting-events
                              "aqtSetAcceptingEvents"
                              :source)
                             ((flag :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (_get-last-event
                              "aqtGetLastEvent"
                              :source)
                             ((buffer (:reference-return
                                       (:ef-mb-string :limit 64))))
                             :result-type :int
                             :language    :ansi-c)

(def-proxy-fli-function (_wait-next-event
                              "aqtWaitNextEvent"
                              :source)
                             ((buffer (:reference-return
                                       (:ef-mb-string :limit 64))))
                             :result-type :int
                             :language    :ansi-c)

(def-proxy-fli-function (aqtcolormapsize
                              "aqtColormapSize"
                              :source)
                             nil
                             :result-type :int
                             :language    :ansi-c)

(def-proxy-fli-function (aqtsetcolormapentry
                              "aqtSetColormapEntry"
                              :source)
                             ((entryindex :int)
                              (r          :float)
                              (g          :float)
                              (b          :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtgetcolormapentry
                              "aqtGetColormapEntry"
                              :source)
                             ((entryindex :int)
                              (r          (:pointer :float))
                              (g          (:pointer :float))
                              (b          (:pointer :float)))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqttakecolorfromcolormapentry
                              "aqtTakeColorFromColormapEntry"
                              :source)
                             ((index :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqttakebackgroundcolorfromcolormapentry
                              "aqtTakeBackgroundColorFromColormapEntry"
                              :source)
                             ((index :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtsetcolor "aqtSetColor" :source)
                             ((r :float)
			      (g :float)
			      (b :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtsetalphacolor "aqtSetAlphaColor" :source)
                             ((r :float)
			      (g :float)
			      (b :float)
			      (alpha :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtsetbackgroundcolor
                              "aqtSetBackgroundColor"
                              :source)
                             ((r :float)
			      (g :float)
			      (b :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtgetcolor "aqtGetColor" :source)
                             ((r (:pointer :float))
                              (g (:pointer :float))
                              (b (:pointer :float)))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (aqtgetbackgroundcolor
                              "aqtGetBackgroundColor"
                              :source)
                             ((r (:pointer :float))
                              (g (:pointer :float))
                              (b (:pointer :float)))
                             :result-type
                             :void
                             :language
                             :ansi-c)

(def-proxy-fli-function (set-font-name "aqtSetFontname" :source)
                             ((newfontname (:reference-pass
					    (:ef-mb-string :limit 256))))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-font-size "aqtSetFontsize" :source)
                             ((newfontsize :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-label "aqtAddLabel" :source)
                             ((text  (:reference-pass
				      (:ef-mb-string :limit 256)))
                              (x     :float)
                              (y     :float)
                              (angle :float)
                              (align :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-line-width
                              "aqtSetLinewidth"
                              :source)
                             ((newlinewidth :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-line-cap-style
                              "aqtSetLineCapStyle"
                              :source)
                             ((capstyle :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (move-to "aqtMoveTo" :source)
                             ((x :float) (y :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-line-to "aqtAddLineTo" :source)
                             ((x :float) (y :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-poly-line "aqtAddPolyline" :source)
                             ((x (:pointer :float))
                              (y (:pointer :float))
                              (pointcount :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (move-to-vertex
                              "aqtMoveToVertex"
                              :source)
                             ((x :float)
			      (y :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-edge-to-vertex
                              "aqtAddEdgeToVertex"
                              :source)
                             ((x :float)
			      (y :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-polygon "aqtAddPolygon" :source)
                             ((x (:pointer :float))
                              (y (:pointer :float))
                              (pointcount :int))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-filled-rect
                              "aqtAddFilledRect"
                              :source)
                             ((originx :float)
                              (originy :float)
                              (width   :float)
                              (height  :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (erase-rect "aqtEraseRect" :source)
                             ((originx :float)
                              (originy :float)
                              (width   :float)
                              (height  :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (set-image-transform
                              "aqtSetImageTransform"
                              :source)
                             ((m11 :float)
                              (m12 :float)
                              (m21 :float)
                              (m22 :float)
                              (tx  :float)
                              (ty  :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (reset-image-transform
                              "aqtResetImageTransform"
                              :source)
                             nil
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-image-with-bitmap
                              "aqtAddImageWithBitmap"
                              :source)
                             ((bitmap     (:pointer (:const :void)))
                              (pixwide    :int)
                              (pixhigh    :int)
                              (destx      :float)
                              (desty      :float)
                              (destwidth  :float)
                              (destheight :float))
                             :result-type :void
                             :language    :ansi-c)

(def-proxy-fli-function (add-transformed-image-with-bitmap
                              "aqtAddTransformedImageWithBitmap"
                              :source)
                             ((bitmap     (:pointer (:const :void)))
                              (pixwide    :int)
                              (pixhigh    :int)
                              (clipx      :float)
                              (clipy      :float)
                              (clipwidth  :float)
                              (clipheight :float))
                             :result-type :void
                             :language    :ansi-c)

(defmethod fcolor ((color integer))
  (/ (float color) 255))

(defmethod fcolor ((color float))
  color)

(defun set-color (red green blue &optional (alpha 1.0))
  (aqtsetalphacolor red green blue alpha))

(defun set-rgb-color (rgb)
  (let ((alpha (ldb (byte 8 24) rgb)))
    (aqtsetalphacolor (ldb (byte 8  0) rgb)
                      (ldb (byte 8  8) rgb)
                      (ldb (byte 8 16) rgb)
                      (if (zerop alpha) 1.0 alpha))
    ))

(defun set-background-color (red green blue)
  (aqtsetbackgroundcolor red green blue))

(defun set-rgb-background-color (rgb)
  (aqtsetbackgroundcolor (ldb (byte 8  0) rgb)
                         (ldb (byte 8  8) rgb)
                         (ldb (byte 8 16) rgb)))

#|
(open-plot 1)
(set-plot-size 400.0 300.0)
(add-label "Hello AquaTerm from Lisp!" 200.0 150.0 45.0 1)
(render-plot)
|#

(nregex:defregex :aqt-event #M"({:digit})\\:\\{({:number}), ({:number})\\}\\:({:alnum})")

(defun decode-event (event-str)
  (labels ((read-field (ix fields)
             (destructuring-bind (start end) (aref fields ix)
               (read-from-string (subseq event-str start end)))))
    (let* ((matches (nregex:match-regex (nregex:regex :aqt-event) event-str))
           (prefix  (char event-str 0))
           (mouse-p (char= prefix #\1))
           (x       (read-field 2 matches))
           (y       (read-field 3 matches))
           (ch/but  (read-field 4 matches)))
      (list
       :evkind (if mouse-p
                   :mouse-button
                 :key-down)
       :x      x
       :y      y
       :button (when   mouse-p ch/but)
       :char   (unless mouse-p ch/but))
      )))

(defun get-last-event ()
  (multiple-value-bind (ans event-str) (_get-last-event nil)
    (declare (ignore ans))
    (decode-event event-str)))

(defun wait-next-event ()
  (multiple-value-bind (ans event-str) (_wait-next-event nil)
    (declare (ignore ans))
    (decode-event event-str)))

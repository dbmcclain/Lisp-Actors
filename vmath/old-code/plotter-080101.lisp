;; plotter.lsp -- Plotting support for Lisp
;; DM 02/07
;;
;; The basic notions are as follows:
;;
;; For Mac OS/X we want to utilize Display PDF to the max. We can do that by drawing
;; directly in the pane. Making a backing store image of the screen looks good only while
;; viewing the screen. Making such a backing image interferes with nice PDF file output, or
;; copy/paste. So for those times, we avoid constructing an image of the screen to allow the
;; full PDF elegance to shine through.
;;
;; OS/X Cocoa cannot perform XOR image combination in PDF space. So for full cross-hairs
;; that follow the cursor position, we resort to a fast(!!) copying of the backing store
;; image to the screen followed by overdrawing of the crosshairs.

;; For Win/XP the output does not look as nice since Win/XP is limited to bitmapped
;; graphics. Furthermore, it is necessary to draw initially in an off-screen compatible
;; pixmap, then create an image of that pixmap for backing store and for direct transfer
;; the output screen. Only in this way is it possible to produce backing images unmolested
;; by overlapping windows. In general Win/XP is severely limited. E.g., it cannot use
;; fractional line widths or coordinates. Bad things happen if you try. So we intercept
;; those possibilities and produce corrected requests on behalf of the caller.
;;
;; Win/XP can produce output with XOR combination, so we don't have to use the heavy-handed
;; approach of constantly refreshing the image on the screen for full crosshair cursors. We
;; just need to use the overall BG-complement of the desired cursor color in XOR draw mode.
;;
;; So, to accommodate both kinds of drawing needs with one body of source code, most of
;; the drawing primitive routines take two arguments - pane and port -- in addition to
;; other specializing arguments. The pane refers to the <plotter-pane> object described
;; below, which contains all of the plotting specific information. The port object is
;; that used by the GP primitives for the actual drawing operations. On OS/X the pane and the
;; port point to the same underlying object. But for Win/XP the pane is the <plotter-pane>
;; and the port is a temporary off-screen pixmap port.
;;
;; Until the backing store image of the screen exists, both systems utilze an ordered
;; collection of lambda closures representing the various plotting commands needed to build
;; up the whole image. Once those commands have executed we can grab a copy of the
;; screen image for use as a fast-copy backing store.
;;
;; ------------------------------------------
;; All of the plotting commands now require a keyword PANE argument
;; so that our plotting routines are multiprocessing safe, and can operate on
;; an embedded <plotter-pane> or some subclass thereof...
;; There is no longer any notion of a "current plotting window".
;;

(in-package "PLOTTER")

(defvar *cross-cursor*
  (ignore-errors
    (capi:load-cursor
     '((:win32 (translate-logical-pathname "PROJECTS:LIB;crosshair.cur"))
       (:cocoa (translate-logical-pathname "PROJECTS:LIB;crosshair.gif"))
        :x-hot 7
        :y-hot 7))
    ))

(defclass <line-style> ()
  ((line-thick    :accessor line-thick
                  :initarg :line-thick
                  :initform 1)
   (line-dashing  :accessor line-dashing
                  :initarg  :line-dashing
                  :initform nil)
   (line-color    :accessor line-color
                  :initarg :line-color
                  :initform :darkgreen)
   (line-alpha    :accessor line-alpha
                  :initarg  :line-alpha
                  :initform nil)))

(defclass <symbol-style> ()
  ((plot-symbol   :accessor plot-symbol
                  :initarg :plot-symbol
                  :initform :circle)
   (fill-color    :accessor fill-color
                  :initarg :fill-color
                  :initform nil)
   (fill-alpha    :accessor fill-alpha
                  :initarg  :fill-alpha
                  :initform nil)
   (border-color  :accessor border-color
                  :initarg :border-color
                  :initform :black)
   (border-alpha  :accessor border-alpha
                  :initarg  :border-alpha
                  :initform nil)
   (border-thick  :accessor border-thick
                  :initarg :border-thick
                  :initform 1)
   (bar-width     :accessor bar-width
                  :initarg  :bar-width
                  :initform nil)
   (bar-offset    :accessor bar-offset
                  :initarg  :bar-offset
                  :initform nil)))

(defclass <plot-style> ()
  ((line-style    :accessor line-style
                  :initarg :line-style
                  :initform (make-instance '<line-style>
                                           :line-thick 1
                                           :line-color :darkgreen))

   (symbol-style  :accessor symbol-style
                  :initarg :symbol-style
                  :initform nil)))

(defclass <plotter-mixin> ()
  ;; stuff used by 2-D plot scaling and plotting
  ;; The mixin has all the information needed to produce plots
  ;; but has nothing to draw on...
  ((lock          :accessor plotter-lock           :initform (mp:make-lock))
   
   (xlog          :accessor plotter-xlog           :initform nil)
   (xmin          :accessor plotter-xmin           :initform 0.0d0)
   (xmax          :accessor plotter-xmax           :initform 1.0d0)

   (ylog          :accessor plotter-ylog           :initform nil)
   (ymin          :accessor plotter-ymin           :initform 0.0d0)
   (ymax          :accessor plotter-ymax           :initform 1.0d0)
   
   (box           :accessor plotter-box)
   (xform         :accessor plotter-xform          :initform '(1 0 0 1 0 0))
   (inv-xform     :accessor plotter-inv-xform      :initform '(1 0 0 1 0 0))
   (dlist         :accessor plotter-display-list   :initform  (um:make-mpsafe-monitored-collector))
   (delayed       :accessor plotter-delayed-update :initform 0)
   
   ;; info for nice looking zooming
   (def-wd        :accessor plotter-nominal-width  :initarg :nominal-width  :initform nil)
   (def-ht        :accessor plotter-nominal-height :initarg :nominal-height :initform nil)

   (sf            :accessor plotter-sf    :initform 1)
   (magn          :accessor plotter-magn  :initform 1)

   (legend        :accessor plotter-legend-info        :initform (um:make-collector))
   (legend-x      :accessor plotter-legend-x           :initform '(:frac 0.95))
   (legend-y      :accessor plotter-legend-y           :initform '(:frac 0.95))
   (legend-anchor :accessor plotter-legend-anchor      :initform :auto)

   (dirty         :accessor plotter-dirty              :initform nil)
   (reply-mbox    :accessor reply-mbox                 :initform nil)
   )
  (:default-initargs
   :nominal-width      400
   :nominal-height     300
   :name               "Plot"
   ))

(defclass <plotter-pane> (<plotter-mixin> capi:output-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ;; The pane adds something to draw on...
  ;; And it also adds some user gestures and any display related items
  ;; like cross hairs, cursors, backing images, etc.
  ((backing-image   :accessor plotter-backing-image  :initform nil)
   (timer           :accessor plotter-resize-timer   :initform nil)
   (backing-pixmap  :accessor plotter-backing-pixmap :initform nil)
   (delay-backing   :accessor plotter-delay-backing  :initform nil)
   (full-crosshair  :accessor plotter-full-crosshair :initform nil   :initarg :full-crosshair)
   (prev-x          :accessor plotter-prev-x         :initform nil)
   (prev-y          :accessor plotter-prev-y         :initform nil)
   (x-ro-hook       :accessor plotter-x-readout-hook :initform #'identity)
   (y-ro-hook       :accessor plotter-y-readout-hook :initform #'identity)
   (plotter-valid   :accessor plotter-valid          :initform t))
  (:default-initargs
   :display-callback 'display-callback
   :resize-callback  'resize-callback
   :destroy-callback 'destroy-callback
   :input-model      '((:motion mouse-move)
                       ((:button-1 :press) show-x-y-at-cursor)
                       ((:gesture-spec "Control-c")
                        copy-image-to-clipboard)
                       ((:gesture-spec "Control-p")
                        print-plotter-pane)
                       ((:gesture-spec "Control-s")
                        save-image-from-menu)
                       ((:gesture-spec "C")
                        toggle-full-crosshair)
                       ((:gesture-spec "c")
                        toggle-full-crosshair)
                       )
   :cursor   (or *cross-cursor*
                 :crosshair)
   :visible-min-width  200
   :visible-min-height 150
   :visible-max-width  800
   :visible-max-height 600
   ))

(defun destroy-callback (pane)
  (setf (plotter-valid pane) nil)
  (discard-backing-image  pane)
  (discard-backing-pixmap pane))

;; ------------------------------------------
(defstruct legend-info
  plot-style   ; item plotting style
  ;; color        ; line color
  ;; thick        ; line thickness
  ;; linedashing  ; line dashing of lines
  ;; symbol       ; plotting symbol
  ;; plot-joined  ; whether symbols are joined by lines
  ;; border-color ; border color for filled symbols
  ;; symbol-filled ; symbol is filled
  ;; fill-color   ; symbol fill color for filled symbol
  ;; border-thick ; symbol border thickness
  text)        ; actual text of legend entry

;; ---------------------------------------------------------
(defgeneric plotter-mixin-of (pane-rep)
  ;; rep might be a <plotter-pane>,
  ;; a subclass of capi:interface,
  ;; or a symbolic name of a window
  )

(defmethod plotter-mixin-of ((pane <plotter-mixin>))
  ;; it is me...
  pane)

;; ------------------------------------------
(defmethod display-pane-of (pane)
  pane)

(defmethod display-pane-of ((obj capi:pinboard-object))
  (display-pane-of (capi:element-parent obj)))

;; ------------------------------------------
;; Win32/OS X compatibility constants and functions
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
  
(defun find-best-font (pane &key
                            (family "Times")
                            size
                            (weight :normal)
                            (slant  :roman))
  (gp:find-best-font (display-pane-of pane)
                     (gp:make-font-description
                      :family family
                      :size   #+:COCOA size #+:WIN32 (round size)
                      :weight weight
                      :slant  slant)))

;; ------------------------------------------
#+:WIN32
(defun adjust-linewidth (wd)
  ;; Win/XP can't handle fractional linewidths
  (max 1 (round wd)))

#+:COCOA
(defun adjust-linewidth (wd)
  ;; ... but Display PDF can...
  wd)

;; ------------------------------------------
(defun background-color (pane)
  (gp:graphics-state-background
   (gp:get-graphics-state pane)))

(defun foreground-color (pane)
  (gp:graphics-state-foreground
   (gp:get-graphics-state pane)))

#+:WIN32
(defun adjust-color (pane color &optional alpha)
  ;; Win/XP can't handle true alpha blending. So we use a make-pretend system
  ;; that assumes the color will be blending with- the background color. That only
  ;; works properly as long as the drawing is actually over that background color.
  (let* ((c (color:get-color-spec color))
         (a (or alpha (color:color-alpha c))))
    (if (= 1 a)
      color
      (let* ((bg  (color:get-color-spec (background-color pane)))
             (1-a (- 1.0 a)))
        (labels ((mix (fn)
                   (+ (* 1-a (funcall fn bg))
                      (* a   (funcall fn c)))))
        (color:make-rgb 
         (mix #'color:color-red)
         (mix #'color:color-green)
         (mix #'color:color-blue))
        )))))
      
#+:COCOA
(defun adjust-color (pane color &optional alpha)
  ;; Mac OS/X Cocoa can do real alpha blending. Here we take the user's
  ;; requested color, and a possibly separate alpha level (alpha might be nil)
  ;; to produce a color that will be properly alpha blended over a varying background.
  (declare (ignore pane))
  (if (null alpha)
      color
    (let ((c (color:get-color-spec color)))
      (color:make-rgb
       (color:color-red   c)
       (color:color-green c)
       (color:color-blue  c)
       alpha)
      )))

#+:WIN32
(defun complementary-color (color background)
  ;; produce a color such that XOR mode of that color against the background
  ;; will produce the requested color...
  (let* ((c  (color:get-color-spec color))
         (bg (color:get-color-spec background)))
    
    (labels ((color-xor (compon1 compon2)
               (let ((icompon1  (round (* 255 compon1)))
                     (icompon2  (round (* 255 compon2))))
                 (/ (logxor icompon1 icompon2) 255.0)))

             (color-xor-components (fn)
               (color-xor (funcall fn c) (funcall fn bg))))
      
      (color:make-rgb
       (color-xor-components #'color:color-red)
       (color-xor-components #'color:color-green)
       (color-xor-components #'color:color-blue))
      )))

;; ------------------------------------------
#+:WIN32
(defun adjust-box (box)
  ;; Win/XP can't handle fractional box coords
  (mapcar #'round box))

#+:COCOA
(defun adjust-box (box)
  ;; ... but OS/X Cocoa can...
  box)

;; ------------------------------------------
;; infinitep true if non-zero numeric arg with zero reciprocal
;; works for plus or minus infinity. As a secondary effect,
;; the truth value will be the largest double precision value.
(defun infinitep (v)
  (and (not (zerop v))
       (zerop (/ v))
       (if (plusp v)
           most-positive-double-float
         most-negative-double-float)))

;; nanp true if numeric v not equal to itself
(defun nanp (v)
  (/= v v))

(defun inf-nan-p (v)
  (or (infinitep v)
      (nanp v)))

(defun simple-real-number (v)
  (and (realp v)
       (not (inf-nan-p v))))

(defun real-eval-with-nans (fn &rest args)
  (handler-case
      (let ((v (apply fn args)))
        (if (simple-real-number v)
            v
          :nan))
    (arithmetic-error (err)
      (declare (ignore err))
      :nan)))

(defun nan-or-infinite-p (v)
  (not (simple-real-number v)))

;; -------------------------------------------------------------------
(defmethod coerce-to-vector ((v vector))
  v)

(defmethod coerce-to-vector ((lst list))
  (coerce lst 'vector))

(defmethod coerce-to-vector ((a array))
  (make-array (array-total-size a)
              :element-type (array-element-type a)
              :displaced-to a))

(defmethod coerce-to-vector ((cv c-arrays:<carray>))
  (coerce-to-vector (c-arrays:convert-to-lisp-object cv)))

;; ---------------------------------------------
;; filtering out nans and infinities
;;
(defmethod filter-x-y-nans-and-infinities ((xs cons) (ys cons))
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x in xs
                     for y in ys
                     when (and (simple-real-number x)
                               (simple-real-number y))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-x-y-nans-and-infinities ((xs vector) (ys vector))
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x across xs
                     for y across ys
                     when (and (simple-real-number x)
                               (simple-real-number y))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-x-y-nans-and-infinities (xs ys)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (filter-x-y-nans-and-infinities (coerce-to-vector xs) (coerce-to-vector ys)))


(defun filter-nans-and-infinities (xs)
  ;; remove values from the sequence if they are nans or infinities
  (remove-if (complement #'simple-real-number) xs))

;; ----------------------------------------------------------------------
;; filter out potential nans and infinities for logarithmic axes
(defun acceptable-value (v islog)
  (and (simple-real-number v)
       (or (not islog)
           (and islog
                (plusp v)))))

(defmethod filter-potential-x-y-nans-and-infinities ((xs cons) (ys cons) xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x in xs
                     for y in ys
                     when (and (acceptable-value x xlog)
                               (acceptable-value y ylog))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-potential-x-y-nans-and-infinities ((xs vector) (ys vector) xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x across xs
                     for y across ys
                     when (and (acceptable-value x xlog)
                               (acceptable-value y ylog))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-potential-x-y-nans-and-infinities (xs ys xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (filter-x-y-nans-and-infinities (coerce-to-vector xs) (coerce-to-vector ys)))


(defun filter-potential-nans-and-infinities (xs islog)
  ;; remove values from the sequence if they are nans or infinities
  (remove-if (complement (um:rcurry #'acceptable-value islog)) xs))

;; ---------------------------------------------
;; Define some safe image access macros...
;;
(defmacro with-image ((port (image imgexpr)) &body body)
  ;; returned value will be that of the body
  `(let ((,image ,imgexpr))
     (unwind-protect
         (progn
           ,@body)
       (gp:free-image ,port ,image))
     ))

(defmacro with-image-access ((acc access-expr) &body body)
  ;; returned value will be that of the body
  `(let ((,acc ,access-expr))
     (unwind-protect
         (progn
           ,@body)
       (gp:free-image-access ,acc))
     ))

;; ---------------------------------------------------------
;; WIN32 has some low-level pixmap routines that aren't reentrant
;; so we fix that up here by using our own lock to prevent multiple
;; threads from concurrent execution of those routines...

#+:WIN32
(defvar *win32-pixmap-lock* (mp:make-lock))

#+:WIN32
(defmacro create-pixmap-port (&rest args)
  `(mp:with-lock (*win32-pixmap-lock*)
     (gp:create-pixmap-port ,@args)))

#+:WIN32
(defmacro with-pixmap-graphics-port (args &body body)
  `(mp:with-lock (*win32-pixmap-lock*)
     (gp:with-pixmap-graphics-port ,args
       ,@body)))

#+:COCOA
(defmacro create-pixmap-port (&rest args)
  `(gp:create-pixmap-port ,@args))


#+:COCOA
(defmacro with-pixmap-graphics-port (args &body body)
  `(gp:with-pixmap-graphics-port ,args
     ,@body))

;; ------------------------------------------
;; We can use WITH-DELAYED-UPDATE to ward off immediate and slowing
;; direct drawing operations. Delayed sections can be nested. Meanwhile,
;; within a delayed section we are simply building up a display list of
;; parameterized lambda closures that collectively will produce the sum
;; of all delayed operations, once the delay goes back to zero.
;;
(defun do-sync-with-capi (capi-fn capi-elt fn args)
  (funcall capi-fn capi-elt
           #'apply fn args))

(defmethod sync-with-capi ((intf capi:interface) fn &rest args)
  (do-sync-with-capi #'capi:execute-with-interface intf fn args))

(defmethod sync-with-capi ((pane capi:simple-pane) fn &rest args)
  (do-sync-with-capi #'capi:apply-in-pane-process pane fn args))

(defmethod sync-with-capi ((pane capi:pinboard-object) fn &rest args)
  (let ((layout (capi:pinboard-object-pinboard pane)))
    (when layout
      (apply 'sync-with-capi layout fn args))
    ))

(defmethod sync-with-capi (pane fn &rest args)
  (declare (ignore pane fn args)))

;; ------------------------------------------
(defun do-with-locked-plotter-pane (pane fn)
  (mp:with-lock ((plotter-lock (plotter-mixin-of pane)))
    (funcall fn)))

(defmacro with-locked-plotter-pane (pane &body body)
  `(do-with-locked-plotter-pane ,pane (lambda () ,@body)))

(defmethod redraw-entire-pane ((pane <plotter-pane>))
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (setf (plotter-dirty pane) t)
                                #+:COCOA
                                (gp:invalidate-rectangle pane)
                                #+:WIN32
                                (win32-display-callback pane 0 0
                                                        (gp:port-width pane)
                                                        (gp:port-height pane))
                                )))

#|
(defmethod redraw-entire-pane ((pane capi:pinboard-object))
  (progn ;; with-locked-plotter-pane pane
    (setf (plotter-dirty pane) t)
    #+:COCOA
    (gp:invalidate-rectangle pane)
    ))
|#

(defun do-with-delayed-update (pane fn)
  (let ((pane (plotter-mixin-of pane))) ;; could be called with symbolic name for pane
    (with-locked-plotter-pane pane
       (let ((prev-ct (um:post-incf (plotter-delayed-update pane))))
         (when (zerop prev-ct)
           ;; asking if changed, resets the changed indiction
           (um:changed-p (plotter-display-list pane)))
         (unwind-protect
             (progn
               (funcall fn)
               (when (and (zerop prev-ct)
                          (um:changed-p (plotter-display-list pane)))
                 (setf (plotter-dirty pane) t)
                 (sync-with-capi pane 'redraw-entire-pane pane)))
           (decf (plotter-delayed-update pane))
           ))
       )))

#|
;; test delayed updates -- entire composite plot should appear at one time
(let ((win (plt:wset 'myplot)))
  (plt:with-delayed-update (win)
    (plt:clear win)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x)) :thick 2 :title "Sinc")
    (sleep 2)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x)) :symbol :circle :color :blue)))
|#
#|
(defun do-with-delayed-update (pane fn)
  (let* ((pane (plotter-mixin-of pane)) ;; could be called by user with symbolic name for pane
         (ct   (plotter-delayed-update pane)))
    (incf (plotter-delayed-update pane))
    (when (zerop ct) ;; asking if changed resets the changed indiction
      (um:changed-p (plotter-display-list pane)))
    (unwind-protect
        (progn
          (funcall fn)
          (when (and (zerop ct)
                     (um:changed-p (plotter-display-list pane)))
            (sync-with-capi pane
                            (lambda ()
                              (discard-backing-pixmap pane)
                              (gp:invalidate-rectangle pane))
                            )))
      (decf (plotter-delayed-update pane))
      )))
|#
#|
;; capi:with-atomic-redisplay does not nest properly
(defun do-with-delayed-update (pane fn)
  (capi:with-atomic-redisplay (pane)
    (funcall fn)
    (discard-backing-pixmap pane)
    (gp:invalidate-rectangle pane)))
|#

;; user callable macro
(defmacro with-delayed-update ((pane) &body body)
  `(do-with-delayed-update ,pane
    (lambda ()
      ,@body)))


(defun do-wait-until-finished (pane mbox timeout fn)
  (if (eq mp:*current-process* mp:*main-process*)
      (funcall fn)
    (let ((mbox (or mbox (mp:make-mailbox))))
      (with-delayed-update (pane)
        (setf (reply-mbox (plotter-mixin-of pane)) mbox)
        (funcall fn))
      (mp:mailbox-read mbox "Waiting for plotter to finish" timeout)
      )))
    
(defmacro wait-until-finished ((pane &key mbox timeout) &body body)
  `(do-wait-until-finished ,pane ,mbox ,timeout (lambda () ,@body)))



(defun append-display-list (pane item)
  (um:collector-append-item (plotter-display-list pane) item))

(defun discard-display-list (pane)
  (um:collector-discard-contents (plotter-display-list pane))
  (um:collector-discard-contents (plotter-legend-info pane)))

(defun display-list-items (pane &key discard)
  (um:collector-contents (plotter-display-list pane) :discard discard))

(defun display-list-empty-p (pane)
  (um:collector-empty-p (plotter-display-list pane)))


(defun append-legend (pane item)
  (um:collector-append-item (plotter-legend-info pane) item))

(defun all-legends (pane)
  (um:collector-contents (plotter-legend-info pane)))

;; ------------------------------------------

(defun log10 (x)
  (if (not (plusp x))
      -300
    (log x 10.0d0)))

(defun pow10 (x)
  (expt 10.0d0 x))

;; ------------------------------------------
(defun inset-box-sides (box dxleft dytop 
                            &optional (dxright dxleft)
                                      (dybottom dytop))
  (list (+ (gp:rectangle-left   box) dxleft)
        (+ (gp:rectangle-top    box) dytop)
        (- (gp:rectangle-right  box) dxright)
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
;; generalized operators to accommodate <carrays> and others
;;

;;---------
(defmethod length-of (arg)
  (length arg))

(defmethod length-of ((arg array))
  (array-total-size arg))

(defmethod length-of ((arg ca:<carray>))
  (ca:carray-total-size arg))

;;---------
(defmethod vmax-of (arg)
  (vmax arg))

(defmethod vmax-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        maximize (row-major-aref arg ix)))

(defmethod vmax-of ((arg ca:<carray>))
  (loop for ix from 0 below (ca:carray-total-size arg)
        maximize (ca:row-major-caref arg ix)))

;;---------
(defmethod vmin-of (arg)
  (vmin arg))

(defmethod vmin-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        minimize (row-major-aref arg ix)))

(defmethod vmin-of ((arg ca:<carray>))
  (loop for ix from 0 below (ca:carray-total-size arg)
        minimize (ca:row-major-caref arg ix)))

;;---------
(defmethod array-total-size-of (arg)
  (array-total-size arg))

(defmethod array-total-size-of ((arg ca:<carray>))
  (ca:carray-total-size arg))

;;---------
(defmethod array-dimension-of (arg n)
  (array-dimension arg n))

(defmethod array-dimension-of ((arg ca:<carray>) n)
  (ca:carray-dimension arg n))

;;---------
(defmethod aref-of (arg &rest indices)
  (apply #'aref arg indices))

(defmethod aref-of ((arg ca:<carray>) &rest indices)
  (apply #'ca:caref arg indices))

;;---------
(defmethod subseq-of (arg start &optional end)
  (subseq arg start end))

(defmethod subseq-of ((arg array) start &optional end)
  (let* ((limit (array-total-size arg))
         (nel   (- (or end limit) start))
         (ans   (make-array nel :element-type (array-element-type arg))))
    (loop for ix from start below (or end limit)
          for jx from 0
          do
          (setf (aref ans jx) (row-major-aref arg ix)))
    ans))

(defmethod subseq-of ((arg ca:<carray>) start &optional end)
  (let* ((limit  (ca:carray-total-size arg))
         (nel    (- (or end limit) start))
         (ans    (make-array nel
                             :element-type
                             (cond ((ca:is-float-array  arg) 'single-float)
                                   ((ca:is-double-array arg) 'double-float)
                                   (t 'bignum))
                             )))
    (loop for ix from start below (or end limit)
          for jx from 0
          do
          (setf (aref ans jx) (ca:caref arg ix)))
    ans))
          
;; ------------------------------------------
(defun get-range (range v islog)
  (if (and range
           (/= (first range) (second range)))
      range
    (let ((v (if islog
                 (remove-if (complement #'plusp) v)
               v)))
      (if (plusp (length-of v))
          (let* ((vmin (vmin-of v))
                 (vmax (vmax-of v)))
            (if (= vmin vmax)
                (setf vmax (if (zerop vmin)
                               0.1
                             (* 1.1 vmin))))
            (list vmin vmax))
        (list (if islog 0.1 0) 1))
      )))

(defconstant $largest-permissible-value
  (/ least-positive-normalized-single-float))

(defmethod pw-init-xv-yv ((cpw <plotter-mixin>) xv yv
                          &key xrange yrange box xlog ylog aspect
                          &allow-other-keys)
  ;; initialize basic plotting parameters -- log scale axes, axis ranges,
  ;; plotting interior region (the box), and the graphic transforms to/from
  ;; data space to "pixel" space.  Pixel in quotes because they are real pixels
  ;; on Win/XP, but something altogether different on OS/X Display PDF.
  (let* ((_box (or box
                   (inset-box-sides (list 0 0
                                          (plotter-nominal-width  cpw)
                                          (plotter-nominal-height cpw))
                                    30 20 10 30)
                   )))
    (destructuring-bind (_xmin _xmax)
        (if xv
            (get-range xrange xv xlog)
          (get-range xrange (list 0 (1- (length-of yv))) xlog))
      (destructuring-bind (_ymin _ymax) (get-range yrange yv ylog)

        (if xlog
            (setf _xmin (log10 _xmin)
                  _xmax (log10 _xmax)))
        (if ylog
            (setf _ymin (log10 _ymin)
                  _ymax (log10 _ymax)))
        
        (unless yrange
          (let ((dy (/ (qrange (- _ymax _ymin)) 18)))
            (setf _ymin (max (- _ymin dy) (- $largest-permissible-value)))
            (setf _ymax (min (+ _ymax dy) $largest-permissible-value))
            ))
        
        (unless xrange
          (let ((dx (/ (qrange (- _xmax _xmin)) 18)))
            (setf _xmin (max (- _xmin dx) (- $largest-permissible-value)))
            (setf _xmax (min (+ _xmax dx) $largest-permissible-value))
            ))
        
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

;; ---------------------------------------------------------

(defun vector-group-min (yvecs)
  (reduce #'min (mapcar #'vmin-of yvecs)))

(defun vector-group-max (yvecs)
  (reduce #'max (mapcar #'vmax-of yvecs)))

(defun pw-init-bars-xv-yv (cpw xvec yvecs &rest args)
  ;; just run the usual scaling initialization
  ;; but against a y-vector that contains those values
  ;; from the multiple vectors which have the largest absolute values
  (apply #'pw-init-xv-yv cpw
         (or (and xvec
                  (list (vmin-of xvec) (vmax-of xvec)))
             (and yvecs
                  (list 0 (1- (length-of (first yvecs))))
                  ))
         (and yvecs
              (list (vector-group-min yvecs)
                    (vector-group-max yvecs)))
         args))

;; ------------------------------------------
(defun draw-path (port &rest positions)
  (gp:draw-polygon port
                   (mapcan #'append positions)))

(defun bounds-overlap-p (bounds1 bounds2)
  (labels ((overlaps-p (bounds1 bounds2)
             (destructuring-bind (left1 right1) bounds1
               (destructuring-bind (left2 right2) bounds2
                 (declare (ignore right2))
                 (<= left1 left2 right1))
               )))
    (or (overlaps-p bounds1 bounds2)
        (overlaps-p bounds2 bounds1))
    ))

(defun expand-bounds (bounds dx)
  (list (- (first bounds) dx)
        (+ (second bounds) dx)))

;; ------------------------------------------
;; Convenience macros

(defmacro with-color ((pane color) &body body)
  `(gp:with-graphics-state
       (,pane
        :foreground ,color)
     ,@body))
  
(defmacro with-mask ((pane mask) &body body)
  `(gp:with-graphics-state
       (,pane
        :mask ,mask)
     ,@body))

;; ------------------------------------------
(defun draw-string-x-y (pane port string x y
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
  ;; Draw a string at some location, unless the bounds of the new string
  ;; overlap the previous bounds. This is used to avoid placing axis labels
  ;; too closely together along the grid.
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent port string font)
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

        (with-color (port color)
          (with-mask (port (and clip
                                (adjust-box (plotter-box pane))))
              (gp:draw-string port string (+ x dx) (+ y dy)
                              :font font
                              :block (not transparent))
            new-bounds
            )))
      )))

;; ------------------------------------------
#+:COCOA
(defun draw-vert-string-x-y (port string x y
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
      (gp:get-string-extent port string font)
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
          (add-label port string x  y
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
(defun draw-vert-string-x-y (port string x y
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
      (gp:get-string-extent port string font)

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

        (progn
          (with-pixmap-graphics-port (ph port wd ht
                                         :background :white
                                         :foreground :black
                                         :clear t)
            (gp:draw-string ph string
                            0 (- tp)
                            :font       font
                            :foreground color
                            :block      (not transparent))
            
            (with-image (port (v-image #+:COCOA (gp:make-image port ht wd)
                                       #+:WIN32 (gp:make-image port ht wd
                                                               :alpha nil)
                                       ))
              (with-image (ph (h-image (gp:make-image-from-port ph)))
                (with-image-access (ha (gp:make-image-access ph h-image))
                  (with-image-access (va (gp:make-image-access port v-image))
                    (gp:image-access-transfer-from-image ha)
                    (loop for ix from 0 below wd do
                          (loop for iy from 0 below ht do
                                (setf (gp:image-access-pixel va iy (- wd ix 1))
                                      (gp:image-access-pixel ha ix iy))
                                ))
                    (gp:image-access-transfer-to-image va)
                    )))
              (gp:draw-image port v-image
                             (+ x dy)
                             (+ y dx))
              ))
          new-bounds))
      )))

;;-------------------------------------------------------------------
;; Abstract superclass <scanner> represent objects that respond to the NEXT-ITEM method
;;

(defclass <scanner> ()
  ())

(defclass <limited-scanner> (<scanner>)
  ((limit  :accessor scanner-limit    :initarg :limit)
   (pos    :accessor scanner-position :initform 0)))

(defclass <counting-scanner> (<limited-scanner>)
  ())

(defclass <vector-scanner> (<limited-scanner>)
  ((vec  :accessor scanner-vector :initarg :vector)))

(defclass <list-scanner> (<limited-scanner>)
  ((lst        :accessor scanner-list :initarg :list)
   (lst-backup :accessor scanner-list-backup)))

(defclass <array-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

(defclass <carray-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

;; ===============
(defmethod make-scanner ((limit integer) &key (max-items limit))
  (make-instance '<counting-scanner>
                 :limit (min limit max-items)))

(defmethod make-scanner ((vec vector) &key (max-items (length vec)))
  (make-instance '<vector-scanner>
                 :limit   (min (length vec) max-items)
                 :vector  vec))

(defmethod make-scanner ((lst list) &key (max-items (length lst)))
  (make-instance '<list-scanner>
                 :list  lst
                 :limit (min (length lst) max-items)))

(defmethod initialize-instance :after ((self <list-scanner>)
                                       &rest args &key &allow-other-keys)
  (setf (scanner-list-backup self) (scanner-list self)))

(defmethod make-scanner ((arr array) &key (max-items (array-total-size arr)))
  (make-instance '<array-scanner>
                 :array  arr
                 :limit  (min (array-total-size arr) max-items)))

(defmethod make-scanner ((arr ca:<carray>) &key (max-items (ca:carray-total-size arr)))
  (make-instance '<carray-scanner>
                 :array  arr
                 :limit  (min (ca:carray-total-size arr) max-items)))

;; ===============
;; All scanners pass through NIL as the terminal value
(defmethod next-item ((cscanner <counting-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )) cscanner
    (let ((ans position))
      (when (< ans limit)
        (incf position)
        ans)
      )))

(defmethod next-item ((lscanner <list-scanner>))
  (with-accessors ((limit    scanner-limit   )
                   (position scanner-position)
                   (its-list scanner-list    )) lscanner
    (when (< position limit)
      (incf position)
      (pop its-list))
    ))

(defmethod next-item ((vscanner <vector-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )
                   (its-vector scanner-vector  )) vscanner
  
  (when (< position limit)
    (let ((ans (aref its-vector position)))
      (incf position)
      ans))
  ))

(defmethod next-item ((ascanner <array-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) ascanner
    (when (< position limit)
      (let ((ans (row-major-aref its-array position)))
        (incf position)
        ans))
    ))

(defmethod next-item ((cascanner <carray-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) cascanner
    (when (< position limit)
      (let ((ans (ca:row-major-caref its-array position)))
        (incf position)
        ans))
    ))

;; ===============
(defmethod reset-scanner ((scanner <limited-scanner>))
  (setf (scanner-position scanner) 0))

(defmethod reset-scanner :after ((scanner <list-scanner>))
  (setf (scanner-list scanner) (scanner-list-backup scanner)))

;; ===============
(defclass <transformer> (<scanner>)
  ((src   :accessor transformer-source  :initarg :source)
   (xform :accessor transformer-xform   :initarg :xform)))

(defmethod make-transformer ((src <scanner>) (xform function))
  (make-instance '<transformer>
                 :source src
                 :xform  xform))

(defmethod next-item ((xf <transformer>))
  ;; pass along NIL as a terminal value
  (with-accessors  ((source   transformer-source)
                    (xform    transformer-xform )) xf
    (let ((item (next-item source)))
      (when item
        (funcall xform item)))
    ))

(defmethod reset-scanner ((xf <transformer>))
  (reset-scanner (transformer-source xf)))

;; ===============
(defclass <pair-scanner> (<scanner>)
  ((xsrc   :accessor pair-scanner-xsrc   :initarg :xsrc)
   (ysrc   :accessor pair-scanner-ysrc   :initarg :ysrc)
   (pair   :accessor pair-scanner-values :initform (make-array 2))
   ))

(defmethod make-pair-scanner ((xs <scanner>) (ys <scanner>))
  (make-instance '<pair-scanner>
                 :xsrc  xs
                 :ysrc  ys
                 ))

(defmethod next-item ((pairs <pair-scanner>))
  (with-accessors ((xs    pair-scanner-xsrc  )
                   (ys    pair-scanner-ysrc  )
                   (pair  pair-scanner-values)) pairs
    (let* ((x (next-item xs))
           (y (next-item ys)))
      (when (and x y)
        (setf (aref pair 0) x
              (aref pair 1) y)
        pair))
    ))

(defmethod reset-scanner ((pairs <pair-scanner>))
  (reset-scanner (pair-scanner-xsrc pairs))
  (reset-scanner (pair-scanner-ysrc pairs)))

;; ------------------------------------------
#|
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

|#
;; -------------------------------------------------------
(defmethod draw-vertical-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-x
         (wd   (* 0.1 (gp:port-width port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (loop for pair = (next-item bars)
          while pair
          do
          (destructure-vector (x y) pair
            (when xprev
              (setf wd   (abs (- x xprev))
                    wd/2 (* 0.5 wd))
              (unless (= y yprev)
                (let ((next-x (+ xprev wd/2))
                      (prev-x (or last-x
                                  (- xprev wd/2))
                              ))
                  (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
                  (setf last-x next-x)
                  )))
            (setf xprev x
                  yprev y))
          finally
          (when xprev
            ;; use the last known width
            (let ((next-x (+ xprev wd/2))
                  (prev-x (or last-x
                              (- xprev wd/2))
                          ))
              (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
              ))
          )))
                             
(defmethod draw-horizontal-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-y
         (wd   (* 0.1 (gp:port-height port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (loop for pair = (next-item bars)
          while pair
          do
          (destructure-vector (x y) pair
            (when yprev
              (setf wd   (abs (- y yprev))
                    wd/2 (* 0.5 wd))
              (unless (= x xprev)
                (let ((next-y (+ yprev wd/2))
                      (prev-y (or last-y
                                  (- yprev wd/2))
                              ))
                  (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
                  (setf last-y next-y)
                  )))
            (setf xprev x
                  yprev y))
          finally
          (when xprev
            ;; use the last known width
            (let ((next-y (+ yprev wd/2))
                  (prev-y (or last-y
                              (- yprev wd/2))
                          ))
              (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
              ))
          )))

(defmethod draw-staircase (port (pairs <pair-scanner>))
  (let* (xprev
         yprev
         last-x
         (wd    (* 0.1 (gp:port-width port)))     ;; default for only one data point
         (wd/2  (* 0.5 wd)))
    (loop for pair = (next-item pairs)
          while pair
          do
          (destructure-vector (x y) pair
            (when xprev
              (setf wd   (abs (- x xprev))
                    wd/2 (* 0.5 wd))
              (unless (= y yprev)
                (let ((next-x (- x wd/2)))
                  (gp:draw-polygon port
                                   (list (or last-x (- xprev wd/2)) yprev
                                         next-x yprev
                                         next-x y)
                                   :closed nil)
                  (setf last-x next-x)
                  )))
            (setf xprev x
                  yprev y))
          finally
          (when xprev
            (gp:draw-line port
                          (or last-x (- xprev wd/2)) yprev
                          (+ xprev wd/2) yprev))
          )))

(defmethod draw-polyline (port (pairs <pair-scanner>))
  (let (xprev yprev)
    (loop for pair = (next-item pairs)
          while pair
          do
          (destructure-vector (x y) pair
            (when (and (simple-real-number x)
                       (simple-real-number y))
              (when (and xprev
                         (or (/= x xprev)
                             (/= y yprev)))
                  (gp:draw-line port xprev yprev x y))
              (setf xprev x
                    yprev y)))
          )))
  
;; ----------------------------------------------------------  
#|
(defun get-symbol-plotfn (port symbol border-color border-thick symbol-filled fill-color)
  (labels ((translucent+frame (fn)
             #+:COCOA
             (with-color (port #.(color:make-gray 1.0 0.25))
               ;; translucent interior
               (funcall fn t))
             ;; solid frame
             (gp:with-graphics-state (port
                                      :thickness border-thick
                                      :foreground border-color)
               (funcall fn)))
           
           (solid+frame (fn)
             (with-color (port fill-color)
               (funcall fn t))
             (with-color (port border-color)
               (gp:with-graphics-state (port
                                        :thickness border-thick)
                 (funcall fn))))

           (draw-symbol (fn)
             (if symbol-filled
                 (solid+frame fn)
               (translucent+frame fn))))
    
    (ecase symbol
      (:cross     (lambda (x y)
                    (gp:with-graphics-state (port
                                             :thickness border-thick)
                      (with-color (port (or border-color
                                            (foreground-color port)))
                        (gp:draw-line port (- x 3) y (+ x 3) y)
                        (gp:draw-line port x (- y 3) x (+ y 3))
                        ))))
      
      (:circle
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle port
                                    x 
                                    #+:COCOA (- y 0.5)
                                    #+:WIN32 y
                                    3
                                    :filled filled)))
           (draw-symbol #'draw-circle)
           )))
      
      (:filled-circle
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle port
                                    ;; (if filled (1+ x) x)
                                    x
                                    #+:COCOA (- y 0.5)
                                    #+:WIN32 y
                                    ;;(1- y)
                                    3
                                    :filled filled)))
           (solid+frame #'draw-circle)
           )))
      
      ((:box :square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle port (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (draw-symbol #'draw-rectangle)
           )))
      
      ((:filled-box :filled-square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle port (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (solid+frame #'draw-rectangle)
           )))
      
      ((:triangle :up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:right-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           (+ x 4) y
                                           (- x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:left-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (+ x 3) (- y 3)
                                           (- x 4) y
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      ((:filled-triangle :filled-up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (solid+frame #'draw-triangle)
           )))
      
      (:filled-down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (solid+frame #'draw-triangle)
           )))
      
      (:dot
       (lambda (x y)
         (with-color (port (or border-color
                               (foreground-color port)))
           (gp:draw-circle port x (1- y) 0.5))
         ))
      )))
|#

(defun get-symbol-plotfn (port symbol-style)
  (labels ((draw-symbol (fn)
             #+:COCOA
             (with-color (port (or (fill-color symbol-style)
                                   #.(color:make-gray 1.0 0.25)))
               (funcall fn t))
             #+:WIN32
             (when (fill-color symbol-style)
               (with-color (port (fill-color symbol-style))
                 (funcall fn t)))
             (gp:with-graphics-state (port
                                      :thickness  (border-thick symbol-style)
                                      :foreground (border-color symbol-style))
               (funcall fn))))
    
    (ecase (plot-symbol symbol-style)
      (:cross     (lambda (x y)
                    (gp:with-graphics-state (port
                                             :thickness  (border-thick symbol-style)
                                             :foreground (border-color symbol-style))
                      (gp:draw-line port (- x 3) y (+ x 3) y)
                      (gp:draw-line port x (- y 3) x (+ y 3))
                      )))
      
      (:x         (lambda (x y)
                    (gp:with-graphics-state (port
                                             :thickness  (border-thick symbol-style)
                                             :foreground (border-color symbol-style))
                      (gp:draw-line port (- x 3) (- y 3) (+ x 3) (+ y 3))
                      (gp:draw-line port (+ x 3) (- y 3) (- x 3) (+ y 3))
                      )))
      
      (:circle
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle port
                                    x 
                                    #+:COCOA (- y 0.5)
                                    #+:WIN32 y
                                    3
                                    :filled filled)))
           (draw-symbol #'draw-circle)
           )))

      #|
      ((:filled-circle :sampled-data)
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle port
                                    ;; (if filled (1+ x) x)
                                    x
                                    #+:COCOA (- y 0.5)
                                    #+:WIN32 y
                                    ;;(1- y)
                                    3
                                    :filled filled)))
           (solid+frame #'draw-circle)
           )))
      |#

      ((:box :square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle port (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (draw-symbol #'draw-rectangle)
           )))

      #|
      ((:filled-box :filled-square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle port (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (solid+frame #'draw-rectangle)
           )))
      |#
      
      ((:triangle :up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:right-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           (+ x 4) y
                                           (- x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:left-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (+ x 3) (- y 3)
                                           (- x 4) y
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))

      #|
      ((:filled-triangle :filled-up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (solid+frame #'draw-triangle)
           )))
      
      (:filled-down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (solid+frame #'draw-triangle)
           )))
      |#

      (:dot
       (lambda (x y)
         (with-color (port (border-color symbol-style))
           (gp:draw-circle port x (1- y) 0.5))
         ))
      )))


(defmethod pw-plot-xv-yv ((cpw <plotter-mixin>) port xvector yvector 
                          &key
                          ;; (color #.(color:make-rgb 0.0 0.5 0.0))
                          ;; alpha
                          ;; thick
                          ;; (linewidth (or thick 1))
                          ;; linedashing
                          ;; symbol
                          ;; plot-joined
                          legend
                          legend-x
                          legend-y
                          legend-anchor
                          ;; (border-color color)
                          ;; symbol-filled
                          ;; (fill-color color)
                          ;; (border-thick linewidth)
                          ;; barwidth
                          ;; bar-offset
                          plot-style
                          &allow-other-keys)
  ;; this is the base plotting routine
  ;; called only from within the pane process
  (let* ((sf        (plotter-sf  cpw))
         (box       (let ((box (plotter-box cpw)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform cpw))
         
         ;; (color     (adjust-color cpw color alpha))
         ;; (linewidth (adjust-linewidth (* sf linewidth)))
         (line-style   (line-style   plot-style))
         (symbol-style (symbol-style plot-style))

         (nel       (if xvector
                        (min (length-of xvector) (length-of yvector))
                      (length-of yvector)))

         (xs         (let ((scanner (make-scanner (or xvector
                                                      nel))
                                    ))
                       (if (plotter-xlog cpw)
                           (make-transformer scanner #'log10)
                         scanner)))

         (ys         (let ((scanner (make-scanner yvector)))
                       (if (plotter-ylog cpw)
                           (make-transformer scanner #'log10)
                         scanner)))
         (pairs     (make-pair-scanner xs ys)))

    (when legend
      (append-legend cpw
                     (make-legend-info
                      :plot-style   plot-style
                      ;; :color        color
                      ;; :thick        linewidth
                      ;; :linedashing  linedashing
                      ;; :symbol       symbol
                      ;; :border-color border-color
                      ;; :symbol-filled symbol-filled
                      ;; :fill-color   fill-color
                      ;; :border-thick border-thick
                      ;; :plot-joined  plot-joined
                      :text         legend)))

    (when legend-x
      (setf (plotter-legend-x cpw) legend-x))
    (when legend-y
      (setf (plotter-legend-y cpw) legend-y))
    (when legend-anchor
      (setf (plotter-legend-anchor cpw) legend-anchor))

    (gp:with-graphics-state (port
                             ;; :thickness  linewidth
                             ;; :dashed     (not (null linedashing))
                             ;; :dash       (mapcar (um:expanded-curry (v) #'* sf) linedashing)
                             ;; :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)

      (labels ((draw-lines ()
                 (gp:with-graphics-state (port
                                          :thickness  (adjust-linewidth (line-thick line-style))
                                          :foreground (adjust-color port (line-color line-style))
                                          :dashed     (line-dashing line-style)
                                          :dash       (mapcar (um:expanded-curry (v) #'* sf)
                                                              (line-dashing line-style)))
                   (gp:with-graphics-scale (port sf sf)
                     (gp:with-graphics-transform (port xform)
                       (draw-polyline port pairs)))
                   )))

        (cond (symbol-style
               (case (plot-symbol symbol-style)
                 (:steps
                  (gp:with-graphics-state (port
                                           :thickness  (adjust-linewidth (line-thick line-style))
                                           :foreground (adjust-color port (line-color line-style)))
                    (gp:with-graphics-scale (port sf sf)
                      (gp:with-graphics-transform (port xform)
                        (draw-staircase port pairs)))
                    ))
                   
                   (:vbars
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port (fill-color symbol-style)))
                      (gp:with-graphics-scale (port sf sf)
                        (if (bar-width symbol-style)
                          (let* ((wd   (get-x-width cpw (bar-width symbol-style)))
                                 (wd/2 (* 0.5 wd))
                                 (off  (if (bar-offset symbol-style)
                                         (get-x-width cpw (bar-offset symbol-style))
                                         0)))
                            (loop for pair = (next-item pairs)
                                  while pair
                                  do
                                  (destructure-vector (x y) pair
                                    (multiple-value-bind (xx yy)
                                        (gp:transform-point xform x y)
                                      (multiple-value-bind (_ yy0)
                                          (gp:transform-point xform x 0)
                                        (declare (ignore _))
                                        (gp:draw-rectangle port
                                                           (+ off (- xx wd/2)) yy0
                                                           wd (- yy yy0)
                                                           :filled t)
                                        )))
                                  ))
                          (gp:with-graphics-transform (port xform)
                            (draw-vertical-bars port pairs))
                          ))))
                   
                   (:hbars
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port (fill-color symbol-style)))
                      (gp:with-graphics-scale (port sf sf)
                        (if (bar-width symbol-style)
                          (let* ((wd   (get-y-width cpw (bar-width symbol-style)))
                                 (wd/2 (* 0.5 wd))
                                 (off  (if (bar-offset symbol-style)
                                         (get-y-width cpw (bar-offset symbol-style))
                                         0)))
                            (loop for pair = (next-item pairs)
                                  while pair
                                  do
                                  (destructure-vector (x y) pair
                                    (multiple-value-bind (xx yy)
                                        (gp:transform-point xform x y)
                                      (multiple-value-bind (xx0 _)
                                          (gp:transform-point xform 0 y)
                                        (declare (ignore _))
                                        (gp:draw-rectangle port
                                                           xx0 (+ off (- yy wd/2))
                                                           (- xx xx0) wd
                                                           :filled t)
                                        )))
                                  ))
                          (gp:with-graphics-transform (port xform)
                            (draw-horizontal-bars port pairs))
                          ))))
                   
                   (:sampled-data
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port (line-color line-style))
                                             :thickness  (adjust-linewidth (line-thick line-style)))
                      (gp:with-graphics-scale (port sf sf)
                        (let ((dotfn (get-symbol-plotfn port (symbol-style plot-style))))
                          (loop for pair = (next-item pairs)
                                while pair
                                do
                                (destructure-vector (x y) pair
                                  (multiple-value-bind (xx yy)
                                      (gp:transform-point xform x y)
                                    (multiple-value-bind (_ yy0)
                                        (gp:transform-point xform x 0)
                                      (declare (ignore _))
                                      (gp:draw-line port xx yy0 xx yy)
                                      (funcall dotfn xx yy))
                                    ))
                                )))
                      ))
                   
                   (otherwise
                    (when line-style
                      (draw-lines)
                      (reset-scanner pairs))
                    
                    (gp:with-graphics-scale (port sf sf)
                      (let ((plotfn (get-symbol-plotfn port symbol-style)))
                        (loop for pair = (next-item pairs)
                              while pair
                              do
                              (destructure-vector (x y) pair
                                (multiple-value-bind (xx yy)
                                    (gp:transform-point xform x y)
                                  (funcall plotfn xx yy)
                                  )))
                        )))
                   ))
               
               
              (line-style (draw-lines))
              ))
      )))

;; ------------------------------------------------------------------------------
(defun get-bar-symbol-plotfn (port symbol color neg-color bar-width testfn)
  ;; bear in mind that the y values at this point are absolute screen
  ;; coords and are inverted with respect to data ordering
  (ecase symbol
    (:sigma
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line port x ymin x ymax)
         (gp:draw-line port (- x (/ bar-width 2)) ymin (+ x (/ bar-width 2)) ymin)
         (gp:draw-line port (- x (/ bar-width 2)) ymax (+ x (/ bar-width 2)) ymax)
         )))

    (:hl-bar
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line port x ymin x ymax)
         )))
    
    (:hlc-bar
     (lambda (x ys)
       (destructure-vector (h l c) ys
         (gp:draw-line port x l x h)
         (gp:draw-line port x c (+ x (/ bar-width 2)) c)
         )))
    
    (:ohlc-bar
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (with-color (port (if (funcall testfn c o) neg-color color))
           (gp:draw-line port x l x h)
           (gp:draw-line port (- x (/ bar-width 2)) o x o)
           (gp:draw-line port x c (+ x (/ bar-width 2)) c)
           ))))
    
    (:candlestick
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (if (funcall testfn c o)
             (with-color (port neg-color)
               (gp:draw-line port x l x h)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
           (progn
             (with-color (port :black)
               (gp:draw-line port x l x h))
             (with-color (port color)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
             (with-color (port :black)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)))
             ))
         )))
    ))

;;-------------------------------------------------------------------
(defmethod pw-plot-bars-xv-yv ((cpw <plotter-mixin>) port xvector yvectors 
                          &key
                          (color #.(color:make-rgb 0.0 0.5 0.0))
                          (neg-color color)
                          alpha
                          thick
                          (linewidth (or thick 1))
                          (bar-width 6)
                          (symbol (ecase (length yvectors)
                                    (2 :sigma)
                                    (3 :hlc-bar)
                                    (4 :ohlc-bar)))
                          &allow-other-keys)
  ;; this is the base bar-plotting routine
  ;; called only from within the pane process
  (let* ((sf        (plotter-sf  cpw))
         (box       (let ((box (plotter-box cpw)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform cpw))
         (color     (adjust-color cpw color alpha))
         (neg-color (adjust-color cpw neg-color alpha))
         (linewidth (adjust-linewidth (* sf linewidth)))

         (nel       (let ((nely (reduce #'min (mapcar #'length-of yvectors))))
                      (if xvector
                          (min (length-of xvector) nely)
                        nely)))
         
         (xs        (let* ((xform   (lambda (x)
                                      (gp:transform-point xform x 0)))
                           (scanner (make-scanner (or xvector
                                                      nel)
                                                  :max-items nel)))
                      (make-transformer scanner
                                        (if (plotter-xlog cpw)
                                            (um:compose xform #'log10)
                                          xform))
                      ))

         (xform-y   (lambda (y)
                      (second (multiple-value-list
                               (gp:transform-point xform 0 y)))
                      ))

         (ys        (let* ((scanners (mapcar #'make-scanner yvectors)))
                      (mapcar (um:rcurry #'make-transformer
                                         (if (plotter-ylog cpw)
                                             (um:compose xform-y #'log10)
                                           xform-y))
                              scanners)
                      ))
         (c<o-testfn (let ((y1 (funcall xform-y 0))
                           (y2 (funcall xform-y 1)))
                       (if (< y2 y1)
                           #'>
                         #'<)))
         (plotfn (get-bar-symbol-plotfn port symbol
                                        color neg-color bar-width
                                        c<o-testfn))
         (tmp       (make-array (length ys))))
    
    (gp:with-graphics-state (port
                             :thickness  linewidth
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)
      
      (gp:with-graphics-scale (port sf sf)
        (loop for x = (next-item xs)
              while x
              do
              (map-into tmp #'next-item ys)
              (funcall plotfn x tmp)))
      )))

;; ============================================================
(defun plt-draw-shape (pane port shape x0 y0 x1 y1
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
    
    (gp:with-graphics-state (port
                             :thickness  linewidth
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)

      (gp:with-graphics-scale (port sf sf)

        (gp:with-graphics-transform (port xform)
          (when filled
            (ecase shape
              (:rect
               (gp:draw-rectangle port
                                  x0 y0 wd ht
                                  :filled t))
              (:ellipse
               (gp:draw-ellipse port
                                x0 y0 x1 y1
                                :filled t))
              
              (:arc
               (gp:draw-arc port
                            x0 y0 wd ht
                            start-angle sweep-angle
                            :filled t))
              ))
          
          (when border-thick
            (with-color (port bcolor)
              (case shape
                (:rect
                 (gp:draw-rectangle port
                                    x0 y0 wd ht
                                    :filled nil))
                (:ellipse
                 (gp:draw-ellipse port
                                  x0 y0 x1 y1
                                  :filled nil))

                (:arc
                 (gp:draw-arc port
                              x0 y0 wd ht
                              start-angle sweep-angle
                              :filled nil))
                )))
          )))
    ))


;; ------------------------------------------
(defun calc-start-delta (vmin vmax)
  ;; compute a good axis increment and starting value
  ;; these are considered good if the increment is a multiple of 1, 2, or 5.
  ;; The starting value must be the largest whole part of the axis values:
  ;; e.g.,
  ;; if the axis ranges from 1.23 to 3.28, then the largest whole part will be 2.00.
  ;; That will be our starting label, and we then number by (non-overlapping strings)
  ;; at increment spacings on either side of that largest whole part.
  ;;
  ;; This avoid bizarre labels like 1.23 ... 1.37 ... 2.45 ...
  ;; giving instead, someting like  1.2 .. 1.6 .. 2.0 .. 2.4 ...
  ;; which is enormously more readable than what most plotting packages produce.
  ;; (This is the way a human would chart the axes)
  ;;
  (destructuring-bind (sf c)
      (loop for sf = (/ (pow10
                         (ceiling (log10 (max (abs vmin)
                                              (abs vmax))
                                         ))
                         ))
            then (* 10.0d0 sf)
            do
            ;;
            ;; this loop finds the scale factor sf and minimum integer value c such that
            ;; the scaled min and max values span a range greater than 1
            ;; and c is no further from the scaled min value than that range.
            ;; It is the case that a <= c <= b, where a and b are the scaled min and max values,
            ;; and abs(c) is some integer multiple (positive, zero, or negative) of 10.
            ;;
            (let* ((a   (* sf vmin))
                   (b   (* sf vmax))
                   (rng (abs (- b a)))
                   (c   (* 10.0d0 (ceiling (min a b) 10.0d0))))
              (if (and (> rng 1.0d0)
                       (<= (abs (- c a)) rng))
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
                                  ((> rng 2.0d0) 0.5d0)
                                  (t             0.2d0)))
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
(defvar *ext-logo*
  (ignore-errors
    (gp:read-external-image
     (translate-logical-pathname
      #+:COCOA "PROJECTS:DYLIB;Logo75Img-Alpha25y.pdf"
      #+:WIN32 "PROJECTS:DYLIB;Logo75Img-Alpha25y.bmp")
     )))

(defvar *ext-logo-alpha* 1)

(defun stamp-logo (pane port logo logo-alpha)
  (when logo
    (let* ((box (plotter-box pane))
           (bwd (box-width  box))
           (bht (box-height box))
           (sf  (plotter-sf  pane)))
      (with-image (port
                   (image (gp:convert-external-image port logo)))
        (let* ((iwd  (gp:image-width  image))
               (iht  (gp:image-height image))
               (isf  (min (/ bwd iwd)
                          (/ bht iht)))
               (top  (+ (box-top box)
                        (* 0.5 (- bht (* isf iht)))
                        ))
               (left (+ (box-left box)
                        (* 0.5 (- bwd (* isf iwd)))
                        )))
          (gp:with-graphics-scale (port sf sf)
            (gp:draw-image port image left top
                           :from-width  iwd
                           :from-height iht
                           :to-width    (* isf iwd)
                           :to-height   (* isf iht)
                           ;; WATCH OUT! if we don't have a float for global alpha
                           ;; then the COCOA system bombs out really badly...
                           :global-alpha (float logo-alpha 1.0)))
          ))
      )))

(defun watermark (pane port logo logo-alpha)
  (let* ((box     (plotter-box pane))
         (sf      (plotter-sf  pane))
         (cright1 "Copyright (c) 2006-2007 by Refined Audiometrics Laboratory, LLC")
         (cright2 "All rights reserved.")
         (font2   (find-best-font pane
                                  :size   (* sf $tiny-times-font-size)))
         (color2  #.(color:make-gray 0.7)))
    
    (stamp-logo pane port logo logo-alpha)
    
    (let* ((left   (+ (box-left   box) 10))
           (bottom (- (box-bottom box) 14)))
      (draw-string-x-y pane port cright1
                       (* sf left)
                       (* sf (- bottom 11))
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2)
      (draw-string-x-y pane port cright2
                       (* sf left)
                       (* sf bottom)
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2)
      )))

;; ------------------------------------------
(defparameter *log-subdivs*
  (mapcar #'log10
          '(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
                2 3 4 5 6 7 8 9)))

(defparameter $axis-style
  (make-instance '<plot-style>
                 :line-style (make-instance '<line-style>
                                            :line-color #.(color:make-gray 0.5))))

(defmethod pw-axes ((cpw <plotter-mixin>) port
                    &key
                    (fullgrid t)
                    (xtitle "X")
                    (ytitle "Y")
                    (title  "Plot")
                    (watermarkfn #'watermark)
                    (logo *ext-logo*)
                    (logo-alpha *ext-logo-alpha*)
                    &allow-other-keys)
  (let* ((box   (plotter-box cpw))
         (sf    (plotter-sf cpw))
         (font  (find-best-font cpw
                                :size (* (plotter-sf cpw) $normal-times-font-size)
                                ))
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

      (gp:clear-graphics-port port)
      (if watermarkfn
          (funcall watermarkfn cpw port logo logo-alpha))

      (when title
        (draw-string-x-y cpw port title
                         (floor (* sf (+ (box-left box) (box-right box))) 2)
                         0
                         :x-alignment :center
                         :y-alignment :top
                         :font        (find-best-font cpw
                                                      :size (* (plotter-sf cpw)
                                                               $big-times-font-size))
                         ))

      (gp:with-graphics-scale (port sf sf)
        (gp:with-graphics-state (port :scale-thickness t)
          (draw-path port
                     (box-top-left     box)
                     (box-bottom-left  box)
                     (box-bottom-right box)
                     )))
        
      (pw-plot-xv-yv cpw port
                     (vector (iqxlog (plotter-xmin cpw))
                             (iqxlog (plotter-xmax cpw)))
                     (vector (iqylog 0) (iqylog 0))
                     ;; :color #.(color:make-gray 0.5)
                     :plot-style $axis-style)
        
      (pw-plot-xv-yv cpw port
                     (vector (iqxlog 0) (iqxlog 0))
                     (vector (iqylog (plotter-ymin cpw))
                             (iqylog (plotter-ymax cpw)))
                     ;; :color #.(color:make-gray 0.5)
                     :plot-style $axis-style)

      (when xtitle
        (draw-string-x-y cpw port xtitle
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
                                      cpw port (plabel (iqxlog xval))
                                      (* sf xpos)
                                      (* sf (+ 4 (box-bottom box)))
                                      :prev-bounds xprev
                                      :margin (* 2 sf)
                                      :x-alignment :center
                                      :y-alignment :top
                                      :font font)))
                           
                         (gp:with-graphics-scale (port sf sf)
                           (gp:with-graphics-state
                               (port
                                :scale-thickness t)
                             (when fullgrid
                               (when xlog
                                 (with-color (port #.(color:make-gray 0.75))
                                   (let ((xscale (first (plotter-xform cpw))))
                                     (loop for ix in *log-subdivs* do
                                           (let ((x (+ xpos (* xscale ix))))
                                             (if (< (box-left box) x
                                                    (box-right box))
                                                 (gp:draw-line
                                                  port
                                                  x (box-top box)
                                                  x (box-bottom box))
                                               )))
                                     )))
                               (unless (zerop xval)
                                 (with-color (port (if (vectorp fullgrid)
                                                       fullgrid
                                                     (color:make-gray
                                                      (if xlog 0.5 0.75))))
                                   (gp:draw-line port
                                                 xpos (box-top box)
                                                 xpos (box-bottom box))
                                   )))
                               
                             (gp:draw-line port
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
        (draw-vert-string-x-y port ytitle
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
                                       port
                                       (plabel (iqylog yval))
                                       (* sf (- (box-left box) #+:WIN32 1 #+:COCOA 3))
                                       (* sf ypos)
                                       :prev-bounds yprev
                                       :margin (* 2 sf)
                                       :x-alignment :center
                                       :y-alignment :bottom
                                       :font font)))
                             
                           (gp:with-graphics-scale (port sf sf)
                             (gp:with-graphics-state
                                 (port :scale-thickness t)
                               (when fullgrid
                                 (when ylog
                                   (with-color (port #.(color:make-gray 0.75))
                                     (let ((yscale (fourth (plotter-xform cpw))))
                                       (loop for ix in *log-subdivs* do
                                             (let ((y (+ ypos (* yscale ix))))
                                               (if (> (box-bottom box) y
                                                      (box-top box))
                                                   (gp:draw-line
                                                    port
                                                    (1+ (box-left box)) y
                                                    (box-right box) y)
                                                 ))))
                                     ))
                                 (unless (zerop yval)
                                   (with-color (port (if (vectorp fullgrid)
                                                         fullgrid
                                                       (color:make-gray
                                                        (if ylog 0.5 0.75))))
                                     (gp:draw-line port
                                                   (1+ (box-left box))  ypos
                                                   (box-right box) ypos)
                                     )))
                                 
                               (gp:draw-line port
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
;; pane location decoding for strings and legends
;;
(defun parse-location (sym pane)
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
                        
                        ((char-equal #\t (char s ix))
                         ;; 't' for top
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel (plotter-nominal-height pane))
                               ))
                        
                        ((char-equal #\r (char s ix))
                         ;; 'r' for right
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel (plotter-nominal-width pane))
                               ))

                        ((or (char-equal #\b (char s ix))
                             (char-equal #\l (char s ix)))
                         ;; 'b' for bottom, 'l' for left
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel 0)
                               ))
                        
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

(defun get-location (pane pos-expr axis &key scale)
  (cond
   
   ((consp pos-expr)
    (let* ((sym (um:mkstr (first pos-expr)))
           (val (second pos-expr)))
      (ecase (char-upcase (char sym 0))
        ;; accommodates :DATA :DAT :D :DATUM, :FRAC :F :FRACTION, :PIXEL :P :PIX :PIXELS, etc.
        (#\F  ;; pane fraction  0 = left, bottom;  1 = right, top
              (ecase axis
                (:x  (* val (plotter-nominal-width pane)))

                ;; port y axis is inverted, top at 0
                (:y  (* (- 1 val) (plotter-nominal-height pane)))
                ))
        
        (#\D  ;; data coordinates
              (ecase axis
                (:x
                 (let ((ans (gp:transform-point (plotter-xform pane)
                                                (if (plotter-xlog pane)
                                                    (log10 val)
                                                  val)
                                                0)))
                   (if scale
                       (- ans (gp:transform-point (plotter-xform pane) 0 0))
                     ans)))
                                                  
                (:y
                 (let ((ans (multiple-value-bind (xx yy)
                                (gp:transform-point (plotter-xform pane)
                                                    0
                                                    (if (plotter-ylog pane)
                                                        (log10 val)
                                                      val))
                              (declare (ignore xx))
                              yy)))
                   (if scale
                       (- ans (second (multiple-value-list
                                       (gp:transform-point (plotter-xform pane)
                                                           0 0))))
                     ans)))
                ))
        
        (#\P  ;; direct pixel positioning
              (ecase axis
                (:x val)

                ;; port y axis is inverted, top at 0
                (:y (- (plotter-nominal-height pane) val 1))
                ))
        )))

   ((numberp pos-expr) ;; assume :DATA
    (get-location pane (list :data pos-expr) axis :scale scale))

   (t ;; else, expect a parsable symbol or string '1.2data+3pix
      (destructuring-bind (vtype v &optional (dv 0)) (parse-location pos-expr pane)
        (+ (get-location pane (list vtype v) axis :scale scale)
           (ecase axis
             (:x dv)
             (:y (- dv))  ;; port y axis is inverted, top at 0
             ))))
   ))
  
(defun get-x-location (pane x)
  (get-location pane x :x))

(defun get-y-location (pane y)
  (get-location pane y :y))

(defun get-x-width (pane wd)
  (get-location pane wd :x :scale t))

(defun get-y-width (pane wd)
  (get-location pane wd :y :scale t))

;; ----------------------------------------------------------------
(defun draw-legend (pane port)
  (let ((items (all-legends pane)))
    (when items
      (let* ((sf   (plotter-sf pane))
             (font (find-best-font port
                                   :size (* sf $tiny-times-font-size)))
             (nitems (length items)))
        
        (multiple-value-bind (txtwd txtht txtbase)
            (let ((maxwd   0)
                  (maxht   0)
                  (maxbase 0))
              (loop for item in items do
                    (multiple-value-bind (lf tp rt bt)
                        (gp:get-string-extent port (legend-info-text item) font)
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
                 (x     (let ((x (get-x-location pane (plotter-legend-x pane))))
                          (ecase (plotter-legend-anchor pane)
                            (:auto         (if (> x (/ (plotter-nominal-width pane) 2))
                                             (- x effwd)
                                             x))
                            ((:nw :w :sw)  (- x effwd))
                            ((:ne :e :se)  x)
                            ((:n  :ctr :s) (- x (/ effwd 2)))
                            )))
                 (y     (let ((y (get-y-location pane (plotter-legend-y pane))))
                          (case (plotter-legend-anchor pane)
                            (:auto         (if (> y (/ (plotter-nominal-height pane) 2))
                                             (- y effht)
                                             y))
                            ((:nw :n :ne)  y)
                            ((:sw :s :sw)  (- y effht))
                            ((:w  :ctr :e) (- y (/ effht 2)))
                            ))))
            
            (gp:with-graphics-scale (port sf sf)
              
              (with-color (port (adjust-color port :white 0.75))
                (gp:draw-rectangle port x y effwd effht
                                   :filled t))
              
              (gp:with-graphics-state (port :thickness (adjust-linewidth sf))
                (gp:draw-rectangle  port x y effwd effht))
              
              (loop for item in items
                    for y from (+ y effht1) by effht1
                    do
                    (let* ((plot-style   (legend-info-plot-style item))
                           (line-style   (line-style   plot-style))
                           (symbol-style (symbol-style plot-style)))

                      ;; ---------------------------------------------
                      (labels ((draw-line (&optional thickness)
                                 (gp:with-graphics-state
                                     (port
                                      :thickness  (adjust-linewidth
                                                   (or thickness
                                                       (line-thick line-style)))
                                      :dashed     (line-dashing line-style)
                                      :dash       (mapcar (um:expanded-curry (v) #'* sf)
                                                          (line-dashing line-style))
                                      :foreground (adjust-color port (line-color line-style)))
                                   (let ((y (floor (- y (/ effht1 2)))))
                                     (gp:draw-line port
                                                   (+ x  3) y
                                                   (+ x 33) y)
                                     ))))

                        ;; ---------------------------------------------
                        (cond  (symbol-style
                                (case (plot-symbol symbol-style)
                                  ((:vbars :hbars) (draw-line 5))
                                  (:steps          (draw-line))
                                  (otherwise
                                   (when line-style
                                     (draw-line))
                                   (funcall (get-symbol-plotfn port symbol-style)
                                            (+ x 18) (- y (/ effht1 2))
                                            ))
                                  ))
                               
                               (line-style (draw-line))
                               ))
                      
                      ;; ---------------------------------------------
                      (gp:draw-string port (legend-info-text item) (+ x 36) (- y 3)
                                      :font font)
                      
                      ))
              ))
          ))
      )))
                 
;; ----------------------------------------------------------------
    
(defun get-plot-style (&key
                       (color #.(color:make-rgb 0.0 0.5 0.0))
                       (line-color color)
                       alpha
                       (line-alpha alpha)
                       thick
                       (linewidth (or thick 1))
                       (line-thick linewidth)
                       line-dashing
                       symbol
                       plot-joined
                       (border-color color)
                       (border-alpha alpha)
                       symbol-filled
                       fill-color
                       (fill-alpha alpha)
                       (border-thick linewidth)
                       bar-width
                       bar-offset
                       plot-style
                       line-style
                       symbol-style
                       &allow-other-keys)
  (or (if plot-style
        (if (consp plot-style)
          (apply 'make-instance '<plot-style> plot-style)
          plot-style))
      (make-instance '<plot-style>
                     :line-style (or (if line-style
                                       (if (consp line-style)
                                         (apply 'make-instance '<line-style> line-style)
                                         line-style))
                                     (if (or (null symbol)
                                             (eq symbol :steps)
                                             (eq symbol :sampled-data)
                                             plot-joined)
                                       (make-instance '<line-style>
                                                      :line-thick   line-thick
                                                      :line-dashing line-dashing
                                                      :line-color   line-color
                                                      :line-alpha   line-alpha)
                                       ))
                     :symbol-style (or (if symbol-style
                                         (if (consp symbol-style)
                                           (apply 'make-instance '<symbol-style> symbol-style)
                                           symbol-style))
                                       (if symbol
                                         (make-instance '<symbol-style>
                                                        :plot-symbol  (case symbol
                                                                        ((:filled-circle :sampled-data)         :circle)
                                                                        ((:filled-square :filled-box)           :square)
                                                                        ((:filled-triangle :filled-up-triangle) :up-triangle)
                                                                        (:filled-down-triangle                  :down-triangle)
                                                                        (otherwise symbol))
                                                        :fill-color   (or fill-color
                                                                          (and (or symbol-filled
                                                                                   (member symbol
                                                                                           '(:filled-circle
                                                                                             :sampled-data
                                                                                             :filled-square
                                                                                             :filled-box
                                                                                             :filled-triangle
                                                                                             :filled-up-triangle
                                                                                             :filled-down-triangle)))
                                                                               color))
                                                        :fill-alpha   fill-alpha
                                                        :border-color border-color
                                                        :border-alpha border-alpha
                                                        :border-thick border-thick
                                                        :bar-width    bar-width
                                                        :bar-offset   bar-offset))
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

(defun do-plot (cpw port xvector yvector
                    &rest args)
  (apply #'pw-init-xv-yv cpw xvector yvector args)
  (apply #'pw-axes cpw port args)
  ;;
  ;; Now plot the data points
  ;; 
  (apply #'pw-plot-xv-yv cpw port xvector yvector args))

(defun do-plot-bars (cpw port xvector yvectors
                         &rest args)
  (apply #'pw-init-bars-xv-yv cpw xvector yvectors args)
  (apply #'pw-axes cpw port args)
  ;;
  ;; Now plot the data points
  ;; 
  (apply #'pw-plot-bars-xv-yv cpw port xvector yvectors args))

;; -------------------------------------------------------------------

(defparameter *default-args*
  (list
   :watermarkfn #'watermark))

(defun do-with-default-args (args fn)
  (let ((*default-args* (append args *default-args*)))
    (declare (special *default-args*))
    (funcall fn)))
     
(defmacro with-default-args ((&rest args) &body body)
  `(do-with-default-args ',args (lambda () ,@body)))

;; -------------------------------------------------------------------

(defun set-reply-mbox (pane mbox)
  (unless (eq mp:*current-process* mp:*main-process*)
    (when mbox
      (loop until (mp:mailbox-empty-p mbox) do
            (mp:mailbox-read mbox))
      (setf (reply-mbox pane) mbox)
      )))

;; -------------------------------------------------------------------

(defun draw-shape (shape pane x0 y0 x1 y1
                     &key
                     (color :darkgreen)
                     (filled t)
                     (alpha 1)
                     border-thick
                     (border-color :black)
                     (border-alpha 1)
                     start-angle  ;; for arc
                     sweep-angle  ;; for arc
                     reply-mbox
                     )
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list pane
                           #'(lambda (pane port x y width height)
                               (declare (ignore x y width height))
                               (plt-draw-shape pane port shape
                                               x0 y0 x1 y1
                                               :color  color
                                               :alpha  alpha
                                               :filled filled
                                               :border-thick border-thick
                                               :border-color border-color
                                               :border-alpha border-alpha
                                               :start-angle  start-angle
                                               :sweep-angle  sweep-angle))
                           ))))

;; user callable function
(defun draw-rect (&rest args)
  (apply #'draw-shape :rect (append args *default-args*)))

;; user callable function
(defun draw-ellipse (&rest args)
  (apply #'draw-shape :ellipse (append args *default-args*)))

;; user callable function
(defun draw-arc (&rest args)
  (apply #'draw-shape :arc (append args *default-args*)))


;; -------------------------------------------------------------------

(defun oplot2 (pane xv yv 
                  &rest args
                  &key
                  clear
                  ;;draw-axes
                  ;;(color :darkgreen)
                  ;; thick
                  xlog
                  ylog
                  ;; (linewidth (or thick 1))
                  (logo *ext-logo*)
                  (logo-alpha *ext-logo-alpha*)
                  ;;(fullgrid t)
                  reply-mbox
                  &allow-other-keys)
  
  (multiple-value-bind (xv yv)
      (cond (xv
             (filter-potential-x-y-nans-and-infinities xv yv xlog ylog))
            (yv
             (values nil
                     (filter-potential-nans-and-infinities yv ylog)))
            (t (values nil nil)))
    
    (let ((pane  (plotter-mixin-of pane))
          (style (apply 'get-plot-style args)))
      (with-delayed-update (pane)
        (set-reply-mbox pane reply-mbox)
        (if (or clear
                (display-list-empty-p pane))
            (progn
              #|
                (setf (plotter-x-readout-hook pane) #'identity
                      (plotter-y-readout-hook pane) #'identity)
                |#
              (discard-display-list pane)
              (append-display-list pane
                                   #'(lambda (pane port x y width height)
                                       (declare (ignore x y width height))
                                       (apply #'do-plot pane port xv yv
                                              :plot-style style
                                              ;; :color     color
                                              ;; :linewidth linewidth
                                              ;; :fullgrid  fullgrid
                                              :logo       logo
                                              :logo-alpha logo-alpha
                                              args))
                                   ))
          (append-display-list pane
                               #'(lambda (pane port x y width height)
                                   (declare (ignore x y width height))
                                   (apply #'pw-plot-xv-yv pane port xv yv
                                          :plot-style style
                                          ;; :color color
                                          args))
                               ))
        ))))

;; -------------------------------------------------------------------

(defun oplot-bars2 (pane xv yvs
                       &rest args
                       &key
                       ;; draw-axes
                       clear
                       (color     :black)
                       (neg-color color)
                       thick
                       (linewidth (or thick 1))
                       ;; (fullgrid t)
                       (logo *ext-logo*)
                       (logo-alpha *ext-logo-alpha*)
                       reply-mbox
                       &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (if (or clear
              (display-list-empty-p pane))
          (progn
            #|
                (setf (plotter-x-readout-hook pane) #'identity
                      (plotter-y-readout-hook pane) #'identity)
                |#
            (discard-display-list pane)
            (append-display-list pane
                                 #'(lambda (pane port x y width height)
                                     (declare (ignore x y width height))
                                     (apply #'do-plot-bars pane port xv yvs
                                            :color     color
                                            :neg-color neg-color
                                            :linewidth linewidth
                                            ;; :fullgrid  fullgrid
                                            :logo logo
                                            :logo-alpha logo-alpha
                                            args))
                                 ))
        (append-display-list pane
                             #'(lambda (pane port x y width height)
                                 (declare (ignore x y width height))
                                 (apply #'pw-plot-bars-xv-yv pane port xv yvs 
                                        :color color
                                        :neg-color neg-color
                                        args))
                             ))
      )))

;; ------------------------------------------
(defun find-x-y-parms (args)
  (let* ((nargs (or (position-if #'keywordp args)
                    (length args))))
    (case nargs
      (0   (list nil nil args))
      (1   (list nil (first args) (rest args)))
      (2   (list (first args) (second args) (rest (rest args))))
      (otherwise (error "Too many arguments"))
      )))

(defun vector-to-plotfn (fn pane args)
  (destructuring-bind (xs ys parms) (find-x-y-parms args)
    (apply fn pane xs ys parms)))

;; user callable function
(defun plot (pane &rest args)
  (vector-to-plotfn #'oplot2 pane (append args *default-args*)))

;; user callable function
(defun plot-bars (pane &rest args)
  (vector-to-plotfn #'oplot-bars2 pane (append args *default-args*)))

;; ------------------------------------------

;; user callable function
(defun clear (pane &key reply-mbox)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (discard-display-list pane))))

;; -------------------------------------------------------------------

(defun stuff-display-list (pane lst &key reply-mbox)
  ;; be careful here... the list is expected to be a list of lambda forms
  ;; as if from another plotter's display list...
  (let ((pane (plotter-mixin-of pane)))
    (with-locked-plotter-pane pane
      (discard-display-list pane)
      (with-delayed-update (pane)
        (set-reply-mbox pane reply-mbox)
        (um:collector-stuff-contents (plotter-display-list pane) lst))
      )))
      
;; -------------------------------------------------------------------

(defun axes2 (pane xvector yvectors &rest args &key xrange xlog ylog
                   (logo *ext-logo*)
                   (logo-alpha *ext-logo-alpha*)
                   reply-mbox
                   &allow-other-keys)
  ;; allow a list of yvectors to be given
  ;; so that we can find the best fitting autoscale that accommodates all of them
  (multiple-value-bind (xv yv)
      (let ((ylist (remove nil (um:mklist yvectors))))
        (values (or (and xvector
                         (let ((xv (filter-potential-nans-and-infinities xvector xlog)))
                           (vector (vmin-of xv) (vmax-of xv))))
                    (and (null xrange)
                         ylist
                         (vector (if xlog 0.1 0) (1- (length-of (first ylist))))
                         ))
                (and ylist
                     (let ((ys (mapcar (um:rcurry #'filter-potential-nans-and-infinities ylog) ylist)))
                       (vector (vector-group-min ys)
                               (vector-group-max ys))))
                ))
    (let ((pane (plotter-mixin-of pane)))
      (with-delayed-update (pane)
        (set-reply-mbox pane reply-mbox)
        #|
    (setf (plotter-x-readout-hook pane) #'identity
          (plotter-y-readout-hook pane) #'identity)
    |#
        (clear pane)
        (append-display-list pane 
                             #'(lambda (pane port x y width height)
                                 (declare (ignore x y width height))
                                 (apply #'pw-init-xv-yv pane
                                        xv yv args)
                                 (apply #'pw-axes pane port
                                        :logo logo
                                        :logo-alpha logo-alpha
                                        args))
                             ))
      )))

;; user callable function
(defun axes (pane &rest args)
  (vector-to-plotfn #'axes2 pane (append args *default-args*)))

;; ------------------------------------------
;; these callbacks are only called from the capi process
;;
;; For COCOA the backing store is a pixmap image
;; For Win32 the backing store is a pixmap

(defun save-backing-pixmap (pane port)
  (with-accessors ((backing-pixmap  plotter-backing-pixmap )) pane
    (with-locked-plotter-pane pane
      (when backing-pixmap
        (gp:destroy-pixmap-port backing-pixmap))
      (setf backing-pixmap port))))
  
(defun discard-backing-pixmap (pane)
  (with-accessors ((backing-pixmap plotter-backing-pixmap)) pane
    (with-locked-plotter-pane pane
      (when backing-pixmap
        (gp:destroy-pixmap-port backing-pixmap)
        (setf backing-pixmap nil))
      )))

;; ---------------------------------------------------
;;
#+:COCOA
(defun save-backing-image (pane port)
  (with-accessors ((backing-image  plotter-backing-image )
                   (sf             plotter-sf            )
                   (nominal-width  plotter-nominal-width )
                   (nominal-height plotter-nominal-height)) pane
    (when backing-image
      (gp:free-image pane backing-image))
    (setf backing-image
          (gp:make-image-from-port port
                                   0 0
                                   (* sf nominal-width)
                                   (* sf nominal-height)))
    ))

#+:WIN32 ;; Vista
(defun save-backing-image (pane port)
  (with-accessors ((backing-image  plotter-backing-image )
                   (sf             plotter-sf            )
                   (nominal-width  plotter-nominal-width )
                   (nominal-height plotter-nominal-height)) pane
    (when backing-image
      (gp:free-image pane backing-image))
    (setf backing-image
          (gp:make-image-from-port port
                                   0 0
                                   (round (* sf nominal-width))
                                   (round (* sf nominal-height))))
    ))

;;#+:COCOA
(defun discard-backing-image (pane)
  (with-accessors ((backing-image  plotter-backing-image)) pane
    (with-locked-plotter-pane pane
      (when backing-image
        (gp:free-image pane backing-image)
        (setf backing-image nil)
        (discard-backing-pixmap pane))
      )))

;; --------------------------------------------------
#+:WIN32x
(defun save-backing-image (pane port)
  (with-accessors ((backing-image  plotter-backing-image)) pane
    (when backing-image
      (gp:destroy-pixmap-port backing-image))
    (setf backing-image port)))

#+:WIN32x
(defun discard-backing-image (pane)
  (with-accessors ((backing-image plotter-backing-image)) pane
    (with-locked-plotter-pane pane
      (when backing-image
        (gp:destroy-pixmap-port backing-image)
        (setf backing-image nil)
        (discard-backing-pixmap pane))
      )))


;; --------------------------------------------------
#+:WIN32
(defun draw-crosshair-lines (pane color x y)
  (when (and x y)
    (gp:with-graphics-state
        (pane
         :foreground color
         :operation  boole-xor)
      (gp:draw-line pane x 0 x (gp:port-height pane))
      (gp:draw-line pane 0 y (gp:port-width  pane) y))
    ))

#+:COCOA
(defun draw-crosshair-lines (pane color x y)
  (when (and x y)
    (with-color (pane color)
      (gp:draw-line pane x 0 x (gp:port-height pane))
      (gp:draw-line pane 0 y (gp:port-width pane) y)
      )))

#+:WIN32
(defmethod win32-display-callback ((pane <plotter-pane>) x y width height)
  (when (gp:port-representation pane)
    (display-callback pane x y width height)))

(defmethod display-callback ((pane <plotter-pane>) x y width height)
  (with-accessors ((nominal-width   plotter-nominal-width )
                   (nominal-height  plotter-nominal-height)
                   (sf              plotter-sf            )
                   (magn            plotter-magn          )
                   (xform           plotter-xform         )
                   (port-width      gp:port-width         )
                   (port-height     gp:port-height        )
                   (backing-pixmap  plotter-backing-pixmap)
                   (backing-image   plotter-backing-image )
                   (full-crosshair  plotter-full-crosshair)
                   (delay-backing   plotter-delay-backing )
                   (dirty           plotter-dirty         )
                   (prev-x          plotter-prev-x        )
                   (prev-y          plotter-prev-y        )
                   (reply-mbox      reply-mbox            )) pane

    (with-locked-plotter-pane pane
                              
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
      
      (cond  (backing-image
              (gp:draw-image pane backing-image 0 0
                             :from-width  (gp:image-width  backing-image)
                             :from-height (gp:image-height backing-image)
                             :to-width    (* sf nominal-width)
                             :to-height   (* sf nominal-height)))
             
             (delay-backing
              (dolist (item (display-list-items pane))
                (funcall item pane pane x y width height))
              (draw-legend pane pane))
             
             (t
              (when (or (null backing-pixmap)
                        (/= port-width  (gp:port-width backing-pixmap))
                        (/= port-height (gp:port-height backing-pixmap)))
                (discard-backing-pixmap pane)
                (let* ((gs   (gp:get-graphics-state pane))
                       (fg   (gp:graphics-state-foreground gs))
                       (bg   (gp:graphics-state-background gs))
                       (port (create-pixmap-port pane port-width port-height
                                                 :background bg
                                                 :foreground fg
                                                 :clear      nil)))
                  (save-backing-pixmap pane port)
                  (setf dirty t)))
              
              (when dirty
                (gp:clear-graphics-port backing-pixmap)
                (dolist (item (display-list-items pane))
                  (funcall item pane backing-pixmap x y width height))
                (draw-legend pane backing-pixmap)
                (setf dirty nil))
              
              (gp:copy-pixels pane backing-pixmap 0 0 port-width port-height 0 0)
              
              (when full-crosshair
                (draw-crosshair-lines pane full-crosshair prev-x prev-y))
              
              ))
      (let ((mbox reply-mbox))
        (when mbox
          (setf reply-mbox nil)
          (mp:mailbox-send mbox :done)))
      )))
    
(defun resize-callback (pane x y width height)
  (declare (ignore x y width height))
  (with-accessors ((resize-timer  plotter-resize-timer   )
                   (backing-image plotter-backing-image  )
                   (backing-pixmap plotter-backing-pixmap)) pane
    (with-locked-plotter-pane pane
      (unless backing-image
        ;; if we already have a backing image then this is another call
        ;; in the same resize operation
        (if backing-pixmap
            ;; but if we don't already have a backing pixmap then we
            ;; probably haven't been instantiated just yet, so don't do anything.
            (save-backing-image pane pane)))
      (unless resize-timer
        (setf resize-timer
              (mp:make-timer
               (lambda ()
                 (capi:apply-in-pane-process pane
                                             (lambda ()
                                               (discard-backing-image  pane)
                                               (redraw-entire-pane pane)
                                               )))
               )))
      (mp:schedule-timer-relative-milliseconds resize-timer 100)
      )))

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
        (list (funcall ;;real-eval-with-nans
               (if xlog
                   (um:compose x-readout-hook #'pow10)
                 x-readout-hook)
               xx)
              (funcall ;;real-eval-with-nans
               (if ylog
                   (um:compose y-readout-hook #'pow10)
                 y-readout-hook)
               yy)
              ))
      )))

(defmethod display-cursor-readout (intf name x y)
  (declare (ignore intf name x y))
  ;; base method does nothing
  nil)

(defmethod display-cursor-readout ((obj capi:simple-pane) name x y)
  ;; default for CAPI panes - defer to parent object
  (display-cursor-readout (capi:element-parent obj) name x y))

(defun mouse-move (pane x y &rest args)
  (declare (ignore args))
  (with-locked-plotter-pane pane
    
    (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
      (display-cursor-readout pane
                              (capi:capi-object-name pane) xx yy))

    (with-accessors ((full-crosshair plotter-full-crosshair)
                     (prev-x         plotter-prev-x)
                     (prev-y         plotter-prev-y)) pane
      
      (when full-crosshair ;; NIL or a color spec

        #+:WIN32
        (progn
          (draw-crosshair-lines pane full-crosshair prev-x prev-y)
          (draw-crosshair-lines pane full-crosshair x      y)
          
          (setf prev-x x
                prev-y y))
        
        #+:COCOA
        (let ((xx (shiftf prev-x x))
              (yy (shiftf prev-y y)))
          (if (plotter-backing-pixmap pane)
              (let ((wd (gp:port-width pane))
                    (ht (gp:port-height pane)))
                (gp:invalidate-rectangle pane xx 0 1 ht)
                (gp:invalidate-rectangle pane 0 yy wd 1)
                (gp:invalidate-rectangle pane x 0 1 ht)
                (gp:invalidate-rectangle pane 0 y wd 1))
            (gp:invalidate-rectangle pane)
            ))
        ))))

(defun show-x-y-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (with-locked-plotter-pane pane
    (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
      (let ((xstr (format nil "~,5g" xx))
            (ystr (format nil "~,5g" yy)))
        (capi:display-tooltip pane
                              :x  (+ x 10)
                              :y  (+ y 10)
                              :text (format nil "(~A, ~A)"
                                            (string-trim " " xstr)
                                            (string-trim " " ystr))
                              )))))

;; user callable function
(defun set-x-readout-hook (pane fn)
  (let ((pane (plotter-mixin-of pane)))
    (with-locked-plotter-pane pane
      (setf (plotter-x-readout-hook pane) fn))
    ))

;; user callable function
(defun set-y-readout-hook (pane fn)
  (let ((pane (plotter-mixin-of pane)))
    (with-locked-plotter-pane pane
      (setf (plotter-y-readout-hook pane) fn))
    ))

;; -----------------------------------------------------------

(defun do-with-bare-pdf-image (pane fn)
  (setf (plotter-delay-backing pane) t)
  (gp:invalidate-rectangle pane)
  (unwind-protect
      (funcall fn)
    (setf (plotter-delay-backing pane) nil)
    ))
  
(defmacro with-bare-pdf-image ((pane) &body body)
  `(do-with-bare-pdf-image ,pane (lambda () ,@body)))

(defun draw-nominal-image (pane port)
  (with-accessors ((nominal-width   plotter-nominal-width )
                   (nominal-height  plotter-nominal-height)
                   (sf              plotter-sf            )
                   (magn            plotter-magn          )
                   (xform           plotter-xform         )) pane

    (let ((save-xform  xform)
          (save-magn   magn)
          (save-sf     sf))
      (unwind-protect
          (progn
            (gp:clear-graphics-port-state pane)
            
            (setf xform '(1 0 0 1 0 0)
                  magn  1
                  sf    1)

            (dolist (item (display-list-items pane))
              (funcall item pane port 0 0 nominal-width nominal-height))
            (draw-legend pane port))
        
        (progn
          (setf sf    save-sf
                magn  save-magn
                xform save-xform))
        ))))

(defun get-nominal-image (pane)
  ;; should only be called by the capi process
  (let* ((xpane (create-pixmap-port pane
                                    (plotter-nominal-width pane)
                                    (plotter-nominal-height pane)
                                    :background (background-color pane)
                                    :foreground (foreground-color pane)
                                    :clear      t)))
    ;; this avoids image artifacts due to image shrinkage or expansion
    ;; just draw at original (nominal) scale
    (draw-nominal-image pane xpane)
    (values xpane (gp:make-image-from-port xpane))
    ))

(defmacro with-nominal-image ((pane img) &body body)
  ;; should only be used by functions called by the capi process
  (let ((xpane (gensym)))
    `(multiple-value-bind (,xpane ,img)
         (get-nominal-image ,pane)
       (unwind-protect
           (progn
             ,@body)
         (progn
           (gp:free-image ,xpane ,img)
           (gp:destroy-pixmap-port ,xpane))))
    ))

;; ----------------------------------------------------------
;;
;; user callable function
(defun save-image (pane file &key &allow-other-keys)
  ;; can be called from anywhere
  (let ((dest (or file
                  (capi:prompt-for-file
                   "Write Image to File"
                   :operation :save
                   :filter #+:COCOA "*.pdf"
                           #+:WIN32 "*.bmp"))))
    (when dest
      (let ((pane (plotter-mixin-of pane)))
        (sync-with-capi pane
                        #+:COCOA
                        (lambda ()
                          (with-bare-pdf-image (pane)
                            (save-pdf-plot pane (namestring dest))
                            ))
                        #+:WIN32
                        (lambda ()
                          (with-nominal-image (pane img)
                            (let ((eimg (gp:externalize-image pane img)))
                              (gp:write-external-image eimg dest
                                                       :if-exists :supersede)
                              )))
                        )))
    ))

;; user callable function
(defun save-plot (&rest args)
  (apply #'save-image args))

(defun save-image-from-menu (pane &rest args)
  ;; called only in the pane's process
  (declare (ignore args))
  (let ((dest (capi:prompt-for-file
               "Write Image to File"
               :operation :save
               :filter #+:COCOA "*.pdf"
                       #+:WIN32 "*.bmp")))
    (when dest
      #+:COCOA
      (with-bare-pdf-image (pane)
        (save-pdf-plot pane (namestring dest)))
      #+:WIN32
      (with-locked-plotter-pane pane
        (save-image pane (namestring dest)))
      )))

(defun copy-image-to-clipboard (pane &rest args)
  ;; called only as a callback in the capi process
  (declare (ignore args))
  (with-locked-plotter-pane pane
    #+:COCOA
    (with-bare-pdf-image (pane)
      (copy-pdf-plot pane))
    #+:WIN32
    (with-nominal-image (pane img)
      (capi:set-clipboard pane nil nil (list :image img)))
    ))

(defun print-plotter-pane (pane &rest args)
  (declare (ignore args))
  ;; executed in the process of the capi pane
  (with-locked-plotter-pane pane
    #+:COCOA
    (with-bare-pdf-image (pane)
      (capi:simple-print-port pane
                              :interactive t))
    #+:WIN32
    (with-nominal-image (pane img)
      (capi:simple-print-port pane
                              :interactive t))
    ))

;; user callable function
(defun set-full-crosshair (pane full-crosshair)
  (let ((pane (plotter-mixin-of pane)))
    (sync-with-capi pane
                    (lambda ()
                      (setf (plotter-full-crosshair pane)
                            #+:COCOA full-crosshair
                            #+:WIN32
                            (and full-crosshair
                                 (complementary-color full-crosshair
                                                      (background-color pane))
                                 ))
                      (when (null full-crosshair)
                        (setf (plotter-prev-x pane) nil
                              (plotter-prev-y pane) nil))

                      (redraw-entire-pane pane))
                    )))

;; called only from the plotter-window menu (CAPI process)
(defun toggle-full-crosshair (pane &rest args)
  (declare (ignore args))
  (with-locked-plotter-pane pane
    (setf (plotter-full-crosshair pane)
          (if (plotter-full-crosshair pane)
              (setf (plotter-prev-x pane) nil
                    (plotter-prev-y pane) nil)
            #+:COCOA :red
            #+:WIN32 (complementary-color :red
                                          (background-color pane))))
    (redraw-entire-pane pane)
    ))
                                        
;; ------------------------------------------
(defmethod plotter-mixin-of (name)
  ;; allow for symbolic names in place of plotter-windows or <plotter-pane>s
  ;; names must match under EQUALP (i.e., case insensitive strings, symbols, numbers, etc.)
  (wset name))

;; -------------------------------------
(defun locate-plotter-window (name)
  (find name (capi:collect-interfaces '<plotter-window>)
        :test #'equalp
        :key  #'capi:capi-object-name))
;; --------------------------------------

(defun find-named-plotter-pane (name)
  ;; locate the named plotter window and return its <plotter-pane> object
  (let ((win (locate-plotter-window name)))
    (and win
         (plotter-mixin-of win))))

;; ---------------------------------------------------------------
(defclass <plotter-window> (capi:interface)
  ((drawing-area  :accessor drawing-area  :initarg :drawing-area)))

(defmethod plotter-mixin-of ((intf <plotter-window>))
  (drawing-area intf))

(defmethod display-cursor-readout ((intf <plotter-window>) name x y)
  (setf (capi:interface-title intf)
        (format nil "~A  x = ~,5g  y = ~,5g"
                name x y)))
  
(defun make-plotter-window (&key
                            (name       0)
                            (title      "Plot")
                            (fg         :black)
                            (bg         :white)
                            (foreground fg)
                            (background bg)
                            (xsize      400)
                            (ysize      300)
                            xpos
                            ypos
                            (best-width         xsize)
                            (best-height        ysize)
                            (best-x             xpos)
                            (best-y             ypos)
                            (visible-min-width  (/ xsize 2))
                            (visible-min-height (/ ysize 2))
                            (visible-max-width  (* xsize 2))
                            (visible-max-height (* ysize 2))
                            full-crosshair
                            window-styles)
  
  (let ((pane (make-instance '<plotter-pane>
                             :name               name
                             :background         background
                             :foreground         foreground
                             :nominal-width      best-width
                             :nominal-height     best-height
                             :visible-min-width  visible-min-width
                             :visible-max-width  visible-max-width
                             :visible-min-height visible-min-height
                             :visible-max-height visible-max-height
                             :full-crosshair
                             #+:COCOA full-crosshair
                             #+:WIN32 (and full-crosshair
                                           (complementary-color
                                            full-crosshair background))
                             )))
    (make-instance '<plotter-window>
                   :name           name
                   :title          title
                   :drawing-area   pane
                   ;; :window-styles '(:internal-borderless)
                   :layout         (make-instance 'capi:simple-layout
                                                  :description (list pane))
                   :menu-bar-items
                   (list
                    (make-instance 'capi:menu
                                   :title "Pane"
                                   :items (list
                                           (make-instance 'capi:menu-item
                                            :text          "Copy"
                                            :callback      'copy-image-to-clipboard
                                            :accelerator   "accelerator-c")
                                           (make-instance 'capi:menu-item
                                            :text          "Save as..."
                                            :callback      'save-image-from-menu
                                            :accelerator   "accelerator-s")
                                           (make-instance 'capi:menu-item
                                            :text          "Print..."
                                            :callback      'print-plotter-pane
                                            :accelerator   "accelerator-p"))
                                   :callback-type :data
                                   :callback-data-function  (constantly pane)))
                   :visible-min-width  #+:COCOA visible-min-width #+:WIN32 (+ visible-min-width 4)
                   :visible-max-width  #+:COCOA visible-max-width #+:WIN32 (+ visible-max-width 4)
                   :visible-min-height #+:COCOA visible-min-height #+:WIN32 (+ visible-min-height 4)
                   :visible-max-height #+:COCOA visible-max-height #+:WIN32 (+ visible-max-height 4)
                   :best-width         #+:COCOA best-width #+:WIN32 (+ best-width 4)
                   :best-height        #+:COCOA best-height #+:WIN32 (+ best-height 4)
                   :best-x             best-x
                   :best-y             best-y
                   :window-styles      window-styles)
    ))

;; ------------------------------------------
(defun window (name &key
                    (title      (format nil "~A" name))
                    (background #.(color:make-gray 1))
                    (foreground #.(color:make-gray 0))
                    (width      400)
                    (height     300)
                    (xsize      width)
                    (ysize      height)
                    x
                    y
                    (xpos       x)
                    (ypos       y)
                    (best-width         xsize)
                    (best-height        ysize)
                    (best-x             xpos)
                    (best-y             ypos)
                    (visible-min-width  (/ xsize 2))
                    (visible-min-height (/ ysize 2))
                    (visible-max-width  (* xsize 2))
                    (visible-max-height (* ysize 2))
                    full-crosshair
                    (window-styles '(:internal-borderless))
                    &allow-other-keys)

  (wclose name)
  (let* ((intf (make-plotter-window
                :name                name
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
                :foreground          foreground
                :full-crosshair      full-crosshair
                :window-styles       window-styles))
         (pane (drawing-area intf)))
    (capi:display intf)
    pane))

;; ------------------------------------------
(defun wset (name &rest args &key clear &allow-other-keys)
  ;; If window exists don't raise it to the top.
  ;; If window does not exist then create it with default parameters.
  ;; Return the plotting pane object
  (let ((pane (or (find-named-plotter-pane name)
                  (apply #'window name args)))) ;; locate existing or create anew
    (when clear
      (clear pane))
    pane))

;; ------------------------------------------
(defun wshow (name &rest args &key clear &allow-other-keys)
  ;; If window exists then raise it to the top.
  ;; If window does not exist then create it with default parameters
  (let* ((pane (or (find-named-plotter-pane name)
                   (apply #'window name args)))  ;; locate existing or create anew
         (intf (capi:top-level-interface pane)))
    (capi:execute-with-interface intf
                                 #'capi:raise-interface intf)
    (when clear
      (clear pane))
    pane))

;; ------------------------------------------
(defun wclose (name)
  ;; if window exists then ask it to commit suicide and disappear
  (let ((intf (locate-plotter-window name)))
    (when intf
      (capi:execute-with-interface intf #'capi:destroy intf))
    ))

;; ------------------------------------------
;; ------------------------------------------
(defun outsxy (pane x y str
                  &rest args
                  &key
                  (font-size $normal-times-font-size)
                  (font "Times")
                  anchor
                  (align :w)
                  (offset-x 0) ;; pixel offsets
                  (offset-y 0)
                  clip
                  (color :black)
                  alpha
                  reply-mbox
                  &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list pane
                           #'(lambda (pane port xarg yarg width height)
                               (declare (ignore xarg yarg width height))
                               (let* ((xx (+ offset-x (get-x-location pane x)))
                                      (yy (+ offset-y (get-y-location pane y)))
                                      (font (find-best-font pane
                                                            :family font
                                                            :size   (* (plotter-sf pane) font-size)))
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
                                                  (mapcar (um:expanded-curry (v) #'* sf)
                                                          (plotter-box pane))
                                                  )))
                                      (color (adjust-color pane color alpha)))
                                 
                                 #+:WIN32
                                 (with-mask (port mask)
                                     (apply #'draw-string-x-y pane port str
                                            (* sf xx) (* sf yy)
                                            :font font
                                            :x-alignment x-align
                                            :y-alignment y-align
                                            :color       color
                                            args))
                                 #+:COCOA
                                 (let* ((font-attrs (gp:font-description-attributes
                                                     (gp:font-description font)))
                                        (font-name  (getf font-attrs :name))
                                        (font-size  (getf font-attrs :size)))
                                   (apply #'add-label port str (* sf xx) (* sf yy)
                                          :font        font-name
                                          :font-size   font-size
                                          :color       color
                                          :x-alignment x-align
                                          :y-alignment y-align
                                          :box         mask
                                          args))
                                 
                                 ))))
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

(defun draw-text (pane str org &rest args)
  (destructuring-bind (xorg yorg) (get-xy-orgs org)
    (apply #'outsxy pane xorg yorg str (append args *default-args*))))

;; ------------------------------------------
(defun do-plot-histogram (pane v &rest args
                            &key min max range nbins binwidth
                            ylog cum norm
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
        (setf tot (vsum h))
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
      (apply #'plot pane x h :symbol symbol args)
      )))

;; user callable routine
(defun plot-histogram (pane v &rest args)
  (apply #'do-plot-histogram pane v (append args *default-args*)))

;; ------------------------------------------
(defconstant $gray-colormap
  (let ((map (make-array 256)))
    (loop for ix from 0 to 255 do
          (setf (aref map ix) (color:make-gray (/ (float ix) 255.0))
                ))
    map))

(defconstant $heat-colormap
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

(defparameter *current-colormap* $heat-colormap) ;;$gray-colormap

(defparameter *tst-img*
  (let ((img (make-array '(64 64))))
    (loop for row from 0 below 64 do
          (loop for col from 0 below 64 do
                (setf (aref img row col) (* row col))
                ))
    img))

(defun do-tvscl (pane arr
              &key (magn 1)
              (colormap *current-colormap*)
              reply-mbox
              &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list
       pane
       #'(lambda (pane port x y width height)
           (declare (ignore x y width height))
           (let* ((wd   (array-dimension-of arr 1))
                  (ht   (array-dimension-of arr 0))
                  (mn   (vmin-of arr))
                  (mx   (vmax-of arr))
                  (sf   (/ 255 (- mx mn))))
             
             (with-image (port (img #+:COCOA (gp:make-image port wd ht)
                                    #+:WIN32 (gp:make-image port wd ht :alpha nil)
                                    ))
               (with-image-access (acc (gp:make-image-access port img))
                 (loop for row from 0 below ht do
                       (loop for col from 0 below wd do
                             (setf (gp:image-access-pixel acc row col)
                                   (color:convert-color
                                    pane
                                    (aref colormap
                                          (round (* sf (- (aref-of arr row col) mn)))))
                                   )))
                 (gp:image-access-transfer-to-image acc))
               
               (let ((sf (* magn (plotter-sf pane))))
                 (gp:with-graphics-scale (port sf sf)
                   (gp:draw-image port img 0 0))
                 ))
             (setf (plotter-magn pane) magn)
             )))
      )))

;; user callable routine
(defun tvscl (pane arr &rest args)
  (apply #'do-tvscl pane arr (append args *default-args*)))

(defun do-render-image (pane ext-img
                     &key
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
                     reply-mbox
                     &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list
       pane
       #'(lambda (pane port x y wd ht)
           (declare (ignore x y wd ht))
           (let ((sf (* magn (plotter-sf pane))))
             (with-image (port (img (gp:convert-external-image port ext-img)))
               (gp:with-graphics-scale (port sf sf)
                 (gp:draw-image port img to-x to-y
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
      )))

;; user callable routine
(defun render-image (pane ext-img &rest args)
  (apply #'do-render-image pane ext-img (append args *default-args*)))

(defun read-image (&optional file)
  (gp:read-external-image (or file
                              (capi:prompt-for-file
                               "Select Image File"
                               :filter "*.*"))
                          ))

;; ------------------------------------------------------------------------------------
#|
;; for debugging...
(defun dump-hex (arr &key (nlines 10))
  (loop for ix from 0 below (array-total-size-of arr) by 16
        for line from 0 below nlines
        do
        (format t "~%~4,'0x: ~{~{~2,'0x ~} ~} ~A"
                ix
                (loop for jx from 0 below 16 by 4
                      collect
                      (coerce (subseq-of arr (+ ix jx) (+ ix jx 4)) 'list))
                (let ((s (make-string 16)))
                  (loop for jx from 0 below 16 do
                        (setf (aref s jx)
                              (let ((v (code-char (aref-of arr (+ ix jx)))))
                                (if (graphic-char-p v)
                                    v
                                  #\.))
                              ))
                  s))
        ))

(defun sinc (x)
  (/ (sin x) x))
|#

#|
(window 'tst)
(fplot 'tst '(0.001 10) (lambda (x) (/ (sin x) x)))
(tvscl 'tst *tst-img* :magn 4)
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
  
  (window 'tst :xsize 400 :ysize 300)
  (plot 'tst x y 
        :color (color:make-rgb 1.0 0.0 0.0 0.25) ;;:red
        :thick 2
        :title "Sinc(x)"
        :xtitle "X Values"
        :ytitle "Y Values")

  ;;  (window 'tst :background :black :foreground :yellow :xsize 400 :ysize 300)
  ;;  (plot 'tst x y 
  ;;        :color (color:make-rgb 1.0 0.0 1.0 0.25) ;;:magenta
  ;;        :linewidth 2
  ;;        :fullgrid (color:make-gray 0.25)
  ;;        :title "Sinc(x)"
  ;;        :xtitle "X Values"
  ;;        :ytitle "Y Values")
  )
|#


;; *eof* ;;


#|
;; test code for alternate logos...
(let ((logo (gp:read-external-image "/Users/davidmcclain/Desktop/MMWSim Logo4.pdf"))
      (logo-alpha 0.2)
      (win (plt:wset 'sinc :clear t)))
  (plt:fplot win '(-20 20) (lambda (x) (/ (sin x ) x))
             :thick 2 :color :darkgreen
             :logo logo
             :logo-alpha logo-alpha))

;; test default logo
(let ((win (plt:wset 'sinc :clear t)))
  (plt:fplot win '(-20 20) (lambda (x) (/ (sin x ) x))
             :logo *ext-logo*
             :thick 2 :color :darkgreen))

|#

;; ======================================================================================

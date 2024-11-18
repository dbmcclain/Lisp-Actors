;; scigraph-for-mac.lisp -- Scientific Plotting and Image Rendering
;;
;; DM/MCFA  08/99
;; --------------------------------------------------------

(in-package #:com.ral.scigraph)

(defconstant $WHITE     (rgb 255 255 255))
(defconstant $BLACK     (rgb   0   0   0))
(defconstant $RED       (rgb 255   0   0))
(defconstant $GREEN     (rgb   0 255   0))
(defconstant $BLUE      (rgb   0   0 255))
(defconstant $ORANGE    (rgb 255 160   0))
(defconstant $SKYBLUE   (rgb   0 160 255))
(defconstant $PURPLE    (rgb 160   0 255))
(defconstant $MAGENTA   (rgb 255   0 255))
(defconstant $CYAN      (rgb   0 255 255))
(defconstant $YELLOW    (rgb 255 255   0))
(defconstant $DARKGREEN (rgb   0 128   0))
(defconstant $DARKRED   (rgb 128   0   0))
(defconstant $DARKBLUE  (rgb   0   0 128))
(defconstant $GRAY25    (rgb  64  64  64))
(defconstant $GRAY50    (rgb 128 128 128))
(defconstant $GRAY75    (rgb 192 192 192))

(defmethod translate-color-code ((color integer))
  color)

(defun make-int8 (v)
  (truncate (* 255 v)))

(defmethod translate-color-code ((color vector))
  (cond ((eq :rgb (aref color 0))
         (rgb (make-int8 (aref color 1))
              (make-int8 (aref color 2))
              (make-int8 (aref color 3))))

        ((every #'integerp color)
         (rgb (aref color 0)
              (aref color 1)
              (aref color 2)))

        (t
         (rgb (make-int8 (aref color 0))
              (make-int8 (aref color 1))
              (make-int8 (aref color 2))))
        ))

(defmethod translate-color-code ((color symbol))
  (translate-color-code (color:get-color-translation color)))

;; -----------------------------------------
;; Define the generic functions so that the lambda list
;; will show the available keywords during editing.
;;

(defun tvscl (img
              &key
              min max
              magn xmagn ymagn
              xpos ypos
              xmin xmax ymin ymax
              xlog ylog zlog
              neg
              range
              flipv fliph
              &allow-other-keys)
  (let* ((min (or min 0))
         (max (or max 0))
         (magn (or magn 1))
         (xmagn (or xmagn magn))
         (ymagn (or ymagn magn))
         (xpos (or xpos 0))
         (ypos (or ypos 0))
         (xmin (or xmin 0))
         (ymin (or ymin 0)))
    
    (with-float-carray (fimg img)
      (destructuring-bind (dimy dimx)
          (ca:carray-dimensions fimg)
        (fli:with-dynamic-foreign-objects ()
          (let ((p (fli:allocate-dynamic-foreign-object
                    :type 'timage-parms)))
            (set-foreign-slots p ((xsiz dimx)
                                  (ysiz dimy)
                                  (minval (float (if range (elt range 0) min)))
                                  (maxval (float (if range (elt range 1) max)))
                                  (xmagn  (float xmagn))
                                  (ymagn  (float ymagn))
                                  (xpos  xpos)
                                  (ypos  ypos)
                                  (parr  (ca:carray-data fimg))
                                  (neg   (if neg 1 0))
                                  (flipv (if flipv 1 0))
                                  (fliph (if fliph 1 0))
                                  (xmin  (float xmin))
                                  (xmax  (float (or xmax
                                                    dimx)))
                                  (ymin  (float ymin))
                                  (ymax  (float (or ymax
                                                    dimy)))
                                  (xlog  (if xlog 1 0))
                                  (ylog  (if ylog 1 0))
                                  (zlog  (if zlog 1 0))
                                  ))
            (with-sg-lock (lpShowImage p))))
        ))))

#|
(defun copy-graphic-to-clipboard ()
  (with-sg-lock (lpCopyToClipboard)))

(defun wsave ()
  (with-sg-lock (lpSaveToBacking)))

(defun wkill (wid)
  (with-sg-lock (lpKillWindow wid)))
|#

(defun werase (&optional (bg $white))
  (with-sg-lock (lpEraseWindow bg)))

(defun delay-update ()
  (with-sg-lock (lpDelayUpdateWindow)))

(defun update ()
  (with-sg-lock (lpUpdateWindow)))


#|
(let ((img (scids:getvar)))
  (tvscl (vm:elementwise (img) (- (log img)))))
|#

;; -------------------------------------------------------------
;; Connected symbols are the negative of these
(defconstant $SYM-CIRCLE     1)
(defconstant $SYM-BOX        2)
(defconstant $SYM-SQUARE     2)
(defconstant $SYM-DOT        3)
(defconstant $SYM-CROSS      4)
(defconstant $SYM-TRIANGLE   5)
(defconstant $SYM-HISTOGRAM 10)

(defconstant $PENPAT-SOLID        0)
(defconstant $PENPAT-DASH         1)
(defconstant $PENPAT-DOT          2)
(defconstant $PENPAT-DASHDOT      3)
(defconstant $PENPAT-DASHDOTDOT   4)
(defconstant $PENPAT-NULL         5)
(defconstant $PENPAT-INSIDE-FRAME 6)

#|
(defun get-hwnd (pane)
  (capi-win32-lib::r-output-pane-hwnd
   (capi-internals:representation pane)))

(defun wattach (pane)
  (let ((hwnd (get-hwnd pane)))
    (with-sg-lock (lpAttachWindow hwnd))
    hwnd))

(defun wdetach (pane)
  (wkill (get-hwnd pane)))

(defmethod wset ((pane capi:output-pane))
  (wattach pane))

(defmethod wshow ((pane capi:output-pane))
  (wattach pane))
|#

(defparameter *current-window* nil)
(defparameter *wdict*          (make-hash-table))

(defun lookup-plot-info ()
  (gethash *current-window* *wdict*))

(defun set-plot-info (info)
  (setf (gethash *current-window* *wdict*) info))
  
(defmethod wset ((wid integer))
  (setf *current-window* wid)
  (with-sg-lock (lpSelectWindow wid)))

(defun window (wid &key
                   xpos ypos
                   xsize ysize
                   bgcolor title
                   &allow-other-keys)
  (let* ((wix (mod wid 10))
         (xpos (or xpos -1))
         (ypos (or ypos -1))
         (xsize (or xsize 400))
         (ysize (or ysize 300))
         (bgcolor (translate-color-code (or bgcolor $BLACK)))
         (title (or title (format nil "SG/~D" wid)))
         (dx  (* 20 (/ wid 10)))
         (defoff (+ dx (* 50 (1+ wix))))
         (xpos (if (plusp xpos) xpos defoff))
         (ypos (if (plusp ypos) ypos defoff)))
    (setf *current-window* wid)
    (fli:with-dynamic-foreign-objects ()
      (let ((p (fli:allocate-dynamic-foreign-object
                :type 'twindow-setup))
            (tw (fli:convert-to-dynamic-foreign-string title)))
        (set-foreign-slots p ((wid     wid)
                              (xpos    xpos)
                              (ypos    ypos)
                              (xsize   xsize)
                              (ysize   ysize)
                              (title   tw)
                              (bgcolor bgcolor)))
        (with-sg-lock (lpOpenWindow p))))
    ))

(defmethod wshow ((wid integer))
  (with-sg-lock (lpShowWindow wid)))

(defun get-wsize ()
  (fli:with-dynamic-foreign-objects ()
    (let ((p (fli:allocate-dynamic-foreign-object
              :type 'twindow-info)))
      (with-sg-lock (lpGetWindowSize p))
      (foreign-slot-values p '(ysize xsize))
      )))

#|
(defun get-mouse ()
  (fli:with-dynamic-foreign-objects ()
    (let ((p (fli:allocate-dynamic-foreign-object
              :type 'tmouse-info)))
      (with-sg-lock (lpGetMouse p))
      (foreign-slot-values p '(mousex mousey mousez))
      )))

(defun get-coord-values (x y)
  (fli:with-dynamic-foreign-objects ()
    (let ((p (fli:allocate-dynamic-foreign-object
              :type 'tmouse-info)))
      (set-foreign-slots p ((mousex  (float x))
                              (mousey  (float y))))
      (with-sg-lock (lpGetCoordValues p))
      (foreign-slot-values p '(mousex mousey mousez))
      )))
|#

(defun set-cmap (rs gs bs)
  (fli:with-dynamic-foreign-objects ()
    (let ((prs (fli:allocate-dynamic-foreign-object
                :type '(:unsigned :char)
                :nelems 256
                :initial-contents (map 'list 'truncate rs)))
          (pgs (fli:allocate-dynamic-foreign-object
                :type '(:unsigned :char)
                :nelems 256
                :initial-contents (map 'list 'truncate gs)))
          (pbs (fli:allocate-dynamic-foreign-object
                :type '(:unsigned :char)
                :nelems 256
                :initial-contents (map 'list 'truncate bs))))
      (with-sg-lock (lpSetCMap prs pgs pbs)))))

(fli:define-foreign-function (set-heat-colormap
                              "lpSetHeatColormap"
                              :source)
    ()
  :result-type :void
  :language    :ansi-c)

(fli:define-foreign-function (set-gray-colormap
                              "lpSetGrayColormap"
                              :source)
    ()
  :result-type :void
  :language    :ansi-c)

;; ------------------------------------------
(defmethod autoscale ((arr null) log &optional npts)
  (if log
      (list 0 (log (1- npts) 10))
    (list 0 (1- npts))))

(defmethod autoscale (arr log &optional npts)
  (with-float-cvector (farr arr)
    (fli:with-dynamic-foreign-objects ()
      (let ((p (fli:allocate-dynamic-foreign-object
                :type 'tscale-parms)))
        (set-foreign-slots p ((nel  (or npts
                                          (ca:carray-total-size farr)))
                                (parr (ca:carray-data farr))
                                (log  (if log 1 0))))
        (with-sg-lock (lpAutoscale p))
        (foreign-slot-values p '(minval maxval))
        ))))

(defun scale (arr rng log &optional npts)
  (if rng
      (if log
          (list (log (elt rng 0) 10) (log (elt rng 1) 10))
        rng)
    (let* ((rng (autoscale arr log npts))
           (minv (elt rng 0))
           (maxv (elt rng 1))
           (dv   (/ (- maxv minv) 18.0)))
      (list (- minv dv) (+ maxv dv)))
    ))

;; ------------------------------------------

(destructuring-bind (&optional logo-wd logo-ht logo-img)
    (let* ((ext-logo (ignore-errors
                       (with-open-file (f ;;"/usr/local/lib/Logo75-Alpha25.bmp"
                                          "/usr/local/lib/Logo75Img-Alpha25y.bmp"
                                          :direction :input
                                          :element-type '(unsigned-byte 8))
                         (let ((data (make-array (file-length f)
                                                 :element-type '(unsigned-byte 8))))
                           (read-sequence data f)
                           data)))))
      (when ext-logo
        (let* ((offs (aref ext-logo 10))
               (wd   (+ (aref ext-logo 18)
                        (* 256 (aref ext-logo 19))))
               (ht   (+ (aref ext-logo 22)
                        (* 256 (aref ext-logo 23))))
               (xtra (rem (- 4 (rem (* 3 wd) 4)) 4))
               (nel  (* 3 wd ht))
               (img  (make-array nel :element-type '(unsigned-byte 8))))
          
          (loop for row from 0 below ht
                for revrow = (- ht 1 row) then (1- revrow)
                do
                (loop for col from 0 below wd
                      for jx = (* 3 wd row) then (+ jx 3)
                      for ix = (+ offs (* xtra revrow) (* 3 wd revrow))
                      then (+ ix 3)
                      do
                      (setf (aref img (+ 0 jx)) (aref ext-logo (+ 2 ix))
                            (aref img (+ 1 jx)) (aref ext-logo (+ 1 ix))
                            (aref img (+ 2 jx)) (aref ext-logo (+ 0 ix)))
                      ))
          (list wd ht (coerce img 'list))
          )))
  
  (defun stamp-logo ()
    (when logo-wd
      (fli:with-dynamic-foreign-objects ()
        (let ((cdata (fli:allocate-dynamic-foreign-object
                      :type   '(:unsigned :byte)
                      :nelems (* logo-wd logo-ht)
                      :initial-contents logo-img)))
          ;;(aqt:add-image-with-bitmap cdata logo-wd logo-ht
          ;;                           35 220 logo-wd logo-ht)
          (aqt:add-image-with-bitmap cdata logo-wd logo-ht
                                     40 100 logo-wd logo-ht)
          ))
      )))
    
    
(defun watermark ()
  #|
  (sg:draw-text "R A L"
                `((:frac 0.5)
                  (:frac 0.4))
                :anchor :ctr
                :font-name "Zapfino" ;; "Times-Italic"
                :font-size 70 ;;130
                :color (sg:rgb 0.90 0.90 0.95))
  |#
  (sg:draw-text "Copyright (c) 2006-2007 by Refined Audiometrics Laboratory, LLC."
                '((:frac 0.1)
                  (:frac 0.18))
                :anchor :sw
                :font-size 10
                :color (sg:rgb 0.6 0.6 0.6))
  
  (sg:draw-text "All rights reserved."
                '((:frac 0.1)
                  (:frac 0.14))
                :anchor :sw
                :font-size 10
                :color (sg:rgb 0.6 0.6 0.6))
  (stamp-logo))

(defun draw-grid (&key (grid-color $gray75))
  (with-sg-lock (lpdrawgrid (translate-color-code grid-color))))

(defun plot-axes (x y
                    &key 
                    xlog ylog
                    aspect
                    bg fg
                    xrange yrange
                    xtitle ytitle title
                    npts
                    (grid t)
                    (ticks :inside)
                    (wmarkfn #'watermark)
                    &allow-other-keys)
  (with-float-cvector (fx x)
    (with-float-cvector (fy y)
      (let* ((npts (or npts
                       (if fx
                           (min (ca:carray-total-size fx)
                                (ca:carray-total-size fy))
                         (ca:carray-total-size fy))))
             (xrng (scale fx xrange xlog npts))
             (yrng (scale fy yrange ylog npts)))
        (fli:with-dynamic-foreign-objects ()
          (let ((p (fli:allocate-dynamic-foreign-object
                    :type 'taxes-parms)))
            (with-dynamic-foreign-strings ((tx (or xtitle "X"))
                                           (ty (or ytitle "Y"))
                                           (tp (or title "")))
              (set-foreign-slots p ((xmin     (float (elt xrng 0)))
                                    (xmax     (float (elt xrng 1)))
                                    (ymin     (float (elt yrng 0)))
                                    (ymax     (float (elt yrng 1)))
                                    (aspect   (if aspect (float aspect) 0.0))
                                    (xlog     (if xlog 1 0))
                                    (ylog     (if ylog 1 0))
                                    (xtitle   tx)
                                    (ytitle   ty)
                                    (title    tp)
                                    (bgcolor  (translate-color-code (or bg $WHITE)))
                                    (fgcolor  (translate-color-code (or fg $BLACK)))
                                    (grid     (if grid 1 0))
                                    (ticks-inside (if (eq ticks :inside) 1 0))
                                    ))
              (with-delayed-update
               (werase)
               (if wmarkfn
                   (funcall wmarkfn))
               (with-sg-lock (lpPlotAxes p))
               )))))
      )))

;; ------------------------------------------
(defun oplot2 (x y
                 &rest args
                 &key
                 npts
                 symbol thick penpat
                 clip draw-axes (alpha 255)
                 (color (if draw-axes $darkgreen $red))
                 &allow-other-keys)
  (with-float-cvector (fx x)
    (with-float-cvector (fy y)
      (let ((npts (or npts
                      (if fx
                          (min (ca:carray-total-size fx)
                               (ca:carray-total-size fy))
                        (ca:carray-total-size fy)))))
        (fli:with-dynamic-foreign-objects ()
          (let ((p    (fli:allocate-dynamic-foreign-object
                       :type 'tdata-parms)))
            (with-delayed-update
              (when draw-axes
                (apply #'plot-axes fx fy :npts npts args))
              (set-foreign-slots p ((npts  npts)
                                    (pxarr  (and fx (ca:carray-data fx)))
                                    (pyarr  (ca:carray-data fy))
                                    (sym    (or symbol 0))
                                    (color  (translate-color-code (or color $BLACK)))
                                    (thick  (or thick 1))
                                    (clip   (if clip 1 0))
                                    (penpat (or penpat $PENPAT-SOLID))
                                    (alpha  (rgb-int alpha))
                                    ))
              (with-sg-lock (lpPlotData p))
              )))
        ))))

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
(defun plot-image (img &rest args
                       &key
                       xrange yrange zrange
                       xlog ylog zlog
                       neg
                       flipv fliph
                       (plot-axes t)
                       (xoff 0)
                       &allow-other-keys)
  (with-float-carray (fimg img)
   (destructuring-bind (dimy dimx)
       (ca:carray-dimensions fimg)
     (let* ((xrange (or xrange (list 0 dimx)))
            (yrange (or yrange (list 0 dimy))))
       (with-delayed-update
         (when plot-axes ;; else must have been done prior to this call...
             (apply #'plot-axes #(0) #(0)
                    :xrange xrange
                    :yrange yrange
                    args))
         (fli:with-dynamic-foreign-objects ()
           (let ((p (fli:allocate-dynamic-foreign-object
                     :type 'timage-parms)))
             (set-foreign-slots p ((xsiz   dimx)
                                   (ysiz   dimy)
                                   (minval (float (if zrange (elt zrange 0) 0)))
                                   (maxval (float (if zrange (elt zrange 1) 0)))
                                   (xmagn  1.0) ;; unused
                                   (ymagn  1.0) ;; unused
                                   (xpos   0)   ;; unused
                                   (ypos   0)   ;; unused
                                   (parr  (ca:carray-data fimg))
                                   (neg   (if neg 1 0))
                                   (flipv (if flipv 1 0))
                                   (fliph (if fliph 1 0))
                                   (xmin  (float (elt xrange 0)))
                                   (xmax  (float (elt xrange 1)))
                                   (ymin  (float (elt yrange 0)))
                                   (ymax  (float (elt yrange 1)))
                                   (xlog  (if xlog 1 0))
                                   (ylog  (if ylog 1 0))
                                   (zlog  (if zlog 1 0))
                                   (xoff  xoff)
                                   ))
             (with-sg-lock (lpPlotImage p))))
         )))))

;; ------------------------------------------
(defun flatten-polygon (poly color)
  (cons (length poly)
        (cons (translate-color-code color)
              (mapcar #'truncate 
                      (reduce #'nconc poly
                              :from-end t
                              :initial-value nil)
                      ))))

(defun flatten-polygons (polys colors)
  (cons (length polys)
        (um:flatten
         (mapcar #'flatten-polygon
                 polys colors))))
#|
(defun plot-polys (polys colors)
  ;; polys is a list of vertex coords (y x)
  ;; colors is a corresponding list of color values
  (let* ((ser (flatten-polygons polys colors))
         (np (length ser)))
    (fli:with-dynamic-foreign-objects ()
      (let ((ptop (fli:allocate-dynamic-foreign-object
                   :type :int
                   :nelems np
                   :initial-contents ser)))
        (with-sg-lock (lpPlotPolys ptop))
        ))
    ))
|#

(defun plot-polys (polys colors)
  ;; polys is a list of vertex coords (y x)
  ;; colors is a corresponding list of color values
  (with-long-cvector (arr (flatten-polygons polys colors))
     (with-sg-lock (lpPlotPolys (ca:carray-data-address arr 0)))
     ))

#|
(defun plot-polys (polys colors)
  ;; polys is a list of vertex coords (y x)
  ;; colors is a corresponding list of color values
  (let ((np (length polys)))
    (fli:with-dynamic-foreign-objects ()
      (let ((ptop (fli:allocate-dynamic-foreign-object
                   :type 'tpolys-parms))
            (pverts (fli:allocate-dynamic-foreign-object
                     :type 'vertinfo
                     :nelems np)))
        (set-foreign-slots ptop ((npolys np)
                                 (polys  pverts)))
        (fli:with-coerced-pointer (p :type 'vertinfo) pverts
          (mapc
           #'(lambda (poly color)
               (let* ((npts (length poly))
                      (pts (fli:allocate-dynamic-foreign-object
                            :type 'point
                            :nelems npts)))
                 (fli:with-coerced-pointer (q :type 'point) pts
                   (dolist (pt poly)
                     (set-foreign-slots q ((x (truncate (second pt)))
                                           (y (truncate (first pt)))))
                     (fli:incf-pointer q)))
                 (set-foreign-slots p  ((m-color  
                                         (translate-color-code color))
                                        (m-nverts npts)
                                        (m-pts    pts)))
                 (fli:incf-pointer p)))
           polys colors))
        (with-sg-lock (lpPlotPolys ptop))
        ))
    ))
|#
;; ------------------------------------------
(defun plotfft (vec &rest plot-options)
  (apply #'plot (vm:shifth
                 (vm:vectorwise (vec)
                                (abs vec)))
         plot-options))

(defun tvfft (arr &rest tv-options)
  (apply #'tvscl
         (vm:shifth (if (getf tv-options :log nil)
                        (vm:elementwise (arr)
                                        (log (abs arr)))
                      (vm:elementwise (arr)
                                      (abs arr))))
         tv-options))


;; ------------------------------------------
(defun histo (x h &rest plot-options)
  (apply #'plot x h
         :symbol $sym-histogram
         plot-options))

(defun log-histo (x h &rest plot-options)
  (apply #'histo x h
         :ylog t
         :yrange (list 0.9 (reduce #'max h))
         plot-options))

;; ------------------------------------------
(defun fill-x-origin (xorg p)
  (destructuring-bind (xtype x) xorg
    (set-foreign-slots p ((xorg (coerce x 'single-float))))
    (ecase xtype
      (:data  1)
      (:frac  2)
      (:pixel 3))
    ))

(defun fill-y-origin (yorg p)
  (destructuring-bind (ytype y) yorg
    (set-foreign-slots p ((yorg (coerce y 'single-float))))
    (ecase ytype
      (:data  1)
      (:frac  2)
      (:pixel 3))
    ))

;; org can be a list of (type xorg yorg), e.g., '(:frac 0.9 0.96)
;; or a pair of typed values ((type xorg) (type yorg)), e.g., '((:frac 0.9) (:data 14.3))
(defun fill-origin (org p)
  (if (= 3 (length org))
      (destructuring-bind (typ xorg yorg) org
        (let ((xtype (fill-x-origin (list typ xorg) p))
              (ytype (fill-y-origin (list typ yorg) p)))
        (set-foreign-slots p ((org-type (+ xtype (* 4 ytype)))))
        ))
    (let ((xtype (fill-x-origin (first org) p))
          (ytype (fill-y-origin (second org) p)))
      (set-foreign-slots p ((org-type (+ xtype (* 4 ytype)))))
      )))

(defun draw-text (str org &key
                      (color $BLACK)
		      (font-name "Times-Roman") ;; PostScript Name from FontBook
                      ;;font-name
                      (font-size 10.0)
		      (clip t)
                      (anchor :nw)
                      (angle  0.0)
                      (alpha 255))
  (fli:with-dynamic-foreign-objects ()
    (let ((p (fli:allocate-dynamic-foreign-object
              :type 'ttext-parms)))
      (with-dynamic-foreign-strings ((txt  str)
                                     (font font-name))
        (fill-origin org p)				    
        (set-foreign-slots p ((color    (translate-color-code color))
                              (alpha    alpha)
                              (text     txt)
			      (fontName font)
                              (fontSize (float font-Size))
			      (clip     (if clip 1 0))
                              (anchor   (ecase anchor
                                          (:ctr  0)
                                          (:n    1)
                                          (:ne   2)
                                          (:e    3)
                                          (:se   4)
                                          (:s    5)
                                          (:sw   6)
                                          (:w    7)
                                          (:nw   8)))
                              (angle    (coerce angle 'single-float))))
        (with-sg-lock (lpDrawText p))
        ))
    ))

(defun save-plot (&optional (filename
			     (capi:prompt-for-file
                              "Save Plot as File?"
                              :operation :save
                              :filter "*.pdf")))
  (when filename
    (with-sg-lock (lpSavePlot (um:mkstr filename)))
    ))

;; ------------------------------------------
#|
(defun load-cursor-from-file (fname)
  (fli:with-foreign-string (cstr element-count byte-count) fname
    (declare (ignore element-count byte-count))
    (with-sg-lock (lpGetCursorHandle cstr))))

(defun direct-redraw (pane)
  (let ((hwnd (get-hwnd pane)))
    (with-sg-lock (lpDirectRedraw hwnd))))

;; ------------------------------------------
;; ...why is this one here?
(defun prompt-for-filenames (title)
  (let ((maxbuf 4096))
    (fli:with-dynamic-foreign-objects ()
      (let ((cbuf   (fli:allocate-dynamic-foreign-object
                     :type '(:unsigned :char)
                     :nelems maxbuf)))
        (fli:with-foreign-string (ctitle element-count byte-count) title
          (declare (ignore element-count byte-count))
          (lpPromptForMultipleFiles ctitle cbuf maxbuf))
        ;;
        ;; The primitive routine stuffs a buffer with null terminated strings,
        ;; and the last one is terminated by an addition null.
        ;;
        (labels
            ((cvt-substring (offs)
                            (fli:with-coerced-pointer (cp) cbuf
                              (fli:incf-pointer cp offs)
                              (fli:convert-from-foreign-string cp))))
          (do ((names nil)
               (ix    0))
              ((zerop (fli:dereference cbuf :index ix))
               ;;
               ;; If there is no selection the lst is NIL.
               ;; If there is a single selection lst is a list of
               ;;   one element equal to the full pathname of the file.
               ;; If there are multiple selections then fhe first element of lst
               ;; is the directory pathname, and the remaining elements are
               ;; filenames in that directory. We distribute the directory path over
               ;; all the filenames here to produce a homogeneous list.
               ;;
               (let ((lst (nreverse names)))
                 (if (rest lst)
                     (mapcar #'(lambda (fname)
                                 (concatenate 'string (first lst) "\\" fname))
                             (rest lst))
                   lst)))
            (let ((name (cvt-substring ix)))
              (push name names)
              (incf ix (1+ (length name)))))
          )))))

;; low level routines for c-array manipulations
;; (why isn't this stuff in C-ARRAYS.LISP ?? )
;; (... because they exist in the LispGraphics DLL)
(defun move-memory (pdst psrc nbytes)
  (lpMoveMemory pdst psrc nbytes))

(defun clear-memory (pdst nbytes)
  (lpClearMemory pdst nbytes))

(defun shift-image-left (pdst nshift-bytes ncol-bytes nrows)
  (lpShiftImageLeft pdst nshift-bytes ncol-bytes nrows))
|#

;; -----------------------------------------------------------------
;; Functional plotting with adaptive gridding
;; DM/RAL 12/06

(defun log10 (x) (log x (float 10 x)))
(defun pow10 (x) (expt (float 10 x) x))

(defstruct fplot-info
  trange xsf ysf tprepfns xprepfns yprepfns)

;; ----------------------------------------------------------------------------------------
;; Parametric Plotting with adaptive gridding
;;
(defun do-param-plotting (plotfn info xfn yfn args)
  (destructuring-bind (tmin tmax) (fplot-info-trange info)
    (destructuring-bind (tprepfn itprepfn) (fplot-info-tprepfns info)
      (declare (ignore tprepfn))
      (destructuring-bind (xprepfn ixprepfn) (fplot-info-xprepfns info)
        (destructuring-bind (yprepfn iyprepfn) (fplot-info-yprepfns info)
          (let* ((xsf (fplot-info-xsf info))
                 (ysf (fplot-info-ysf info))
                 (ts  (loop for ix from 0 to 16 collect
                            (+ tmin (* ix 0.0625 (- tmax tmin)))))
                 (xfn (um:compose xprepfn xfn itprepfn))
                 (yfn (um:compose yprepfn yfn itprepfn))
                 (xs  (mapcar xfn ts))
                 (ys  (mapcar yfn ts)))
            
            (um:with-tail-pure-code
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
        ))))

(defun paramplot (domain xfn yfn &rest args
                         &key over tlog xlog ylog xrange yrange
                         &allow-other-keys)
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
                  (let* ((info (make-fplot-info
                                :trange   (list tmin tmax)
                                :xsf      (/ 4/3 500.0 (- xmax xmin))
                                :ysf      (/ 500.0 (- ymax ymin))
                                :tprepfns (list tprepfn itprepfn)
                                :xprepfns (list xprepfn (and xlog ixprepfn))
                                :yprepfns (list yprepfn (and ylog iyprepfn)))))
                    (set-plot-info info)
                    (do-param-plotting (if over #'oplot #'plot) info xfn yfn args)
                    )))
              )))
        ))))

(defun oparamplot (xfn yfn &rest args)
  (do-param-plotting #'oplot (lookup-plot-info) xfn yfn args))

(defun fplot (domain fn &rest args)
  (apply #'paramplot domain #'identity fn args))

(defun ofplot (fn &rest args)
  (apply #'oparamplot #'identity fn args))

;; ---------------------------------------------------------

(defun get-next-event ()
  ;; only works against the latest wset window
  (aqt:wait-next-event))

;; -- end of scigraph.lisp -- ;;

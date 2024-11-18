
(defpackage vector-ops
  (:use common-lisp)
  (:export
   #:vec
   #:blit
   #:vlog
   #:vexp
   #:vabs
   #:vsub
   #:vadd
   #:vmul
   #:vdiv
   #:vscale
   #:voffset
   #:vmin
   #:vmax
   #:vsum
   #:vprod
   #:vround
   #:vfloor
   #:vceiling
   #:vsqr
   #:vsqrt
   #:vlog10
   #:vpow10
   #:vexpt
   #:destructure-vector
   ))

(defpackage fft
  (:use common-lisp)
  (:export
   #:r2c
   #:c2r
   #:fwd
   #:inv
   #:fwd-magnitude
   #:fwd-magnitude-db
   #:fwd-power
   #:fwd-phase
   #:fwd-phase-deg
   #:siglab_sbFFT))

#+:MAC
(defpackage aquaterm
  (:use common-lisp)
  (:nicknames aqt)
  (:export
   #:def-proxy-fli-function
   #:terminate
   #:open-plot
   #:select-plot
   #:set-plot-size
   #:set-plot-title
   #:save-context
   #:restore-context
   #:set-cliprect
   #:set-affine-transform
   #:render-plot
   #:clear-plot
   #:close-plot
   #:save-plot
   #:set-color
   #:set-background-color
   #:set-font-name
   #:set-font-size
   #:add-label
   #:set-line-width
   #:set-line-cap-style
   #:move-to
   #:add-line-to
   #:add-poly-line
   #:move-to-vertex
   #:add-edge-to-vertex
   #:add-polygon
   #:add-filled-rect
   #:erase-rect
   #:set-image-transform
   #:reset-image-transform
   #:add-image-with-bitmap
   #:add-transformed-image-with-bitmap

   #:set-accepting-events
   #:wait-next-event
   #:get-last-event

   #:butt-line-cap-style
   #:round-line-cap-style
   #:square-line-cap-style

   #:align-left
   #:align-center
   #:align-right

   #:align-middle
   #:align-baseline
   #:align-bottom
   #:align-top ))

(defpackage scigraph
  (:use common-lisp)
  (:nicknames sg)
  (:export
   #:tvscl
   #:plot
   #:oplot
   #:draw-text
   #:plot-axes
   #:plot-image
   #:draw-grid
   #:window
   #:wset
   #:wshow
   #:werase
   #:wkill
   #:wattach
   #:wdetach
   #:wsave
   #:delay-update
   #:update
   #:with-delayed-update
   #:plot-polys
   #:set-cmap
   #:get-mouse
   #:get-coord-values
   #:get-wsize
   #:copy-graphic-to-clipboard
   #:$sym-circle
   #:$sym-square
   #:$sym-box
   #:$sym-dot
   #:$sym-cross
   #:$sym-triangle
   #:$sym-histogram
   #:$penpat-solid
   #:$penpat-dash
   #:$penpat-dot
   #:$penpat-dashdot
   #:$penpat-dashdotdot
   #:$penpat-null
   #:$penpat-inside-frame
   #:rgb
   #:$black
   #:$white
   #:$red
   #:$green
   #:$blue
   #:$darkred
   #:$darkgreen
   #:$darkblue
   #:$skyblue
   #:$purple
   #:$magenta
   #:$cyan
   #:$yellow
   #:$orange
   #:$gray50
   #:$gray25
   #:$gray75

   #:plotfft
   #:tvfft
   #:histo
   #:log-histo

   #:load-cursor-from-file
   #:direct-redraw
   
   #:prompt-for-filenames
   #:move-memory
   #:clear-memory
   #:shift-image-left
   #:set-image-cursor-transform

   #:set-heat-colormap
   #:set-gray-colormap

   #:save-plot

   #:fplot
   #:ofplot
   #:paramplot
   #:oparamplot

   #:stamp-logo

   #:get-next-event
   ))

(defpackage vectorized-math
  (:use common-lisp vector-ops)
  (:nicknames vmath vm)
  (:export
   #:iramp
   #:framp
   #:dramp
   #:bipolar-framp
   #:unoise
   #:hypot
   #:sinc
   #:logabs
   #:vector-of
   #:gasdev
   #:gnoise
   #:median
   #:mad
   #:negmad
   #:total
   #:mean
   #:stdev
   #:variance
   #:wtmean
   #:wtvariance
   #:wtstdev
   #:percentile
   #:percentiles
   #:standard-percentiles
   #:copy
   #:histogram

   #:fzeros
   #:fones
   #:izeros
   #:iones
   
   #:inner-prod
   #:general-inner-prod
   #:outer-prod
   #:general-outer-prod
   #:dist
   #:shift
   #:shifth
   #:slice
   #:reshape
   #:map-dimension
   #:reduce-dimension
   #:vrotl
   #:vrotr
   #:vshifth
   #:split
   #:transpose
   #:acanon
   #:xplane
   #:yplane
   #:bipolar-xplane
   #:bipolar-yplane
   #:shifted-bipolar-framp
   #:shifted-bipolar-xplane
   #:shifted-bipolar-yplane

   #:vectorwise
   #:elementwise
   #:elementwise-reduce
   #:gensyms-for-args
   #:ixsort

   #:require-same-shape
   #:require-same-size
   #:defun-ffp

   #:dgesvd-solve
   #:dgesvd
   #:dgesvd-bksb
   #:dgesvd-predict
   ))
   
(defpackage surface-plots
  (:nicknames surf surfplots)
  (:use common-lisp)
  (:export
   #:plot-surface
   #:lego-plot
   #:rot
   #:red-color-fn
   #:gray-color-fn
   #:lamps-color-fn
   #:*lamps*
   #:make-lamp
   #:lamp-dir-vector
   #:lamp-rgb-triple
   #:lamp-intensity
   ))

(defpackage image-processing
  (:use common-lisp)
  (:nicknames img)
  (:export
   #:<image-array>
   #:<matrix-array>
   #:image-array-arena
   "(SETF IMAGE-ARRAY-ARENA)"
   #:make-image
   #:make-matrix
   #:make-similar
   #:pixelwise
   #:pixelwise-reduce
   #:as-image
   #:as-matrix
   #:<subimage-array>
   #:subimage-centered
   #:toroidal-coords
   #:subimage
   #:place-subimage
   #:fill-subimage
   #:shift
   #:shifth
   #:vshift
   #:col-vector
   #:row-vector
   #:get-col
   #:get-row
   #:get-column-vector
   #:get-row-vector
   #:x-slice
   #:y-slice
   #:flipv
   #:fliph
   #:transpose
   #:matrix-diagonal
   #:tvscl
   #:max-ix
   #:fft
   #:ifft
   #:xplane
   #:yplane
   #:where
   #:total
   #:mean
   #:plot-surface
   #:lego-plot
   #:indices-of
   #:find-peak
   #:show-peak))

(defpackage photometry
  (:use common-lisp)
  (:nicknames phot)
  (:export
   #:photom))

;; ---------------------------------------------------------------------
;;

#+:MAC
(defsystem aquaterm
  (:package "CL-USER"
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;vmath;"))
  :members (("data-objects"  :type :system)
            ("regex"         :type :system)
            "aquaterm-macros" "aquaterm-dff")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))

(defsystem vmath
  (:package "CL-USER"
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;vmath;"))
  :members (("data-objects"  :type :system)
            #+:MAC ("aquaterm"      :type :system :features :mac)
            ("c-arrays"      :type :system)
            ("../scids-ole/safearrays" :features :win32)
            "simple-vector-ops"
            "vmath" 
            ("win_scigraph_intf" :features :win32)
            ("win-scigraph" :features :win32)
            ("win_fft_intf" :features :win32)
            ("win-fft"      :features :win32)
            ("mac-scigraph-macros" :features :mac)
            ("mac_scigraph_intf"   :features :mac)
            ("mac-scigraph"        :features :mac)
            ("mac_fft_intf"        :features :mac)
            ("mac-fft"             :features :mac)
            ("dgesvd"              :features :mac))
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))

(defsystem plotting
  (:package "CL-USER"
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;vmath;"))
  :members (("vmath"         :type :system)
            ("regex"         :type :system)
            ("mac-plotter-stuff" :features :mac)
            ("plotter"       :type :system)
            "surfplot" 
            "images")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))
  
(defsystem photom
  (:package "CL-USER"
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;vmath;"))
  :members (("vmath" :type :system)
	    ("data-objects" :type :system)
	    ("scids" :type :system)
            "photom")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires  (:load    :previous)))))

#|
(defsystem photest
  (:package "CL-USER"
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;vmath;"))
  :members (("useful-macros" :type :system)
	    ("data-objects"  :type :system)
	    ("scids"         :type :system)
            ("c-arrays"      :type :system)
            "../scids-ole/safearrays"
            "vmath" 
	    "scigraph_intf" "scigraph" 
	    "fft_intf" "fft" 
	    "surfplot" 
	    "images"
            "photom")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires  (:load    :previous)))))
|#
  
(defun load-vmath ()
  (compile-system 'vmath :load t))

(defun load-vmath2 ()
  (compile-system 'vmath2 :load t))

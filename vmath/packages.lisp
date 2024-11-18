
;; (in-package #:CL-USER)

(project:defproject
 (#:fft              #:com.ral.fft)
 (#:sfft             #:com.ral.sfft)
 (#:dfft             #:com.ral.dfft)
 (#:fftt2d           #:com.ral.fft2d)
 (#:interpolation    #:interp)
 (#:interp           #:com.ral.interpolation)
 (#:integration      #:com.ral.integration)
 (#:roots            #:com.ral.roots)
 (#:aquaterm         #:com.ral.aquaterm)
 (#:aqt              #:aquaterm)
 (#:scigraph         #:com.ral.scigraph)
 (#:sg               #:scigraph)
 (#:surface-plots    #:com.ral.surface-plots)
 (#:surf             #:surface-plots)
 (#:vm               #:vmath)
 (#:vmath            #:com.ral.vectorized-math)
 (#:image-processing #:com.ral.image-processing)
 (#:photometry       #:com.ral.photometry)
 (#:kaiser           #:com.ral.kaiser)
 (#:matrix           #:com.ral.matrix-ops)
 (#:nrglue           #:com.ral.nrglue))
 
(defpackage #:com.ral.vector-ops
  (:use #:common-lisp)
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
   #:vlimit<
   #:vlimit>
   #:extrema
   #:vextrema
   #:clip
   #:vclip
   #:wrap
   #:vwrap
   ))

(defpackage #:com.ral.fft
  (:use #:common-lisp)
  (:export
   #:$fftw-forward
   #:$fftw-inverse

   #:d2zfft
   #:z2dfft
   #:z2zfft
   #:unsafe-z2zfft

   #:r2cfft
   #:c2rfft
   #:c2cfft
   #:unsafe-c2cfft
   
   #:d2zfft2d
   #:z2dfft2d
   #:z2zfft2d
   #:unsafe-z2zfft2d
   
   #:r2cfft2d
   #:c2rfft2d
   #:c2cfft2d
   #:unsafe-c2cfft2d

   #:twids
   #:twids-p
   #:make-twids
   #:twids-expir
   #:twids-refct
   #:twids-prec
   #:twids-log2n
   #:twids-psetup
   
   #:fft-buffer
   #:fft-buffer-p
   #:make-fft-buffer
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:fft-buffer-ny
   #:get-real
   #:get-imag
   #:set-real
   #:set-imag
   #:r2c
   #:c2r
   #:c2c
   #:fwd
   #:inv
   #:fwd-magnitude
   #:fwd-magnitude-db
   #:fwd-power
   #:fwd-phase
   #:fwd-phase-deg
   #:get-align16-offset
   #:siglab_sbFFT

   #:symmetric-fill
   #:symmetric-replace
   #:center-implant

   #:with-stwids
   #:with-dtwids
   #:get-process-split-tmp
   #:get-stmp
   #:get-dtmp
   #:get-c-address

   #:half-dim
   #:copy-fft-buffer-contents
   #:effective-type
   #:effective-array-element-type
   #:effective-ctype
   #:vec
   #:convert-complex-cvect-to-array
   #:convert-complex-cvect-magnitudes-to-array
   #:convert-complex-cvect-power-to-array
   #:convert-complex-cvect-magnitudes-db-to-array
   #:convert-complex-cvect-phases-to-array
   #:convert-complex-cvect-phases-deg-to-array
   #:do-convert-split-complex-cvect-to-array
   #:do-copy-array-to-split-complex-cvect
   ))

#+:MACOSX
(defpackage #:com.ral.fft2d
  (:use #:common-lisp)
  (:import-from #:fft
   #:half-dim
   #:make-fft-buffer
   #:get-real
   #:get-imag
   #:set-real
   #:set-imag
   #:copy-fft-buffer-contents
   #:effective-type
   #:effective-array-element-type
   #:effective-ctype
   #:vec
   #:fft-buffer
   #:fft-buffer-p
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:fft-buffer-ny
   )
  (:export
   #:fft-buffer
   #:fft-buffer-p
   #:make-fft-buffer
   #:make-fft-buf
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:fft-buffer-ny
   #:get-real
   #:get-imag
   #:set-real
   #:set-imag
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

#+:MACOSX
(defpackage #:com.ral.sfft
  (:use #:common-lisp)
  (:import-from #:fft
   #:get-real
   #:get-imag
   #:copy-fft-buffer-contents
   #:fft-buffer
   #:fft-buffer-p
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:fft-buffer-ny
   #:get-c-address
   )
  (:export
   #|
   #:make-fft-buffer
   #:fft-buffer-p
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:get-real
   #:get-imag
   #:set-real
   #:set-imag
   |#
   #:pwr
   #:ampl
   #:db10
   #:rtod
   #:phs-deg
   #:phs

   #:r2c
   #:c2r
   #:c2c
   #:fwd
   #:inv
   #:fwd-magnitude
   #:fwd-magnitude-db
   #:fwd-power
   #:fwd-phase
   #:fwd-phase-deg))

#+:MACOSX
(defpackage #:com.ral.dfft
  (:use #:common-lisp)
  (:import-from #:fft
   #:get-real
   #:get-imag
   #:copy-fft-buffer-contents
   #:set-imag
   #:fft-buffer
   #:fft-buffer-p
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:fft-buffer-ny
   #:get-c-address
   )
  (:export
   #|
   #:make-fft-buffer
   #:fft-buffer-p
   #:fft-buffer-r
   #:fft-buffer-pr
   #:fft-buffer-roff
   #:fft-buffer-i
   #:fft-buffer-pi
   #:fft-buffer-ioff
   #:fft-buffer-hr
   #:fft-buffer-nx
   #:get-real
   #:get-imag
   #:set-real
   #:set-imag
   |#
   #:pwr
   #:ampl
   #:db10
   #:rtod
   #:phs-deg
   #:phs
   
   #:d2z
   #:z2d
   #:z2z
   #:fwd
   #:inv
   #:fwd-magnitude
   #:fwd-magnitude-db
   #:fwd-power
   #:fwd-phase
   #:fwd-phase-deg))

#+:MACOSX
(defpackage #:com.ral.aquaterm
  (:use #:common-lisp)
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

(defpackage #:com.ral.scigraph
  (:use #:common-lisp)
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

(defpackage #:com.ral.vectorized-math
  (:use #:common-lisp #:com.ral.vector-ops)
  (:export
   #:horner
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
   #:make-overlay-vector

   #:require-same-shape
   #:require-same-size
   #:defun-ffp

   #:dgesvd-solve
   #:dgesvd
   #:dgesvd-bksb
   #:dgesvd-predict

   #:simplex
   
   #:without-denormals
   ))
   
(defpackage #:com.ral.surface-plots
  (:use #:common-lisp)
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

(defpackage #:com.ral.image-processing
  (:use #:common-lisp)
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

(defpackage #:com.ral.old-photometry
  (:use #:common-lisp #:com.ral.actors)
  (:export
   #:photom))

(defpackage #:com.ral.interpolation
  (:use #:common-lisp)
  (:export
   #:linint
   #:polint
   #:ratint
   #:locate
   #:locate-subtable
   #:spline
   #:splint
   #:monotonic-spline
   #:monotonic-splint
   ))

(defpackage #:com.ral.integrate
  (:use #:common-lisp)
  (:export
   #:trapm
   #:mtriple
   #:qromb
   #:qrombm
   #:sub-array
   ))

(defpackage #:com.ral.roots
  (:use #:common-lisp)
  (:export
   #:find-root
   #:newton
   #:deriv
   #:zbrac
   #:rtbis
   #:rtsec
   #:zbrent
   #:rtsafe
   ))

(defpackage #:com.ral.kaiser
  (:use #:cl)
  (:export
   #:kaiser-window
   #:sinc
   #:beta-factor
   #:compute-fir-filter-order
   #:bessel-i0
   ))

#|
;; generate HTML documentation
(in-package #:vmath)
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :com.ral.vmath
 :package-name     :vmath
 :directory        (translate-logical-pathname "PROJECTS:LISP;vmath;")
 :subtitle         "a library for vectorized math")
|#

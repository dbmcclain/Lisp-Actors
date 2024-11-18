
;; this works fine -- No graphics transforms defined
(let ((pane (capi:contain
             (make-instance 'capi:output-pane))))
  (capi:apply-in-pane-process pane
                              #'gp:draw-arc pane 50 50 100 100 0 (/ pi 4) :filled t))

;; this doesn't work properly
;; the non-square scaling in the X- and Y-axes causes severe distortion of the arc
(let ((pane (capi:contain
             (make-instance 'capi:output-pane)
             :best-width  408
             :best-height 323)))
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (let ((xform (gp:make-transform)))
                                  (gp:apply-translation xform 1/18 1/18)

                                  ;; non-square X- and Y-scaling
                                  (gp:apply-scale       xform 360  -250)

                                  (gp:apply-translation xform 30    270)
                                  (gp:with-graphics-transform (pane xform)
                                    (gp:draw-arc pane 0.3 0.3 0.5 0.5 0 (/ pi 4) :filled t)
                                    (gp:draw-line pane 0 0 0.9 0.9)
                                    (gp:draw-line pane 0.9 0.01 0.1 0.4)
                                    )))
                              ))


;; this sort-of works because the scaling in X and Y is the same
;; but the sweep-angle appears to be left-handed (due to the negative Y-axis scaling)
(let ((pane (capi:contain
             (make-instance 'capi:output-pane)
             :best-width  408
             :best-height 323)))
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (let ((xform (gp:make-transform)))
                                  (gp:apply-translation xform 1/18 1/18)

                                  ;; square X- and Y-scaling
                                  (gp:apply-scale       xform 300 -300)
                                  
                                  (gp:apply-translation xform 30   270)
                                  (gp:with-graphics-transform (pane xform)
                                    (gp:draw-arc pane 0.3 0.3 0.5 0.5 0 (/ pi 4) :filled t)
                                    (gp:draw-line pane 0 0 0.9 0.9)
                                    (gp:draw-line pane 0.9 0.01 0.1 0.4)
                                    )))
                              ))


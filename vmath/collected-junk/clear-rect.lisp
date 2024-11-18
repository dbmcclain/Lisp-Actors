
(defun display-callback (pane x y wd ht)
  (let ((sf (/ (gp:port-width pane) 300)))
    (gp:with-graphics-scale (pane sf sf)
      (gp:with-graphics-state
          (pane
           :foreground :green)
        (gp:draw-rectangle pane 10 10 200 100 :filled t)
        (gp:clear-rectangle pane 30 30 150 50)
        ))))
  
(capi:contain
 (make-instance 'capi:output-pane
                :display-callback 'display-callback))

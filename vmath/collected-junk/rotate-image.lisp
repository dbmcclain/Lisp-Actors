(defun rotate-image (pane)
  (capi:apply-in-pane-process
   pane
   (lambda ()
     (let* ((wd (gp:port-width pane))
            (ht (gp:port-height pane))
            ;;(rimg (gp:make-image pane ht wd)) ;; <--- appears to be the culprit
            (rimg (gp:make-image-from-port pane 0 0 ht wd)) ; <-- replacement
            (img  (gp:make-image-from-port pane))
            (ria  (gp:make-image-access pane rimg))
            (ia   (gp:make-image-access pane img)))
       
       (gp:image-access-transfer-from-image ia)
       (loop for ix from 0 below wd do
             (loop for iy from 0 below ht do
                   (setf (gp:image-access-pixel ria iy (- wd ix 1))
                         (gp:image-access-pixel ia  ix iy))
                   ))
       (gp:image-access-transfer-to-image ria)

       (gp:free-image-access ia)
       (gp:free-image-access ria)
       (gp:free-image pane img)

       (gp:draw-image pane rimg 0 0)

       (gp:free-image pane rimg)
       ))
   ))

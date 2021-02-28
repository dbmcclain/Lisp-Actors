
(in-package :editor)

#|
(lw:defadvice (section-form dwc :around)
    (start end return-anonymous-p)
  (let ((this (call-next-advice start end return-anonymous-p)))
    (when this
      (if (typep this 'definition)
          (let ((def (definition-dspec this)))
            (if (consp def)
                (let ((inner (get (car def) 'dwc-subparser)))
                  (if inner
                      (with-input-from-region (stream start end)
                        (ignore-errors (read-carefully stream))
                        (ignore-errors (read-carefully stream))
                        (let* ((sub-start (editor-region-stream-point stream))
                               (grp       (in-section-region
                                           :start sub-start
                                           :end   end
                                           :start-offset (point-to-offset sub-start)
                                           :type 'progn-definition-group)))
                          (if grp
                              (progn
                                (push this (slot-value grp 'contents))
                                grp)
                            ;; else
                            this)))
                    ;; else
                    this))
              ;; else
              this))
        this))
    ))
|#
#|
(hcl:delete-advice section-form dwc)
|#

(defun indent-like (sym like-sym)
  (let* ((tbl  (slot-value *default-syntax-table* 'operators))
         (inds (gethash (string like-sym) tbl)))
    (if inds
        (apply #'setup-indent (string sym) (coerce inds 'list))
      (error "No indentation for symbol: ~A" like-sym))
    ))
(export 'indent-like)

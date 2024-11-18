
(defstruct once-thereafter
  first-time
  thereafter)

(defmethod get-value ((v once-thereafter))
  (shiftf (once-thereafter-first-time v) (once-thereafter-thereafter v)))


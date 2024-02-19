;; kvdb-gui.lisp
;;
;; DM/RAL  2023/10/04 05:40:13 UTC
;; ----------------------------------

(in-package #:com.ral.actors.kvdb)

;; ----------------------------------

;; ---------------------------------------------------------
;; GUI for KVDB - an interesting interplay between CAPI thread and
;; Actors.

(capi:define-interface kvdb-display ()
  ((db-pathname :reader kv-db-pathname :initarg :path))
  (:panes
   #|
   (search-pane   capi:text-input-pane
                  :accessor           search-pane
                  :title              "Search..."
                  :callback           'search-keys)
   |#
   (db-path-pane  capi:title-pane
                  :text               db-pathname
                  :foreground         :seagreen)
   (keys-list     capi:list-panel
                  :accessor           keys-panel
                  :visible-min-width  300
                  :visible-min-height 300
                  :selection-callback 'click-show-value
                  :callback-type      :item-element
                  :foreground         #+:MACOSX :yellow #-:MACOSX :black
                  :title              "KVDB Key"
                  :title-args         '(:foreground #+:MACOSX :skyblue #-:MACOSX :gray50)
                  :print-function     'key-to-string)
   (value-display capi:editor-pane
                  :accessor           value-panel
                  :title              "KVDB Value"
                  :title-args         '(:foreground #+:MACOSX :skyblue #-:MACOSX :gray50)
                  :text               ""
                  :buffer-name        :temp
                  :foreground         #+:MACOSX :yellow #-:MACOSX :black
                  :visible-min-width  400
                  :visible-min-height 300)
   (refr-but      capi:push-button
                  :text               "Refresh"
                  :foreground         :skyblue
                  :callback           'click-refresh-keys)
   (del-but       capi:push-button
                  :text               "Delete"
                  :foreground         :skyblue
                  :callback           'click-delete-key)
   (add-but       capi:push-button
                  :text               "Add/Change"
                  :foreground         :skyblue
                  :callback           'click-add/change-key))
  (:layouts
   (main-layout capi:column-layout
                '(path-layout :separator central-layout))
   (path-layout capi:row-layout
                '(nil db-path-pane nil))
   (central-layout capi:row-layout
                '(keys-layout :divider value-display))
   (keys-layout capi:column-layout
                '(#|search-pane|# keys-list but-layout))
   (but-layout capi:row-layout
               '(refr-but del-but add-but)))
  (:default-initargs
   :title "KVDB Browser"))

;; -----------------------------------------------------------
;; Show the GUI

(defun show-kvdb ()
  ;; Show a KVDB Browser
  (β (path)
      (send kvdb β :db-path)
    (let ((intf (capi:display
                 (make-instance 'kvdb-display
                                :path (namestring path)))))
      (refresh-select-and-show-first-item intf))
    ))

;; -----------------------------------------------------------

(defun select-and-show-key (intf key)
  (let ((keys-pane  (keys-panel intf)))
    (capi:apply-in-pane-process
     keys-pane
     (λ ()
       (setf (capi:choice-selected-item keys-pane) key)
       (click-show-value key keys-pane))
     )))
  
(defun refresh-select-and-show-first-item (intf)
  (β _
      (refresh-keys nil intf β)
    (let* ((keys-pane (keys-panel intf))
           (keys      (capi:collection-items keys-pane)))
      (when (plusp (length keys))
        (let ((key (aref keys 0)))
          (capi:apply-in-pane-process
           keys-pane
           (λ ()
             (setf (capi:choice-selected-item keys-pane) key)
             (click-show-value key keys-pane))
           )))
      )))

(defun refresh-select-and-show-key (intf key)
  (β _
      (refresh-keys nil intf β)
    (select-and-show-key intf key)))
  
;; -----------------------------------------------------------
;; GUI CAPI Callback Functions

(defun refresh-keys (xxx intf &optional cust)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (β (keys)
      (collect-keys β)
    (let ((keys-panel (keys-panel intf)))
      (capi:apply-in-pane-process
       keys-panel
       (λ ()
         (setf (capi:collection-items keys-panel) keys)
         (send cust :ok))) ;; for sequencing
      )))

(defun click-show-value (key pane)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (β (val)
      (send kvdb β :find key)
    (let* ((key-string (key-to-string val :pretty t :right-margin 62))
           (intf       (capi:element-interface pane))
           (ed-pane    (value-panel intf)))
      (capi:apply-in-pane-process
       ed-pane
       (λ ()
         (setf (capi:editor-pane-text ed-pane) key-string)
         (capi:call-editor ed-pane "Beginning of Buffer")))
      )))

#|
(defun search-keys (text intf)
  (declare (ignore intf))
  (print text))
|#

(defun click-refresh-keys (xxx intf)
  (declare (ignore xxx))
  (refresh-select-and-show-first-item intf))

(defun click-delete-key (xxx intf)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (let* ((keys-pane (keys-panel intf))
         (key       (capi:choice-selected-item keys-pane)))
    (with-actors
      (when (capi:prompt-for-confirmation
             (format nil "Delete ~S" (key-to-string key)))
        (β _
            (send kvdb β :remove key)
          (refresh-select-and-show-first-item intf))
        ))
    ))

;; ------------------------------------------------------------
;; Popup Dialog for Add/Change...

(capi:define-interface kv-query-intf ()
  ((key-text :initarg :key-text)
   (val-text :initarg :val-text))
  (:panes
   (key-pane capi:text-input-pane
             :accessor          key-pane
             :title             "Key"
             :text              key-text
             :visible-min-width 300)
   (val-pane capi:text-input-pane
             :accessor          val-pane
             :title             "Value"
             :text              val-text
             :visible-min-width 400))
  (:layouts
   (main-layout capi:row-layout
                '(key-pane val-pane))) )

(defun grab-text-value (pane)
  (read-from-string (capi:text-input-pane-text pane)))

(defun grab-dialog-values (pane)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  ;; PANE points to our dialog pane.
  (ignore-errors ;; this works out nicely!
    (list
     (grab-text-value (key-pane pane))
     (grab-text-value (val-pane pane)))
    ))

(defun click-add/change-key (xxx intf)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (declare (ignore xxx))
  (let* ((keys-pane (keys-panel intf))
         (key       (capi:choice-selected-item keys-pane)))
    (β (val)
        (send kvdb β :find key)
      (let ((dlg  (make-instance 'kv-query-intf
                                 :key-text (key-to-string key)
                                 :val-text (key-to-string val))
                  ))
        (multiple-value-bind (result successp)
            (capi:popup-confirmer dlg "Add/Change a KVDB Entry"
                                  :value-function 'grab-dialog-values
                                  :owner intf)
          (when successp
            (destructuring-bind (key val) result
              (β _
                  (send kvdb β :add key val)
                (refresh-select-and-show-key intf key))
              ))))
      )))

#|
(show-kvdb)
|#

;; kvdb-gui.lisp
;;
;; DM/RAL  2023/10/04 05:40:13 UTC
;; ----------------------------------

(in-package #:com.ral.actors.kvdb)

;; ----------------------------------

;; ---------------------------------------------------------
;; GUI for KVDB - an interesting interplay between CAPI thread and
;; Actors.

;; ---------------------------------------------------------
;; For our own sanity... Let's keep things, as much as possible, on
;; one playing field.

(deflex i-capi
  (create
   ;; Expects intf, fn, args in message
   #'capi:execute-with-interface))

(deflex p-capi
  (create
   ;; Expects pane, fn, args in message
   #'capi:apply-in-pane-process))

;; ------------

(defmacro with-capi-intf (intf &body body)
  `(send i-capi ,intf (lambda () ,@body)))

(defmacro with-capi-pane (pane &body body)
  `(send p-capi ,pane (lambda () ,@body)))

;; -----------------------------------------------------------
;; Now for the GUI...

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
;; Utility Functions

(defun key-to-string (key)
  ;; Also used for value displays
  (with-output-to-string (s)
    (with-maximum-io-syntax ;; with-standard-io-syntax
      (let ((*package* (find-package :cl)))
        (handler-case
            (let ((*print-readably* t))
              (write key :stream s))
          (error ()
            ;; Some compiled functions refuse to display in
            ;; *PRINT-READABLE* mode, and throw us here with an ERROR.
            (let ((*print-readably* nil))
              (write key :stream s)))
          ))
      )))

(defun collect-keys (cust)
  (β (db)
      (send kvdb β :req)
    (let (keys)
      (db-map db
              (lambda (k v)
                (declare (ignore v))
                (push k keys)))
      (send cust (sort keys #'string< :key #'key-to-string))
      )))

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
  (with-capi-intf intf
    (let ((keys-pane (keys-panel intf)))
      (setf (capi:choice-selected-item keys-pane) key)
      (click-show-value key keys-pane))
    ))
  
(defun refresh-select-and-show-first-item (intf)
  (β _
      (refresh-keys nil intf β)
    (with-capi-intf intf
      (let* ((keys-pane (keys-panel intf))
             (keys      (capi:collection-items keys-pane)))
        (when (plusp (length keys))
          (select-and-show-key intf (aref keys 0))
          )))
    ))

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
    (with-capi-intf intf
      (let ((keys-panel (keys-panel intf)))
        (setf (capi:collection-items keys-panel) keys)
        (send cust :ok))) ;; for sequencing
    ))

(defun click-show-value (key pane)
  ;; CAPI Callback function - on entry we are running in CAPI thread.
  (β (val)
      (send kvdb β :find key)
    (with-capi-pane pane
      (let* ((intf    (capi:element-interface pane))
             (ed-pane (value-panel intf)))
        (setf (capi:editor-pane-text ed-pane) (key-to-string val))
        (capi:call-editor ed-pane "End of Buffer")))
    ))

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

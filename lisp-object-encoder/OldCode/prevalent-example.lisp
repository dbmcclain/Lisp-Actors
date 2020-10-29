
(in-package :prevalent-object)

(defstruct stock
  date
  open
  high
  low
  close
  volume)

(defstruct stock-table
  (history (memory-btrees:make-btree
            :compare '-
            :key     'stock-date)))

(defun stock-table-name (stock-name)
  (intern (string stock-name) :keyword))

(defun find-or-create-table (stock-name)
  (let ((table-name (stock-table-name stock-name)))
    (or (get-root-object table-name)
        (add-root-object table-name (make-stock-table)))))

(defmethod ensure-mjd (val)
  val)

(defmethod ensure-mjd ((str string))
  (stocks::convert-date-string-to-mjd str))

(defun save-stock (stock-name)
  (let* ((data (stocks::get-data (stocks::market-data
                                  (concatenate 'string stock-name ".csv"))))
         (table (find-or-create-table stock-name))
         (stock (make-stock
                 :date   (reverse (csv:get-column "Date" data))
                 :open   (stocks::get-numeric-column "Open"   data)
                 :high   (stocks::get-numeric-column "High"   data)
                 :low    (stocks::get-numeric-column "Low"    data)
                 :close  (stocks::get-numeric-column "Close"  data)
                 :volume (stocks::get-numeric-column "Volume" data))))
    (loop for ix from 0
          for date   across (stock-date   stock)
          for open   across (stock-open   stock)
          for high   across (stock-high   stock)
          for low    across (stock-low    stock)
          for close  across (stock-close  stock)
          for volume across (stock-volume stock)
          do
          (let ((mjd (ensure-mjd date)))
            (btree:insert-item (stock-table-history table) mjd
                               (make-stock
                                :date   mjd
                                :open   open
                                :high   high
                                :low    low
                                :close  close
                                :volume volume))
            ))
    (log-full-update table 'history (stock-table-history table))
    ))

;; ---------------------------------------------------

(defun print-heading (stock-name count)
  (format t "~&Stock: ~A~%"  stock-name)
  (format t "~&~D entries~%" count)
  (format t "~&Index  MJD       Date        Open     High      Low    Close        Volume~%")
  (format t "~&--------------------------------------------------------------------------~%"))

(defun show-stock (stock-name &key from to direction)
  (um:when-let (table (get-root-object (stock-table-name stock-name)))
    (let ((count 0))
      (print-heading stock-name (btree:items-count (stock-table-history table)))
      (btree:map-tree (stock-table-history table)
                      (lambda (prices)
                        (incf count)
                        (format t "~&~4D: ~6D  ~10A ~8,2F ~8,2F ~8,2F ~8,2F ~13:D~%"
                                count
                                (stock-date prices)
                                (stocks::convert-mjd-to-date-string (stock-date prices))
                                (stock-open  prices)
                                (stock-high  prices)
                                (stock-low   prices)
                                (stock-close prices)
                                (round (stock-volume prices))))
                      :from (ensure-mjd from)
                      :to   (ensure-mjd to)
                      :direction direction)
      )))

;; ---------------------------------------------------------------------------------

(defun view-btree (stock-name)
  (um:when-let (table (get-root-object (stock-table-name stock-name)))
    (let* ((tree  (stock-table-history table))
           (root  (btree:root-node tree)))
      (if root
          (capi:contain
           (make-instance
            'capi:graph-pane
            :title (format nil "B-Tree for ~A" stock-name)
            :roots (list root)
            :children-function (lambda (node)
                                 (let* ((height (btree:node-height node))
                                        (limit  (btree:node-fill-pointer node)))
                                   (when (> height 1)
                                     (nreverse
                                      (loop for ix from 0 below limit by 2 collect
                                            (btree:node-list-cell node ix))))))
            :print-function (lambda (node)
                              (let* ((height (btree:node-height node))
                                     (limit  (btree:node-fill-pointer node)))
                                (format nil "(H:~D, N:~D)"
                                        height
                                        (if (> height 1) (truncate limit 2) limit))))
            ))
        
        ;; else
        (error "Nothing to show")
        ))))

;; ----------------------------------------------------------------
#|
(open-system)
(reset-system)
(save-system)

(show-stock "VIX")
(view-btree "VIX")

(save-stock "VIX")

(time
 (let ((stocks '("BKX" "CAC" "DAX" "DIA" "FTSE" "GLD" "HSI" "IWM" "N225"
                 "NDX" "OEF" "OEX" "OIH" "QQQQ" "RUT" "SMH" "SPX" "SPY"
                 "TLT" "VIX" "XAU" "XBD"
                 
                 "AA"  "AIG"  "AMGN" "AXP"  "BA"  "BAC" "C"   "CAT" "CMCSK"
                 "COP" "CSCO" "CVX"  "DD"   "DIS" "DOW" "GE"  "GM"  "HD"
                 "HPQ" "IBM"  "INTC" "IP"   "JNJ" "JPM" "KFT" "KO"  "LLY" "MCD"
                 "MMM" "MO"   "MRK"  "MSFT" "OXY" "PFE" "PG"  "SLB" "T"
                 "TWX" "UPS"  "UTX"  "VZ"   "WAG" "WFC" "WMT" "WY"  "XOM")))
   (print "Adding daily histories")
   (dolist (stock stocks)
     (print stock)
     (save-stock stock))
   ;; (save-system)
   ))

(time (open-system))

|#

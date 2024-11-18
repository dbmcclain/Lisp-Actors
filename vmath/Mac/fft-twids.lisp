
(in-package #:com.ral.fft)

;; --------------------------------------------------------------

(defvar *twids-lock*  (mp:make-lock))
(defvar *twids*       nil)
(defvar *twids-timer* nil)

(defun scan-twids ()
  (mp:with-lock (*twids-lock*)
    (let ((now (get-universal-time)))
      (if (um:deletef-if *twids* (lambda (twids)
                                   (when (and (zerop (twids-refct twids))
                                              (> now (twids-expir twids)))
                                     (if (eql (twids-prec twids) 'double-float)
                                         (destroy-fft-setupD (twids-psetup twids))
                                       (destroy-fft-setup (twids-psetup twids)))
                                     t)))
          (mp:schedule-timer-relative *twids-timer* 10)
        (setf *twids-timer* nil)))
    ))

(defun get-twids (nx prec)
  (let ((log2nx (um:ceiling-log2 nx)))
    (assert (<= 3 log2nx 24))
    (mp:with-lock (*twids-lock*)
      (let ((twids (find-if (lambda (twids)
                              (and (eql prec (twids-prec twids))
                                   (>= (twids-log2n twids) log2nx)))
                            *twids*)))
        (unless twids
          (setf twids (make-twids
                       :refct  0
                       :prec   prec
                       :log2n  log2nx
                       :psetup (if (eql prec 'double-float)
                                   (create-fft-setupD log2nx)
                                 (create-fft-setup log2nx))))
          (push twids *twids*))
        (setf (twids-expir twids) (+ (get-universal-time) 30))
        (sys:atomic-fixnum-incf (twids-refct twids))
        (unless *twids-timer*
          (setf *twids-timer* (mp:make-timer #'mp:funcall-async #'scan-twids))
          (mp:schedule-timer-relative *twids-timer* 10))
        twids))
    ))

(defun get-stwids (nx)
  ;; called by FFT routines
  (get-twids nx 'single-float))

(defun get-dtwids (nx)
  ;; called by FFT routines
  (get-twids nx 'double-float))

(defun do-with-twids (twids fn)
  (unwind-protect
      (funcall fn (twids-psetup twids))
    (sys:atomic-fixnum-decf (twids-refct twids))))

;; ---------------------------------------------------------------------

(defun get-process-split-tmp ()
  (mp:process-private-property 'fft-split-tmp))

(defun set-process-split-tmp (tmp)
  (setf (mp:process-private-property 'fft-split-tmp) tmp))

(defsetf get-process-split-tmp  set-process-split-tmp)

(defun get-split-temp-array (nx type)
  (let ((tmp (get-process-split-tmp)))
    (unless (and (fft-buffer-p tmp)
                 (eql type (array-element-type (fft-buffer-r tmp)))
                 (>= (array-total-size (fft-buffer-r tmp)) nx))
      (setf tmp (make-1d-fft-buffer nx type)
            (get-process-split-tmp) tmp))
    (unless (= (fft-buffer-nx tmp) nx)
      (setf (fft-buffer-nx tmp) nx
            (fft-buffer-hr tmp) (make-array (half-dim nx)
                                            :element-type type
                                            :displaced-to (fft-buffer-r tmp)
                                            :displaced-index-offset (fft-buffer-roff tmp))))
    ;; (chk-buf tmp)
    (values (fft-buffer-r tmp) (fft-buffer-roff tmp) (fft-buffer-pr tmp)
            (fft-buffer-i tmp) (fft-buffer-ioff tmp) (fft-buffer-pi tmp))
    ))

;; ---------------------------------------------------------------------
;; Per-process tmp buffer for FFT's. We never peek inside. Just need
;; an aligned split array of size equal or greater for the NX.

(defun get-process-tmp ()
  (mp:process-private-property 'fft-tmp))

(defun set-process-tmp (tmp)
  (setf (mp:process-private-property 'fft-tmp) tmp))

(defsetf get-process-tmp  set-process-tmp)

(defun get-temp-array (nx)
  (let ((tmp  (get-process-tmp)))
    (unless (and (fft-buffer-p tmp)
                 (>= (array-total-size (fft-buffer-r tmp)) nx))
      (setf tmp (make-1d-fft-buffer nx 'single-float)
            (get-process-tmp) tmp))
    ;; (chk-buf tmp)
    (values (fft-buffer-pr tmp)
            (fft-buffer-pi tmp))
    ))

(defun get-stmp (nx)
  ;; buffer is max of 16384 bytes
  (get-temp-array (min nx 4096)))

(defun get-dtmp (nx)
  (get-stmp (* 2 nx)))

;; ---------------------------------------------------------------------


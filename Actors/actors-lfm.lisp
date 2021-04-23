;; -------------------------------------------------------------------------
;; BBLFM.lisp -- Manage a logfile pool through BlueBird.
;;
;; -------------------------------------------------------------------------
;; Bluebird -- a system for easy distributed computing, going beyond what
;; is available in Erlang with the full power of Common Lisp.
;;
;; Copyright (C) 2008-2010 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; -------------------------------------------------------------------------

(in-package #:actors/lfm)

;; -------------------------------------------------------------
#|
(defmethod make-logging-handler ((stream stream) nlogfiles bg-logging)
  (declare (ignore nlogfiles))
  (stream-based-logging (effective-stream stream bg-logging)))

(defmethod make-logging-handler ((stream (eql t)) nlogfiles bg-logging)
  (declare (ignore nlogfiles bg-logging))
  (stream-based-logging (get-background-stream)))
|#
;; --------------------------------------------------------------------------
;; Behaviors

(defun log-error (service fmtstr &rest args)
  (tell-logger service :ERROR fmtstr args))

(defun log-warning (service fmtstr &rest args)
  (tell-logger service :WARNING fmtstr args))

(defun log-info (service fmtstr &rest args)
  (tell-logger service :INFO fmtstr args))

(defmethod tell-logger (service entry-kind (fmtstr string) args)
  (if (or (null args)
          (find #\~ fmtstr))
      (normalized-tell-logger service entry-kind
                              (apply 'format nil fmtstr args))
    ;; else
    (call-next-method) ))

(defmethod tell-logger (service entry-kind fmtstr args)
  (normalized-tell-logger service entry-kind
                          (with-output-to-string (s)
                            (xprint (if args
                                        (cons fmtstr args)
                                      fmtstr)
                                    s)) ))
#|
                          (format nil "~A" (if args
                                               (cons fmtstr args)
                                             fmtstr)) ))
|#

(defun normalized-tell-logger (service entry-kind msg)
  (let ((actor (find-actor service)))
    (log-message actor entry-kind msg (timestamp)
                 (or (current-actor)
                     (mp:get-current-process)))
    ))
  
;; --------------------------------------------------------------

(defun get-background-stream ()
  #+:LISPWORKS mp:*background-error-output*
  #+:ALLEGRO   excl:*initial-terminal-io*
  #+:CLOZURE   *terminal-io*)
  
(defun effective-stream (stream bg-logging)
  (let ((bg-stream (get-background-stream)))
    (if (and bg-logging
             (not (eq stream bg-stream)))
        (make-broadcast-stream stream bg-stream)
      stream)))

;; -----------------------------------------------------------------

(defun timestamp ()
  (multiple-value-bind (s m h d mo y dow dsav-p tz)
      (get-decoded-time)
    (declare (ignore dsav-p))
    (let ((mo-name (aref #.#("???" "Jan" "Feb" "Mar"
                             "Apr" "May" "Jun"
                             "Jul" "Aug" "Sep"
                             "Oct" "Nov" "Dec")
                         mo))
          (dow-name (aref #.#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                          dow)))
      (format nil "~A ~D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D (Z~@D)"
              dow-name y mo-name d
              h m s (- tz))
      )))
                       
;; -----------------------------------------------------------------
;; composite logger printing - performed by caller

(defmethod xprint ((msg list) stream &optional (level 0))
  (if (> level 1)
      (princ msg stream)
    (progn
      (princ #\( stream)
      (um:foreach (um:rcurry #'xprint stream (1+ level)) msg)
      ;; (um:foreach (um:rcurry #'describe stream) msg)
      (princ #\) stream))))

(defmethod xprint ((msg structure-object) stream &optional (level 0))
  (xprint-struct msg stream level))

(defmethod xprint ((msg standard-object) stream &optional (level 0))
  (xprint-struct msg stream level))

(defun xprint-struct (msg stream level)
  (multiple-value-bind (slots vals ?1 ?2 name) (lw:get-inspector-values msg nil)
    (declare (ignore ?1 ?2))
    (format stream "~&#S(~A~{~%    :~A~t~A~})~%"
            name (mapcan #'list slots
                         (if (>= level 0)
                             vals
                           (mapcar (lambda (val)
                                     (with-output-to-string (s)
                                       (xprint val s (1+ level))))
                                   vals))))
    ))

(defmethod xprint (msg stream &optional (level 0))
  (princ msg stream)
  (princ #\space stream))

;; -----------------------------------------------------------------
;; Server behavior

(defun default-printer (f entry-kind msg timestamp from-pid)
  (let* ((hdr (case entry-kind
                (:ERROR     "ERROR REPORT")
                (:WARNING   "WARNING REPORT")
                (:INFO      "INFORMATION REPORT")
                (:STARTUP   "STARTUP REPORT")
                (:SHUTDOWN  "SHUTDOWN REPORT")
                (otherwise  "??? UNKNOWN REPORT")))
         (*print-circle* t))
    (progn ;; stream:with-stream-output-lock f
      (format f "~&=~A==== ~A ===~%" hdr timestamp)
      (format f "Node: ~A~%" (machine-instance))
      (format f "From: ~A~%" from-pid)
      (format f "~A~%~%" msg)
      ;; (xprint msg f)
      ;; (describe msg f)
      ;; (terpri f)
      ;; (terpri f)
      ) ))
  
;; -----------------------------------------------------------------
#|
(defstruct state
  printer
  stream
  needs-close)

;; --------------------------------------------------------------------------

(defun stream-based-logging (stream)
  (make-stream-logging-handler (make-state
                                :printer  'default-printer
                                :stream   stream)))

(defun sign-on (state)
  (with-accessors ((printer state-printer)
                   (stream  state-stream)) state
    (stream:with-stream-output-lock stream
      (funcall printer stream :STARTUP "Logging Service Startup"
               (timestamp)
               (..int:self))
      state)))

(defun sign-off (state)
  (with-accessors ((printer state-printer)
                   (stream  state-stream)) state
    
    (stream:with-stream-output-lock stream
      (funcall printer stream :SHUTDOWN
               "Logging Service Terminating Normally"
               (timestamp)
               (..int:self))
      )))

(defun terminate-stream (state)
  (with-accessors ((stream      state-stream)
                   (needs-close state-needs-close)) state
    (sign-off state)
    (when needs-close
      (close stream))))

(defun make-stream-logging-handler (state)
  (with-accessors ((printer     state-printer)
                   (stream      state-stream)
                   (needs-close state-needs-close)) state
    
    (ac:make-actor
     (um:dlambda*
       (:sign-on ()
        (sign-on state))
        
       (:log-message (entry-kind msg timestamp from-pid)
        (stream:with-stream-output-lock stream
          (funcall printer stream entry-kind msg timestamp from-pid)))
       
       (:set-printer (new-printer)
        (setf printer new-printer))
       
       (:set-stream (new-stream &optional new-needs-close)
        (terminate-stream state)
        (setf stream      new-stream
              needs-close new-needs-close)
        (sign-on state))
       
       (:get-state ()
        state)
       
       (:get-printer ()
        printer)
       
       (:get-stream ()
        stream)
       
       (:shutdown ()
        (terminate-stream state))
       
       (t (&rest args)
          (error "Unknown message type: ~S" args))
       ))))
|#

;; =========================================================================
;; FILE BASED LOGGING
;; --------------------------------------------------------------------------
;; Deciding which file of a pool to use or discard

(defun get-oldest-and-newest (wild-filename)
  ;; Using a wild pathname, find all the matching items in the
  ;; specified directory and of the specified file type.
  ;; Return 3 values:
  ;;     n = how many matching files there were
  ;;     newset = filename of most recently written file
  ;;     oldest = filename of oldest file based on file write date.
  (let* ((dir (directory wild-filename))
         (pairs (sort (mapcar (lambda (d)
                                (list d (file-write-date d)))
                              dir)
                      '<
                      :key 'second)))
    
    (values (length pairs)
            (caar pairs)
            (caar (last pairs)))
    ))

(defun todays-filename (filename-base)
  ;; Get today's expected filename = basename of file proper
  ;; plus -YYMMDD for today. Tack back together with directory
  ;; and file type.
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    
    (make-pathname
     :directory (pathname-directory filename-base)
     :name      (format nil "~A-~{~2,'0D~}"
                        (pathname-name filename-base)
                        (list (rem y 100) mo d))
     :type      (pathname-type filename-base))
    ))

(defun get-wild-filename (filename-base)
  ;; take a pathname and make it into a wild pathname by
  ;; appending the name proper with '*', and then
  ;; tacking back together with the directory and file type.
  (make-pathname
   :directory (pathname-directory filename-base)
   :name      (um:mkstr (pathname-name filename-base) #\*)
   :type      (pathname-type filename-base)))

(defun file-written-today? (filename)
  ;; file should already have been determined to exist
  (assert (probe-file filename))
  
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (multiple-value-bind (sf mf hf df mof yf)
        (decode-universal-time (file-write-date filename))
      (declare (ignore sf mf hf))
      
      (and (= d df)
           (= mo mof)
           (= y yf))
    )))
  
(defun decide-which-output-logfile (filename-base &optional (nmax 5))
  ;; Given a base filename and a maximum desired number of rotating log files,
  ;; return values => list, filename
  ;; where:
  ;;    list contains the filename to be used for today's logfile
  ;;         and parameters regarding the OPEN if for :OUTPUT or :IO.
  ;;
  ;;    filename of the oldest file and which
  ;;         also needs to be removed after writing today's file. This result
  ;;         could be NIL if no such file.
  ;;
  (let ((wanted-filename (todays-filename filename-base)))
    
    ;; does the file for today already exist?
    (if (probe-file wanted-filename)
        (values (list wanted-filename
                      :if-exists :append))

      ;; take a look at what's already there...
      (multiple-value-bind (n oldest newest)
          (get-oldest-and-newest (get-wild-filename filename-base))

        ;; else, is there a newest file?
        (if newest

            ;; yes there is a newest file.
            (if (file-written-today? newest)
                
                ;; yes - newest is for today, so use it, despite the difference in naming
                (values (list newest
                              :if-exists :append)
                        (and (> n nmax) ;; delete oldest if already more than we want
                             (> nmax 1)
                             oldest))
                  
              ;; newest is not today's, so create another for today
              (progn
                ;; create and then close the file - to avoid removal on abort
                (with-open-file (f wanted-filename
                                   :direction :output
                                   :if-does-not-exist :create)
                  (declare (ignore f)))
                
                (values (list wanted-filename
                              :if-exists :append)
                      (and (>= n nmax) ;; delete oldest if we will have enough backup
                           (> nmax 1)
                           oldest))))

          ;; else, no newest file, we have to create from scratch
          (progn
            ;; create and then close the file - to avoid removal on abort
            (with-open-file (f wanted-filename
                               :direction :output
                               :if-does-not-exist :create)
              (declare (ignore f)))
            (values (list wanted-filename
                          :if-exists :append)))
          ))
      )))

(defun open-logfile-for-output (output-filename open-args)
  ;; open args are as from decide-which-output-logfile above
  (apply 'open output-filename :direction :output open-args))

;; -----------------------------------------------------------------
#|
(defmethod make-logging-handler ((filename-base string) nlogfiles bg-logging)
  (file-based-logging filename-base nlogfiles bg-logging))

(defmethod make-logging-handler ((filename-base pathname) nlogfiles bg-logging)
  (file-based-logging filename-base nlogfiles bg-logging))

(defun file-based-logging (filename-base nlogfiles bg-logging)
  (um:bind*
      ((:values (new-file-info oldest-file) (decide-which-output-logfile filename-base nlogfiles))
       ((output-filename &rest open-args) new-file-info)
       (stream  (effective-stream
                 (open-logfile-for-output output-filename open-args)
                 bg-logging)))
    
    (when oldest-file
      (delete-file oldest-file))
    (make-stream-logging-handler (make-state
                                  :printer     'default-printer
                                  :stream      stream
                                  :needs-close t)) ))
|#

;; -----------------------------------------------------------------

;; -----------------------------------------------------------------

#|
;; generate some logfiles...
(with-open-file (f "/tmp/logfile-6.txt"
                   :if-does-not-exist :create
                   :if-exists         :supersede
                   :direction         :output)
  (let ((fb (make-broadcast-stream f (get-background-stream))))
    (format f "~&Howdy!")))


(..lfm:start :logger
	      #+:LISPWORKS mp:*background-error-output*
	      #+:ALLEGRO   excl:*initial-terminal-io*
	      #+:CLOZURE   *terminal-io*)
(..lfm:log-error   :logger "Uh oh...")
(..lfm:log-warning :logger "Look out! Will Rogers...")
(..lfm:log-info    :logger "Thought you might want to know..")
(..lfm:set-logging-handler :logger (lambda ()
                                      nil)) ;; induce an intentional error
(..lfm:log-warning :logger "Look out! Will Rogers...")
(! :logger '(What is this thing?))
(..lfm:shutdown :logger))
|#

;; --------------------------------------------------------------------------
;; User API

(define-actor-class lfm ()
  ((log-file    :accessor lfm-log-file    :initarg :log-file)
   (nlogfiles   :accessor lfm-nlogfiles   :initarg :nlogfiles)
   (bg-logging  :accessor lfm-bg-logging  :initarg :bg-logging)
   (printer     :accessor lfm-printer     :initarg :printer)
   (stream      :accessor lfm-stream      :initarg :stream)
   (needs-close :accessor lfm-needs-close :initarg :needs-close))
  (:default-initargs
   :log-file    t
   :nlogfiles   5
   :bg-logging  t
   :printer     'default-printer
   :stream      (effective-stream (get-background-stream) t)
   :needs-close nil))

(defvar *syslog*  nil)

(defun lfm ()
  *syslog*)

(defun ensure-system-logger ()
  (unless *syslog*
    (setf *syslog* (make-instance 'lfm))
    (register-actor :SYSTEM-LOG *syslog*)))

(defun kill-system-logger ()
  (unregister-actor :SYSTEM-LOG)
  (setf *syslog* nil))

(defmethod set-printer ((actor lfm) new-printer)
  ;; new-handler should be a function of 4 args:
  ;;  output-stream, entry-type, format-string, optional args
  ;;  args will always be presented as a list.
  (with-slots (printer) actor
    (perform-in-actor actor
      (setf printer new-printer)
      )))

(defmethod set-stream ((actor lfm) new-stream &optional new-needs-close)
  (with-slots (stream needs-close) actor
    (perform-in-actor actor
      (when needs-close
        (close stream))
      (setf stream new-stream
            needs-close new-needs-close)
      )))

(defmethod log-message ((actor lfm) entry-kind msg timestamp who-from)
  (with-slots (printer stream) actor
    (perform-in-actor actor
      (stream:with-stream-output-lock stream
        (princ (with-output-to-string (s)
                 (funcall printer s entry-kind msg timestamp who-from))
               stream)
        (force-output stream))
      )))


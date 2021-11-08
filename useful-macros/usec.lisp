;; usec.lisp -- timestamps to the nearest microsecond, when supported
;; DM/RAL  08/09
;; ----------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defpackage #:usec
  (:use #:common-lisp)
  (:export
   #:get-time-usec
   #:adjust-to-standard-universal-time-usec
   #:get-universal-time-usec
   #:_getTickCount
   ))

;; ----------------------------------------------------------------
(in-package :usec)
;; ----------------------------------------------------------------
;; ----------------------------------------------------------------
;; Timestamps to the nearest microsecond

;;--- MAC OS/X ---

#-:WIN32
(defun adjust-to-standard-universal-time-usec (tm)
  (declare (integer tm))
  (+ tm #.(* 1000000 (encode-universal-time 0 0 0 1 1 1970 0))))


#+(AND :LISPWORKS (OR :LINUX :MACOSX))
(PROGN
 (fli:define-foreign-function (_get-time-of-day "gettimeofday" :source)
    ((tsinfo :pointer)
     (tzinfo :pointer))
   :result-type :long)

 (defun get-time-usec ()
   ;; time since midnight Jan 1, 1970, measured in microseconds
   (fli:with-dynamic-foreign-objects ()
	(um:bind*
	 ((arr (fli:allocate-dynamic-foreign-object
		:type   '(:unsigned :long)
		:nelems 2
		:fill   0)))
	 (if (zerop (_get-time-of-day arr fli:*null-pointer*))
             (+ (* 1000000 (the integer (fli:dereference arr :index 0)))
                (the integer (fli:dereference arr :index 1)))
           (error "Can't perform Posix gettimeofday()"))
	 ))))

#-(OR (AND :LISPWORKS (OR :LINUX :MACOSX))
      :WIN32
      :ALLEGRO)
(defun adjust-to-standard-universal-time-usec (tm)
   (declare (integer tm))
   (+ tm #.(* 1000000 (encode-universal-time 0 0 0 1 1 1970 0))))

#+:CLOZURE
(defun get-time-usec ()
  ;; time since midnight Jan 1, 1970, measured in microseconds
  (declare (optimize (speed 3) (debug 0)))
  (ccl::rlet ((now :timeval))
    (ccl::gettimeofday now)
    (+ (* 1000000 (the (unsigned-byte 32) (ccl:pref now :timeval.tv_sec)))
       (the fixnum (ccl:pref now :timeval.tv_usec)))))

#-(OR :CLOZURE
      (AND :LISPWORKS (OR :LINUX :MACOSX))
      :WIN32
      :ALLEGRO)
(defun get-time-usec ()
  (error "Not yet implemented"))

(defun get-universal-time-usec ()
  (adjust-to-standard-universal-time-usec (get-time-usec)))

;; ------- WIN/32 -----------------
#|
#+(AND :LISPWORKS :WIN32)
(fli:define-foreign-function (_getSystemTime "GetSystemTime" :source)
    ((lpSystemTime :pointer)))

#+(AND :LISPWORKS :WIN32)
(defun get-system-time ()
  ;; only provides information to 1ms resolution
  (fli:with-dynamic-foreign-objects ()
    (let ((buf (fli:allocate-dynamic-foreign-object
                :type :uint16 :nelems 8)))
      (_getSystemTime buf)
      (loop for ix from 0 below 8 collect
            (fli:dereference buf :index ix)))))
|#

#|
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *ut-delta*
  (locally
    #f
    (let ((utc (get-universal-time))
          (now (get-internal-real-time)))
      (declare (integer utc now))
      ;; only accurate to nearest second
      (- (* internal-time-units-per-second utc) now))))

#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defun adjust-to-standard-universal-time-usec (tm)
  (declare (integer tm))
  #F
  tm)

#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *last-time* 0)
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *time-incr* 0)
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defun get-time-usec ()
  (let ((this-time (get-internal-real-time)))
    (if (= this-time *last-time*)
        (incf *time-incr*)
      (setf *time-incr* 0
            *last-time* this-time))
    (+ *time-incr*
       (/ (* 1000000 (+ this-time *ut-delta*))
          internal-time-units-per-second))
    ))
|#

#+(OR :WIN32 :ALLEGRO)
(progn
  #-:ALLEGRO
  (fli:define-foreign-function (_getTickCount "GetTickCount" :source)
      ()
    :result-type :int
    :calling-convention :stdcall)

  (fli:define-foreign-function (_queryPerformanceCounter "QueryPerformanceCounter" :source)
      ((ans :pointer))
    :result-type :int
    :calling-convention :stdcall)

  (defun queryPerformanceCounter ()
    (fli:with-dynamic-foreign-objects ()
      (let ((ctr  (fli:allocate-dynamic-foreign-object
                   :type   :long-long
                   :nelems 1
                   :fill   0)))
        (_queryPerformanceCounter ctr)
        (fli:dereference ctr))))
  
  
  (fli:define-foreign-function (_queryPerformanceFrequency "QueryPerformanceFrequency" :source)
      ((ans :pointer))
    :result-type :int
    :calling-convention :stdcall)

  (defun queryPerformanceFrequency ()
    (fli:with-dynamic-foreign-objects ()
      (let ((ctr  (fli:allocate-dynamic-foreign-object
                   :type   :long-long
                   :nelems 1
                   :fill   0)))
        (_queryPerformanceFrequency ctr)
        (fli:dereference ctr))))
  
  
  (fli:define-foreign-function (_GetSystemTimeAsFileTime "GetSystemTimeAsFileTime" :source)
      ((ans :pointer))
    :result-type :int
    :calling-convention :stdcall)

  (defun getSystemTimeAsFileTime ()
    ;; returns time in 100ns units - but seems only good to 1ms
    (fli:with-dynamic-foreign-objects ()
      (let ((ctr  (fli:allocate-dynamic-foreign-object
                   :type   '(:unsigned :long)
                   :nelems 2
                   :fill   0)))
        (_GetSystemTimeAsFileTime ctr)
        (logior (ash (fli:dereference ctr :index 1) 32)
                (fli:dereference ctr :index 0))
        )))
  

  #|
  
  (defvar *last-time* 0)
  (defvar *time-incr* 0)

  (defun get-time-usec ()
    (let ((now (get-universal-time)))
      (if (= now *last-time*)
	  (incf *time-incr*)
	(setf *time-incr* 0
	      *last-time* now))
      (+ (* now 1000 1000) *time-incr*)))
  
  (defun adjust-to-standard-universal-time-usec (tm)
    tm)
  |#
  #|
  (defun get-time-usec ()
    (round (getSystemTimeAsFileTime) 10))

  (defun adjust-to-standard-universal-time-usec (tm)
    (declare (integer tm))
    (- tm #.(* 1000000 9435484800)))
  |#

  (defvar *rate* nil)
  (defvar *utc-offs* nil)
  
  (defun get-time-usec ()
    (let ((rate (or *rate*
                    (setf *rate* (queryPerformanceFrequency)))))
      (round (* (queryPerformanceCounter) 1000000) rate)))

  (defun adjust-to-standard-universal-time-usec (tm)
    (declare (integer tm))
    (let ((offs (or *utc-offs*
                    (setf *utc-offs* (- (* 1000000 (get-universal-time))
                                        (get-time-usec))))))
      (+ tm offs)))
  )

#|
(- (truncate (get-time-usec) 1000000) (get-universal-time))
|#

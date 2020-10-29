;; exec.lisp -- a collection of routines to support synchronous spawning of
;; external processes...
;;
;; DM/MCFA  07/01
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

(defpackage "EXEC"
  (:use "COMMON-LISP")
  (:export
   "EXEC"
   "EXEC-AND-WAIT-PROCESS"
   "SYSTEM"))

(in-package "EXEC")

;; ShowWindow() Commands
(um:eval-always
  (um:def-enum
   SW_HIDE
   SW_SHOWNORMAL
   (SW_NORMAL       SW_SHOWNORMAL)
   SW_SHOWMINIMIZED
   SW_SHOWMAXIMIZED
   (SW_MAXIMIZE     SW_SHOWMAXIMIZED)
   SW_SHOWNOACTIVATE
   SW_SHOW
   SW_MINIMIZE
   SW_SHOWMINNOACTIVE
   SW_SHOWNA
   SW_RESTORE
   SW_SHOWDEFAULT
   (SW_MAX          SW_SHOWDEFAULT))
  )
  

(um:ez-define-foreign-function (%winexec "WinExec")
   ((app-name      ct:out-cstring) 
    (startup-state :int))
   ;; :encoding :object
   :result-type :int
   :calling-convention :stdcall
   :module "Kernel32.dll")

(defun winexec (cmd &optional (window-state SW_SHOWNORMAL))
  (if (<= (%winexec cmd window-state) 31)
      (error "Can't start Windows app: ~A" cmd)))

(fli:define-c-struct startup-info
  (cb              :int)
  (lpReserved      :int) ;; must be zero
  (lpDesktop       :int) ;; make it zero
  (lpTitle         :int) ;; make it zero
  (dwX             :int)
  (dwY             :int)
  (dwXSize         :int)
  (dwYSize         :int)
  (dwXCountChars   :int)
  (dwYCountChars   :int)
  (dwFillAttribute :int)
  (dwFlags         :int)
  (wShowWindow     :int)
  (cbReserved2     :int) ;; must be zero
  (lpReserved2     :int) ;; must be zero
  (hStdInput       :int)
  (hStdOutput      :int)
  (hStdError       :int)
  )

;;
;; dwFlags values
;;

(defconstant STARTF_USESHOWWINDOW    #x00000001)
(defconstant STARTF_USESIZE          #x00000002)
(defconstant STARTF_USEPOSITION      #x00000004)
(defconstant STARTF_USECOUNTCHARS    #x00000008)
(defconstant STARTF_USEFILLATTRIBUTE #x00000010)
(defconstant STARTF_RUNFULLSCREEN    #x00000020)
(defconstant STARTF_FORCEONFEEDBACK  #x00000040)
(defconstant STARTF_FORCEOFFFEEDBACK #x00000080)
(defconstant STARTF_USESTDHANDLES    #x00000100)
(defconstant STARTF_USEHOTKEY        #x00000200)

(fli:define-c-struct proc-info
  (hProcess   :int)
  (hThread    :int)
  (dwProcID   :int)
  (dwThreadID :int)
  )

;;
;; startup flags
;;
(defconstant DEBUG_PROCESS               #x00000001)
(defconstant DEBUG_ONLY_THIS_PROCESS     #x00000002)

(defconstant CREATE_SUSPENDED            #x00000004)

(defconstant DETACHED_PROCESS            #x00000008)

(defconstant CREATE_NEW_CONSOLE          #x00000010)

(defconstant NORMAL_PRIORITY_CLASS       #x00000020)
(defconstant IDLE_PRIORITY_CLASS         #x00000040)
(defconstant HIGH_PRIORITY_CLASS         #x00000080)
(defconstant REALTIME_PRIORITY_CLASS     #x00000100)

(defconstant CREATE_NEW_PROCESS_GROUP    #x00000200)
(defconstant CREATE_UNICODE_ENVIRONMENT  #x00000400)

(defconstant CREATE_SEPARATE_WOW_VDM     #x00000800)
(defconstant CREATE_SHARED_WOW_VDM       #x00001000)
(defconstant CREATE_FORCEDOS             #x00002000)

(defconstant CREATE_DEFAULT_ERROR_MODE   #x04000000)
(defconstant CREATE_NO_WINDOW            #x08000000)

(defconstant PROFILE_USER                #x10000000)
(defconstant PROFILE_KERNEL              #x20000000)
(defconstant PROFILE_SERVER              #x40000000)

(um:ez-define-foreign-function (%createprocess "CreateProcessA")
   ((appname    ct:out-cstring) ;; will pick it up from cmdline if null
    (cmdline    ct:out-cstring)
    (:constant  0 :int)  ;; proc-tr
    (:constant  0 :int)  ;; tread-attr
    (:constant  0 :int)  ;; no inherit handles...
    (:constant  CREATE_NEW_CONSOLE :int) ;; creation flags
    (:constant  0 :int) ;; inherit our environ
    (currdir    ct:out-cstring) ;; will use our dir if null
    (srtinfo    (:pointer startup-info))
    (procinf    (:pointer proc-info))
    )
   :result-type :int
   :calling-convention :stdcall
   :module "Kernel32.dll")

(defun make-startup-info ()
  ;; must be called inside a "with-dynamic-foreign-objects" clause.
  ;; caller must coerce to a startup-info pointer
  (let* ((info (fli:allocate-dynamic-foreign-object
                :type :int
                :nelems (/ (fli:size-of 'startup-info) (fli:size-of :int))
                :initial-element 0)))
    (fli:with-coerced-pointer (p :type 'startup-info) info
      (setf (fli:foreign-slot-value p 'cb) (fli:size-of 'startup-info)
            (fli:foreign-slot-value p 'dwFlags) STARTF_USESHOWWINDOW
            (fli:foreign-slot-value p 'wShowWindow) SW_SHOWNORMAL
            ))
    info))

(defun exec (cmd &key appname directory)
  "Execute a command line as another process. Returns immediately
with a keylist of Win32 process information."
  (fli:with-dynamic-foreign-objects ()
    (let* ((start-info (make-startup-info))
           (proc-info  (fli:allocate-dynamic-foreign-object
                        :type 'proc-info)))
      (fli:with-coerced-pointer (p :type 'startup-info) start-info
        (if (zerop (%createprocess appname cmd directory p proc-info))
            (error "~%Can't exec command: ~A" cmd)
          (list :hProcess   (fli:foreign-slot-value proc-info 'hProcess)
                :hThread    (fli:foreign-slot-value proc-info 'hThread)
                :dwProcID   (fli:foreign-slot-value proc-info 'dwProcID)
                :dwThreadID (fli:foreign-slot-value proc-info 'dwThreadID))
          ))
      )))

(um:eval-always
(defconstant SYNCHRONIZE               #x00100000)
(defconstant STANDARD_RIGHTS_REQUIRED  #x000F0000)
(defconstant PROCESS_QUERY_INFORMATION #x0400)
)

(fli:define-foreign-function (%openProcess "OpenProcess")
    ((:constant   #.(+ PROCESS_QUERY_INFORMATION SYNCHRONIZE) :int)
     (:constant   0 :int)
     (dwProcessID :int))
  :result-type :int
  :calling-convention :stdcall
  :module "Kernel32.dll")

(fli:define-foreign-function (%closeHandle "CloseHandle")
    ((handle :int))
  :result-type :int
  :calling-convention :stdcall
  :module "Kernel32.dll")

(fli:define-foreign-function (%getExitCodeProcess "GetExitCodeProcess")
    ((hProcess   :int)
     (lpExitCode (:pointer :int)))
  :result-type :int
  :calling-convention :stdcall
  :module "Kernel32.dll")

(defun getExitCodeProcess (hProcess)
  (fli:with-dynamic-foreign-objects ()
    (let ((code (fli:allocate-dynamic-foreign-object
                 :type :int
                 :nelems 1
                 :initial-element 0)))
      (%getExitCodeProcess hProcess code)
      (fli:dereference code))))

(fli:define-foreign-function (%waitForSingleObject "WaitForSingleObject")
    ((handle :int)
     (dwMilliseconds :int))
  :result-type :int
  :calling-convention :stdcall
  :module "Kernel32.dll")

(defconstant INFINITE  -1)

(defun waitForSingleObject (handle &optional (timeoutMilliseconds INFINITE))
  (%waitForSingleObject handle timeoutMilliseconds))

(defun exec-and-wait-process (cmd &rest args)
  "Call EXEC on the command line and wait for the process to terminate.
Returns the process exit code."
  (let* ((info (apply 'exec cmd args))
         (handle (%openProcess (getf info :dwProcID))))
    (unwind-protect
        (progn
          (waitForSingleObject handle)
          (getExitCodeProcess handle))
      (%closeHandle handle))))

;; ------------------------------------------------
(defconstant **lisp-com-lib** "lispcom.dll")

(um:ez-define-foreign-function (system "_LispSystemCall@4")
   ((cmd  ct:out-cstring))
   :result-type :int
   :calling-convention :stdcall
   :module **lisp-com-lib**)

;; -- end of exec.lisp -- ;;

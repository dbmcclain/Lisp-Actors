#| DATE           : 26 Sep 2023 
 | USER           : davidmcclain 
 | PROCESSED FILE : /Users/davidmcclain/projects/Lispworks/xTActors/zmq-stuff/zmq.h
 |#

;; zmq-intf.lisp
;;
;; DM/RAL  2023/09/26 11:26:37
;; ----------------------------------

(defpackage #:zmq-intf
  (:use #:common-lisp)
  (:export
   #:+ZMQ-REQ+
   #:+ZMQ-REP+
   #:zmq-ctx-new
   #:zmq-ctx-destroy
   #:zmq-socket
   #:zmq-connect
   #:zmq-send
   #:zmq-str-send
   #:zmq-recv
   #:zmq-close
   #:zmq-bind
   ))

(in-package #:zmq-intf)

(fli:register-module :libzmq
                     :dlopen-flags t ;; non-nil needed for Mac to unload dylib on disconnect-module
                     :file-name    #P"/usr/local/lib/libzmq.dylib")
#|
(fli:disconnect-module :libzmq :remove t)
 |#
;; ----------------------------------

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_errno_t.h"

(fli:define-c-typedef (errno-t (:foreign-name "errno_t")) :int)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/errno.h"

(fli:define-foreign-function (--error "__error" :source)
                             nil
                             :result-type
                             (:pointer :int)
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0/include/stddef.h"

(fli:define-c-typedef (ptrdiff-t (:foreign-name "ptrdiff_t")) :long)
(fli:define-c-typedef (size-t (:foreign-name "size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (rsize-t (:foreign-name "rsize_t"))
                      (:unsigned :long))
(fli:define-c-typedef (wchar-t (:foreign-name "wchar_t")) :int)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0/include/__stddef_max_align_t.h"

(fli:define-c-typedef (max-align-t (:foreign-name "max_align_t"))
                      :long-double)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/_types.h"

(fli:define-c-typedef (--int8-t (:foreign-name "__int8_t"))
                      (:signed :char))
(fli:define-c-typedef (--uint8-t (:foreign-name "__uint8_t"))
                      (:unsigned :char))
(fli:define-c-typedef (--int16-t (:foreign-name "__int16_t")) :short)
(fli:define-c-typedef (--uint16-t (:foreign-name "__uint16_t"))
                      (:unsigned :short))
(fli:define-c-typedef (--int32-t (:foreign-name "__int32_t")) :int)
(fli:define-c-typedef (--uint32-t (:foreign-name "__uint32_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--int64-t (:foreign-name "__int64_t"))
                      :long-long)
(fli:define-c-typedef (--uint64-t (:foreign-name "__uint64_t"))
                      (:unsigned :long-long))
(fli:define-c-typedef (--darwin-intptr-t
                       (:foreign-name "__darwin_intptr_t"))
                      :long)
(fli:define-c-typedef (--darwin-natural-t
                       (:foreign-name "__darwin_natural_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-ct-rune-t
                       (:foreign-name "__darwin_ct_rune_t"))
                      :int)
(fli:define-c-typedef (--mbstate-t (:foreign-name "__mbstate_t"))
                      (:union
                       (--mbstate8 (:c-array :char 128))
                       (-mbstatel :long-long)))
(fli:define-c-typedef (--darwin-mbstate-t
                       (:foreign-name "__darwin_mbstate_t"))
                      --mbstate-t)
(fli:define-c-typedef (--darwin-ptrdiff-t
                       (:foreign-name "__darwin_ptrdiff_t"))
                      :long)
(fli:define-c-typedef (--darwin-size-t
                       (:foreign-name "__darwin_size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-va-list
                       (:foreign-name "__darwin_va_list"))
                      (:pointer :void))
(fli:define-c-typedef (--darwin-wchar-t
                       (:foreign-name "__darwin_wchar_t"))
                      :int)
(fli:define-c-typedef (--darwin-rune-t
                       (:foreign-name "__darwin_rune_t"))
                      --darwin-wchar-t)
(fli:define-c-typedef (--darwin-wint-t
                       (:foreign-name "__darwin_wint_t"))
                      :int)
(fli:define-c-typedef (--darwin-clock-t
                       (:foreign-name "__darwin_clock_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-socklen-t
                       (:foreign-name "__darwin_socklen_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-ssize-t
                       (:foreign-name "__darwin_ssize_t"))
                      :long)
(fli:define-c-typedef (--darwin-time-t
                       (:foreign-name "__darwin_time_t"))
                      :long)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types.h"
#|
(fli:define-c-typedef (--darwin-blkcnt-t
                       (:foreign-name "__darwin_blkcnt_t"))
                      --int64-t)
(fli:define-c-typedef (--darwin-blksize-t
                       (:foreign-name "__darwin_blksize_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-dev-t (:foreign-name "__darwin_dev_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-fsblkcnt-t
                       (:foreign-name "__darwin_fsblkcnt_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-fsfilcnt-t
                       (:foreign-name "__darwin_fsfilcnt_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-gid-t (:foreign-name "__darwin_gid_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-id-t (:foreign-name "__darwin_id_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-ino64-t
                       (:foreign-name "__darwin_ino64_t"))
                      --uint64-t)
(fli:define-c-typedef (--darwin-ino-t (:foreign-name "__darwin_ino_t"))
                      --darwin-ino64-t)
(fli:define-c-typedef (--darwin-mach-port-name-t
                       (:foreign-name "__darwin_mach_port_name_t"))
                      --darwin-natural-t)
(fli:define-c-typedef (--darwin-mach-port-t
                       (:foreign-name "__darwin_mach_port_t"))
                      --darwin-mach-port-name-t)
(fli:define-c-typedef (--darwin-mode-t
                       (:foreign-name "__darwin_mode_t"))
                      --uint16-t)
(fli:define-c-typedef (--darwin-off-t (:foreign-name "__darwin_off_t"))
                      --int64-t)
(fli:define-c-typedef (--darwin-pid-t (:foreign-name "__darwin_pid_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-sigset-t
                       (:foreign-name "__darwin_sigset_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-suseconds-t
                       (:foreign-name "__darwin_suseconds_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-uid-t (:foreign-name "__darwin_uid_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-useconds-t
                       (:foreign-name "__darwin_useconds_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-uuid-t
                       (:foreign-name "__darwin_uuid_t"))
                      (:c-array (:unsigned :char) 16))
(fli:define-c-typedef (--darwin-uuid-string-t
                       (:foreign-name "__darwin_uuid_string_t"))
                      (:c-array :char 37))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_types.h"

(fli:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec")
                      (:forward-reference-p t)))
(fli:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec"))
                     (--routine
                      (:pointer (:function ((:pointer :void)) :void)))
                     (--arg (:pointer :void))
                     (--next
                      (:pointer
                       (:struct --darwin-pthread-handler-rec))))
(fli:define-c-struct (-opaque-pthread-attr-t
                      (:foreign-name "_opaque_pthread_attr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(fli:define-c-struct (-opaque-pthread-cond-t
                      (:foreign-name "_opaque_pthread_cond_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 40)))
(fli:define-c-struct (-opaque-pthread-condattr-t
                      (:foreign-name "_opaque_pthread_condattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-mutex-t
                      (:foreign-name "_opaque_pthread_mutex_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(fli:define-c-struct (-opaque-pthread-mutexattr-t
                      (:foreign-name "_opaque_pthread_mutexattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-once-t
                      (:foreign-name "_opaque_pthread_once_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-rwlock-t
                      (:foreign-name "_opaque_pthread_rwlock_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 192)))
(fli:define-c-struct (-opaque-pthread-rwlockattr-t
                      (:foreign-name "_opaque_pthread_rwlockattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 16)))
(fli:define-c-struct (-opaque-pthread-t
                      (:foreign-name "_opaque_pthread_t"))
                     (--sig :long)
                     (--cleanup-stack
                      (:pointer
                       (:struct --darwin-pthread-handler-rec)))
                     (--opaque (:c-array :char 8176)))
(fli:define-c-typedef (--darwin-pthread-attr-t
                       (:foreign-name "__darwin_pthread_attr_t"))
                      (:struct -opaque-pthread-attr-t))
(fli:define-c-typedef (--darwin-pthread-cond-t
                       (:foreign-name "__darwin_pthread_cond_t"))
                      (:struct -opaque-pthread-cond-t))
(fli:define-c-typedef (--darwin-pthread-condattr-t
                       (:foreign-name "__darwin_pthread_condattr_t"))
                      (:struct -opaque-pthread-condattr-t))
(fli:define-c-typedef (--darwin-pthread-key-t
                       (:foreign-name "__darwin_pthread_key_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-pthread-mutex-t
                       (:foreign-name "__darwin_pthread_mutex_t"))
                      (:struct -opaque-pthread-mutex-t))
(fli:define-c-typedef (--darwin-pthread-mutexattr-t
                       (:foreign-name "__darwin_pthread_mutexattr_t"))
                      (:struct -opaque-pthread-mutexattr-t))
(fli:define-c-typedef (--darwin-pthread-once-t
                       (:foreign-name "__darwin_pthread_once_t"))
                      (:struct -opaque-pthread-once-t))
(fli:define-c-typedef (--darwin-pthread-rwlock-t
                       (:foreign-name "__darwin_pthread_rwlock_t"))
                      (:struct -opaque-pthread-rwlock-t))
(fli:define-c-typedef (--darwin-pthread-rwlockattr-t
                       (:foreign-name "__darwin_pthread_rwlockattr_t"))
                      (:struct -opaque-pthread-rwlockattr-t))
(fli:define-c-typedef (--darwin-pthread-t
                       (:foreign-name "__darwin_pthread_t"))
                      (:pointer (:struct -opaque-pthread-t)))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types.h"

(fli:define-c-typedef (--darwin-nl-item
                       (:foreign-name "__darwin_nl_item"))
                      :int)
(fli:define-c-typedef (--darwin-wctrans-t
                       (:foreign-name "__darwin_wctrans_t"))
                      :int)
(fli:define-c-typedef (--darwin-wctype-t
                       (:foreign-name "__darwin_wctype_t"))
                      --uint32-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int8_t.h"

(fli:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int16_t.h"

(fli:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int32_t.h"

(fli:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int64_t.h"

(fli:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int8_t.h"

(fli:define-c-typedef (u-int8-t (:foreign-name "u_int8_t"))
                      (:unsigned :char))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int16_t.h"

(fli:define-c-typedef (u-int16-t (:foreign-name "u_int16_t"))
                      (:unsigned :short))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int32_t.h"

(fli:define-c-typedef (u-int32-t (:foreign-name "u_int32_t"))
                      (:unsigned :int))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int64_t.h"

(fli:define-c-typedef (u-int64-t (:foreign-name "u_int64_t"))
                      (:unsigned :long-long))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(fli:define-c-typedef (register-t (:foreign-name "register_t")) int64-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_intptr_t.h"

(fli:define-c-typedef (intptr-t (:foreign-name "intptr_t"))
                      --darwin-intptr-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_uintptr_t.h"

(fli:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(fli:define-c-typedef (user-addr-t (:foreign-name "user_addr_t"))
                      u-int64-t)
(fli:define-c-typedef (user-size-t (:foreign-name "user_size_t"))
                      u-int64-t)
(fli:define-c-typedef (user-ssize-t (:foreign-name "user_ssize_t"))
                      int64-t)
(fli:define-c-typedef (user-long-t (:foreign-name "user_long_t"))
                      int64-t)
(fli:define-c-typedef (user-ulong-t (:foreign-name "user_ulong_t"))
                      u-int64-t)
(fli:define-c-typedef (user-time-t (:foreign-name "user_time_t"))
                      int64-t)
(fli:define-c-typedef (user-off-t (:foreign-name "user_off_t")) int64-t)
(fli:define-c-typedef (syscall-arg-t (:foreign-name "syscall_arg_t"))
                      u-int64-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_va_list.h"

(fli:define-c-typedef (va-list (:foreign-name "va_list"))
                      --darwin-va-list)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/stdio.h"

(fli:define-foreign-function (renameat "renameat" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 :int)
                              (arg-4 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (renamex-np "renamex_np" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (renameatx-np "renameatx_np" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 :int)
                              (arg-4 (:pointer (:const :char)))
                              (arg-5 (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_stdio.h"

(fli:define-c-typedef (fpos-t (:foreign-name "fpos_t")) --darwin-off-t)
(fli:define-c-struct (--sbuf (:foreign-name "__sbuf"))
                     (-base (:pointer (:unsigned :char)))
                     (-size :int))
(fli:define-c-struct (--sfilex
                      (:foreign-name "__sFILEX")
                      (:forward-reference-p t)))
(fli:define-c-struct (--sfile (:foreign-name "__sFILE"))
                     (-p (:pointer (:unsigned :char)))
                     (-r :int)
                     (-w :int)
                     (-flags :short)
                     (-file :short)
                     (-bf (:struct --sbuf))
                     (-lbfsize :int)
                     (-cookie (:pointer :void))
                     (-close
                      (:pointer (:function ((:pointer :void)) :int)))
                     (-read
                      (:pointer
                       (:function
                        ((:pointer :void) (:pointer :char) :int)
                        :int)))
                     (-seek
                      (:pointer
                       (:function
                        ((:pointer :void) fpos-t :int)
                        fpos-t)))
                     (-write
                      (:pointer
                       (:function
                        ((:pointer :void)
                         (:pointer (:const :char))
                         :int)
                        :int)))
                     (-ub (:struct --sbuf))
                     (-extra (:pointer (:struct --sfilex)))
                     (-ur :int)
                     (-ubuf (:c-array (:unsigned :char) 3))
                     (-nbuf (:c-array (:unsigned :char) 1))
                     (-lb (:struct --sbuf))
                     (-blksize :int)
                     (-offset fpos-t))
(fli:define-c-typedef (file (:foreign-name "FILE")) (:struct --sfile))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdio.h"

(fli:define-foreign-variable (--stdinp "__stdinp" :source)
                             :type
                             (:pointer file))
(fli:define-foreign-variable (--stdoutp "__stdoutp" :source)
                             :type
                             (:pointer file))
(fli:define-foreign-variable (--stderrp "__stderrp" :source)
                             :type
                             (:pointer file))
(fli:define-foreign-function (clearerr "clearerr" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (fclose "fclose" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (feof "feof" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ferror "ferror" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fflush "fflush" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fgetc "fgetc" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fgetpos "fgetpos" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer fpos-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fgets "fgets" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 :int)
                              (arg-3 (:pointer file)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fopen "fopen" :source)
                             ((--filename (:pointer (:const :char)))
                              (--mode (:pointer (:const :char))))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fprintf "fprintf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fputc "fputc" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fputs "fputs" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fread "fread" :source)
                             ((--ptr (:pointer :void))
                              (--size size-t)
                              (--nitems size-t)
                              (--stream (:pointer file)))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (freopen "freopen" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 (:pointer file)))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fscanf "fscanf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fseek "fseek" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 :long)
                              (arg-3 :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (fsetpos "fsetpos" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer (:const fpos-t))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftell "ftell" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :long
                             :language
                             :ansi-c)
(fli:define-foreign-function (fwrite "fwrite" :source)
                             ((--ptr (:pointer (:const :void)))
                              (--size size-t)
                              (--nitems size-t)
                              (--stream (:pointer file)))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (getc "getc" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (getchar "getchar" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (gets "gets" :source)
                             ((arg-1 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (perror "perror" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (printf "printf" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (putc "putc" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (putchar "putchar" :source)
                             ((arg-1 :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (puts "puts" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (remove "remove" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (rename "rename" :source)
                             ((--old (:pointer (:const :char)))
                              (--new (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (rewind "rewind" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (scanf "scanf" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (setbuf "setbuf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer :char)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (setvbuf "setvbuf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer :char))
                              (arg-3 :int)
                              (arg-4 size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (sprintf "sprintf" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (sscanf "sscanf" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (tmpfile "tmpfile" :source)
                             nil
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (tmpnam "tmpnam" :source)
                             ((arg-1 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ungetc "ungetc" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vfprintf "vfprintf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vprintf "vprintf" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vsprintf "vsprintf" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_ctermid.h"

(fli:define-foreign-function (ctermid "ctermid" :source)
                             ((arg-1 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdio.h"

(fli:define-foreign-function (fdopen "fdopen" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fileno "fileno" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (pclose "pclose" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (popen "popen" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (--srget "__srget" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--svfscanf "__svfscanf" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--swbuf "__swbuf" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--sputc "__sputc" :source)
                             ((-c :int) (-p (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (flockfile "flockfile" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftrylockfile "ftrylockfile" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (funlockfile "funlockfile" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (getc-unlocked "getc_unlocked" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (getchar-unlocked
                              "getchar_unlocked"
                              :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (putc-unlocked "putc_unlocked" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (putchar-unlocked
                              "putchar_unlocked"
                              :source)
                             ((arg-1 :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (getw "getw" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (putw "putw" :source)
                             ((arg-1 :int) (arg-2 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (tempnam "tempnam" :source)
                             ((--dir (:pointer (:const :char)))
                              (--prefix (:pointer (:const :char))))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_off_t.h"

(fli:define-c-typedef (off-t (:foreign-name "off_t")) --darwin-off-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdio.h"

(fli:define-foreign-function (fseeko "fseeko" :source)
                             ((--stream (:pointer file))
                              (--offset off-t)
                              (--whence :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ftello "ftello" :source)
                             ((--stream (:pointer file)))
                             :result-type
                             off-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (snprintf "snprintf" :source)
                             ((--str (:pointer :char))
                              (--size size-t)
                              (--format (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vfscanf "vfscanf" :source)
                             ((--stream (:pointer file))
                              (--format (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vscanf "vscanf" :source)
                             ((--format (:pointer (:const :char)))
                              (arg-2 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vsnprintf "vsnprintf" :source)
                             ((--str (:pointer :char))
                              (--size size-t)
                              (--format (:pointer (:const :char)))
                              (arg-4 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vsscanf "vsscanf" :source)
                             ((--str (:pointer (:const :char)))
                              (--format (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_ssize_t.h"

(fli:define-c-typedef (ssize-t (:foreign-name "ssize_t"))
                      --darwin-ssize-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdio.h"

(fli:define-foreign-function (dprintf "dprintf" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vdprintf "vdprintf" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (getdelim "getdelim" :source)
                             ((--linep (:pointer (:pointer :char)))
                              (--linecapp (:pointer size-t))
                              (--delimiter :int)
                              (--stream (:pointer file)))
                             :result-type
                             ssize-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (getline "getline" :source)
                             ((--linep (:pointer (:pointer :char)))
                              (--linecapp (:pointer size-t))
                              (--stream (:pointer file)))
                             :result-type
                             ssize-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (fmemopen "fmemopen" :source)
                             ((--buf (:pointer :void))
                              (--size size-t)
                              (--mode (:pointer (:const :char))))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-function (open-memstream "open_memstream" :source)
                             ((--bufp (:pointer (:pointer :char)))
                              (--sizep (:pointer size-t)))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
(fli:define-foreign-variable (sys-nerr "sys_nerr" :source)
                             :type
                             (:const :int)
                             :accessor
                             :read-only)
(fli:define-foreign-variable (sys-errlist "sys_errlist" :source)
                             :type
                             (:c-array
                              (:const (:pointer (:const :char))))
                             :accessor
                             :address-of)
(fli:define-foreign-function (asprintf "asprintf" :source)
                             ((arg-1 (:pointer (:pointer :char)))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctermid-r "ctermid_r" :source)
                             ((arg-1 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fgetln "fgetln" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer size-t)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (fmtcheck "fmtcheck" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char))))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (fpurge "fpurge" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (setbuffer "setbuffer" :source)
                             ((arg-1 (:pointer file))
                              (arg-2 (:pointer :char))
                              (arg-3 :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (setlinebuf "setlinebuf" :source)
                             ((arg-1 (:pointer file)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (vasprintf "vasprintf" :source)
                             ((arg-1 (:pointer (:pointer :char)))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (funopen "funopen" :source)
                             ((arg-1 (:pointer (:const :void)))
                              (arg-2
                               (:pointer
                                (:function
                                 ((:pointer :void)
                                  (:pointer :char)
                                  :int)
                                 :int)))
                              (arg-3
                               (:pointer
                                (:function
                                 ((:pointer :void)
                                  (:pointer (:const :char))
                                  :int)
                                 :int)))
                              (arg-4
                               (:pointer
                                (:function
                                 ((:pointer :void) fpos-t :int)
                                 fpos-t)))
                              (arg-5
                               (:pointer
                                (:function ((:pointer :void)) :int))))
                             :result-type
                             (:pointer file)
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/secure/_stdio.h"

(fli:define-foreign-function (--sprintf-chk "__sprintf_chk" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 :int)
                              (arg-3 size-t)
                              (arg-4 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--snprintf-chk "__snprintf_chk" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 size-t)
                              (arg-3 :int)
                              (arg-4 size-t)
                              (arg-5 (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--vsprintf-chk "__vsprintf_chk" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 :int)
                              (arg-3 size-t)
                              (arg-4 (:pointer (:const :char)))
                              (arg-5 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (--vsnprintf-chk
                              "__vsnprintf_chk"
                              :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 size-t)
                              (arg-3 :int)
                              (arg-4 size-t)
                              (arg-5 (:pointer (:const :char)))
                              (arg-6 va-list))
                             :result-type
                             :int
                             :language
                             :ansi-c)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint8_t.h"
|#
(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t"))
                      (:unsigned :char))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint16_t.h"

(fli:define-c-typedef (uint16-t (:foreign-name "uint16_t"))
                      (:unsigned :short))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h"

(fli:define-c-typedef (uint32-t (:foreign-name "uint32_t"))
                      (:unsigned :int))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h"

(fli:define-c-typedef (uint64-t (:foreign-name "uint64_t"))
                      (:unsigned :long-long))
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdint.h"

(fli:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(fli:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(fli:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(fli:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(fli:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(fli:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(fli:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(fli:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(fli:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(fli:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_intmax_t.h"

(fli:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long)
;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uintmax_t.h"

(fli:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long))
;;; Derived from file : "/private/var/tmp/lwtemp_Fornax_183858trBy8I.h"

;; Socket types.                                                             */
(defconstant +ZMQ-PAIR+    0)
(defconstant +ZMQ-PUB+     1)
(defconstant +ZMQ-SUB+     2)
(defconstant +ZMQ-REQ+     3)
(defconstant +ZMQ-REP+     4)
(defconstant +ZMQ-DEALER+  5)
(defconstant +ZMQ-ROUTER+  6)
(defconstant +ZMQ-PULL+    7)
(defconstant +ZMQ-PUSH+    8)
(defconstant +ZMQ-XPUB+    9)
(defconstant +ZMQ-XSUB+   10)
(defconstant +ZMQ-STREAM+ 11)

(fli:define-foreign-function (zmq-errno "zmq_errno" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-strerror "zmq_strerror" :source)
                             ((errnum- :int))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-version "zmq_version" :source)
                             ((major- (:pointer :int))
                              (minor- (:pointer :int))
                              (patch- (:pointer :int)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-new "zmq_ctx_new" :source)
                             nil
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-term "zmq_ctx_term" :source)
                             ((context- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-shutdown
                              "zmq_ctx_shutdown"
                              :source)
                             ((context- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-set "zmq_ctx_set" :source)
                             ((context- (:pointer :void))
                              (option- :int)
                              (optval- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-get "zmq_ctx_get" :source)
                             ((context- (:pointer :void))
                              (option- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-init "zmq_init" :source)
                             ((io-threads- :int))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-term "zmq_term" :source)
                             ((context- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-ctx-destroy
                              "zmq_ctx_destroy"
                              :source)
                             ((context- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-c-struct (zmq-msg-t (:foreign-name "zmq_msg_t"))
                     (- (:c-array (:unsigned :char) 64)))
(fli:define-c-typedef (zmq-msg-t (:foreign-name "zmq_msg_t"))
                      (:struct zmq-msg-t))
(fli:define-c-typedef (zmq-free-fn (:foreign-name "zmq_free_fn"))
                      (:function
                       ((:pointer :void) (:pointer :void))
                       :void))
(fli:define-foreign-function (zmq-msg-init "zmq_msg_init" :source)
                             ((msg- (:pointer zmq-msg-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-init-size
                              "zmq_msg_init_size"
                              :source)
                             ((msg- (:pointer zmq-msg-t))
                              (size- size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-init-data
                              "zmq_msg_init_data"
                              :source)
                             ((msg- (:pointer zmq-msg-t))
                              (data- (:pointer :void))
                              (size- size-t)
                              (ffn- (:pointer zmq-free-fn))
                              (hint- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-send "zmq_msg_send" :source)
                             ((msg- (:pointer zmq-msg-t))
                              (s- (:pointer :void))
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-recv "zmq_msg_recv" :source)
                             ((msg- (:pointer zmq-msg-t))
                              (s- (:pointer :void))
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-close "zmq_msg_close" :source)
                             ((msg- (:pointer zmq-msg-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-move "zmq_msg_move" :source)
                             ((dest- (:pointer zmq-msg-t))
                              (src- (:pointer zmq-msg-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-copy "zmq_msg_copy" :source)
                             ((dest- (:pointer zmq-msg-t))
                              (src- (:pointer zmq-msg-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-data "zmq_msg_data" :source)
                             ((msg- (:pointer zmq-msg-t)))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-size "zmq_msg_size" :source)
                             ((msg- (:pointer (:const zmq-msg-t))))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-more "zmq_msg_more" :source)
                             ((msg- (:pointer (:const zmq-msg-t))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-get "zmq_msg_get" :source)
                             ((msg- (:pointer (:const zmq-msg-t)))
                              (property- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-set "zmq_msg_set" :source)
                             ((msg- (:pointer zmq-msg-t))
                              (property- :int)
                              (optval- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-msg-gets "zmq_msg_gets" :source)
                             ((msg- (:pointer (:const zmq-msg-t)))
                              (property- (:pointer (:const :char))))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-socket "zmq_socket" :source)
                             ((arg-1 (:pointer :void)) (type- :int))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-close "zmq_close" :source)
                             ((s- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-setsockopt "zmq_setsockopt" :source)
                             ((s- (:pointer :void))
                              (option- :int)
                              (optval- (:pointer (:const :void)))
                              (optvallen- size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-getsockopt "zmq_getsockopt" :source)
                             ((s- (:pointer :void))
                              (option- :int)
                              (optval- (:pointer :void))
                              (optvallen- (:pointer size-t)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-bind "zmq_bind" :source)
                             ((s- (:pointer :void))
                              (addr- (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-connect "zmq_connect" :source)
                             ((s- (:pointer :void))
                              ;; (addr- (:pointer (:const :char)))
                              (addr (:reference-pass :ef-mb-string)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-unbind "zmq_unbind" :source)
                             ((s- (:pointer :void))
                              (addr- (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-disconnect "zmq_disconnect" :source)
                             ((s- (:pointer :void))
                              (addr- (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-send "zmq_send" :source)
                             ((s- (:pointer :void))
                              (buf- (:pointer (:const :void)))
                              (len- size-t)
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-str-send "zmq_send" :source)
                             ((s- (:pointer :void))
                              (buf- (:reference-pass :ef-mb-string))
                              (len- size-t)
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-send-const "zmq_send_const" :source)
                             ((s- (:pointer :void))
                              (buf- (:pointer (:const :void)))
                              (len- size-t)
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-recv "zmq_recv" :source)
                             ((s- (:pointer :void))
                              (buf- (:pointer :void))
                              (len- size-t)
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-socket-monitor
                              "zmq_socket_monitor"
                              :source)
                             ((s- (:pointer :void))
                              (addr- (:pointer (:const :char)))
                              (events- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-c-typedef (zmq-fd-t (:foreign-name "zmq_fd_t")) :int)
(fli:define-c-struct (zmq-pollitem-t (:foreign-name "zmq_pollitem_t"))
                     (socket (:pointer :void))
                     (fd zmq-fd-t)
                     (events :short)
                     (revents :short))
(fli:define-c-typedef (zmq-pollitem-t (:foreign-name "zmq_pollitem_t"))
                      (:struct zmq-pollitem-t))
(fli:define-foreign-function (zmq-poll "zmq_poll" :source)
                             ((items- (:pointer zmq-pollitem-t))
                              (nitems- :int)
                              (timeout- :long))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-proxy "zmq_proxy" :source)
                             ((frontend- (:pointer :void))
                              (backend- (:pointer :void))
                              (capture- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-proxy-steerable
                              "zmq_proxy_steerable"
                              :source)
                             ((frontend- (:pointer :void))
                              (backend- (:pointer :void))
                              (capture- (:pointer :void))
                              (control- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-has "zmq_has" :source)
                             ((capability- (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-device "zmq_device" :source)
                             ((type- :int)
                              (frontend- (:pointer :void))
                              (backend- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-sendmsg "zmq_sendmsg" :source)
                             ((s- (:pointer :void))
                              (msg- (:pointer zmq-msg-t))
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-recvmsg "zmq_recvmsg" :source)
                             ((s- (:pointer :void))
                              (msg- (:pointer zmq-msg-t))
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-c-struct (iovec
                      (:foreign-name "iovec")
                      (:forward-reference-p t)))
(fli:define-foreign-function (zmq-sendiov "zmq_sendiov" :source)
                             ((s- (:pointer :void))
                              (iov- (:pointer (:struct iovec)))
                              (count- size-t)
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-recviov "zmq_recviov" :source)
                             ((s- (:pointer :void))
                              (iov- (:pointer (:struct iovec)))
                              (count- (:pointer size-t))
                              (flags- :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-z85-encode "zmq_z85_encode" :source)
                             ((dest- (:pointer :char))
                              (data- (:pointer (:const uint8-t)))
                              (size- size-t))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-z85-decode "zmq_z85_decode" :source)
                             ((dest- (:pointer uint8-t))
                              (string- (:pointer (:const :char))))
                             :result-type
                             (:pointer uint8-t)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-curve-keypair
                              "zmq_curve_keypair"
                              :source)
                             ((z85-public-key- (:pointer :char))
                              (z85-secret-key- (:pointer :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-curve-public
                              "zmq_curve_public"
                              :source)
                             ((z85-public-key- (:pointer :char))
                              (z85-secret-key-
                               (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-new
                              "zmq_atomic_counter_new"
                              :source)
                             nil
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-set
                              "zmq_atomic_counter_set"
                              :source)
                             ((counter- (:pointer :void))
                              (value- :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-inc
                              "zmq_atomic_counter_inc"
                              :source)
                             ((counter- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-dec
                              "zmq_atomic_counter_dec"
                              :source)
                             ((counter- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-value
                              "zmq_atomic_counter_value"
                              :source)
                             ((counter- (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-atomic-counter-destroy
                              "zmq_atomic_counter_destroy"
                              :source)
                             ((counter-p- (:pointer (:pointer :void))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-c-typedef (zmq-timer-fn (:foreign-name "zmq_timer_fn"))
                      (:function (:int (:pointer :void)) :void))
(fli:define-foreign-function (zmq-timers-new "zmq_timers_new" :source)
                             nil
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-destroy
                              "zmq_timers_destroy"
                              :source)
                             ((timers-p (:pointer (:pointer :void))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-add "zmq_timers_add" :source)
                             ((timers (:pointer :void))
                              (interval size-t)
                              (handler (:pointer zmq-timer-fn))
                              (arg (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-cancel
                              "zmq_timers_cancel"
                              :source)
                             ((timers (:pointer :void))
                              (timer-id :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-set-interval
                              "zmq_timers_set_interval"
                              :source)
                             ((timers (:pointer :void))
                              (timer-id :int)
                              (interval size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-reset
                              "zmq_timers_reset"
                              :source)
                             ((timers (:pointer :void))
                              (timer-id :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-timeout
                              "zmq_timers_timeout"
                              :source)
                             ((timers (:pointer :void)))
                             :result-type
                             :long
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-timers-execute
                              "zmq_timers_execute"
                              :source)
                             ((timers (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-stopwatch-start
                              "zmq_stopwatch_start"
                              :source)
                             nil
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-stopwatch-intermediate
                              "zmq_stopwatch_intermediate"
                              :source)
                             ((watch- (:pointer :void)))
                             :result-type
                             (:unsigned :long)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-stopwatch-stop
                              "zmq_stopwatch_stop"
                              :source)
                             ((watch- (:pointer :void)))
                             :result-type
                             (:unsigned :long)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-sleep "zmq_sleep" :source)
                             ((seconds- :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-c-typedef (zmq-thread-fn (:foreign-name "zmq_thread_fn"))
                      (:function ((:pointer :void)) :void))
(fli:define-foreign-function (zmq-threadstart
                              "zmq_threadstart"
                              :source)
                             ((func- (:pointer zmq-thread-fn))
                              (arg- (:pointer :void)))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(fli:define-foreign-function (zmq-threadclose
                              "zmq_threadclose"
                              :source)
                             ((thread- (:pointer :void)))
                             :result-type
                             :void
                             :language
                             :ansi-c)

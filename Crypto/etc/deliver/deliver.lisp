(in-package "CL-USER")
(load-all-patches)

;; QuickLisp should be properly configured by now
(load "~/quicklisp/setup.lisp")

;; This Lisp code loads memory with the Emotiq system and call DELIVER to build
;; a binary version of Emotiq.  The binary must call the entry point EMOTIQ:START
;; (or a reasonable facsimile).  Building the binary requires "special treatment",
;; since we try to allow developers to load-and-go, using Lisp LOAD to initialize
;; various systems.  In particular ACTORS:INSTALL-ACTOR-SYSTEM needs to be called
;; at runtime in the Binary (as opposed to being called at LOAD time for development).
;; We also need to ensure that the Emotiq code does not create malloc'ed data
;; in C address space at LOAD time, as the data will be lost before the binary
;; runs.

;; Create a special variable that will direct emotiq/src/Actors/actor-startup.lisp
;; to not call (INSTALL-ACTOR-SYSTEM) while the binary is being built.  This must
;; be set before we load the :emotiq/startup defsystem.

(defparameter cl-user::*performing-binary-build* :performing-binary-delivery)

(ql:quickload :swank) ;; HACK: Needed for implicit dependency in the
                      ;; RESTAS dependency of EMOTIQ-REST.  Needs to
                      ;; occur before other quickload of
                      ;; EMOTIQ/STARTUP dependencies.
(ql:quickload :emotiq/startup)

;; After loading the Emotiq system into memory, we call DELIVER, which is somewhat
;; like SAVE-IMAGE.  DELIVER can perform various optimizations (tree-shaking, symbol-removal)
;; but, it cannot do that in a Lisp image the has multiprocessing turned on.  DELIVER
;; CAN produce a binary which will start up multiprocessing, but DELIVER cannot, itself
;; run with multiprocessing.  See the LW Deliver manual.

;; Args to DELIVER:

;; Arg (1) is the Lisp fully-qualified symbol of the entry point for the Binary.

;; Arg (2) is a string which is used as the name of the delivered binary.  This final
;; name is O/S-dependent, e.g. on Windows, the name will be suffixed with ".EXE" (e.g.
;; emotiq.exe, below).

;; Arg (3) is a numeric "delivery level".  It must be between 0 to 5.  0 produces
;; the least optimization of the image.  Anything above 1, currently, needs extra
;; work in checking which symbols need to be kept or thrown away.  The LW Delivery
;; manual explains how to determine such an analysis.

;; Keywords:
;;
;; :multiprocessing t --- means that the delivered binary image will start up
;;   multiprocessing before calling the Emotiq code
;;
;; :console t -- means that the delivered binary will include only code for a simple
;;   console (e.g. if there are runtime errors), instead of the full-blown GUI code.
;;   This saves space.

;; For other options, refer to the LW Delivery manual.

(deliver 'emotiq:start "emotiq" 0 :multiprocessing t :console t)

;; editor-enhancements.lisp
;;
;; DM/RAL  2022/10/16 16:22:01
;; ----------------------------------

(defpackage #:editor-enhancements
  (:use #:common-lisp))

(in-package #:editor-enhancements)

;; --------------------------------------------------------------------
;; Editor Enhancements

;; file types for Lisp mode
(editor:define-file-type-hook 
    ("lispworks" "lisp" "lsp" "cl" "asd")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Lisp"))

;; the following two forms make sure the "Find Source" command works
;; with the editor source
#-:lispworks-personal-edition
(load-logical-pathname-translations "EDITOR-SRC")

#-:lispworks-personal-edition
(setf dspec:*active-finders*
      (append dspec:*active-finders*
              (list "EDITOR-SRC:editor-tags-db")))

;; if I press ESC followed by < during a search operation I want to go
;; to the beginning of the buffer and /not/ insert the #\< character
#-:LISPWORKS8 ;; BLOCK #2
(editor::set-logical-char= #\escape :exit nil
                           (editor::editor-input-style-logical-characters
                            editor::*emacs-input-style*))

;; Make for more undo's
(setf (editor:variable-value 'editor::undo-ring-size) 1000)

;; ----------------------------------------------------------
;; Current Line

(editor:defcommand "Current Line" (p)  
     "Computes the line number of the current point and
                           prints it in the Echo Area"
     "Prints the line number of the current point"
  (declare (ignore p))
  (let* ((cpoint (editor:current-point))
         (svpoint (editor:copy-point cpoint))
         (count 0))
    (editor:beginning-of-buffer-command nil)
    (loop
     (if (editor:point> cpoint svpoint)
         (return))
     (unless (editor:next-line-command nil)
       (return))
     (incf count))
    (editor:move-point cpoint svpoint)
    (editor:message "Current Line Number: ~S " count)))

;; --------------------------------------------------------------
;; Goto Line

(editor:defcommand "Goto Line" (p)
     "Moves the current point to a specified line number.
                           The number can either be supplied via the prefix
                           argument, or, if this is nil, it is prompted for."
     "Moves the current point to a specified line number."
  (let ((line-number  
         (or p (editor:prompt-for-integer  
                :prompt "Line number: "
                :help "Type in the number of the line to
                           go to"))))
    (editor:beginning-of-buffer-command nil)
    (editor:next-line-command line-number)))

;; ---------------------------------------------------------------
;; Show Function Calls (F4)
#||#
(editor:defcommand "Show Function Calls" (p)
     "Fires up the Function Call Browser on the function beneath the
                           cursor."
  (declare (ignore p))
  (let ((sym (editor:get-symbol-from-point (editor:current-point))))
    (editor::editor-show-call-graph sym :called-by)))

(editor:bind-key "Show Function Calls" "F4")
#||#
;; (editor:bind-key "Show Paths To" "F4")

;; ---------------------------------------------------------------
;; Insert UUID (F16)

(editor:defcommand "Insert UUID" (p)
     "Creates a new UUID and inserts at the current point"
     "Inserts a new UUID at the current point"
  (declare (ignore p))
  (editor:insert-string (editor:current-point) (princ-to-string (uuid:make-v1-uuid))))
(editor:bind-key "Insert UUID" "F16")

(editor:defcommand "Insert UUID String" (p)
     "Creates a new UUID and inserts its string-repr at the current point"
     "Inserts a new UUID string-repr at the current point"
  (declare (ignore p))
  (editor:insert-string (editor:current-point) (uuid:uuid-string (uuid:make-v1-uuid))))
(editor:bind-key "Insert UUID String" "shift-F16")

;; ---------------------------------------------------------------
;; Insert C-style UUID (ctrl-F16)

(editor:defcommand "Insert UUID C-name" (p)
     "Creates a new UUID C-name and inserts at the current point"
     "Inserts a new UUID C-name at the current point"
  (declare (ignore p))
  (editor:insert-string (editor:current-point)
                        (concatenate 'string
                                     "_"
                                     (string-trim "{}"
                                                  (substitute #\_ #\- (uuid:uuid-string (uuid:make-v1-uuid)))))))
(editor:bind-key "Insert UUID C-name" "ctrl-F16")

;; -----------------------------------------------------
;; Insert Program Scaffolding (F7)

(editor:defcommand "Insert Stump" (p)
     "Inserts generic scaffolding for new projects."
     "Inserts generic scaffolding at current point"
  (declare (ignore p))
  (let* ((file     (editor:buffer-pathname (editor:current-buffer)))
         (package  (pathname-name file))
         (realname (namestring file))
         (fname    (subseq realname (1+ (position #\/ realname :from-end t))))
         (str      #1>.end
;; $fname
;;
;; DM/RAL  $(um:zulu-date-string)
;; ----------------------------------

(defpackage #:$package
  (:use #:common-lisp))

(in-package #:$package)

;; ----------------------------------

.end))
    (editor:insert-string (editor:current-point) str)))
(editor:bind-key "Insert Stump" "F7")

;; ---------------------------------------------------
;; Insert Timestamp (F8)

(editor:defcommand "Insert Timestamp" (p)
     "Inserts a date and time stamp at the current point."
     "Inserts a date and time stamp at the current point."
  (declare (ignore p))
  (editor:insert-string (editor:current-point)
                        #1";; DM/RAL  $(um:zulu-date-string)"#
                        ))
(editor:bind-key "Insert Timestamp" "F8")

;; --------------------------------------------

(editor:bind-string-to-key ";; --------------------------------------------
"
                           "Control-1")

;; ---------------------------------------------------
;; Insert Unit Test Block (aka #| |#) - Ctrl-3 (below the #)

(editor:defcommand "Insert Unit Test Block" (p)
     "Inserts a #| |# block at the current point."
     "Inserts a #| |# block at the current point."
  (declare (ignore p))
  (editor:insert-string (editor:current-point)
                        "#|
                      
|#")
  (editor:line-offset (editor:current-point) -1)
  (editor:line-start (editor:current-point)))

(editor:bind-key "Insert Unit Test Block" "Control-3" :mode "Lisp")

;; ------------------------------------------------------------
;; Give us the Greek alphabet mapped to #(Ctrl-z char)

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(let ((ctr  0))
  (defun editor::make-string-command (string)
    ;; Something changed with LWM 8.1.1, breaking the previous behavior.
    ;; Patch over editor to allow the unambiguous Greek letters below, with case distinction.
    ;; DM/RAL  2025/09/26 22:35:49 UTC
    (let ((name  (format nil " %%String Command ~A ~A" (incf ctr) string)))
      (or (editor::find-command name)
          (editor::make-command name nil
                                #'(lambda (p)
                                    (declare (ignore p))
                                    (editor::insert-string (editor::current-point) string)))))))
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(let ((ascii-str "abcdefghijklmnoprstuvxyzABCDEFGHIJKLMNOPRSTUVXYZ•ª")
      (greek-str "αβψδεφγηιξκλμνοπρστθωχυζΑΒΨΔΕΦΓΗΙΞΚΛΜΝΟΠΡΣΤΘΩΧΥΖ✕∊"))
  ;; ° is Option-Shift-8
  ;; · is Option-Shift-9
  ;; Interesting chars:
  ;;  #\U+03f5  ϵ #\U+220A
  ;;  #\U+2715  ✕
  ;;
  (loop for ascii-ch across ascii-str
        for greek-ch across greek-str
        do
          (editor:bind-string-to-key (string greek-ch)
                                     (vector "Ctrl-z" ascii-ch))
        ))

#|

(format t "#\\U+~4,'0x" (char-code #\∊))

(let* ((base #x2700)
       (v (loop for ix from 0 below 256 collect ix)))
  (format t "
~x
~{~A
~}"
  base
  (um:group (map 'string (lambda (ix)
                            (code-char (+ ix base)))
                  v)
            16)))
|#

;; --------------------------------------------
;; Greek letters via Editor Abbrevs...
;;
;; NOTE: Editor Abbrev file needs a very specific format, encoded in
;; Unicode. Very specific requirements for each line of input too.
;;
;; From Editor Source:

;;; Read Word Abbrev file <filename>   Define word abbrevs from a definition file.

;;; Ignores all lines less than 4 characters, i.e. blankspace or errors. That is
;;; the minimum number of characters possible to define an abbrev.  It thinks the 
;;; current abbrev "wraps" if there is no #\" at the end of the line or there are
;;; two #\"s at the end of the line (unless that is the entire definition string,
;;; i.e, a null-abbrev).

;;; The format of the Abbrev files is 
;;;
;;;                   ABBREV<tab><tab>"ABBREV DEFINITION"
;;;
;;; for Global Abbrevs, and
;;;
;;;                   ABBREV<tab>(MODE)<tab>"ABBREV DEFINITION"
;;;
;;; for Modal Abbrevs.  
;;; Double-quotes contained within the abbrev definition are doubled.  If the first
;;; line of an abbrev definition is not closed by a single double-quote, then
;;; the subsequent lines are read in until a single double-quote is found.

#|
;; Read from normal (UTF-8) text file, and rewrite as Unicode
(let* ((filename "./startup/abbrev.defns" )
       (txt (hcl:file-string filename)))
  (with-open-file (file filename :direction :output :element-type 'lispworks:simple-char
                        #+DBCS-ENV :external-format #+DBCS-ENV :unicode
                        :if-exists :rename)
    (write-sequence txt file)))
|#
;; (editor:read-word-abbrev-file-command nil "./startup/abbrev.defns")
(dolist (pair '(("ga"  . "α")
                ("gb"  . "β")
                ("gg"  . "γ")
                ("gd"  . "δ")
                ("ge"  . "ε")
                ("gl"  . "λ")
                ("gi"  . "ι")
                ("gh"  . "η")
                ("gk"  . "κ")
                ("gn"  . "ν")
                ("gu"  . "υ")
                ("gm"  . "μ")
                ("gom" . "ο")
                ("gp"  . "π")
                ("gr"  . "ρ")
                ("gth" . "θ")
                ("gt"  . "τ")
                ("gf"  . "φ")
                ("gps" . "ψ")
                ("gs"  . "σ")
                ("gx"  . "ξ")
                ("gch" . "χ")
                ("gz"  . "ζ")
                ("gw"  . "ω")

                ("galf"  . "α")
                ("gbet"  . "β")
                ("ggam"  . "γ")
                ("gdel"  . "δ")
                ("geps"  . "ε")
                ("glam"  . "λ")
                ("giot"  . "ι")
                ("geta"  . "η")
                ("gkap"  . "κ")
                ("gnu"   . "ν")
                ("gups"  . "υ")
                ("gmu"   . "μ")
                ("gomi"  . "ο")
                ("gpi"   . "π")
                ("grho"  . "ρ")
                ("gthe"  . "θ")
                ("gtau"  . "τ")
                ("gphi"  . "φ")
                ("gpsi"  . "ψ")
                ("gsig"  . "σ")
                ("gxi"   . "ξ")
                ("gchi"  . "χ")
                ("gzet"  . "ζ")
                ("gome"  . "ω")

                ("gmem"  . "∊")
                ("gdeg"  . "°")
                ("gfal"  . "∀")
                ("gexi"  . "∃")
                ("gdot"  . "⋅")
                ("gcro"  . "×")
                ("gbot"  . "⊥")
                ("ginf"  . "∞")
                ("gtfo"  . "∴")
                ("gdef"  . "≜")
                ("gsun"  . "∪")
                ("gsin"  . "∩")
                ("gint"  . "∫")
                ("gnot"  . "¬")
                ("gsum"  . "∑")
                ("gprd"  . "∏")
                ("gsrt"  . "√")
                ("gbdl"  . "∆")
                ("gbga"  . "Γ")
                ("gnab"  . "∇")
                ("gder"  . "∂")
                ("getc"  . "⋯")
                ("gphs"  . "∡")
                ))
  (editor:make-word-abbrev-command nil (car pair) (cdr pair) "Global"))

;; ------------------------------------------------------------
;; Misc Keybindings

#+:LISPWORKS7+
(progn
  (dolist (binding '(("End of Buffer"                   #("c-End"))
                     ("Beginning of Buffer"             #("c-Home"))
                     ("Beginning of Line"               :home)
                     ("End of Line"                     :end)
                     ;;(editor:bind-key "Indent New Line" "Return")
                     
                     ;; the following line clears up a really irritating behavior!
                     ("Just One Space"                  "Shift-Space")

                     #+:LW-ADD-ONS
                     ("Insert Space and Show Arglist"   #\Space)

                     ("Compile Defun"                   "F13" :mode "Lisp")
                     ("Compile Buffer"                  "F14" :mode "Lisp")
                     ("Compile and Load Buffer File"    #("Control-c" "Control-k") :mode "Lisp")
                     ("Insert \()"                      "Control-(" :mode "Lisp")
                     ("Insert \()"                      "Control-(" :mode "Execute")
                     ("Indent New Line"                 #\Return :mode "Lisp")
                     ("Function Documentation"          "F1")
                     ("Evaluate Defun"                  "Kp-Enter")
                     ("Compile Defun"                   "c-Kp-Enter")
                     ("Abbrev Expand Only"              #\No-Break-Space)  ;; on Mac kb = option+space
                     ))
    (apply 'editor:bind-key binding))
  (editor:bind-string-to-key "lambda" "c-\\")
  #+:LW-ADD-ONS
  (dolist (binding '(("Tools Apropos"                   #("Control-c" "Control-a"))
                     ("Toggle Trace"                    #("Control-c" "Control-t") :mode "Lisp")
                     ;; ("Evaluate Last Form And Inspect"  #(#\control-\c #\control-\i))
                     ;; ("Evaluate Last Form And Describe" #(#\control-\c #\control-\d))
                     ;; ("Set Mark And Highlight"          #\control-\@)
                     ;; ("Set Mark And Highlight"          #\control-space)
                     ;; ("Indent and Complete Symbol"      #\Tab)
                     ("Indent Selection or Complete Symbol"          #\Tab)
                     ;;("Meta Documentation"              "F5")
                     ("Maybe Invoke Listener Shortcut"  #\, :mode "Execute")
                     ;; ("Tools Listener"                  "F7")
                     ;; ("Tools Editor"                    "F6")
                     ))
    (apply 'editor:bind-key binding))
  
  #+:LW-ADD-ONS
  (setf lw-add-ons:*mop-page* (merge-pathnames #P"./MOP/index.html"
                                               (sys:current-directory)))
  )

;; --------------------------------------------------------------------

;; (setf editor:*shell-shell* "/usr/local/bin/bash") 
#|
(editor:bind-string-to-key "λ" #\not-sign) ;; Option-L on Mac keyboard
(defmacro cl::λ (&body body)
  `(lambda ,@body))
(editor:setup-indent "λ" 1)
(export 'cl::λ :cl)
|#

#|
(in-package "EDITOR")

(defcommand "Insert []" (p)
  "Insert a pair of brackets [].
The point is positioned after the open bracket."
  "Insert a pair of brackets []."
  (set-highlight-buffer-region nil)
  (insert-character (current-point) #\[)
  (insert-character (current-point) #\])
  (point-before (current-point)))

(defcommand "Insert {}" (p)
  "Insert a pair of braces {}.
The point is positioned after the open brace."
  "Insert a pair of braces {}."
  (set-highlight-buffer-region nil)
  (insert-character (current-point) #\{)
  (insert-character (current-point) #\})
  (point-before (current-point)))

(defcommand "Insert \"\"" (p)
  "Insert a pair of double-quotes \"\".
The point is positioned after the opening double-quote."
  "Insert a pair of double-quotes. \"\""
  (set-highlight-buffer-region nil)
  (insert-character (current-point) #\")
  (insert-character (current-point) #\")
  (point-before (current-point)))

(defcommand "Insert «»" (p)
  "Insert a pair of GDef quotes «».
The point is positioned after the open quote."
  "Insert a pair of GDef quotes. «»"
  (set-highlight-buffer-region nil)
  (insert-character (current-point) #\«)
  (insert-character (current-point) #\»)
  (point-before (current-point)))

#|
(bind-key "Self Insert" "(" :global :emacs)
(bind-key "Self Insert" "[" :global :emacs)
(bind-key "Self Insert" "{" :global :emacs)
(bind-key "Self Insert" #\" :global :emacs)
(bind-key "Self Insert" "«" :global :emacs)
|#

#| |#
;; (bind-key "Insert ()" "(" :global :emacs)
;; (bind-key "Insert []" "[" :global :emacs)
;; (bind-key "Insert {}" "{" :global :emacs) 
;; (bind-key "Insert \"\"" #\" :global :emacs)
;; (bind-key "Insert «»" "«" :global :emacs)
#| |#
|#

#|
#+(and :win32 (not :console-image))
(define-action "Initialize LispWorks Tools" "Open Editor And Tile Windows"
               'lw-add-ons::open-editor-and-tile-windows-vertically)
|#

#|
;; `canonical' indentation for IF
(editor:setup-indent "if" 1 2 4)

;; `canonical' indentation for FLI:DEFINE-FOREIGN-FUNCALLABLE
;; (not needed for LW 5.0)
(editor:setup-indent "define-foreign-funcallable" 2 2 4)

;; `canonical' indentation for DEFINE-SYMBOL-MACRO
(editor:setup-indent "define-symbol-macro" 1)
|#


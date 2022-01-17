;; getting the user's preferred language in LWM and LWW

#+cocoa
 (defun current-language ()
   (let ((lang (objc:with-autorelease-pool ()
                 (objc:invoke-into
                  'string
                  (objc:invoke
                   (objc:invoke "NSUserDefaults" "standardUserDefaults")
                   "objectForKey:" "AppleLanguages")
                  "objectAtIndex:" 0))))
     ;; 2019-04-29
     ;; lang can be either on the short form "sv" or the long form "sv-SE".
     ;; It may have changed in later versions of macOS.
     ;; Anyway, make sure that this function returns on the short form "sv".
     (if (and (= (length lang) 5)
              (eq (char lang 2) #\-))
         (subseq lang 0 2)
       lang)
     )
   )
 
 #+win32
 (fli:define-foreign-function (get-system-default-language-id "GetSystemDefaultLangID")
     () :result-type :int)
 #+win32
 (defun current-language ()
   ;; Map language id to code - a complete list is available in the Windows documentation of GetSystemDefaultLangID
   (case (mod (get-system-default-language-id) 1024) ; we are only interested in the primary language, which resides in the last 10 bits
     (#x16 "pt")
     (#x1D "sv")
     (t "default")))


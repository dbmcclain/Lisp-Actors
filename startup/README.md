This folder contains code that I use on Lisp session bootup. Most imporant among them are my keyboard mappings for the Greek alphabet, and PROJECT-MAPPINGS which allows me to use abbreviated package names for my own packages. 

Since Quicklisp does not enforce any conventions regarding the naming of packages, most providers use their own terse names. And this increases the likelihood of colliding with names that I'd like to use for my own packages. As a result, I have to resort to using unique package names, like "com.ral.useful-macros.def-extensions". But I'd rather refer to all of useful-macros with UM: as the package prefix.

The code in PROJECT-PACKAGES-xxx provides for recursive abbreviation expansion during LOAD and IN-PACKAGE, for Lispworks, SBCL, and Allegro, feeding from my abbreviations stored in PROJECT-MAPPINGS.lisp.

For use of the Greek alphabet, I have my Emacs editor allow ^Z-char to produce extended Unicode characters. The mapping from ASCII characters to extended chars is shown in EDITOR-ENHANCEMENTS.lisp.

```
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
```


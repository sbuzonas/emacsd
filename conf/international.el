;;** 26 (info "(emacs)International") Character Set Support ]

(when section-international (message "26 International Character Set Support...")

;; To open (or save) a file in UTF-8, you can press `C-x RET c utf-8 RET'
;; (`universal-coding-system-argument') before the `C-x C-f' (or `C-x C-s')

;; To help you find all the chars you need to replace by escape sequences, you
;; can use `C-u C-s [^[:ascii:]]'

;; To check your locale settings, you can have a look to what Emacs produce
;; (in a mail buffer) under "Important settings" when you type
;; `M-x report-emacs-bug RET foo RET':
;;
;; Important settings:
;;   value of $LC_ALL: nil
;;   value of $LC_COLLATE: nil
;;   value of $LC_CTYPE: nil
;;   value of $LC_MESSAGES: nil
;;   value of $LC_MONETARY: nil
;;   value of $LC_NUMERIC: nil
;;   value of $LC_TIME: nil
;;   value of $LANG: en_US.UTF-8
;;   value of $XMODIFIERS: nil
;;   locale-coding-system: utf-8-unix
;;   default-enable-multibyte-characters: t


;;*** 26.3 (info "(emacs)Language Environments")

;; system locale to use for formatting time values (e.g., timestamps in
;; Org mode files)
(setq system-time-locale "en_US.utf8")  ; "C"?


;;*** 26.5 (info "(emacs)Select Input Method")

;; `M-x describe-coding-system RET RET'

;; ;; default input method for multilingual text
;; (setq default-input-method "latin-1-prefix")

;; To see all the non-ASCII characters you can type with the `C-x 8' prefix,
;; type `C-x 8 C-h'.


;;*** 26.6 (info "(emacs)Coding Systems")

;; For any user who needs symbols that are not in the 7-bit ASCII set, our
;; recommendation is to move to Unicode UTF-8. That is the only encoding that
;; is the same across all platforms and operating systems that support it.


;;*** 26.7 (info "(emacs)Recognize Coding") Systems

(add-to-list 'file-coding-system-alist
             '("\\.owl\\'" utf-8 . utf-8))
             ;; and all the rest is utf-8:
             ;; '("" . utf-8)

;; In GNU Emacs, when you specify the coding explicitly in the file, that
;; overrides `file-coding-system-alist'. Not in XEmacs?

;; The variable `auto-coding-alist' is the strongest way to specify the coding
;; system for certain patterns of file names, or for files containing certain
;; patterns; this variable even overrides `-*-coding:-*-' tags in the file
;; itself.

;; default coding system (for new files),
;; also moved to the front of the priority list for automatic detection
(GNUEmacs
 (cond (running-ms-windows
        (prefer-coding-system 'iso-latin-1))  ; FIXME Temp for PFlow
       (t
        (prefer-coding-system 'utf-8))))
        ; or set environment variables like `LC_CTYPE', `LC_ALL' or `LANG'


;;*** 26.8 (info "(emacs)Specify Coding") System of a File

(GNUEmacs
 ;; to copy and paste to and from Emacs through the clipboard (with coding
 ;; system conversion)
 (cond (running-ms-windows
        (set-selection-coding-system 'compound-text-with-extensions))
       (t
        (set-selection-coding-system 'utf-8))))

(message "26 International Character Set Support... Done"))

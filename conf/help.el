;;** 10 (info "(emacs)Help")

(when section-help (message "10 Help...")

;; `catman' create the database files that are used by `apropos' or `man -k'.

;; You can read the instructions of a file by typing
;; `M-x finder-commentary RET file RET'

;; You might want to check out electric-help (ehelp.el). There is nothing more
;; satisfying than the window disappearing when you think it should!


;;*** 10.4 (info "(emacs)Apropos")

;; You can ask what pertains to a given topic by typing
;; `M-x apropos RET pattern RET'

;; check all variables and non-interactive functions as well
(setq apropos-do-all t)

;; add apropos help about variables (bind `C-h A' to `apropos-variable')
(GNUEmacs
    (define-key help-map (kbd "A") 'apropos-variable))


;;*** 10.8 (info "(emacs)Misc Help") Commands

;; ls -l /usr/share/info/dir*
;; update-info-dir

;; Info documentation browse
(when (try-require 'info)

    ;; enter Info
    (global-set-key (kbd "<f1>") 'info)

    ;; list of directories to search for Info documentation files
    ;; (in the order they are listed)
    (setq Info-directory-list
          (cond (running-ms-windows
                 (append Info-default-directory-list
                         `(,(concat (getenv "SHARE") "/info/")
                           "C:/cygwin/usr/info/")))
                (t
                 '("./"
                   "~/info/"
                   "/usr/local/share/info/"
                   "/usr/local/info/"
                   "/usr/share/info/emacs-snapshot/"
                   "/usr/share/info/emacs-23"
                   "/usr/share/info/"))))

    ;; I find this handy:
    ;; (setq Info-directory-list
    ;;       `("~/.emacs.d/info" ,@Info-directory-list))


;;;     ;; adding doc for Org mode
;;;     (setq Info-default-directory-list
;;;           (cons (expand-file-name
;;;                  (concat org-general-path "org/doc"))
;;;                 Info-default-directory-list))

    ;; adding TexLive?

    ;; FIXME
    (setq Info-directory-list
          (append
           '("~/Downloads/emacs/site-lisp/auctex-11.85/doc"
             "~/Downloads/emacs/site-lisp/emacs-w3m/doc"
             "~/Downloads/emacs/site-lisp/org-mode/doc")
           Info-directory-list))

;;;     dir files will be merged by emacs, so you can create
;;;     one for your local additions. Probably by copying the
;;;     systems one from /usr/share/info/dir or wherever, delete
;;;     all entries after '* Menu:' and put in your own.
;;;
;;;     (info "(texinfo)Other Info Directories")
;;;
;;;     The dir file has to have a special header format. Just copy the
;;;     systems one into a directory in INFOPATH, erase everything after
;;;     '*Menu ' and put in your own stuff.
;;;
;;;     Wouldn't the use of install-info be more appropriate? When the
;;;     actual info file is well prepared, then this programme does a
;;;     perfect job, sorting the info file into its proper section ...

;; The canonical way to do that is to set the environment variable `INFOPATH'
;; outside of Emacs, in the same shell from which you invoke Emacs.
;; `INFOPATH's value should be a list of Info directories in the same format
;; as the `PATH' variable on your system.

;; The environment variable INFOPATH tells GNU Emacs where to look for info
;; files.
;;
;; If you want, you can edit the dir files and remove entries. The utility
;; install-info is used to maintain the dir file.

;; See http://www.emacswiki.org/emacs-se/GnusTutorial, "Installing Gnus":
    ;; In order to get the Gnus info pages added to your documentation, also
    ;; augment the INFOPATH environment variable, like
    ;;   INFOPATH=$INFOPATH:/usr/local/share/emacs/site-lisp/gnus-5.8.8/texi
    ;;   export INFOPATH
    ;; The above should go in your shell startup file, such as ~/.profile.

;;; > Is there an automated procedure to transfer the additional info files
;;; > and info/dir entries into the new emacs?
;;;
;;; Copy the files, then run install-info on them to update the info/dir file.

;; Don't play with `Info-directory-list', it's not intended to be settable by
;; the user

    ;; display symbol definitions, as found in the relevant manual
    ;; (for C, Lisp, and other languages that have documentation in Info)
    (global-set-key (kbd "<C-f1>") 'info-lookup-symbol))

(GNUEmacs
    (try-require 'info+))
    ;; with `info+.el', you can merge an Info node with its subnodes into
    ;; the same buffer, by calling `Info-merge-subnodes' (bound to `+')

    ;; `C-h K' goes to the node in the Emacs manual describing the command
    ;; bound to a key.


;; dictem (dict protocol and dictem for a documentation)

;; describe-function

;; avoid the description of all minor modes
(defun describe-major-mode ()
  "Describe only 'major-mode'."
  (interactive)
  (describe-function major-mode))


;; find convenient unbound keystrokes
(try-require 'unbound)                  ; `M-x describe-unbound-keys'


;; get a Unix manual page and put it in a buffer
(global-set-key (kbd "<S-f1>") 'man-follow)

;; (defun jump-man-page ()
;;   (interactive)
;;   (manual-entry (current-word)))

;; same behavior as woman when manpage is ready
(setq Man-notify-method 'newframe)

;; (setq Man-frame-parameters '((foreground-color . "black")
;;                              (background-color . "grey90")
;;                              (cursor-color . "black")
;;                              (mouse-color . "gold")
;;                              (width . 80)
;;                              (tool-bar-lines . 0)))

;; browse Unix manual pages "W.o. (without) Man"
(when (try-require 'woman)

    ;; list of directory trees to search for Unix manual files
    (setq woman-manpath
          (cond (running-ms-windows
                 `(,(concat (getenv "SHARE") "/man/")
                   "C:/cygwin/usr/man/"
                   "C:/cygwin/usr/share/man"
                   "C:/cygwin/usr/local/share/man"))
                (t
                 '("/usr/share/man/"
                   "/usr/local/share/man/")))))


;;*** Documentation Basics

;; jump to section in XEmacs Lisp Reference manual
(autoload 'lispref-search "lispref")
(define-key help-map (kbd "L") 'lispref-search)

(message "10 Help... Done"))

;;** 37 (info "(emacs)Dired"), the Directory Editor

(when section-dired (message "37 Dired, the Directory Editor...")

;; FIXME Some people report sorting problems in Org agenda view because of
;; this package!
;; provide the same facility of `ls --color' inside Emacs
(try-require 'dircolors)

;; emulate insert-directory completely in Emacs Lisp
(when (try-require 'ls-lisp)

    ;; disable the case sensitive sort of file names
    (setq ls-lisp-ignore-case t)

    ;; sort directories first in any ordering
    (setq ls-lisp-dirs-first t)

    ;; use ISO 8601 dates (on MS-Windows)
    (setq ls-lisp-format-time-list
           '("%Y-%m-%d %H:%M"
             "%Y-%m-%d %H:%M"))

    ;; use localized date/time format
    (setq ls-lisp-use-localized-time-format t))


;; directory-browsing commands
(when (try-require 'dired)

;;*** 37.1 (info "(emacs)Dired Enter")

    ;; switches passed to `ls' for Dired
    ;; (setq dired-listing-switches "-h")
                                 ;; "-alt --time-style=long-iso")


;;*** 37.7 (info "(emacs)Operating on Files")

    ;; try to guess a default target directory
    (setq dired-dwim-target t)

    ;; enable the use of the command `dired-find-alternate-file'
    ;; without confirmation
    (put 'dired-find-alternate-file 'disabled nil)

    ;; recursive deletes allowed, after asking for each directory at top level
    (setq dired-recursive-deletes 'top)

    ;; copy recursively without asking
    (setq dired-recursive-copies 'always)


    ;; extra Dired functionality
    (try-require 'dired-x)
        ; You can jump to the Dired buffer corresponding to the current
        ; buffer by pressing `C-x C-j' (`dired-jump').
        ; If in Dired already, pop up a level and goto old directory's line.

        ; `dired-x' also has a feature to "guess" the right shell command and
        ; the right external viewer for documents (see
        ; `dired-guess-shell-alist-user')


    ;; On top of the traditional ways, there's also an add-on called Extview
    ;; which opens files using outside programs, such as XPDF, based on their
    ;; extension. It does this both from Dired and with `find-file'. One
    ;; advantage is that using the traditional ! switch with Dired locks up
    ;; Emacs until you close the other program. Extview does not and leaves
    ;; Emacs free for continued used.

    ;; >> how to associate external programs to known file types
    ;; >> in Dired. For example, associating
    ;; >
    ;; > Hi, i use `extview.el', it's work fine.
    ;; > It reads in a `.mailcap' file.

    ;; If you need to open a file in Emacs that has an extension that Extview
    ;; will open in another viewer, like HTML, you use `find-file-literally'
    ;; to open it in Emacs.
    (try-require 'extview)

    ;; See news "Opening html-File in Dired with w3m" for extra info


    ;; Dired stuff to open files a la Windows (from Howard Melman):
    ;; execute file using windows associations
    (GNUEmacs (when running-ms-windows
                (defun dired-is-dir ()
                  (file-directory-p (dired-get-filename)))

                (defun dired-execute-file (&optional arg)
                  (interactive "P")
                  (mapcar #'(lambda (file)
                              (w32-shell-execute
                               "open" (convert-standard-filename file)))
                          (dired-get-marked-files nil arg)))

                (defun dired-mouse-execute-file (event)
                  "In Dired, execute the file or goto directory name you click
on."
                  (interactive "e")
                  (set-buffer (window-buffer (posn-window (event-end event))))
                  (goto-char (posn-point (event-end event)))
                  (if (dired-is-dir)
                      (dired-find-file)
                    (dired-execute-file)))
                (global-set-key [?\C-x mouse-2] 'dired-mouse-execute-file)

                (defun hrm-dired-mode-hook ()
                  "Hook run when entering Dired mode."
                  (define-key dired-mode-map (kbd "X") 'dired-execute-file)
                  (define-key dired-mode-map
                    [M-down-mouse-1] 'dired-mouse-execute-file))

                (add-hook 'dired-mode-hook 'hrm-dired-mode-hook)))


    ;; extensions to Dired
    (try-require 'dired+)

    ;; reuse the current Dired directory buffer to visit another directory
    ;; (limit Dired to 1 single buffer)
    (when (try-require 'dired-single)
        (define-key dired-mode-map [return] 'joc-dired-single-buffer)
        (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
        (define-key dired-mode-map "^"
          (function
           (lambda nil (interactive) (joc-dired-single-buffer ".."))))
        (define-key dired-mode-map (kbd "C-x C-j")
          (function
           (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;;;         ;; dired-sort-map.el
;;;         ;; press s then s, x, t or n to sort by Size, eXtension, Time or Name
;;;         (require 'dired-sort-map)

;;; Check this out as well:
;;;     http://www.emacswiki.org/emacs/DiredSortMenu

    )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-column-widths.el")))

(defun my-browse-dir ()
  "Open the current directory in your OS's file manager."
  (interactive)
  (let ((dir-as-string
         (file-name-as-directory (expand-file-name ".")))
        (file-manager
         (cond (running-ms-windows "explorer")
               (t "/usr/lib/kde4/bin/dolphin"))))
                ;; `nautilus --no-desktop', `gnome-open'
                ;; or `xdg-open' (in `xdg-utils')
    (start-process "browse" nil file-manager dir-as-string)))

(defun dired-open-externally ()
  "Open the current directory in your OS's file manager (see `~/.mailcap')."
  (interactive)
  (let ((fileobject (dired-get-file-for-visit)))
    (start-process "dired-external" nil "xdg-open" fileobject)
    (message "Opening file %s" fileobject)))

(define-key dired-mode-map (kbd "e") 'dired-open-externally)

(defun open-in-desktop ()
  "Open the current file's folder in desktop."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") (w32-shell-execute "explore" "."))
   ((string-equal system-type "darwin") (shell-command "open ."))))



;;*** 37.16 (info "(emacs)Dired and Find")

;; For searches in Dired, see `dired-do-search' (`A').
;; Search through all marked files for a match for regexp.
;; Stops when a match is found.
;; To continue searching for next match, use command `M-,'.

;; search for files with names matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?1] 'find-name-dired)

;; search for files with contents matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?2] 'find-grep-dired)

;; run grep via find, with user-specified arguments
(global-set-key [(control c) ?3] 'grep-find)

;; ignore `.svn' and `CVS' directories
(setq grep-find-command
      (concat
       "find . \\( -path '*/.svn' -o -path '*/CVS' \\) -prune -o -type f "
              "-print0 | "
              "xargs -0 -e grep -i -n -e "))


;;*** 37.17 Editing the (info "(emacs)Wdired") Buffer

;; Wdired mode is great for renaming (a lot of) files in a directory, as it
;; allows editing the Dired buffer like a text file, using all the power of
;; Emacs. That is, one can use keyboard macros, search and replace,
;; rectangle mode (great for adding prefixes to file names), flip mode bits
;; with the mouse, etc.!

;; in Dired, put a Dired buffer in a mode in which filenames are editable
(when (try-require 'wdired)
    (autoload 'wdired-change-to-wdired-mode "wdired")
    (add-hook 'dired-load-hook
              (lambda ()
                (define-key dired-mode-map
                  (kbd "E") 'wdired-change-to-wdired-mode))))


;;*** Add-Ons

;; add a binding "w" -> `dired-find-w3m' to Dired
(defun dired-find-w3m () (interactive)
  "In Dired, visit (with find-w3m) the file named on this line."
  (w3m-find-file (file-name-sans-versions (dired-get-filename) t)))

(eval-after-load "dired"
  '(progn (define-key dired-mode-map "w" 'dired-find-w3m)))


;; 2-pane file manager based on Dired and inspired by MC, with tree extension
(try-require 'sunrise-commander)
;; then `M-x sunrise'


(message "37 Dired, the Directory Editor... Done"))

;;* Major Structures of Emacs

;;** 22 (info "(emacs)Files") Handling

(when section-files (message "22 Files Handling...")

;;*** 22.2 (info "(emacs)Visiting") Files

;; visit a file
(global-set-key (kbd "<f3>") 'find-file)

;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs.d/init.el'."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<S-f3>") 'my-open-dot-emacs)

;; open my Gnus configuration file
(defun my-open-dot-gnus ()
  "Opening `~/.gnus'."
  (interactive)
  (find-file "~/.gnus"))
(global-set-key (kbd "<C-f3>") 'my-open-dot-gnus)

;; open my Timeclock file
(defun my-open-timeclock ()
  "Opening `~/Projects/Work.org'."
  (interactive)
  (find-file "~/Projects/Work.org"))
(global-set-key (kbd "<C-f4>") 'my-open-timeclock)


;;*** 22.3 (info "(emacs)Saving") Files

;; make your changes permanent
(global-set-key (kbd "<f2>") 'save-buffer)

;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

;; offer save of `*scratch*' buffer on exit
(save-excursion
  (set-buffer "*scratch*")
  (setq buffer-file-name "~/*scratch*"))
  ;; `(setq buffer-offer-save t)' does not have its intended effect in my
  ;; `.emacs' file (i.e., `buffer-offer-save' still has its global default
  ;; value of nil in the `*scratch*' buffer). But if I immediately evaluate it
  ;; in the `*scratch*' buffer, it works.
  ;; That is because at startup, Emacs sets the major mode of `*scratch*'
  ;; according to `initial-major-mode', _after_ my `.emacs' is read.  Changing
  ;; major modes kills all local variables that are not permanently local,
  ;; including `buffer-offer-save'.

;; ;; major mode command symbol to use for the initial `*scratch*' buffer
;; (setq initial-major-mode 'text-mode)  ; to avoid autoloads for Lisp mode (cedet)

;; ensure a file ends in a newline when it is saved
(setq require-final-newline t)
;; TODO I should do this only for text and Fundamental modes, because I could
;; edit binary files (see `mode-require-final-newline')

;; directory used for temporary files
(XEmacs
    (setq temporary-file-directory (or (getenv "TEMP") "/tmp/")))

;; maintain last change time stamps (`Time-stamp: <>' occurring within the
;; first 8 lines) in files edited by Emacs
(when (try-require 'time-stamp)

    ;; format of the string inserted by `M-x time-stamp'
    (setq time-stamp-format "%Y-%02m-%02d %3a %02H:%02M %u on %s")
                          ; `YYYY-MM-DD Weekday HH:MM user on system'
    ;; see `system-time-locale' for non-numeric formatted items of time

    ;; update time stamps every time you save a buffer
    (add-hook 'write-file-hooks 'time-stamp))

;; insert a time stamp string
(defun my-insert-time-stamp ()
  "Insert a time stamp."
  (interactive "*")
  (insert (format "%s %s %s %s"
                  comment-start
                  (format-time-string "%Y-%m-%d")
                  (user-login-name)
                  comment-end)))

(defun insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument,
add day of week. With two prefix arguments, add day of week and
time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
                      ((equal prefix '(4)) "%Y-%m-%d %a")
                      ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c .") 'insert-date)

(GNUEmacs
    ;; update the copyright notice in current buffer
    (when (try-require 'copyright)
      ; XXX Check no other copyright.el gets in the way
      (add-hook 'write-file-hooks 'copyright-update)))


;;*** 22.4 (info "(emacs)Reverting") a Buffer

;; replace current buffer text with the text of the visited file on disk
(defun my-revert-buffer ()
  "Unconditionally revert current buffer."
  (interactive)
  (flet ((yes-or-no-p (msg) t))
    (revert-buffer)))

;; key binding
(global-set-key (kbd "<C-f12>") 'my-revert-buffer)


;;*** 22.6 (info "(emacs)Auto Save"): Protection Against Disasters

;; how to get Emacs to auto-save to your local disk [`#file#']

;; auto-save every 100 input events
(setq auto-save-interval 100)

;; auto-save after 15 seconds idle time
(setq auto-save-timeout 15)

;; Check out Kevin's `ebackup.el' for saving in a specified common directory
;; (not in local dir) and handling saving of buffers with spaces in their
;; name... (otherwise, problems when composing *mail replies to ...* )

;; ;; The `auto-save.el' and `backup.el' packages collect files in one place
;; (try-require 'auto-save)
;; (try-require 'backup)

;; put backup files (i.e., `foo~' or `foo.~i~') in one place
(GNUEmacs
    ;; regexp => directory mappings
    ;; filenames matching a regexp are backed up in the corresponding directory
    (setq backup-directory-alist
          '((".*" . "~/.emacs.d/backups/"))))  ;; '(("." . "~/.saves"))
              ;; or "/tmp/"?
          ;; Emacs will `make-directory' it if necessary

(XEmacs
    (when (try-require 'backup-dir)
        (make-variable-buffer-local 'backup-inhibited)
        (setq bkup-backup-directory-info
              '((t "~/.saves" ok-create full-path prepend-name)))))

;; always use copying to create backup files (don't clobber symlinks)
(setq backup-by-copying t)

;; make numeric backup versions
(setq version-control t)

;; number of oldest versions to keep when a new numbered backup is made
(setq kept-old-versions 0)  ; 2

;; number of newest versions to keep when a new numbered backup is made
(setq kept-new-versions 20)  ; 2

;; delete excess backup versions silently
(setq delete-old-versions t)


;; make the message "FILE has auto save data" unmissable
(defface recover-this-file
  '((t :background "orange"))
  "Face for buffers visiting files with auto save data."
  :group 'files)

(defvar recover-this-file nil
  "If non-nil, an overlay indicating that the visited file has auto save data.")

(defun recover-this-file-find-file-hook ()
  ;; see after-find-file
  (let ((warn (not buffer-read-only)))
    (when (and warn
               ;; No need to warn if buffer is auto-saved
               ;; under the name of the visited file.
               (not (and buffer-file-name
                         auto-save-visited-file-name))
               (file-newer-than-file-p (or buffer-auto-save-file-name
                                           (make-auto-save-file-name))
                                       buffer-file-name))
      (set (make-local-variable 'recover-this-file)
           (make-overlay (point-min) (point-max)))
      (overlay-put recover-this-file 'face 'recover-this-file))))

(add-hook 'find-file-hook 'recover-this-file-find-file-hook)


;;*** 22.9 (info "(emacs)Comparing Files")

;; default to unified diffs
(setq diff-switches "-u")

;; compare text in current window with text in next window
(global-set-key (kbd "C-c =") 'compare-windows)
;;;;;;;;;;;;;;;; FIXME Conflict with reftex


;;*** 22.10 (info "(emacs)Diff Mode")

;; run `diff' in compilation-mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)

;; extensions to `diff-mode.el'
(try-require 'diff-mode-)

;; ediff, a comprehensive visual interface to diff & patch
;; setup for Ediff's menus and autoloads
(try-require 'ediff-hook)

;; auto-refine only the regions of this size (in bytes) or less
(setq ediff-auto-refine-limit (* 2 14000))

;; do everything in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; split the window depending on the frame width
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 160)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))


;;*** 22.12 Accessing (info "(emacs)Compressed Files")

;; Using the Emacs Dired utility, you can compress or uncompress a file by
;; pressing `Z'

;; easy editing of arc/zip/zoo/lhz archives
(GNUEmacs
    (require 'arc-mode)

    ;; TODO See `archive-zip-extract'

    (when running-ms-windows
      ;; Unfortunately, if you are trying to use gunzip to uncompress a file
      ;; under Dired, you will probably encounter a problem saying "Failed to
      ;; uncompress ..." or "spawning child process: exec format error". The
      ;; problem is due to that gunzip provided by Cygwin is not an executable
      ;; file. It is a symbolic link to gzip. (You can verify this by
      ;; "ls -l /usr/bin/gunzip". Since Gnu Emacs does not understand Cygwin's
      ;; symbolic link, it cannot execute gunzip. Here is the solution.

      (require 'dired-aux)

      (defun dired-call-process (program discard &rest arguments)
        ;; 09Feb02, sailor overwrite this function because Gnu Emacs cannot
        ;; recognize gunzip is a symbolic link to gzip. Thus, if the program
        ;; is "gunzip", replace it with "gzip" and add an option "-d".

        ;; "Run PROGRAM with output to current buffer unless DISCARD is t.
        ;; Remaining arguments are strings passed as command arguments to
        ;; PROGRAM."
        ;; Look for a handler for default-directory in case it is a
        ;; remote file name.
        (let ((handler
               (find-file-name-handler (directory-file-name default-directory)
                                       'dired-call-process)))
          (if handler (apply handler 'dired-call-process
                             program discard arguments)
            (progn
              (if (string-equal program "gunzip")
                  (progn
                    (setq program "gzip")
                    (add-to-list 'arguments "-d")))
              (apply 'call-process
                     program nil (not discard) nil arguments)))))))


;;*** 22.13 (info "(emacs)File Archives")

;; simple editing of tar files as a Dired-like listing of its contents
(try-require 'tar-mode)

;; reading/writing/loading compressed files
(try-idle-require 'jka-compr)

;; ;; code for handling all sorts of compressed and encrypted files
;; (try-idle-require 'crypt++)  ; EasyPG takes care of the encryption
;; ;; allows you to encrypt/decrypt files within Emacs. I use it regularly and
;; ;; it works very reliably. When I use `C-x C-f' to access an encrypted
;; ;; file, Emacs asks me for the passphrase and then decrypts the file before
;; ;; displaying it. When I save the file, Emacs automatically encrypts it
;; ;; again.


;; Once you've encrypted a file once, either externally or using a
;; crypt++-provided command, you don't need any new functions or
;; key-bindings -- you just visit it, and you get prompted for the
;; key, and when you save it it gets re-crypted with that same key.

;; This is achieved by sniffing every file as it is visited, and
;; prompting for a key if it "looks binary".  If you blow off the prompt,
;; you get the raw file.

;; I use `epa' now, but don't like the fact that it remembers the password
;; forever...
;;
;; Hmmm, interesting -- crypt++ remembers for output, but _always_ prompts on
;; input. Which did you mean wrt epa? And by 'forever' do you mean across
;; sessions?
;;
;; Crypt++ would forget the password after a certain time. Epa seems to
;; remember the password for each XEmacs session, which for me means between
;; reboots.
;;
;; For gpg-encrypted files you can use EasyPG (aka epg, the successor of pgg)
;; <http://www.easypg.org/> (which is also used by Gnus, when available, and
;; ships with current Emacs versions).
;;
;; See http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
;;
;; I think the only setup necessary is:
;;
;; (require 'epa-setup)


;; See further section "(info "(pgg)Top") (Emacs interface to GnuPG)"

;; enable EPA to get `.gpg' files to be automatically encrypted
(require 'epa)
;; (require 'epa-file)
(epa-file-enable)

;; stop EasyPG from asking for the recipient
(setq epa-file-encrypt-to "steve@fancyguy.com")

;; If no one is selected, symmetric encryption will be performed
;; (setq epa-file-encrypt-to "")

;; cache passphrase for symmetric encryption (VERY important)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Not to sound paranoid. But if you want caching, it's recommended to use
;; public-key encryption instead of symmetric encryption. gpg-agent is the
;; preferred way to do this. For more information see here:
;; (info "(pgg) Prerequisites")
;; Works fine with epa as well.

;; prompt for the password in the Emacs minibuffer (instead of using a
;; graphical password prompt for gpg)
(setenv "GPG_AGENT_INFO" nil)


;; What is PGP/MIME, what is OpenPGP, and how Gnus handles them.
;; - PGP/MIME is a standard, which mml2015* implements using ep[ag]-*.
;; - OpenPGP is a standard, which ep[ag]-* implements.

;; > `mml2015-use'
;; >      Symbol indicating elisp interface to OpenPGP implementation for
;; >      PGP/MIME messages.  The default is `pgg', but `mailcrypt' and
;; >      `gpg' are also supported although deprecated.


;;*** 22.14 (info "(emacs)Remote Files")

;;**** Ange-FTP

;; transparent FTP support
(when (try-require 'ange-ftp)

    ;; try to use passive mode in ftp, if the client program supports it
    (setq ange-ftp-try-passive-mode t))  ; needed for Ubuntu


;;**** (info "(tramp)Top") TRAMP - Transparent Remote Access, Multiple Protocols

;; (other protocols than just FTP)

;; Examples: C-x C-f /ssh:fni@server:/home/fni/.bashrc
;;           C-x C-f /plink:fni@server:/home/fni/.bashrc (from Windows)
;;           C-x C-f /sudo:root@localhost:/etc/group

;; Note -- `sshfs' can give me the same functionality as TRAMP: it is like a
;; personal NFS (another mounted file system) over SSH. If you can SSH to a
;; server, you can probably do `sshfs'.

;; > I'm in shell mode, logged in on another machine over ssh, and I want to
;; > do some 'crontab -e' editing on that machine. But that will bring up a
;; > new editor, which is whatever you set in your EDITOR env variable, and
;; > both vi and emacs cannot be used in this dumb shell. How can I edit the
;; > crontab in my emacs session?
;;
;; Create the crontab on the remote machine, open it using TRAMP from your
;; machine, edit and save and then reinstall it.
;; That or simply enable x forwarding so running emacs on the remote
;; bring up emacs gtk/x on your main machine editing your cron file
;; on the remote.

;; TRAMP is very slow to load! You definitely want to autoload!
(try-idle-require 'tramp)
(eval-after-load "tramp"
  '(progn

;;** 4 (info "(tramp)Configuration") of TRAMP for use

;;*** 4.6 Selecting a (info "(tramp)Default Method")

    ;; /method:user@host:/path/file

    ;; default transfer method
    (setq tramp-default-method  ; `scp' by default
          (cond (running-ms-windows
                 ;; (issues with Cygwin `ssh' which does not cooperate with
                 ;; Emacs processes -> use `plink' from PuTTY, it definitely
                 ;; does work under Windows)
                 ;; C-x C-f /plink:myuser@host:/some/directory/file
                 "plink")
                (t
                 "ssh")))

    ;; You might try out the `rsync' method, which saves the remote files
    ;; quite a bit faster than SSH. It's based on SSH, so it works the same,
    ;; just saves faster.


;;*** 4.7 Selecting a (info "(tramp)Default User")

    ;; default user
    (setq tramp-default-user "fni")


;;*** 4.9 Connecting to a remote host using (info "(tramp)Multi-hops")

    ;; ;; new proxy system (introduced with Tramp 2.1, instead of the old
    ;; ;; "multi-hop" filename syntax) to edit files on a remote server by going
    ;; ;; via another server
    ;; (when (boundp 'tramp-default-proxies-alist)
    ;;   (add-to-list 'tramp-default-proxies-alist
    ;;                '("10.10.13.123" "\\`root\\'" "/ssh:%h:")))
    ;; ;; Opening `/sudo:10.10.13.123:' would connect first `10.10.13.123' via
    ;; ;; `ssh' under your account name, and perform `sudo -u root' on that
    ;; ;; host afterwards. It is important to know that the given method is
    ;; ;; applied on the host which has been reached so far.
    ;; ;; The trick is to think from the end.


;;*** 4.12 (info "(tramp)Password handling") for several connections

    ;; how many seconds passwords are cached
    (setq password-cache-expiry 60)  ; default is 16


;;*** 4.15 (info "(tramp)Remote shell setup") hints

    ;; string used for end of line in rsh connections
    (setq tramp-rsh-end-of-line  ; `\n' by default
          (cond (running-ms-windows
                 "\n")
                (t
                 "\r")))


;;*** 4.16 (info "(tramp)Auto-save and Backup") configuration

    ;; faster auto saves
    (setq tramp-auto-save-directory temporary-file-directory)


;;** 9 How to Customize (info "(tramp)Traces and Profiles")

    ;; help debugging
    (setq tramp-verbose 9)  ; default is 0

    ;; call "M-x tramp-submit-bug" to generate an email with several trace
    ;; information

    (setq tramp-debug-buffer t)

;;

    ;; "turn off" the effect of `backup-directory-alist' for TRAMP files
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))

    ;; make Emacs beep after reading from or writing to the remote host
    (defadvice tramp-handle-write-region
      (after tramp-write-beep-advice activate)
      " make tramp beep after writing a file."
      (interactive)
      (beep))
    (defadvice tramp-handle-do-copy-or-rename-file
      (after tramp-copy-beep-advice activate)
      " make tramp beep after copying a file."
      (interactive)
      (beep))
    (defadvice tramp-handle-insert-file-contents
      (after tramp-copy-beep-advice activate)
      " make tramp beep after copying a file."
      (interactive)
      (beep))

    ;; XXX Did not work, from what I remember
    ;; ;; define own abbreviation (for use with bookmarks)
    ;; (add-to-list 'directory-abbrev-alist
    ;;              (if (memq system-type '(cygwin windows-nt))
    ;;                  '("^/RUSSELL" . "//RUSSELL/Users")
    ;;                '("^/RUSSELL" . "/smb:fni@RUSSELL:/Users")))

    ;; after adding:
    ;;     (setq coding-system-for-read 'utf-8)
    ;;     (setq coding-system-for-write 'utf-8)
    ;; to my `.emacs', TRAMP works correctly with UTF-8 files.

;; Open a file as root, easily [from Alex Schroeder]
    (defvar find-file-root-prefix "/sudo:root@localhost:"
      "*The filename prefix used to open a file with `find-file-root'.
      This should look something like \"/sudo:root@localhost:\" (new style
      TRAMP) or \"/[sudo:root@localhost]/\" (XEmacs or old style TRAMP).")

    (defvar find-file-root-history nil
      "History list for files found using `find-file-root'.")

    (defvar find-file-root-hook nil
      "Normal hook for functions to run after finding a \"root\" file.")

    (defun find-file-root ()
      "*Open a file as the root user.
      Prepends `find-file-root-prefix' to the selected file name so that it
      maybe accessed via the corresponding TRAMP method."
      (interactive)
      (require 'tramp)
      (let* (;; We bind the variable `file-name-history' locally so we can
             ;; use a separate history list for "root" files.
             (file-name-history find-file-root-history)
             (name (or buffer-file-name default-directory))
             (tramp (and (tramp-tramp-file-p name)
                         (tramp-dissect-file-name name)))
             path dir file)
        ;; If called from a "root" file, we need to fix up the path.
        (when tramp
          (setq path (tramp-file-name-path tramp)
                dir (file-name-directory path)))
        (when (setq file (read-file-name "Find file (UID = 0): " dir path))
          (find-file (concat find-file-root-prefix file))
          ;; If this all succeeded save our new history list.
          (setq find-file-root-history file-name-history)
          ;; allow some user customization
          (run-hooks 'find-file-root-hook))))

    (defface find-file-root-header-face
      '((t (:foreground "white" :background "red3")))
      "*Face use to display header-lines for files opened as root.")

    (defun find-file-root-header-warning ()
      "*Display a warning in header line of the current buffer.
      This function is suitable to add to `find-file-root-hook'."
      (let* ((warning "WARNING: EDITING FILE WITH ROOT PRIVILEGES!")
             (space (+ 6 (- (frame-width) (length warning))))
             (bracket (make-string (/ space 2) ?-))
             (warning (concat bracket warning bracket)))
        (setq header-line-format
              (propertize warning 'face 'find-file-root-header-face))))

    (add-hook 'find-file-root-hook 'find-file-root-header-warning)

    (global-set-key (kbd "C-x C-S-r") 'find-file-root)
))


;;*** 22.17 (info "(emacs)File Conveniences")

;; setup a menu of recently opened files
(try-idle-require 'recentf)
(eval-after-load "recentf"
  '(progn

     ;; file to save the recent list into
     (setq recentf-save-file "~/.emacs.d/.recentf")

     ;; maximum number of items in the recentf menu
     (setq recentf-max-menu-items 30)

     ;; to protect from TRAMP -- FIXME not correctly supported (yet) under Win32
     (setq recentf-auto-cleanup 'never)

     ;; save file names relative to my current home directory
     (setq recentf-filename-handlers '(abbreviate-file-name))

     ;; toggle `recentf' mode
     (recentf-mode 1)

     ;; add key binding
     (global-set-key (kbd "C-x C-r") 'recentf-open-files)))

;; find file (or URL) at point
(try-idle-require 'ffap)
(eval-after-load "ffap"
  '(progn

  ;; don't use default key bindings, as I want some of them to be defined
  ;; differently (`C-x C-r', for example)

  ;; function called to fetch an URL
  (setq ffap-url-fetcher 'browse-url)   ; could be `browse-url-emacs' or
                                        ; `w3m-browse-url'

  ;; visit a file
  (global-set-key (kbd "<f3>") 'find-file-at-point)))


;; Possible error: Install w3m command in `exec-path' or set `w3m-command'
;; variable correctly
(GNUEmacs
 ;; open anything
 (when (try-require 'anything-config)  ; loads `anything.el' too

   (defun anything-c-define-dummy-source (name func &rest other-attrib)
     `((name . ,name)
       (candidates "dummy")
       ,@other-attrib
       (filtered-candidate-transformer
        . (lambda (candidates source)
            (funcall ',func)))
       (requires-pattern . 1)
       (volatile)
       (category create)))

   (defun anything-c-dummy-candidate ()
     ;; `source' is defined in filtered-candidate-transformer
     (list (cons (concat (assoc-default 'name source)
                         " '" anything-input "'")
                 anything-input)))

   ;;*dummy websearch
   (defun make-anything-c-source-websearch (name url &rest extra)
     (anything-c-define-dummy-source
      (concat "Websearch for " name)
      #'anything-c-dummy-candidate
      `(action . ,(eval `(lambda (args)
                           (browse-url
                            (apply 'concat
                                   ,url anything-pattern (quote ,extra))))))))

   ;; See http://www.emacswiki.org/emacs/RubikitchAnythingConfiguration

   ;; source of candidates for anything
   (setq anything-sources
         (list
          anything-c-source-buffers     ; needs w3m-command to be set!

          anything-c-source-files-in-current-dir
          anything-c-source-file-name-history
          anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-man-pages
          anything-c-source-org-headline ; show Org headlines (when Org mode)
          anything-c-source-emacs-functions
          anything-c-source-imenu
          anything-c-source-bbdb
          anything-c-source-occur
          anything-c-source-kill-ring
          anything-c-source-locate      ; find files everywhere

          ;; ;; FIXME 2010-02-26 sva
          ;; ;; When google suggest is on, selecting a buffer does not work
          ;; ;; anymore: it presents the file as being empty...
          ;; anything-c-source-google-suggest ; do a quick google search

          anything-c-source-emacs-commands
          anything-c-source-fixme
          anything-c-source-emacs-source-defun

          ;; triggered only when no exact match is found
          (make-anything-c-source-websearch "Google"
                                            "http://www.google.com/search?q="
                                            "&client=emacs-anything")
          (make-anything-c-source-websearch "Emacs Wiki"
                                            "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q="
                                            "&client=emacs-anything")
          (make-anything-c-source-websearch "Wikipedia"
                                            "http://en.wikipedia.org/wiki/Special:Search?search="
                                            "&sourceid=emacs-anything")

;; ;; ;;               anything-c-source-bookmarks-local
;; ;; ;;               anything-c-source-bookmarks-su
;; ;; ;;               anything-c-source-bookmarks-ssh
;; ;; ;;               anything-c-source-emms-dired
          anything-c-source-colors
          anything-c-source-customize-face
          anything-c-source-emacs-process
          anything-c-source-info-cl
          anything-c-source-info-elisp
          anything-c-source-info-pages
          anything-c-source-register
          anything-c-source-semantic
          ))

;;;         Here the extensions i use: (you can find all of them on emacswiki)
;;;
;;;         anything-complete.el
;;;         anything-dabbrev-expand.el
;;;         anything-match-plugin.el
;;;         anything-traverse.el (if you don't use traverselisp.el have a look at
;;;                                  anything-grep.el)

   ;; do not show more candidates than this limit from individual sources
   (setq anything-candidate-number-limit 999)

   ;; the user has to be idle for this many seconds, before candidates from
   ;; *delayed* sources are collected (useful for sources involving heavy
   ;; operations, so that candidates from the source are not retrieved
   ;; unnecessarily if the user keeps typing)
   (setq anything-idle-delay 0.9) ; 1.3 works nicely

   ;; ;; make anything minibuffer better input latency
   ;; (defadvice anything-check-minibuffer-input (around sit-for activate)
   ;;   (if (sit-for anything-idle-delay t)
   ;;       ad-do-it))

   ;; the user has to be idle for this many seconds, before ALL candidates are
   ;; collected (also effective for *non-delayed* sources)
   (setq anything-input-idle-delay 0.4) ; 0.6 works nicely

   ;; uses the current window to show the candidates
   (setq anything-samewindow t)

   ;; candidates separator of `multiline' source
   (setq anything-candidate-separator
         (propertize (make-string 42 ?-) 'face 'traverse-match-face))

   ;; suppress displaying sources which are out of screen at first
   (setq anything-quick-update t)

   ;; don't save history information to file
   (remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

   ;; select anything
   (global-set-key (kbd "<f3>") 'anything)))



;; TODO Have a look at gpicker -- a solution for quickly choosing file from
;; (possibly large) projects!

;; show image files as images (not as semi-random bits)
(GNUEmacs
    (auto-image-file-mode 1))

;; inline image minor mode
(when (try-require 'iimage)

  ;; XXX Test me!

    ;; This allows for the viewing of images in-line in Org mode documents.
    (setq iimage-mode-image-search-path (expand-file-name "~/"))

    ;; Match org file: links
    (add-to-list 'iimage-mode-image-regex-alist
                 (cons (concat "\\[\\[file:\\(~?"
                               iimage-mode-image-filename-regex
                               "\\)\\]")  1))

    (defun org-toggle-iimage-in-org ()
      (interactive)
      (let ((turning-on (not iimage-mode)))
        (set-face-underline-p 'org-link (not turning-on))
        (iimage-mode (or turning-on 0)))))

(message "22 Files Handling... Done"))

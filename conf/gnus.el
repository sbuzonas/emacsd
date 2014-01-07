;;** 40 (info "(emacs)Gnus")

(when section-gnus (message "40 Gnus...")

;; perform Caesar ciphers
(when (try-require 'rot13)
  (defvar rot13-translate-table
    (let ((str (make-string 127 0)) (i 0))
      (while (< i 127)
        (aset str i i) (setq i (1+ i)))
      (setq i 0)
      (while (< i 26)
        (aset str (+ i ?a) (+ (% (+ i 13) 26) ?a))
        (aset str (+ i ?A) (+ (% (+ i 13) 26) ?A))
        (setq i (1+ i))) str)
    "String table for rot 13 translation.")

  (defun rot13-string (string)
    "Return Rot13 encryption of STRING."
    (with-temp-buffer
      (insert string)
      (rot13-region (point-min) (point-max))
      (buffer-string)))

  (defun rot13-region (start end)
    "Rot13 encrypt the region between START and END in current buffer."
    (interactive "r")
    (translate-region start end rot13-translate-table))

  ;; XXX vvv Customize this! vvv XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  ;; full name of this user
  (setq user-full-name "Fabrice Niessen")

  ;; full mailing address of this user
  ;; (used in MAIL envelope FROM, and to select the default personality ID)
  (setq user-mail-address
        (concat (rot13-string "sav") "@" "missioncriticalit.com")))

  ;; XXX ^^^ Customize this! ^^^ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; a newsreader for GNU Emacs
(when (try-require 'gnus)

  ;; Gnus startup file name
  (setq gnus-init-file "~/.gnus")

  (global-set-key (kbd "<C-f5>")
                  '(lambda ()
                     (interactive)
                     (let ((buffer (get-buffer "*Group*")))
                       (if buffer
                           (progn
                             (switch-to-buffer buffer)
                             (gnus-group-get-new-news))
                         (call-interactively 'gnus)))))

  ;; package to compose an outgoing mail (Message, with Gnus paraphernalia)
  (setq mail-user-agent 'gnus-user-agent)
  (XEmacs
   (setq toolbar-mail-reader 'gnus))

  ;; reading mail with Gnus
  (setq read-mail-command 'gnus))

;; some info related functions
;; (to insert links such as `(info "(message)Insertion Variables")')
(when (try-require 'rs-info)
  (autoload 'rs-info-insert-current-node "rs-info"
    "Insert reference to current Info node using STYPE in buffer." t nil)
  (autoload 'rs-info-boxquote "rs-info"
    "Yank text (from an info node), box it and use current info node as title."
    t nil)
  (autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
  (autoload 'rs-info-insert-node-for-variable "rs-info"
    "Insert a custom style info node for the top level form at point." t nil)
  (defalias 'boxquote-info 'rs-info-boxquote))


;;*** Insidious (info "(bbdb)Top")

(when (try-require 'bbdb)

    ;; coding system used for reading and writing `bbdb-file' (BBDB 2.35+)
    (setq bbdb-file-coding-system 'utf-8)

    ;; ensure `~/.bbdb' never becomes non utf-8 again (it is defined with
    ;; `defconst', so it is reset whenever `bbdb.el' is loaded)
    (add-hook 'bbdb-load-hook
              (lambda () (setq bbdb-file-coding-system 'utf-8)))


;;**** (info "(bbdb)Installation")

    ;; enable the various package-specific BBDB functions
    (bbdb-initialize 'gnus 'message)

    ;; add bindings for the default keys to Gnus and configure Gnus to
    ;; notify the BBDB when new messages are loaded (required if the BBDB is
    ;; to be able to display BBDB entries for messages displayed in Gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

    ;; add a binding for `M-TAB' to Message mode
    ;; this will enable completion of addresses based on BBDB records
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)

    ;; customizable completion in message headers
    ;; (to avoid conflict between `flyspell' and `BBDB')
    (try-require 'message-x)


;;**** (info "(bbdb)Interfaces")

    ;; mail aliases (local mailing lists)
    (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

    ;; always use full name when sending mail
    ;; (even if User Name has an address of the form <user.name@somedomain>)
    (setq bbdb-dwim-net-address-allow-redundancy t)

    ;; no popup on auto-complete
    (setq bbdb-completion-display-record nil)

    ;; completion is done across the set of all full-names and user-ids
    (setq bbdb-completion-type nil)


;;**** (info "(bbdb)Reader-specific Features")

    ;; marking posters with records in the BBDB
    (setq bbdb/gnus-summary-mark-known-posters t)

    ;; mark authors in the Summary Buffer who have records in the BBDB
    (setq bbdb/gnus-summary-known-poster-mark "B")

    ;; display the poster's name from the BBDB if we have one
    (setq bbdb/gnus-summary-prefer-real-names t)

    ;; replace the information provided in the From header with data from
    ;; the BBDB if we have one
    (setq bbdb/gnus-summary-prefer-bbdb-data t)

    (setq bbdb/gnus-summary-show-bbdb-names t)


;;**** (info "(bbdb)Options")Options

    ;; You can add the author of a mail or posting to the BBDB
    ;; by hitting `:'

    ;; name of the file which contains your personal database
    (setq bbdb-file "~/.bbdb")

    ;; no default area code to use when prompting for a new phone number
    (setq bbdb-default-area-code nil)

    ;; default country to use if none is specified
    (setq bbdb-default-country "")

    ;; disable syntax-checking of telephone numbers
    (setq bbdb-north-american-phone-numbers-p nil)

    ;; restoration of the window configuration
    (setq bbdb-electric-p t)

    ;; don't display a continuously-updating BBDB window while in GNUS
    ;; (setq bbdb-use-pop-up nil)

    ;; desired number of lines in a GNUS pop-up BBDB window
    (setq bbdb-pop-up-target-lines 1)

    ;; default display layout
    (setq bbdb-display-layout 'multi-line)

    ;; default display layout pop-up BBDB buffers
    (setq bbdb-pop-up-display-layout 'one-line)

    ;; omit creation-date and timestamp from BBDB display
    (setq bbdb-display-layout-alist
          '((one-line          (order     . (phones notes))
                               (name-end  . 24)
                               (toggle    . t)
                               (omit      . (net AKA mail-alias gnus-private
                                             creation-date timestamp)))
            (multi-line        (indention . 14)
                               (toggle    . t)
                               (omit      . (AKA creation-date timestamp)))
            (pop-up-multi-line (indention . 14))))

    ;; allow cycling of email addresses while completing them
    (setq bbdb-complete-name-allow-cycling t)

    ;; save the database without asking (any time it would ask)
    (setq bbdb-offer-save 'auto)

    ;; automatically add some text to the notes field of the BBDB record
    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

    ;; capture auto-notes
    (setq bbdb-auto-notes-alist
          ;; organization
          `(("Organization" (".*" Organization 0))

            ;; mailer
            ("User-Agent" (".*" mailer 0 t))  ;; t = overwrite
            ("X-Mailer" (".*" mailer 0 t))
            ("X-Newsreader" (".*" mailer 0 t))

            ;; X-Face bitmaps of the people
            ("x-face" ,(list (concat "[ \t\n]*\\([^ \t\n]*\\)"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?")
                             'face
                             "\\1\\3\\5\\7"))))


;;**** (info "(bbdb)Utilities")

    ;; search the BBDB
    (global-set-key (kbd "<C-f11>") 'bbdb)

    ;; search the BBDB by regexp
    (when (try-require 'bbdb-query)
        (global-set-key (kbd "<C-f11>") 'bbdb-query))

    ;; use BBDB to store PGP preferences
    (when (try-require 'bbdb-pgp)
        ;; what to do if the recipient is not in the BBDB
        (setq bbdb/pgp-default-action nil))

    ;; BBDB SCHDPLUS Filter
    (when (try-require 'bbdb-ldif)
        ;; You can output the `*BBDB*' buffer in SCHDPLUS .CSV format
        ;; by invoking `M-x bbdb-output-schdplus'

        (load "bbdb-schdplus")

        (setq bos-filename "~/bbdb-schdplus.csv")))

(message "40 Gnus... Done"))

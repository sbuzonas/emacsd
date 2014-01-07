;;** 20 Commands for (info "(emacs)Fixit") Typos

(when section-fixit (message "20 Commands for Fixing Typos...")

;;*** 20.1 (info "(emacs)Undo")

;; (GNUEmacs
;;     ;; keep no more undo information once it exceeds this size
;;     (setq undo-limit (* 4 undo-limit))  ; 4 * 20 MB (default)
;;
;;     ;; don't keep more than this much size of undo information
;;     (setq undo-strong-limit (* 4 undo-strong-limit)))  ; 4 * 30 MB (default)


;;*** 20.4 Checking and Correcting (info "(emacs)Spelling")

;;     aspell -H list Article.html
;;     aspell -H check Article.html
;; Then hold your finger down on the `a' (add) or `l' (add lower case) key.

;; use Hunspell or GNU Aspell instead of `ispell'
(setq-default ispell-program-name
  (cond (running-ms-windows
         "C:/Program Files/Aspell/bin/aspell.exe")
        (t
         (if (file-executable-p "/usr/bin/hunspell")
             "/usr/bin/hunspell"
           "/usr/bin/aspell"))))

(my-file-executable-p ispell-program-name)

(when (and ispell-program-name
           (file-executable-p ispell-program-name))

  (require 'ispell)

  (eval-after-load "ispell"
    ;; so that following modifications won't be lost when ispell is loaded
    '(progn

       ;; default dictionary to use (if `ispell-local-dictionary' is nil)
       (setq ispell-dictionary "fr_FR")

       ;; save the personal dictionary without confirmation
       (setq ispell-silently-savep t)

       ;; extra switches to pass to the `ispell' program
       ;; TODO Check they are right!
       (setq ispell-extra-args
             (cond ((equal ispell-program-name "/usr/bin/hunspell")
                    '())
                    ;; '("-a" "-i" "utf-8"))
                                        ; aspell doesn't understand `-i
                                        ; utf-8', hunspell needs it
                   (t
                    '("--sug-mode=ultra"))
                                        ; tell `aspell' to speed up, though
                                        ; this reduces somewhat the quality of
                                        ; its suggestions. According to the
                                        ; `aspell' documentation:
                                        ;
                                        ; - "ultra" is the fastest suggestion
                                        ;   mode, which is still twice as slow
                                        ;   as `ispell'.
                                        ;
                                        ; - If your machine is fast enough, a
                                        ;   better option might be to try
                                        ;   "fast" mode, which is twice as
                                        ;   slow as "ultra", but more
                                        ;   accurate.
                                        ;
                                        ; - The "normal" mode, which is the
                                        ;   `aspell' default, is even more
                                        ;   accurate, but is reportedly 10
                                        ;   times slower than "fast" mode.
                   ))

    ;; redefine the list of installed dictionaries
    ;; customize to ("-B" "-d" "spanish") or ("-C" "-d" "dutch") if
    ;; aliases are needed for the file names
    ;; FIXME This variable is reset once latter in this .emacs file!!!
    (setq ispell-dictionary-alist
          ;; those not here will be "undefined dictionary"
          '(
            ;; default
            (nil
             "[A-Za-z]" "[^A-Za-z]"
             "[']" nil ("-B") nil iso-8859-1)

            ;; Yankee English
            ("en_US"
             "[A-Za-z]" "[^A-Za-z]"
             "[']" nil ("-B") nil utf-8)

            ;; Spanish mode
            ("castellano"
             "[a-zÃ±Ã¡Ã Ã©Ã¨Ã­Ã¬Ã³Ã²ÃºÃ¹Ã¼A-ZÃ‘ÃÃ€Ã‰ÃˆÃÃŒÃ“Ã’ÃšÃ™Ãœ]" "[^a-zÃ±Ã¡Ã Ã©Ã¨Ã­Ã¬Ã³Ã²ÃºÃ¹Ã¼A-ZÃ‘ÃÃ€Ã‰ÃˆÃÃŒÃ“Ã’ÃšÃ™Ãœ]"
             "[-]" nil ("-B") "~tex" iso-8859-1)

            ;; standard French
            ("fr_FR"
             "[a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]" "[^a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]"
             "[-']" t nil "~list" utf-8)

            ;; Nederlands.aff
            ("nederlands"
             "[a-zÃ Ã¢Ã§Ã«Ã®Ã¯Ã´Ã¹Ã»Ã¼A-ZÃ€Ã‚Ã‡Ã‹ÃŽÃÃ”Ã™Ã›Ãœ]" "[^a-zÃ Ã¢Ã§Ã«Ã®Ã¯Ã´Ã¹Ã»Ã¼A-ZÃ€Ã‚Ã‡Ã‹ÃŽÃÃ”Ã™Ã›Ãœ]"
             "[']" t ("-C") nil iso-8859-1)
            ))

    ;; `aspell' extensions should *not* be used
    (setq ispell-really-aspell nil)

    ;; `hunspell' extensions should be used
    (setq ispell-really-hunspell t)

    ;; ;; solve the problem of words separated by `-' flagged as erroneous by
    ;; ;; removing the `-' from the value of otherchars
    ;; (if (fboundp 'ispell-get-decoded-string)
    ;;     (defun ispell-get-otherchars ()
    ;;       (replace-regexp-in-string "-" "" (ispell-get-decoded-string 3))))

    ;; from Alex Schroeder
    (defun my-change-dictionary ()
      "Change the dictionary."
      (interactive)
      (let ((dict (or ispell-local-dictionary ispell-dictionary)))
        (setq dict (if (string= dict "fr_FR") "en_US" "fr_FR"))
        (message "Switched to %S" dict)
        (sit-for 0.4)
        (ispell-change-dictionary dict)
        (when flyspell-mode
          (flyspell-delete-all-overlays)
          (flyspell-buffer))))

    ;; key bindings
    (global-set-key (kbd "<f7>") 'ispell-word)
    (global-set-key (kbd "<S-f7>") 'my-change-dictionary)
    (global-set-key (kbd "<C-f7>") 'ispell-change-dictionary)

    ))


    ;; on-the-fly spelling checking
    (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)

    ;; don't consider that a word repeated twice is an error
    (setq flyspell-mark-duplications-flag nil)

    ;; enable the likeness criteria
    (setq flyspell-sort-corrections nil)

    ;; don't use `M-TAB' to correct word (only use `C-.')
    (setq flyspell-use-meta-tab nil)

    ;; `flyspell-auto-correct-word' is bound to `C-.'
    ;; Press it one time to correct the word under the cursor.
    ;; If several spellings are possible, they appear in the minibuffer. Just
    ;; keep hitting `C-.' to replace the word with the successive suggestions.

    ;; dash character (`-') is considered as a word delimiter
    (setq flyspell-consider-dash-as-word-delimiter-flag t)

    (defun my-turn-on-flyspell-french ()
      "Unconditionally turn on flyspell-mode (in French) and call
`flyspell-buffer'."
      (interactive)
      (flyspell-mode 1) ;; instead of just toggling the mode
      (ispell-change-dictionary "fr_FR")
      (flyspell-buffer))

    (defun my-turn-on-flyspell-english ()
      "Unconditionally turn on flyspell-mode (in American English)
and call `flyspell-buffer'."
      (interactive)
      (flyspell-mode 1)
      (ispell-change-dictionary "en_US")
      (flyspell-buffer))

    ;; turn on `flyspell' when changing a buffer which is unmodified
    (when
        (or (file-readable-p "/usr/share/myspell/dicts/fr_FR.aff") ; hunspell
            (file-readable-p "/usr/lib/aspell/francais.alias")
            (when running-ms-windows
                (file-readable-p
                 "C:/Program Files/Aspell/dict/francais.alias")))
                                        ; check that the French dictionary
                                        ; can be opened for reading

        (defvar my-flyspell-major-mode-list
          '(html-mode
            latex-mode
            message-mode
;;;             nuweb-mode  (emmerdant: je sauve, compile, et recheck spell!)
            nxml-mode
;;            org-mode  ; FIXME Problem with flyspell (.emacs is *completely* screened...)
            text-mode))

        (add-hook 'first-change-hook
                  #'(lambda ()
                      (when (and (memq major-mode my-flyspell-major-mode-list)
                                 (not flyspell-mode))
                        (my-turn-on-flyspell-french)))))

;; Org mode is derived from outline-mode, which is derived from text mode.
;; A derived mode runs all the hooks from the parent modes.
;;
;; I don't know how to turn this off, but you can work around this
;; by changing the function you put into the text-mode-hook:
;;
;; (defun my-turn-on-flyspell-not-in-org-though ()
;;   (or (eq major-mode 'org-mode) (turn-on-flyspell)))


;;;     ;; don't print messages for every word (when checking the entire buffer)
;;;     ;; as it causes an enormous slowdown
;;;     (setq flyspell-issue-message-flag nil)

    ;; flyspell comments and strings in programming modes
    ;; (preventing it from finding mistakes in the code)
    (add-hook 'autoconf-mode-hook   'flyspell-prog-mode)
    (add-hook 'autotest-mode-hook   'flyspell-prog-mode)
    (add-hook 'c++-mode-hook        'flyspell-prog-mode)
    (add-hook 'c-mode-hook          'flyspell-prog-mode)
    (add-hook 'cperl-mode-hook      'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
    (add-hook 'makefile-mode-hook   'flyspell-prog-mode)
    (add-hook 'nxml-mode-hook       'flyspell-prog-mode)
    (add-hook 'python-mode-hook     'flyspell-prog-mode)
    (add-hook 'sh-mode-hook         'flyspell-prog-mode)
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode t)
;; (add-hook 'java-mode-common-hook 'flyspell-prog-mode t)

    ;; spell-check your XHTML
    (eval-after-load "flyspell"
      '(progn
         (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))))

    ;; TODO Have a look at `diction' (style and grammar for English)


;; pull definitions from Google and display them in a buffer
(when (try-require 'google-define)
  (global-set-key (kbd "C-c D") 'google-define))


(defun google-define-word-or-phrase (query)
  (interactive "sInsert word or phrase to search: ")
  (let* ((url (concat "http://www.google.com.pe/search?hl=en&q=define%3A"
                      (replace-regexp-in-string " " "+" query)))
         (definition
           (save-excursion
             (with-temp-buffer
               (mm-url-insert url)
               (goto-char (point-min))
               (if (search-forward "No definitions found of " nil t)
                   "No definitions found"
                 (buffer-substring (search-forward "<li>")
                                   (- (search-forward "<") 1)))))))
    (message "%s: %s" query definition)))
(global-set-key (kbd "C-c d") 'google-define-word-or-phrase)
;; TODO This seems to be the first definition of the list returned by `google-define'


;; excellent!
(defun answers-define ()
  "Look up the word under cursor in a browser."
  (interactive)
  (browse-url
   (concat "http://www.answers.com/main/ntquery?s="
           (thing-at-point 'word))))


(when (try-require 'dictionary-FIXME)
    (load "dictionary-init")

    ;; server contacted for searching the dictionary
;;    (setq dictionary-server "localhost")

    ;; connect via a HTTP proxy (using the CONNECT command)
    (setq dictionary-use-http-proxy t)

    ;; name of the HTTP proxy to use
    (setq dictionary-proxy-server "hellman")

    ;; port of the proxy server
    (setq dictionary-proxy-port 8080)

    ;; ask for a new word to search
    (global-set-key (kbd "C-c s") 'dictionary-search)

    ;; ask for a pattern and list all matching words
    (global-set-key (kbd "C-c m") 'dictionary-match-words))


(when (try-require 'dictem-FIXME)
    (setq dictem-server "localhost")
    (dictem-initialize)
    (define-key mode-specific-map [?s] 'dictem-run-search)

    (define-key dictem-mode-map [tab] 'dictem-next-link)
    (define-key dictem-mode-map [(backtab)] 'dictem-previous-link)

    ;; For creating hyperlinks on database names and found matches.
    ;; Click on them with `mouse-2'
    (add-hook 'dictem-postprocess-match-hook
              'dictem-postprocess-match)

    ;; For highlighting the separator between the definitions found.
    ;; This also creates hyperlink on database names.
    (add-hook 'dictem-postprocess-definition-hook
              'dictem-postprocess-definition-separator)

    ;; For creating hyperlinks in dictem buffer that contains definitions.
    (add-hook 'dictem-postprocess-definition-hook
              'dictem-postprocess-definition-hyperlinks)

    ;; For creating hyperlinks in dictem buffer that contains information
    ;; about a database.
    (add-hook 'dictem-postprocess-show-info-hook
              'dictem-postprocess-definition-hyperlinks))


(message "20 Commands for Fixing Typos... Done"))

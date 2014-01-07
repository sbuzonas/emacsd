;;** 18 Controlling the (info "(emacs)Display")

(when section-display (message "18 Controlling the Display...")

;;*** 18.1 (info "(emacs)Scrolling")

;; scroll line by line
(setq scroll-step 1)

;; better scrolling in Emacs (doing a `Pg Up' followed by a `Pg Dn' will
;; place the point at the same place)
(when (try-require 'pager)
    (global-set-key (kbd "<prior>") 'pager-page-up)
    (global-set-key (kbd "<next>") 'pager-page-down)
    (global-set-key (kbd "<M-up>") 'pager-row-up)
    (global-set-key (kbd "<M-down>") 'pager-row-down))


;;*** 18.8 (info "(emacs)Font Lock")

;; make buffer size irrelevant for fontification
(setq font-lock-maximum-size nil)

(XEmacs
    ;; stop showing that annoying progress bar when fontifying
    (setq progress-feedback-use-echo-area nil)

    ;; enable Font Lock mode
    (font-lock-mode))

;; highlight non-breaking spaces
(GNUEmacs
    (require 'disp-table)
    (aset standard-display-table
          (make-char 'latin-iso8859-1 (- ?\240 128))
          (vector (+ ?\267 (* 524288 (face-id 'nobreak-space))))))

;; special words
;; XXX add `fatal', and `Undefined'
(setq keywords-critical-pattern
      "\\(BUGS\\|FIXME\\|TODO\\|todo\\|XXX\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
(make-face 'keywords-critical)
(GNUEmacs (set-face-attribute 'keywords-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-org-critical-pattern
      "\\(BUGS\\|FIXME\\|XXX\\|[^*] TODO\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
                                        ; smaller subset of keywords for
                                        ; ensuring no conflict with Org mode
                                        ; TODO keywords

;; FIXME Highlighting all special keywords but "TODO" in Org mode is already a
;; good step. Though, a nicer integration would be that "TODO" strings in the
;; headings are not touched by this code, and that only "TODO" strings in the
;; text body would be. Don't know (yet) how to do that...
(make-face 'keywords-org-critical)
(GNUEmacs (set-face-attribute 'keywords-org-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-normal-pattern "\\([Ww][Aa][Rr][Nn][Ii][Nn][Gg]\\)")
(make-face 'keywords-normal)
(GNUEmacs (set-face-attribute 'keywords-normal nil
                              :foreground "magenta2" :background "yellow"))

;; set up highlighting of special words for proper selected major modes only
(dolist (mode '(fundamental-mode
                svn-log-view-mode
                text-mode))  ; no interference with Org mode (which derives
                             ; from text-mode)
  (font-lock-add-keywords mode
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; set up highlighting of special words for Org mode only
(dolist (mode '(org-mode))
  (font-lock-add-keywords mode
    `((,keywords-org-critical-pattern 1 'keywords-org-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
;;;   (font-lock-mode -1)
;;;   (font-lock-mode 1)
  (font-lock-add-keywords nil
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))
;; FIXME                        0                  t

;; set up highlighting of special words for selected major modes *and* all
;; major modes derived from them
(dolist (hook '(c++-mode-hook
                c-mode-hook
                change-log-mode-hook
                cperl-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                java-mode-hook
                latex-mode-hook
                lisp-mode-hook
                makefile-mode-hook
                message-mode-hook
                php-mode-hook
                python-mode-hook
                sh-mode-hook
                shell-mode-hook
                ssh-config-mode-hook))
  (add-hook hook 'fontify-keywords))


;;*** 18.9 (info "(emacs)Highlight Interactively") by Matching

;; You can use `hi-lock-mode' to highlight words:
;;     `M-x hi-lock-mode RET'
;;     `C-x w h <match> RET hi-blue RET'
;; You can also write your settings to the buffer you're using with
;; `C-x w b', and read them back in again next time with `C-x w i'.


;; TODO Have a look at http://www.emacswiki.org/emacs/color-moccur.el for
;; searching regexp in buffer


(GNUEmacs
    ;; "identical token highlighting" commands
    (when (try-require 'highlight)

        (defface hlt-1 '((t (:background "#FFFFA0"))) nil)
        (defface hlt-2 '((t (:background "#A0FFA0"))) nil)
        (defface hlt-3 '((t (:background "#A0FFFF"))) nil)
        (defface hlt-4 '((t (:background "#FFA0FF"))) nil)
        (defface hlt-5 '((t (:background "#FFA0A0"))) nil)
        (defface hlt-6 '((t (:background "#FFFFA0"))) nil)
        (defface hlt-7 '((t (:background "#A0FFA0"))) nil)
        (defface hlt-8 '((t (:background "#A0FFFF"))) nil)
        (defface hlt-9 '((t (:background "#FFA0FF"))) nil)
        (defface hlt-10 '((t (:background "#FFA0A0"))) nil)

        (global-set-key (kbd "C-S-p") 'hlt-previous-highlight)
        (global-set-key (kbd "C-S-n") 'hlt-next-highlight)

        (defun hlt-highlight-current-word ()
          (interactive)
          (let ((var_name (current-word t)))
            (when var_name
              (save-excursion
                (hlt-highlight-regexp-region
                 (point-min)
                 (point-max)
                 (regexp-quote var_name))))))

        ;; emulation of Vim's `*' search
        (global-set-key (kbd "C-*") 'hlt-highlight-current-word)
        ))

;; ;; bind the hi-lock commands to more finger-friendly sequences
;; (define-key hi-lock-map (kbd "C-z i") 'hi-lock-find-patterns)
;; (define-key hi-lock-map (kbd "C-z p") 'highlight-phrase)
;; (define-key hi-lock-map (kbd "C-z r") 'unhighlight-regexp)

;; (define-key hi-lock-map (kbd "C-z h") 'highlight-regexp)
;; (define-key hi-lock-map (kbd "C-z C-h") 'highlight-lines-matching-regexp)
;; (define-key hi-lock-map (kbd "C-z b") 'hi-lock-write-interactive-patterns)

;; ;; Highlight based on regexps
;; (global-set-key [M-f1] 'highlight-regexp)
;; (global-set-key [M-f2] 'highlight-lines-matching-regexp)
;; (global-set-key [M-f3] 'hi-lock-mode)
;; (global-set-key [M-f4] 'hi-lock-write-interactive-patterns)


;; ;; highlight current symbol
;; (when (try-require 'light-symbol)
;;   (light-symbol-mode))


;; highlight current symbol
(setq highlight-symbol-idle-delay 0.5)
(when (try-require 'highlight-symbol)
  (highlight-symbol-mode))





;;*** 18.11 (info "(emacs)Displaying Boundaries")

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)


;;*** 18.12 (info "(emacs)Useless Whitespace")

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)

;; ;; FIXME When turned on, Gnus becomes black and white only...
;; (when (try-require 'show-wspace)
;;   ;; Highlight tabs
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
;;
;;   ;; Highlight trailing whitespace
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)
;;
;;   ;; Highlight non-breaking spaces
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-hard-spaces))


;; FIXME Same problem as above
;; ;; highlight tabs in all modes
;; (add-hook 'font-lock-mode-hook
;;           '(lambda ()
;;              (font-lock-add-keywords
;;               nil
;;               '(("\t" 0 'trailing-whitespace prepend)))))

;; delete all the trailing whitespaces and tabs across the current buffer
(defun my-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

;; visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)


;;*** 18.14 (info "(emacs)Optional Mode Line") Features

;; show the line number in each mode line
(line-number-mode 1)

;; show the column number in each mode line
(column-number-mode 1)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)

(GNUEmacs
    ;; code for including abbreviated file paths in mode line
    (when (try-require 'mode-line)
        (mode-line-toggle-display nil)))


;;*** 18.15 How (info "(emacs)Text Display")ed

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

;; Bad interaction with CVSNT/diff/... (not yet understood)
;; ;; Remove or convert trailing ^M
;; (defun remove-trailing-ctrl-M ()
;;   "Propose to remove trailing ^M from a file."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (and (not (string-match ".gz$" (buffer-file-name)))
;;              (search-forward-regexp "\015$" nil t))
;;                                         ;: a ^M is found
;;         (if (or (= (preceding-char) ?\^J)
;;                 (= (following-char) ?\^J) )
;;             (if (y-or-n-p (format "Remove trailing ^M from %s? "
;;                                   (buffer-file-name)))
;;                 (progn (goto-char (point-min))
;;                        (perform-replace "\015" "" nil nil nil)
;;                        (pop-mark)
;;                        (save-buffer))
;;               (message "No transformation."))))))
;; (add-hook 'find-file-hooks 'remove-trailing-ctrl-M)


;;*** 18.16 The (info "(emacs)Cursor Display")

(GNUEmacs
    ;; using cursor color to indicate some modes (read-only, insert and
    ;; overwrite modes)
    (setq my-set-cursor-color-color "")
    (setq my-set-cursor-color-buffer "")

    (defun my-set-cursor-color-according-to-mode ()
      "Change cursor color according to some minor modes."
      (let ((color
             (if buffer-read-only "purple1"
               (if overwrite-mode "red"
                 "#15FF00"))))  ; insert mode
        (unless (and (string= color my-set-cursor-color-color)
                     (string= (buffer-name) my-set-cursor-color-buffer))
          (set-cursor-color (setq my-set-cursor-color-color color))
          (setq my-set-cursor-color-buffer (buffer-name)))))

    (add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode))

;; highlight columns 78 to 80 in some modes
(when (try-require 'column-marker)

    (dolist (hook '(emacs-lisp-mode-hook
                    cperl-mode-hook
                    shell-mode-hook
                    text-mode-hook
                    change-log-mode-hook
                    makefile-mode-hook
                    message-mode-hook
                    texinfo-mode-hook))
      (add-hook hook (lambda ()
                       (interactive)
                       (column-marker-1 78)
                       (column-marker-2 79)
                       (column-marker-3 80))))

    ;; use `C-c m' interactively to highlight with `column-marker-1-face'
    (global-set-key (kbd "C-c m") 'column-marker-1))


;;*** 18.17 (info "(emacs)Line Truncation")

;; respect the value of `truncate-lines' in all windows less than the full
;; width of the frame
(setq truncate-partial-width-windows nil)


;;*** 18.19 (info "(emacs)Display Custom")ization

;; see what I'm typing *immediately*
(setq echo-keystrokes 0.01)


;;*** Temporary Displays

;; make the help, apropos and completion windows the right height for their
;; contents
(GNUEmacs
    (temp-buffer-resize-mode t))  ; auto-fit the *Help* buffer to its contents

;; enhanced display of temporary windows (such as help buffers)
;; (try-require 'show-temp-buffer)  [bug with XEmacs 21.5]

(message "18 Controlling the Display... Done"))

;;** 13 (info "(emacs)Yanking")

(when section-yanking (message "13 Yanking...")

;;*** 13.1 The (info "(emacs)Kill Ring")

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))


;;*** 13.3 Yanking (info "(emacs)Earlier Kills")

;; interactively insert items from kill ring
(when (try-require 'browse-kill-ring)

    ;; string separating entries in the `separated' style
    (setq browse-kill-ring-separator
          "\n--separator------------------------------")

    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)

    ;; face in which to highlight the `browse-kill-ring-separator'
    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
                                        ; slate gray
    (setq browse-kill-ring-separator-face 'separator-face)

    ;; use `M-y' to invoke `browse-kill-ring'
    (browse-kill-ring-default-keybindings))

(message "13 Yanking... Done"))

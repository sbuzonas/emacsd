(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(defvar night-hour 18
  "When to start with night theme.")

(defvar day-hour 8
  "When to start with day theme.")

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-tol-list 'custom-theme-load-path path)))

;; Initialize dark theme
(require-package 'tango-2-theme)
(setq dark-theme 'tango-dark)

;; Initialize light theme
(require-package 'solarized-theme)
(require-package 'zenburn-theme)
(setq light-theme 'tsdh-dark)

(setq current-theme nil)

;; Dark theme
(defun use-dark-theme ()
  (interactive)
  (if (not (string= current-theme dark-theme))
      (progn
	(disable-theme light-theme)
	(load-theme dark-theme)
	(setq current-theme dark-theme)
	(when (boundp 'slbmeh/dark-font)
	  (set-face-attribute 'default nil :font slbmeh/dark-font)))))

;; Light theme
(defun use-light-theme ()
  (interactive)
  (if (not (string= current-theme light-theme))
      (progn
	(disable-theme dark-theme)
	(load-theme light-theme)
	(setq current-theme light-theme)
	(when (boundp 'slbmeh/light-font)
	  (set-face-attribute 'default nil :font slbmeh/light-font)))))

;; Toggle themes
(defun swap-theme-contrast ()
  (interactive)
  (if (string= current-theme dark-theme)
      (use-light-theme)
    (use-dark-theme)))

(global-set-key (kbd "<f9>") 'swap-theme-contrast)

(defun color-theme-timer ()
  "Sets color theme according to current time. Customize `night-hour' and `day-hour'."
  (interactive)
  (let ((hour (nth 2 (decode-time)))
	(minute (nth 1 (decode-time))))
    (if (or (>= hour night-hour) (< hour day-hour))
	(use-light-theme) ;; Night time color
      (use-dark-theme))))

(color-theme-timer)

(add-hook 'auto-save-hook 'color-theme-timer)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; provide pretty lambda replacement
(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\(lambda\\)\\>" (0 (prog1 ()
			     (compose-region (match-beginning 1)
					     (match-end 1)
					     ?λ))))))

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
       ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)

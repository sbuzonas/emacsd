;; Create the backup directory if it doesn't already exist
(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

;; We should have enough information to set up the load path now
(fg/add-to-load-path vendor-dir t t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Consider large files to be anything >100MB
(setq large-file-warning-threshold 100000000)

;; Answering just 'y' or 'n' is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode t)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 120 characters wide, not 72
(setq-default fill-column 120)

(provide 'fg-defaults)

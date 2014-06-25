;; Some initial default mode settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

(setq initial-scratch-message nil)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Consider large files to be anything >100MB
(setq large-file-warning-threshold 100000000)

;; Answering just 'y' or 'n' will do
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

;; Set line numbers when displayed in a gutter to be 4 characters wide followed by a line and a space
(setq linum-format"%4d \u2502 ")

;; Lines should be 120 characters wide, not 72
(setq-default fill-column 120)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me
(setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontigy org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Default user variables
(setq user-full-name "Steve Buzonas")
;; Set user-mail-address and user-website to carnegielearning if on a carnegielearning machine
(if (not (= 0 (call-process-shell-command "hostname" nil nil nil "| grep -q 'carnegielearning.com$'")))
    (progn
      (setq user-mail-address "steve@fancyguy.com")
      (setq user-website "http://www.stevebuzonas.com"))
  (setq user-mail-address "sbuzonas@carnegielearning.com")
  (setq user-website "http://www.carnegielearning.com"))

(add-to-list 'my-default-packages 'smex)
(add-to-list 'my-default-packages 'undo-tree)

(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile.lock$" . yaml-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock$" . yaml-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("composer.lock$" . json-mode))

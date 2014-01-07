;;* Fundamental Editing Commands

;;** 7 (info "(emacs)Basic") Editing Commands

(when section-basic (message "7 Basic Editing Commands...")

;;*** 7.1 (info "(emacs)Inserting Text")

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)


;;*** 7.2 (info "(emacs)Moving Point") Location

;; don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)

;; XEmacs default for moving point to a given line number
(GNUEmacs
    (global-set-key (kbd "M-g") 'goto-line))

(global-set-key (kbd "M-G") 'what-line)

;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; ;; XXX `M-up' and `M-down' are bound multiple times (to different things)!
;; (global-set-key (kbd "<M-up>") 'move-line-up)
;; (global-set-key (kbd "<M-down>") 'move-line-down)


;;*** 7.4 (info "(emacs)Basic Undo")ing Changes

;; undo some previous changes
(global-set-key (kbd "<f11>") 'undo)

;; redo the most recent undo
(when (try-require 'redo)
    (global-set-key (kbd "<S-f11>") 'redo))


;;*** 7.8 (info "(emacs)Continuation Lines")

(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)))


;;*** 7.11 (info "(emacs)Repeating") a Command

;; repeat last command passed to `shell-command'
(defun repeat-shell-command ()
  "Repeat most recently executed shell command."
  (interactive)
  (save-buffer)
  (or shell-command-history (error "Nothing to repeat."))
  (shell-command (car shell-command-history)))

(global-set-key (kbd "C-c j") 'repeat-shell-command)

(message "7 Basic Editing Commands... Done"))

;; Replace default "M-x" functionality
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; expose the old "M-x"
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Focus list of buffers when displayed
(defun list-buffers-and-other-window () (interactive) (call-interactively 'list-buffers) (call-interactively 'other-window))
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; Don't prompt which buffer to kill, automatically kill the current buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Swap default and regex search bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Bind resizing to arrow keys
;; http://emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Replace comment-dwim to comment out the region
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Git bindings
(global-set-key (kbd "C-x C-g") 'magit-status)
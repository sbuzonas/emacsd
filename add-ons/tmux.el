(defaddon tmux
  nil

  (message "Loaded tmux")
  (defun emacs-or-tmux-windmove (direction &optional ARG)
    "Move the window specified direction or delegate the action to tmux if emacs cannot"
    (interactive "P")
    (if (ignore-errors (funcall (intern (concat "emacs-windmove-" direction)) ARG))
        nil
      (funcall (intern (concat "tmux-windmove-" direction)))))

  ;; If we're not in tmux don't suggest we tell tmux to move when we can't
  (if (getenv "TMUX")
      (progn
        (defun tmux-windmove (direction)
          (interactive "P")
          (let ((tmux-cmd (concat "tmux select-pane -" (upcase (substring direction 0 1)))))
            (shell-command tmux-cmd)))

        (defun tmux-windmove-up ()
          (interactive)
          (tmux-windmove "up"))
        (defun tmux-windmove-down ()
          (interactive)
          (tmux-windmove "down"))
        (defun tmux-windmove-left ()
          (tmux-windmove "left"))
        (defun tmux-windmove-right ()
          (interactive)
          (tmux-windmove "right"))

        (defalias 'emacs-windmove-up 'windmove-up)
        (defalias 'emacs-windmove-down 'windmove-down)
        (defalias 'emacs-windmove-left 'windmove-left)
        (defalias 'emacs-windmove-right 'windmove-right)

        (defun emacs-or-tmux-windmove-up ()
          (interactive)
          (emacs-or-tmux-windmove "up"))

        (defun emacs-or-tmux-windmove-down ()
          (interactive)
          (emacs-or-tmux-windmove "down"))

        (defun emacs-or-tmux-windmove-left ()
          (interactive)
          (emacs-or-tmux-windmove "left"))

        (defun emacs-or-tmux-windmove-right ()
          (interactive)
          (emacs-or-tmux-windmove "right"))

        (define-key global-map [remap windmove-up] 'emacs-or-tmux-windmove-up)
        (define-key global-map [remap windmove-down] 'emacs-or-tmux-windmove-down)
        (define-key global-map [remap windmove-left] 'emacs-or-tmux-windmove-left)
        (define-key global-map [remap windmove-right] 'emacs-or-tmux-windmove-right))
    nil))

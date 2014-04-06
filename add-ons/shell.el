(defcustom addon-shell
  nil
  "Enable shell features"
  :type 'boolean
  :group 'features)

(when addon-shell
  (progn
    (add-to-list 'my-default-packages 'shell-command)
    (try-require 'shell)))

(after shell
  ;; bash-completion
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
            'bash-completion-dynamic-complete)

  ;; tab-completion for shell-command
  (try-require 'shell-command)

  ;; C-d to kill buffer if process is dead.
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eor-or-kill-buffer))))

(after shell-command
  (shell-command-completion-mode))

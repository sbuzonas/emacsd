(setq tmux-session-name 0)
(setq tmux-window-name 1)
(setq tmux-pane-number 2)

(defun tmux-exec (command)
  "Execute command in tmux pane"
  (interactive)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" tmux-session-name tmux-window-name tmux-pane-number command)))

(defun tmux-setup (x y z)
  "Setup global variables for tmux session, window, and pane"
  (interactive "sEnter tmux session name: \nsEnter tmux window name: \nsEnter tmux pane number: ")
  (setq tmux-session-name x)
  (setq tmux-window-name y)
  (setq tmux-pane-number z)
  (message "Tmux Setup, session name: %s, window name: %s, pane number %s" tmux-session-name tmux-window-name tmux-pane-number))

(defun tmux-exec-command ()
  "Execute command defined by `tmux-exec-command-name'"
  (interactive)
  (tmux-exec tmux-exec-command-name))

(defun tmux-exec-command-setup (command)
  (interactive "sEnter command to execute: ")
  (setq tmux-exec-command-name command)
  (message "Tmux Command Setup, command: %s" tmux-exec-command-name))

(global-set-key (kbd "<f6>") 'tmux-exec-command)

(provide 'tmux)

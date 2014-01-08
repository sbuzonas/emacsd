(require 'tmux)

(defun tmux-exec-vagrantup ()
    "Execute 'vagrant up' in tmux pane"
      (interactive)
        (tmux-exec "vagrant up"))

(defun tmux-exec-vagrantprovision ()
    "Execute 'vagrant provision' in tmux pane"
      (interactive)
        (tmux-exec "vagrant provision"))

(global-set-key (kbd "C-x v") 'tmux-exec-vagrantprovision)
(global-set-key (kbd "C-x C-v") 'tmux-exec-vagrantup)

(provide 'vagrant)

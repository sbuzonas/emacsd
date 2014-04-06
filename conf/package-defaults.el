(after 'smex
  (smex-initialize)
  ;; Replace default "M-x" functionality
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "S-M-x") 'smex-major-mode-commands)
  ;; expose the old "M-x"
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  ;; Use C-x C-m to do M-x per Steve Yegge's advice
  (global-set-key (kbd "C-x C-m") 'smex)
  
  ;; Modify smex so that typing a space will insert a hyphen '-' like normal M-x
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it))

  ;; Tell smex to update cache when new functions are loaded
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))
  (add-hook 'after-load-functions 'smex-update-after-load))

(after 'undo-tree
  (global-undo-tree-mode)
  (setq undo-tree-outer-limit 36000000)
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

(try-require 'smex)
(try-require 'undo-tree)

(defaddon smex
  "Provides integration for the M-x ehnancement package Smex."
  (fg/require-package 'smex)

  ;; convert spaces to hyphens in the smex minibuffer
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      ad-do-it))

  (after smex
    (smex-initialize)
    
    ;; update cache when new functions are loaded
    (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache)
	(smex-update)))
    (add-hook 'after-load-functions 'smex-update-after-load)
    
    ;; Replace default "M-x" functionality
    (global-set-key [(meta ?x)] 'smex)
    (global-set-key [(meta ?X)] 'smex-major-mode-commands)
    
    ;; Expose old M-x functionality
    (global-set-key [(control ?c) (control ?c) (meta ?x)] 'execute-extended-command)

    ;; Let enter after a C-x or C-X interact with smex
    (global-set-key [(control ?x) (control ?m)] 'smex)
    (global-set-key [(control ?X) (control ?m)] 'smex-major-mode-commands))
    
  (require 'smex))

(defaddon smex
  "Provides integration for the M-x enhancement package Smex."
  (nomad/require-package 'smex)

  (defun smex--space-inserts-hyphen (orig-fun &rest args)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      (apply orig-fun args)))
  (advice-add 'smex :around #'smex--space-inserts-hyphen)

  (defun smex--update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))
  (add-hook 'after-load-functions 'smex--update-after-load)

  (global-set-key [(meta ?x)] 'smex)
  (global-set-key [(meta ?X)] 'smex-major-mode-commands)

  (global-set-key [(control ?c) (control ?c) (meta ?x)] 'executa-extended-command)

  (global-set-key [(control ?x) (control ?m)] 'smex)
  (global-set-key [(control ?X) (control ?m)] 'smex-major-mode-commands)

  (smex-initialize))

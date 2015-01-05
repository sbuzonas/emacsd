(defaddon ido
  "Configuration for ido completion."
  (fg/require-packages '(flx-ido ido-vertical-mode ido-at-point ido-ubiquitous))

  (defun fg/setup-ido ()
    (define-key ido-file-completion-map [?~] (lambda ()
					     (interactive)
					     (cond ((looking-back "~/") (insert "projects/"))
						   ((looking-back "/") (insert "~/"))
						   (:else (call-interactively 'self-insert-command)))))
    (define-key ido-file-completion-map [(control ?w)] 'ido-delete-backward-updir)
    (define-key ido-file-completion-map [(control ?x) (control ?w)] 'ido-copy-current-file-name))
  (add-hook 'ido-setup-hook 'fg/setup-ido)

  (set-default 'imenu-auto-rescan t)

  (after ido
    (ido-mode t))
  
  (after flx-ido
    (flx-ido-mode 1)
    (setq ido-use-faces nil))

  (after ido-vertical-mode
    (ido-vertical-mode))

  (after ido-at-point
    (ido-at-point-mode))

  (after ido-ubiquitous
    (ido-ubiquitous-mode 1)
    (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
      `(eval-after-load ,package
	 '(defadvice ,cmd (around ido-ubiquitous-new activate)
	    (let ((ido-ubiquitous-enable-compatibility nil))
	      ad-do-it))))
    (ido-ubiquitous-use-new-completing-read webjump 'webjump)
    (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
    (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet))

  (add-hook 'after-ido-addon-hook (lambda ()
				    (require 'ido)
				    (require 'flx-ido)
				    (require 'ido-vertical-mode)
				    (require 'ido-at-point)
				    (require 'ido-ubiquitous))))

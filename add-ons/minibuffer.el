(defaddon minibuffer
  "Minibuffer enhancements"
  (add-hook 'minibuffer-setup-hook (lambda ()
				     (if (eq this-command 'eval-expression)
					 (smartparens-mode))))
  (setq history-length 1000)
  (setq savehist-file (expand-file-name "history" shared-dir))
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1))

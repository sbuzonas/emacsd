(defaddon elisp
  "Provides extensions for emacs-lisp-mode."
  (autoload 'hdefd-highlight-mode "hl-defined")

  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (hdefd-highlight-mode)
				    (turn-on-eldoc-mode)))

  (add-hook 'minibuffer-setup-hook (lambda ()
				     (if (eq this-command 'eval-expression)
					 (smartparens-mode))))
  
  (setq flycheck-emacs-lisp-load-path "inherit"))

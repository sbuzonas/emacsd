(defaddon flyspell
  "Spell checking"

  (setq ispell-program-name "aspell")

  (add-hook 'text-mode (lambda ()
			 (flyspell-mode 1)))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

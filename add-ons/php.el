(defaddon php
  "Provides extensions developing with PHP."
  (fg/require-packages '(php-mode php-extras))

  (defvar php-tempo-tags nil
    "Tempo tags for PHP mode")

  (defun fg/php-mode-init ()
    (when (and buffer-file-name
	       (fboundp 'gtags-create-or-update))
      (gtags-mode t)
      (gtags-create-or-update))
    (eldoc-mode))
  (add-hook 'php-mode-hook 'fg/php-mode-init))

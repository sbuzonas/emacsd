(defun load-php-settings ()
  (setq indent-tabs-mode t)
  (setq tab-stop-list (number-sequence (3 210 3)))
  (setq tab-width 4)
  (setq indent-line-function 'insert-tab))

(defun php-define-tempo-templates ()
  nil)

(add-hook 'php-mode-hook 'load-php-settings)
(add-hook 'after-php-addon-hook 'php-define-tempo-templates)

(defun load-php-settings ()
  (setq indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence '(4 210 4)))
  (setq tab-width 4)
  (setq indent-line-function 'c-indent-line)
  (c-set-offset 'case-label '+))

(defun php-define-tempo-templates ()
  nil)

(add-hook 'php-mode-hook 'load-php-settings)
(add-hook 'after-php-addon-hook 'php-define-tempo-templates)

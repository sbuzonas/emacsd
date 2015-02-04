(defun fg/json-mode-defaults ()
  (make-variable-buffer-local 'js-indent-level)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq js-indent-level 4)
  (setq standard-indent 4))

(add-hook 'json-mode-hook 'fg/json-mode-defaults)

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js2-basic-offset 2)

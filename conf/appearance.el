(defun configure-toolbars ()
  (let ((mode-value (if window-system 1 -1)))
    (when (fboundp 'menu-bar-mode) (menu-bar-mode mode-value))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode mode-value))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode mode-value))))

(add-hook 'after-appearance-addon-hook
	  (lambda ()
	    (configure-toolbars)
	    (add-hook 'appearance-frame-setup 'configure-toolbars)))

(setq default-theme 'mustard)

(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

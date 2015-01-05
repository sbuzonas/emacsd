(defaddon programming
  "Provides generic extensions for programming modes."
  (fg/require-packages '(smartparens flycheck))
  (defun fg/font-lock-annotations ()
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
	    1 font-lock-warning-face t))))
    
  (defun fg/prog-mode-hook ()
    "Programming mode defaults"
    (require 'which-func)
    (which-function-mode t)
    (fg/font-lock-annotations))

  (add-hook 'prog-mode-hook 'fg/prog-mode-hook))

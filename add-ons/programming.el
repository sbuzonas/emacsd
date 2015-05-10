(defaddon programming
  "Provides generic extensions for programming modes."
  (fg/require-packages '(smart-tabs-mode smartparens flycheck))
  (defun fg/font-lock-annotations ()
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
	    1 font-lock-warning-face t))))

  (defun fg/font-lock-lambda ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
	    (0 (progn (compose-region (match-beginning 1) (match-end 1)
				      ,(make-char 'greek-iso8859-7 107))
		      nil))))))
    
  (defun fg/prog-mode-hook ()
    "Programming mode defaults"
    (require 'which-func)
    (fg/font-lock-lambda)
    (smart-tabs-mode)
    (which-function-mode t)
    (fg/font-lock-annotations))

  (add-hook 'prog-mode-hook 'fg/prog-mode-hook))

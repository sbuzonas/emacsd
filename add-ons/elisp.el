(defaddon elisp
  "Provides extensions for emacs-lisp-mode."
  (autoload 'hdefd-highlight-mode "hl-defined")
  (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode))

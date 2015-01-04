(defaddon lisp
  "Additional functionality for programming with Lisp"
  (fg/require-package 'rainbow-delimiters)

  (defun fg/lisp-mode-hook ()
    "Lisp mode defaults"
    (smartparens-strict-mode t)
    (rainbow-delimiters-mode t))
  
  (add-hook 'lisp-mode 'fg/lisp-mode-hook)

  (define-key read-expression-map (kbd "TAB") 'completion-at-point))
  

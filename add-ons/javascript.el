(defaddon javascript
  "Additional functionality for programming with javascript"
  (fg/require-packages '(js2-mode js-comint))
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (setq inferior-js-program-command "rhino")

  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  
  (defun fg/define-js2-mode-keys ()
    (local-set-key (kbd "RET") 'newline-and-indent)
    (local-set-key [(control ?x) (control ?e)] 'js-send-last-sexp)
    (local-set-key [(control meta ?x)] 'js-send-last-sexp-and-go)
    (local-set-key [(control ?c) ?b] 'js-send-buffer)
    (local-set-key [(control ?c) (control ?b)] 'js-send-buffer-and-go)
    (local-set-key [(control ?c) ?l] 'js-load-file-and-go))
  
  (add-hook 'js2-mode-hook 'fg/define-js2-mode-keys))

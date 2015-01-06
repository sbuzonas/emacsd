(defaddon javascript
  "Additional functionality for programming with javascript"
  (fg/require-packages '(js2-mode js-comint))
  (setq inferior-js-program-command "rhino")

  (defun fg/define-js2-mode-keys ()
    (local-set-key [(control ?x) (control ?e)] 'js-send-last-sexp)
    (local-set-key [(control meta ?x)] 'js-send-last-sexp-and-go)
    (local-set-key [(control ?c) ?b] 'js-send-buffer)
    (local-set-key [(control ?c) (control ?b)] 'js-send-buffer-and-go)
    (local-set-key [(control ?c) ?l] 'js-load-file-and-go))
  
  (add-hook 'js2-mode-hook 'fg/define-js2-mode-keys))


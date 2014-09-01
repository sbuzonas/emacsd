(defun slbmeh-define-alternatives (var alternatives)
  "Provide a command to toggle a variable value between various alternatives.
When it shall toggle between nil and other values: put nil as first value..."
  (let* ((alternatives-list (intern (concat (symbol-name var) "-alternative-list")))
         (toggle-function (intern (concat "toggle-" (symbol-name var))))
         (toggle-function-doc (concat "Toggles the values of " (symbol-name var))))
    (set alternatives-list (cons var alternatives))
    (set var (car alternatives))
    (fset toggle-function
          `(lambda (&optional arg)
             ,toggle-function-doc
             (interactive "P")
             (cond ((car-safe arg)
                    (message "%s: %s" ,(symbol-name var) ,var))
                   (t
                    (let ((next (cadr (memq ,var ,alternatives-list))))
                      (setq ,var (or next (cadr ,alternatives-list))))
                    (message "set %s to %s " ,(symbol-name var) ,var)))))))

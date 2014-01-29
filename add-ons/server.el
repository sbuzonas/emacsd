(defcustom addon-server
  nil
  "Enable server features"
  :type 'boolean
  :group 'features)

(when addon-server
  (progn
    (require 'server)
    (unless (server-running-p) (server-start))
    (add-hook 'server-switch-hook
              (lambda ()
                (when (current-local-map)
                  (use-local-map (copy-keymap (current-local-map))))
                (when server-buffer-clients
                  (local-set-key (kbd "C-x k") 'server-edit))))
    (custom-set-variables '(server-kill-new-buffers t))
    (setq server-visit-hook (quote (save-place-find-file-hook)))))

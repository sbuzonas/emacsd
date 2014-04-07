(defaddon php
  nil
  (require-package 'php-mode)
  (require-package 'gtags)

  (defun gtags-create-or-update ()
    "create or update the gnu global tag file"
    (interactive)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
        (let ((olddir default-directory)
              (topdir (read-directory-name
                       "gtags: top of source tree:" default-directory)))
          (cd topdir)
          (shell-command "gtags && echo 'created tagfile'")
          (cd olddir)) ; restore
      ;; tagfile already exists; update it
      (shell-command "global -u && echo 'updated tagfile'")))

  (require 'skeleton)

  (add-hook 'php-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c n") 'linum-mode)
              (require 'gtags)
              (gtags-mode t)
              (gtags-create-or-update)))

  (add-hook 'gtags-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'gtags-find-tag)
              (local-set-key (kbd "M-,") 'gtags-find-rtag)))

  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") 'gtags-select-tag)
              (setq-local hl-line-face 'underline)
              (hl-line-mode 1))))


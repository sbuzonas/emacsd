(defaddon php
  nil
  (require-package 'php-mode)
  (require-package 'php-extras)
  (require-package 'gtags)

;;  (require 'fancyguy-tempo)
;;  (setq tempo-interactive t)

  (defvar php-tempo-tags nil
    "Tempo tags for PHP mode")

  (defvar php-tempo-keys-alist nil
    "")

;;  (add-hook 'php-mode-hook (lambda ()
;;                             (setq indent-tabs-mode t)
;;                             (setq tab-stop-list (number-sequence 4 200 4))
;;                             (setq tab-width 4)
;;                             (setq indent-line-function 'insert-tab)))

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

  (add-hook 'php-mode-hook
            (lambda ()
              (when buffer-file-name
                (require 'gtags)
;;              (local-set-key (kbd "C-c t") 'tempo-complete-tag)
;;              (tempo-use-tag-list 'php-tempo-tags)
                (gtags-mode t)
                (gtags-create-or-update))
              (which-func-mode)
              (eldoc-mode)))

  (setq initial-major-mode 'php-mode)

  (add-hook 'gtags-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'gtags-find-tag)
              (local-set-key (kbd "M-,") 'gtags-find-rtag)))

  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") 'gtags-select-tag)
              (setq-local hl-line-face 'underline)
              (hl-line-mode 1))))


;; Write backup files to own directory
(setq backup-directory-alist
      `((".*" . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Write autosave files to own directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "backups")) t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

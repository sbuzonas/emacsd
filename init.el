(message "* --[ Loading Emacs init.el ]--")
(setq emacs-load-start-time (current-time))

;; Some initial default mode settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; turn on Common Lisp support
(require 'cl)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(require 'load-path)

;; Keep emacs Custom-settings in separate file
(message "* --[ Loading settings set by Custom ]--")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load environment specific macros
(require 'environment)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." .,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Initialize default packages
(load-config 'packages)

;; Require appearance early, but package configuration is needed
(message "* --[ Loading appearance settings ]--")
(require 'appearance)

(message "* --[ Installing defined packages ]--")
;; Install all of our default packages
(condition-case nil
    (packages-install)
  (error
    (package-refresh-contents)
    (packages-install)))

(message "* --[ Emacs initialization complete ]--"
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))

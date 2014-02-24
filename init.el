(message "* --[ Loading Emacs init.el ]--")
(setq emacs-load-start-time (current-time))

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

(load-config 'defaults)

(load-config 'keybindings)

(load-config 'mouse)

(load-config 'latex)

(MacOSX
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(TMUX
 (require 'vagrant))

;; Make C-a jump between start of line and start of indentation
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Replace comment-dwim with ability to toggle comment on region
(defun region-active-p () (and transient-mark-mode mark-active))
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment the current line.
Replaced default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg))
  (next-line)
  (back-to-indentation-or-beginning))
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Define the features group for Custom to allow addons to show their states
(defgroup features nil
  "Group of features allowing to toggle their configuration"
  :group 'emacs)
(load-directory (expand-file-name "add-ons" user-emacs-directory))

(message "* --[ Installing defined packages ]--")

;; Install all of our default packages
(condition-case nil
    (packages-install)

  (error
    (progn
      (message "Error installing packages, attempting to refresh list.")
      (package-refresh-contents)
      (packages-install))))

(message "* --[ Emacs initialization complete ]--")
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))

(message "Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))
(sit-for 1)

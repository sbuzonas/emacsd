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
(message "* --[ Loading environment libraries ]--")
(require 'environment)

;; Load package management library
(message "* --[ Initializing package management ]--")
(require 'package-management)

;; Require appearance early, but package configuration is needed
(message "* --[ Loading appearance functionality ]--")
(require 'appearance)

(message "* --[ Loading navigation library ]--")
(require 'navigation)

(message "* --[ Loading buffer manipulations ]--")
(require 'buffer-manipulation)

(message "* --[ Initializing org-mode ]--")
(require 'org)

(message "* --[ Initializing add-ons ]--")
(require 'add-ons)

(message "* --[ Loading user configuration ]--")
(load-config 'defaults)
(load-config 'backups)
(load-config 'keybindings)
(load-config 'modeline)
(load-config 'mouse)
(load-config 'latex)

(MacOSX
 (load-config 'darwin))

;; Install all of our default packages
(message "* --[ Installing defined packages ]--")
(package-install-default-packages)

;; Load default package configuration
(load-config 'package-defaults)

;; Output completion messages
(message "* --[ Emacs initialization complete ]--")
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))

(message "Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))

(sit-for 1)

;; turn on Common Lisp support
(require 'cl)

;; Functions / Macros
(require 'fg-load)
(require 'fg-string)
(require 'fg-environment)

;; Variable Initialization / Early Configuration Bootstrap
(require 'fg-config)
(message "* -- [ Loading settings managed by Custom ] --")
(setq custom-file (expand-file-name "custom.el" load-emacs-dir)) ;; Dedicate a file for settings by Custom
(when (file-exists-p custom-file)
  (load custom-file))
;; Now that custom has been loaded, get user defined variables
(when (file-exists-p defaults-file)
  (load defaults-file))
(when (file-exists-p local-defaults-file)
  (load local-defaults-file))
(require 'fg-secrets)

;; Load configuration
(require 'fg-defaults)
(when (file-exists-p config-dir)
  (message "* -- [ Loading configuration files ] --")
  (mapc 'load (directory-files config-dir 't "^[^#].*el$")))

;; Extensions
(require 'fg-packages)
(require 'fg-modes)
(require 'fg-addons)

(provide 'fg-core)

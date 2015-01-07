;; turn on Common Lisp support
(require 'cl)

;; Functions / Macros
(require 'fg-load)
(require 'fg-string)
(require 'fg-environment)
(require 'fg-packages)
(MacOSX
 (fg/require-package 'exec-path-from-shell)
 (exec-path-from-shell-initialize))

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

(defun fg/load-secrets ()
  (require 'fg-secrets))

;; Add lisp directories to load path
(when (file-exists-p vendor-dir)
  (fg/add-to-load-path vendor-dir t t))
(when (file-exists-p site-lisp-dir)
  (fg/add-to-load-path site-lisp-dir t t))
(when (file-exists-p distro-lisp-dir)
  (fg/add-to-load-path distro-lisp-dir t t))

;; Load configuration
(require 'fg-defaults)
(when (file-exists-p config-dir)
  (message "* -- [ Loading configuration files ] --")
  (mapc 'load (directory-files config-dir 't "^[^#].*el$")))

;; Extensions
(require 'fg-modes)
(require 'fg-addons)

(provide 'fg-core)

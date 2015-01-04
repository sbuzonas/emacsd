(setq emacs-load-start-time (current-time))
(message "* -- [ Loading Emacs init.el ]--")

(defun profile-load (the-lib &rest args)
  (when (and the-lib debug-on-error)
    (message "* %#.3f -- [ Loading %s ] --" (time-to-seconds (time-since emacs-load-start-time)) the-lib)))

(advice-add 'load :before #'profile-load)

(when (version< emacs-version "24.1")
  (error "This configuration is built for GNU Emacs 24.1, found %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; turn on Common Lisp support
(require 'cl)

;; Define some standard directories to load from
(defvar load-emacs-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar core-dir (expand-file-name "core" load-emacs-dir)
  "The directory containing our lisp code")
(defvar config-dir (expand-file-name "conf" load-emacs-dir)
  "The directory with configuration files")
(defvar addons-dir (expand-file-name "add-ons" load-emacs-dir)
  "The directory with extensible functionality to toggle on and off.")
(defvar backup-dir (expand-file-name "backup" load-emacs-dir)
  "The directory to store backups and autosaves")
(defvar vendor-dir (expand-file-name "vendor" load-emacs-dir)
  "The directory where 3rd party elisp code is stored")

;; Define the names of the variable files
(defvar variables-file (expand-file-name "variables.el" config-dir)
  "The file containing primary variable definitions")
(defvar local-variables-file (expand-file-name "local-variables.el" config-dir)
  "The file containing primary variable definitions for the local machine")

;; Prevent emacs from touching this file by saving settings by Custom in a separate file
(message "* -- [ Loading settings managed by Custom ] --")
(setq custom-file (expand-file-name "custom.el" load-emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Now that custom has been loaded, get user defined variables
(when (file-exists-p variables-file)
  (load variables-file))
(when (file-exists-p local-variables-file)
  (load local-variables-file))

;; Load configuration files
(when (file-exists-p config-dir)
  (message "* -- [ Loading configuration files ] --")
  (mapc 'load (directory-files config-dir 't "^[^#].*el$")))

;; Create the backup directory if it doesn't already exist
(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

;; We should have enough information to set up the load path now
(add-to-list 'load-path core-dir)
(require 'fg-core)
(fg/add-to-load-path vendor-dir t t)

(require 'fg-packages)
(require 'fg-addons)

(advice-remove 'load 'profile-load)

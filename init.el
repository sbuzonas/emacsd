(when (version< emacs-version "24.1")
  (error "This configuration is built for GNU Emacs 24.1, found %s" emacs-version))

(setq emacs-load-start-time (current-time))
(message "* -- [ Loading Emacs init.el ]--")

(defun profile-load (the-lib &rest args)
  (when (and the-lib debug-on-error)
    (message "* %#.3f -- [ Loading %s ] --" (time-to-seconds (time-since emacs-load-start-time)) the-lib)))
(advice-add 'load :before #'profile-load)

(defun start-load-profiler ()
  (advice-remove 'load 'profile-load)
  (message "Emacs startup time: %d seconds."
	   (time-to-seconds (time-since emacs-load-start-time))))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define some standard directories to load from
(defvar load-emacs-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar core-dir (expand-file-name "core" load-emacs-dir)
  "The directory containing our lisp code")

;; Get the primary set of functions
(add-to-list 'load-path core-dir)
(require 'fg-core)

(add-hook 'after-init-hook 'stop-load-profiler)

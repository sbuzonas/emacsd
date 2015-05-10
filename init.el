(when (version< emacs-version "24.1")
  (error "This configuration is built for GNU Emacs 24.1, found %s" emacs-version))

(setq emacs-load-start-time (current-time))
(message "Starting emacs...")

(defun running-file-name ()
  (or load-file-name buffer-file-name))

(defun debug-message (message)
  (when debug-on-error
    (message "* DEBUG: %s" message)))

(defun profile-load (orig-fun &rest args)
  (let* ((the-lib (car args))
	 (load-message (concat "Loading '" the-lib "'..."))
	 (load-start-time (current-time))
	 (res))
    (debug-message load-message)
    (setq res (apply orig-fun args))
    (debug-message (concat load-message (format "done. (%s seconds)" (time-to-seconds (time-since load-start-time)))))
    res))
(advice-add 'load :around #'profile-load)

(message "Loading '%s'..." (running-file-name))

(setq load-prefer-newer t)

(defvar load-emacs-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar nomad-dir (expand-file-name "nomad" load-emacs-dir)
  "The directory containing the Nomad library")

(add-to-list 'load-path nomad-dir)

(setq custom-file (expand-file-name "custom.el" load-emacs-dir))
(require 'nomad)

(message "Loading '%s'...done. (%s seconds)" (running-file-name) (time-to-seconds (time-since before-init-time)))

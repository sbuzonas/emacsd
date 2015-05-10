;;(defvar elpa-dir (expand-file-name "elpa" load-emacs-dir)
;;  "The directory to store packages downloaded from elpa")
;;(defvar backup-dir (expand-file-name "backup" load-emacs-dir)
;;  "The directory to store backups and autosaves")
;;(defvar shared-dir (expand-file-name "shared" load-emacs-dir)
;;  "The directory that is persistent between machines")
;(defcustom secrets-file (expand-file-name ".secrets.el.gpg" load-emacs-dir)
;  "The file containing sensitive information such as passwords"
;  :type 'file
;  :group 'nomad)

(defgroup libraries nil
  "Configuration for libraries and their load paths"
  :group 'emacs)

(defcustom distro-lisp-dir
  (concat (or (getenv "SHARE")
	      "/usr/share") "/emacs/site-lisp/")
  "Path to distro emacs lisp files"
  :type 'file
  :group 'libraries)

(defcustom site-lisp-dir
  (concat (or (getenv "LOCAL_SHARE")
	      "/usr/local/share") "/emacs/site-lisp")
  "Path to site emacs lisp files"
  :type '(file :must-match t)
  :group 'libraries)

(defcustom vendor-dir (expand-file-name "vendor" load-emacs-dir)
  "The directory where 3rd party elisp code is stored"
  :type 'file
  :group 'libraries)

(defgroup nomad nil
  "Configuration for Nomad"
  :group 'emacs)

(defcustom enable-nomad-addons t
  "Toggle loading addons at startup"
  :type 'boolean
  :group 'nomad)

(defcustom defaults-file (expand-file-name "defaults.el" load-emacs-dir)
  "The file containing primary variable definitions"
  :type 'file
  :group 'nomad)

(defcustom local-defaults-file (expand-file-name "local.el" load-emacs-dir)
  "The file containing primary variable definitions for the local machine"
  :type 'file
  :group 'nomad)

(provide 'nomad-config)

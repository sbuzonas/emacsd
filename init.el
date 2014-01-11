(message "* --[ Loading Emacs init.el ]--")
(setq emacs-load-start-time (current-time))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defmacro GNULinux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
        (cons 'progn body)))

(defmacro MacOSX (&rest body)
  (list 'if (string-match "darwin" (prin1-to-string system-type))
        (cons 'progn body)))

(defmacro TMUX (&rest body)
  (list 'if (getenv "TMUX")
        (cons 'progn body)))

;; Emacs version
(list emacs-version emacs-major-version emacs-minor-version
      system-type system-name system-configuration
      window-system
      (when (boundp 'aquamacs-version) aquamacs-version))

(defadvice load (before debug-log activate)
  (message "Loading %s..." (locate-library (ad-get-arg 0))))

;; load-path enhancement
(defun slbmeh/add-to-load-path (this-directory &optional with-subdirs
                                               recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the optional argument
WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with '~')
      (while (not (string= this-directory (expand-file-name this-directory)))
        (setq this-directory (expand-file-name this-directory)))

      (when debug-on-error (message "Adding '%s' to load-path..." this-directory))
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
          (setq dir-or-file (car files))
          (when (file-directory-p dir-or-file)
            (if recursive
                (slbmeh/add-to-load-path dir-or-file 'with-subdirs 'recursive)
              (slbmeh/add-to-load-path dir-or-file)))
          (setq files (cdr files)))))))

;; *** Features ***
(defvar missing-packages-list nil
  "list of packages that 'try-require' can't find.")

(defmacro with-library (symbol &rest body)
  (condition-case nil
      (progn
        (when debug-on-error (message "Checking for library '%s'..." ',symbol))
        (require ',symbol)
        (when debug-on-error (message "Loaded library '%s'." ',symbol))
        ,@body)

    (error (progn
             (message (format "Unable to load library '%s'!" ',symbol))
             (add-to-list 'missing-packages-list ',symbol 'append))
           nil)))
(put 'with-library 'list-indent-function 1)

(defcustom distro-site-lisp-directory
  (concat (or (getenv "SHARE")
               "/usr/share") "/emacs/site-lisp/")
  "Path to distro emacs lisp files"
  :type '(string)
  :group 'init)
(slbmeh/add-to-load-path distro-site-lisp-directory
                         'with-subdirs)

(defcustom local-site-lisp-directory
  (concat (or (getenv "LOCAL_SHARE")
              "/usr/local/share") "/emacs/site-lisp/")
  "Path to site emacs lisp files"
  :type '(string)
  :group 'init)
(slbmeh/add-to-load-path local-site-lisp-directory
                         'with-subdirs)

(defcustom user-lisp-directory
  "~/.emacs.d/elpa/"
  "Path to user emacs lisp files"
  :type '(string)
  :group 'init)
(slbmeh/add-to-load-path user-lisp-directory
                         'with-subdirs)

;; turn on Common Lisp support
(require 'cl)

(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called."
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path))

(defun load-config (config-name)
  "Loads a configuration file located in .emacs.d/conf/CONFIG-NAME.el"
  (let ((file-relative-path (concat "conf/" config-name ".el")))
    (load (fullpath-relative-to-current-file file-relative-path))))

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)

     (error (message (format "Missing library '%s'." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

(add-to-list 'load-path "~/.emacs.d/elisp")

(load-config "sections")

;; Configuration files to load
(defvar my-sections
  '(loading-libraries
    packages
    mouse
    hooks
    debugging
    screen
    basic
    minibuffer
    help
    mark
    killing
    yanking
    rectangles
    cua-bindings
    registers
    display
    search
    fixit
    keyboard-macros
    buffers
    frames
    international
    major-modes
    indentation
    text
    programs
    building
    maintaining
    abbrevs
    dired
    document-view
    gnus
    shell
    emacs-server
    printing
    sorting
    narrowing
    saving-emacs-sessions
    hyperlinking
    amusements
    customization
    emacs-display
    missing-packages
    extra)
  "A list of configuration files to load.")

(message "* --[ Loading configuration files ]--")
;(dolist (s my-sections)
 ; (load-config (symbol-name s)))


;(require 'fancyguy)
(require 'vagrant)

;(load "~/.emacs.d/conf/settings.el")
;(load "~/.emacs.d/conf/keybindings.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" default)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

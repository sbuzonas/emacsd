;; Set path to configuration
(setq config-dir
      (expand-file-name "conf/" user-emacs-directory))

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

(defgroup libraries nil
  "Settings changing load paths"
  :group 'emacs)

(defcustom distro-site-lisp-directory
  (concat (or (getenv "SHARE")
               "/usr/share") "/emacs/site-lisp/")
  "Path to distro emacs lisp files"
  :type 'string
  :group 'libraries)
(slbmeh/add-to-load-path distro-site-lisp-directory
                         'with-subdirs)

(defcustom local-site-lisp-directory
  (concat (or (getenv "LOCAL_SHARE")
              "/usr/local/share") "/emacs/site-lisp/")
  "Path to site emacs lisp files"
  :type 'string
  :group 'libraries)
(slbmeh/add-to-load-path local-site-lisp-directory
                         'with-subdirs)

(defcustom user-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to user emacs lisp files"
  :type 'string
  :group 'libraries)
(slbmeh/add-to-load-path user-lisp-directory
                         'with-subdirs)

(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called."
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path))

(defun load-directory (directory)
  "Load all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(defun load-config (config-name)
  "Loads a configuration file located in .emacs.d/conf/CONFIG-NAME.el"
  (load (concat config-dir (symbol-name config-name) ".el")))

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)

     (error (message (format "Missing library '%s'." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
    '(progn ,@body)))

(provide 'load-path)

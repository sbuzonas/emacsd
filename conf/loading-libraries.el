;;** Loading Libraries of Lisp Code for Emacs

(when section-loading-libraries (message "0 Loading Libraries...")

;; make loaded files give a message
(GNUEmacs
    (defadvice load (before debug-log activate)
      (message "Loading %s..." (locate-library (ad-get-arg 0)))))

;; load-path enhancement
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
        (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
          (setq dir-or-file (car files))
          (when (file-directory-p dir-or-file)
            (if recursive
                (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
              (fni/add-to-load-path dir-or-file)))
          (setq files (cdr files)))))))

;; Use `M-x list-load-path-shadows RET' to display a list of external Emacs
;; Lisp files that shadow Emacs builtins (listing potential load path
;; problems).


;;*** Features

(defvar missing-packages-list nil
  "List of packages that 'try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))


;;*** Library Search

;; `load-path' is a list of directories where Emacs Lisp libraries (`.el' and
;; `.elc' files) are installed.

;; `exec-path' is different: it is a list of directories where executable
;; programs are installed.
;;
;; Shouldn't be `exec-path' and `PATH' achieve the same goal under Emacs?
;;
;; No. `exec-path' is used by Emacs to search for programs it runs directly.
;; But `M-x grep' does not run `grep.exe' directly; it runs the shell passing
;; it a command that invokes `grep'. So it's the shell that needs to find
;; `grep.exe', and it uses PATH, of course, not `exec-path'.
;;
;; So the right thing to do when you install a new program, in order for Emacs
;; to find it, is *both* to update `exec-path' *and* update `PATH'. This is
;; because some Emacs features invoke programs directly, while others do that
;; through the shell or some other intermediary programs.

;; The most important directories are the last!

;; TODO Specify variables using `defcustom'

;; 1.
(defvar distro-site-lisp-directory
  (concat (or (getenv "SHARE")
              "/usr/share") "/emacs/site-lisp/")
  "Name of directory where additional Emacs goodies Lisp files (from the
distro optional packages) reside.")

(fni/add-to-load-path distro-site-lisp-directory
                     'with-subdirs)

;; If you put stuff you have installed from tar balls, etc. within the same
;; directory hierarchy as the distro packaged Emacs, you can get problems when
;; upgrading the distro version as many package systems will assume once all
;; the packaged stuff is removed, directories are empty. If they are not, the
;; package management scripts can fail or possibly get into a "confused"
;; state.

;; 2.
(defvar local-site-lisp-directory
  (concat (or (getenv "LOCAL_SHARE")
              "/opt") "/emacs/site-lisp/")
  "Name of directory where additional Emacs goodies Lisp files (from the
Internet) reside.")

(fni/add-to-load-path local-site-lisp-directory
                     'with-subdirs 'recursive)

;; `local-site-lisp-directory' is there so that you have an easy way of
;; installing your own (possibly not distro packaged) Emacs add-ons which are
;; specific to the version of Emacs your running. This keeps your local
;; add-ons apart from distro supplied ones. If your have a `/usr/local'
;; partition, it also means you can do a complete re-install of Emacs (or even
;; your Linux distro) without impacting on stuff you have added by hand.

;; 3.
(defvar my-site-lisp-directory "~/.emacs.d/elisp/"
  "Name of directory where my personal additional Emacs Lisp files reside.")

(fni/add-to-load-path my-site-lisp-directory
                     'with-subdirs)

;; 4.
;; automatically compile `.el' files as they're loaded
(setq load-source-file-function 'load-with-code-conversion)  ; for XEmacs
(when (try-require 'byte-code-cache-XXX)

    (require 'bytecomp)

    ;; directory in which we store cached byte-compiled files
    (setq bcc-cache-directory
          ;; FIXME Concat env var (so that it can be stored locally on C:)
          (cond
           (running-gnu-linux "~/.emacs.d/byte-cache-linux")
           (running-ms-windows "~/.emacs.d/byte-cache-ms-windows")))

    (fni/add-to-load-path bcc-cache-directory))


;; load elisp libraries while Emacs is idle
(if (try-require 'idle-require-XXX)
    (progn
      ;; idle time (in seconds) after which autoload functions will be loaded
      (setq idle-require-idle-delay 5)

      ;; time in seconds between automatically loaded functions
      (setq idle-require-load-break 3)

      ;; load unloaded autoload functions when Emacs becomes idle
      (idle-require-mode 1)

      (defun try-idle-require (feature)
        (when (locate-library (symbol-name feature))
          (idle-require feature))))

  (defun try-idle-require (feature)
    (when (locate-library (symbol-name feature))
      (require feature))))


(defun my-make-directory-yes-or-no (dir)
  "Ask user to create the DIR, if it does not already exist."
  (if dir
      (if (not (file-directory-p dir))
          (if (yes-or-no-p (concat "The directory `" dir
                                   "' does not exist currently. Create it? "))
              (make-directory dir t)
            (error
             (concat "Cannot continue without directory `" dir "'"))))
    (error "my-make-directory-yes-or-no: missing operand")))

(defun my-file-executable-p (file)
  "Make sure the file FILE exists and is executable."
  (if file
      (if (file-executable-p file)
          file
        (message "WARNING: Can't find executable `%s'" file)
        ;; sleep 1 s so that you can read the warning
        (sit-for 1))
    (error "my-file-executable-p: missing operand")))

(message "0 Loading Libraries... Done"))

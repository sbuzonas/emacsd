(when section-environment (message "0 Environment...")

;; OS type --- are we running Microsoft Windows?
(defvar running-ms-windows
  (eq system-type 'windows-nt))

(defvar running-ms-windows
  (string-match "windows" (prin1-to-string system-type)))

(defvar running-gnu-linux
  (string-match "linux" (prin1-to-string system-type)))

;; Emacs type --- are we running XEmacs (or GNU Emacs)?
(defvar running-xemacs
  (string-match "XEmacs" emacs-version))

;; OS type --- are we running GNU Linux?
(defmacro GNULinux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
        (cons 'progn body)))

(defmacro Windows (&rest body)
  (list 'if (string-match "windows" (prin1-to-string system-type))
        (cons 'progn body)))

(defmacro XLaunch (&rest body)
  (list 'if (eq window-system 'x)(cons 'progn body)))

;; Emacs type --- are we running GNU Emacs?
(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
        (cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
        (cons 'progn body)))

(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
        (cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
        (cons 'progn body)))

;; Emacs version
(GNUEmacs
 (list emacs-version emacs-major-version emacs-minor-version
       system-type system-name system-configuration
       window-system
       (when (boundp 'aquamacs-version) aquamacs-version)))

(XEmacs
    ;; don't offer migration of the init file
    (setq load-home-init-file t))

(when running-gnu-linux
  (modify-all-frames-parameters
   '((height . 32))))

(message "0 Environment... Done"))

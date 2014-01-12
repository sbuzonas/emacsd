(defconst *is-mac* (string-match "darwin" (prin1-to-string system-type)))
(defconst *is-linux* (string-match "linux" (prin1-to-string system-type)))

(defmacro GNULinux (&rest body)
  (list 'if *is-linux*
        (cons 'progn body)))

(defmacro MacOSX (&rest body)
  (list 'if *is-mac*
        (cons 'progn body)))

(defmacro TMUX (&rest body)
  (list 'if (getenv "TMUX")
        (cons 'progn body)))

;; Emacs version
(list emacs-version emacs-major-version emacs-minor-version
      system-type system-name system-configuration
      window-system
      (when (boundp 'aquamacs-version) aquamacs-version))

(provide 'environment)

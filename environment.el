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

(provide 'environment)

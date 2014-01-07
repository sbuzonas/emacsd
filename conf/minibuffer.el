;;** 8 The (info "(emacs)Minibuffer")

(when section-minibuffer (message "8 The Minibuffer...")

;;*** 8.1 (info "(emacs)Minibuffer File") Names

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(GNUEmacs
    (file-name-shadow-mode 1))


;;*** 8.2 (info "(emacs)Minibuffer Edit")ing

;; minibuffer window expands vertically as necessary to hold the text that you
;; put in the minibuffer
(setq resize-mini-windows t)

;; From Babel.el: "If the output is short enough to display in the echo area
;; (which is determined by the variables `resize-mini-windows' and
;; `max-mini-window-height'), it is shown in echo area."


;;*** 8.3 (info "(emacs)Completion")

;; ;; allow to type space chars in minibuffer input
;; ;; (for `timeclock-in', for example)
;; (define-key minibuffer-local-completion-map " " nil)
;; (define-key minibuffer-local-must-match-map " " nil)

;; minibuffer completion incremental feedback
(GNUEmacs
    (icomplete-mode))

;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;;> I frequently find myself doing `C-M :' for some quick elisp calls, and
;;> find it tedious to type expressions.  Is there some way to allow
;;> completion when writing a sexp in the minibuffer?  I'd like this to work
;;> similar to the way `M-x' helps you complete some command.
;;;;;;(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(message "8 The Minibuffer... Done"))

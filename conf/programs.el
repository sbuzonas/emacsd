;;** 30 Editing (info "(emacs)Programs")

(when section-programs (message "30 Editing Programs...")

;;*** 30.1 Major Modes for (info "(emacs)Program Modes")

(autoload 'awk-mode "cc-mode" "Awk editing mode." t)
;; TODO Or use a new AWK Mode for AWK files, rather than the older mode
;; contained in the file `awk-mode.el'
;; [from http://people.smu.edu/zwang/awk-mode.html]

;; (try-require 'graphviz-dot-mode)

;; Have a look at:
;; - http://cedet.sourceforge.net for C/C++ development,
;; - http://common-lisp.net/project/slime for Common Lisp development,
;; - http://jdee.sunsite.dk/ for Java programs.

;; Emacs tool for ELISP code analysis (to keep overview of the
;; function calls and dependecies between functions/variables):
;; byte-compile-generate-call-tree
;; Also http://whome.phys.au.dk/~harder/who-calls.el


;;*** 30.3 (info "(emacs)Program Indent")ation

;; From (info "(ccmode)Indentation Commands"):
;;    Changing the "hanginess" of a brace and then reindenting, will not
;;    move the brace to a different line. For this, you're better off
;;    getting an external program like GNU `indent', which will rearrange
;;    brace location, amongst other things.

;; turn on auto-fill mode in Lisp modes
(add-hook 'lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

;; use one of several different indentation styles for C-like modes
(setq c-default-style
      '((awk-mode . "stroustrup")
        (other . "stroustrup")))
                                        ; Try the different pre-defined styles
                                        ; of indentation via a call to
                                        ; `c-set-style'

(defun my-c-mode-hook ()
  "Customize my c/c++-mode and awk-mode."
  ;; auto-indentation
  (local-set-key (kbd "<return>") 'newline-and-indent)  ; (control m)
  (local-set-key (kbd "<linefeed>") 'newline))          ; (control j)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'awk-mode-hook 'my-c-mode-hook)

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (/= (point) (line-beginning-position))
      (beginning-of-line)
    (back-to-indentation)))

(defun align-with-spaces (beg end)
  "Align selected using only spaces for whitespace."
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (align beg end)))


;;*** 30.4 Commands for Editing with (info "(emacs)Parentheses")

;; find matching parenthesis (% command in vim)
(defun match-paren (arg)
  "Go to the matching parenthesis, if on parenthesis; otherwise,
insert `%'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "%") 'match-paren)

;; highlight matching parenthesis
(when (try-require 'paren)
    (GNUEmacs
        (show-paren-mode t)
        (setq show-paren-ring-bell-on-mismatch t))
    (XEmacs
        (paren-set-mode 'paren)))

;; if the matching paren is offscreen, show the matching line in the echo area
;; + many other useful things
(when window-system
  ;; advanced highlighting of matching parentheses
  (when (try-require 'mic-paren)

      ;; activating
      (paren-activate)))

;; from hall@grumpy.nl.nuwc.navy.mil
;; goto-matching-paren


;;*** 30.6 (info "(emacs)Documentation") Lookup

;; show the function arglist or the variable docstring in the echo area
(GNUEmacs
 (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))


;;*** 30.7 (info "(emacs)Hideshow") minor mode

;; You can have block-oriented folding in programming modes: Hideshow
;; distinguishes end-of-block.

;; `hs-minor-mode.el' collapses code for a lot of languages, not only Lisp.

;; See `outline-minor-mode' as well.

;; enable `hs-minor-mode' at startup
(add-hook 'emacs-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))


;; Especially after changing a couple of those really awkward keybindings
;; with `@' in the middle.
;; Changing: C-c @ c-s  to C-c s  (hs-show-block)
;;           C-c @ c-h  to C-c h  (hs-hide-block)
;; Seems not to collide with anything when in cperl-mode at least.


;; (define-key hs-minor-mode-map [?\C-c ?\C-\M-h] 'hs-hide-all)
;; (define-key hs-minor-mode-map [?\C-c ?\C-\M-s] 'hs-show-all)


;; (global-set-key (kbd "C-c @ @") 'hs-hide-all)
;; (global-set-key (kbd "C-c @ @") 'hs-show-all)
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding


;;*** 30.8 (info "(emacs)Symbol Completion")

;; It's more or less a convention that each language mode binds its symbol
;; completion command to `M-TAB' which is a reserved hot key under Windows.
;; Way to solve this: when you hit `C-TAB', the command normally bound to
;; `M-TAB' will be called.
(global-set-key (kbd "<C-tab>")
                '(lambda ()
                   (interactive)
                   (call-interactively (key-binding (kbd "M-TAB")))))

;; `M-/' runs the command `dabbrev-expand' by default
;; Expand previous word "dynamically". Expands to the most recent, preceding
;; word for which this is a prefix.
(global-set-key (kbd "C-`") 'dabbrev-expand)

;; `C-M-/' runs the command `dabbrev-completion'
;; Completion on current word. Like `M-/' but finds all expansions in the
;; current buffer and presents suggestions for completion.

;; expand text trying various ways to find its expansion
(when (try-require 'hippie-exp)

    ;; list of expansion functions tried (in order) by `hippie-expand'
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev   ; from current buffer
            try-expand-dabbrev-visible   ; from visible parts of all windows
            try-expand-dabbrev-all-buffers   ; from all other buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-whole-kill))

    ;; expand-function
    (defun my-hippie-expand (arg)
      ;; called with a positive prefix `P', it jumps directly to the `P'-th
      ;; `try-function'
      (interactive "P")
      ;; `hippie-expand' does not have a customization-feature (like
      ;; `dabbrev-expand') to search case-sensitive for completions. So we
      ;; must set `case-fold-search' temporarily to nil!
      (let ((old-case-fold-search case-fold-search))
        (setq case-fold-search nil)
        (hippie-expand arg)
        (setq case-fold-search old-case-fold-search)))

    (global-set-key [(control tab)] 'my-hippie-expand))

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; I recommend you split the key binding of those two command.
;; I binding TAB yas/expand, and binding M-/ hippie-expand.
;; So yas/expand don't conflict with hippie/expand.

;; predictive abbreviation expansion ("Ã  la IntelliSense")
(when (try-require 'pabbrev)

  ;; don't print messages while scavenging on idle timer
  (setq pabbrev-idle-timer-verbose nil)

  ;; tab completion with continual, as-you-type feedback
  (global-pabbrev-mode))

;; > I'm trying to have code completion in Emacs, but i don't know what to
;; > do. In eclipse, when we writing a java code line, for example:
;; > System.out., we do C^SPACE to show a window with several methods
;; > associated (printl, print,etc).
;; > I would like to have something similar in Emacs. Can anybody help me?
;; Try M-TAB with cursor on the symbol; is that what you are looking for?


;; extensible inline text completion mechanism -- really brilliant!
(when (try-require 'company)
  (define-key company-mode-map (kbd "M-SPC") 'company-complete)

  (defun my-turn-on-company-mode ()
    (interactive)
    (company-mode 1))

  (dolist (hook (list
                 'asm-mode-hook
                 'c++-mode-hook
                 'c-mode-hook
                 'clojure-mode-hook
                 'emacs-lisp-mode-hook
                 'emms-tag-editor-mode-hook
                 'haskell-mode-hook
                 'java-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'scheme-mode-hook
                 'sh-mode-hook
                 'slime-repl-mode-hook))
    (add-hook hook 'my-turn-on-company-mode))

  (setq company-idle-delay nil)

  (setq company-eclim-auto-save t)

  (setq company-eclim-executable
        "~/opt/eclipse/plugins/org.eclim_1.4.5/bin/eclim")

  (defun my-java-mode-init ()
    (setq company-backend 'company-eclim))

  (add-hook 'java-mode-hook 'my-java-mode-init))


;;*** 30.9 (info "(emacs)Glasses") minor mode

;; face to be put on capitals of an identifier looked through glasses
(setq glasses-face 'bold)

;; string to be displayed as a visual separator in unreadable identifiers
(setq glasses-separator "")

(message "30 Editing Programs... Done"))

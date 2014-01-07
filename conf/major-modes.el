;;* Advanced Features

;;** 27 (info "(emacs)Major Modes")

(when section-major-modes (message "27 Major Modes...")

;; TODO
;; Have a look at http://www.emacswiki.org/emacs/AutomaticFileHeaders


;;*** 27.1 How (info "(emacs)Choosing Modes")

;; ;; default major mode for new buffers and any files with unspecified mode
;; (when (locate-library "org.el")
;;     (setq-default major-mode 'org-mode))

;; Since `foo-mode' toggles Foo (minor) mode, it's better to use a function on
;; those hooks that unconditionally turns it on. Many minor modes have
;; `turn-on-foo-mode' and `turn-off-foo-mode' convenience functions for that
;; purpose, but you can fake it with an anonymous function:
;;
;;     (add-hook 'text-mode-hook (lambda () (foo-mode 1)))
;;
;; or define your own convenience function and use that instead:
;;
;;     (defun turn-on-foo-mode ()
;;       "Turn on Foo mode."
;;       (foo-mode 1))
;;     (add-hook 'text-mode-hook 'turn-on-foo-mode)

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'sql-mode "sql" nil)
(autoload 'css-mode "css-mode")
(autoload 'nxml-mode  "nxml-mode" "XML mode" t)
(autoload 'ssh-config-mode "ssh-config-mode" t)

;; ledger
(try-require 'ledger)

;; 1. list of filename patterns
;;    vs. corresponding major mode functions
(setq auto-mode-alist
      (append '(
                ("\\.css\\'"                           . css-mode)
                ("\\.\\(htm\\|html\\|xhtml\\)$"        . nxhtml-mode)
                ("\\.sql$"                             . sql-mode)
;;                ("\\.js$"                              . java-mode)
                ("\\.dcl$"                             . dtd-mode)
                ("\\.dec$"                             . dtd-mode)
                ("\\.dtd$"                             . dtd-mode)
                ("\\.ele$"                             . dtd-mode)
                ("\\.ent$"                             . dtd-mode)
                ("\\.mod$"                             . dtd-mode)

                ;; sorted by chapter
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
                ("\\.txt$"                             . org-mode)
                ("\\.dat$"                             . ledger-mode)

                ("\\.log$"                             . text-mode)
                ("\\.tex$"                             . LaTeX-mode)
                ("\\.tpl$"                             . LaTeX-mode)
                ("\\.cgi$"                             . perl-mode)
                ("[mM]akefile"                         . makefile-mode)
                ("\\.bash$"                            . shell-script-mode)
                ("\\.expect$"                          . tcl-mode)

                (".ssh/config\\'"                      . ssh-config-mode)
                ("sshd?_config\\'"                     . ssh-config-mode)
                ) auto-mode-alist))

;; major mode for editing comma-separated value files
(when (try-require 'csv-mode)

    ;; field separators: a list of *single-character* strings
    (setq csv-separators '("," ";")))


;; See (info "(elisp) Syntax of Regexps")
;;     \' matches end of string
;;      $ matches end of line


;; Some Emacs modes are over 10K lines of code. (e.g. js2-mode, nxml-mode,
;; CEDET). Many packages make use of the `autoload' feature, so that you only
;; need to load a single file that define autoloaded functions.

;; For example, nxml-mode's instruction tells you to do:
;; (when (load-library
;;        "~/Downloads/emacs/site-lisp/nxml-1.33-20080630/autostart.el")
;;
;;     ;; always skip the nXhtml welcome message
;;     (setq nxhtml-skip-welcome t))

(add-to-list 'auto-mode-alist
             (cons "\\.\\(xml\\|xsd\\|sch\\|rng\\|xslt\\|svg\\|rss\\|owl\\|xbl\\)\\'"
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)

    ;; instead of superseding the binding in `auto-mode-alist', you can
    ;; replace it (brute force) with
    ;; `(setcdr (rassq 'old-mode auto-mode-alist) 'new-mode)'

;; 2. list of buffer beginnings
;;    vs. corresponding major mode functions (Emacs 22+)
;;    see `magic-mode-alist'

;; 3. list of interpreters specified in the first line (starts with `#!')
;;    vs. corresponding major mode functions
(push '("expect" . tcl-mode) interpreter-mode-alist)

;; multiple major modes
;; - nXhtml includes `mumamo.el' (= one of the most compatible with Org-babel)
;; - MMM mode

;; load generic modes which support e.g. batch files
(try-require 'generic-x)

(message "27 Major Modes... Done"))

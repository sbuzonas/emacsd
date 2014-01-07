;;** 33 (info "(emacs)Abbrevs")

(when section-abbrevs (message "33 Abbrevs...")

;; See (info "(autotype)") as well


;;*** 33.3 Controlling (info "(emacs)Expanding Abbrevs")

;; I am aware of packages such as `ELSE', `tempo', `skeleton', some of which
;; do similar things. However, I believe this package does some things
;; better than these other packages.
;; It's particularly notable for having the easiest syntax of all, making it
;; possible to add new snippets without careful programming, and also very
;; nice navigation within the inserted text.
;; Check out the demo: http://www.bloomington.in.us/~brutt/msf-abbrev.html
(GNUEmacs
    (when (try-require 'msf-abbrev)
        ;; ensure abbrev mode is always on
        (setq-default abbrev-mode t)

        ;; do not bug me about saving my abbreviations
        (setq save-abbrevs nil)

;;;         ;; load up modes I use
;;;         (require 'cc-mode)
;;;         (require 'perl-mode)
;;;         (require 'cperl-mode)
;;;         (require 'sh-script)
;;;         (require 'shell)
;;;         (require 'tex-site) ;; I use AUCTeX
;;;         (require 'latex)    ;; needed to define LaTeX-mode-hook under AUCTeX
;;;         (require 'tex)      ;; needed to define TeX-mode-hook under AUCTeX

        ;; load up abbrevs for these modes
        (require 'msf-abbrev)
        (setq msf-abbrev-verbose t) ;; optional
        (global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)

        ;; FIXME Conflict with Org agenda `C-c a'
        (global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
        (setq msf-abbrev-root (concat my-site-lisp-directory "mode-abbrevs/"))
        (when (file-directory-p msf-abbrev-root)
              (msf-abbrev-load))))

;; There're some difference from msf-abbrev. For example, msf-abbrev doesn't
;; support mirror fields and transformations. It also doesn't support multiple
;; snippet with same name.
(GNUEmacs
    (when (try-require 'yasnippet) ;; not yasnippet-bundle

      ;; do necessary initialization
      (yas/initialize)

      ;; root directory that stores the snippets for each major mode
      (setq yas/root-directory
            (concat my-site-lisp-directory "yasnippet/snippets"))
      (unless (file-directory-p yas/root-directory)
        (setq yas/root-directory
              (concat local-site-lisp-directory "yasnippet/snippets")))

      ;; load snippet definition from a (existing) directory hierarchy
      (when (file-directory-p yas/root-directory)
          (yas/load-directory yas/root-directory))

      ;; ;; the key to bind as a trigger of snippet
      ;; (setq yas/trigger-key (kbd "SPC"))  ; default is TAB
))

;; For simple expansions, I prefer abbrev. Think: pub -> public, pro ->
;; protected, pri -> private, etc.

;; For more complex templates, I use yasnippet. For example, new file
;; templates (where it's lisp evaluation is handy), class and function
;; templates with docblocks, etc. I use it to reduce the repetitious parts of
;; programming, and let me focus on getting things done.

;; yasnippet will also expand snippets containing non-word-constituent
;; characters, which abbrev can't. So I can't have "@p" expand to "@param"
;; with abbrev, but I can with yasnippet.


;;*** 33.7 (info "(emacs)Dabbrev Customization")

;; preserve case when expanding the abbreviation
(setq dabbrev-case-replace nil)

(message "33 Abbrevs... Done"))

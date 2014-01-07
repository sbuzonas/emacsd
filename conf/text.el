;;** 29 Commands for (info "(emacs)Text") Human Languages ]

(when section-text (message "29 Commands for Human Languages...")

;;*** 29.1 (info "(emacs)Words")

;; GNU Emacs default for killing back to the beginning of a word
(XEmacs
    (global-set-key [(control backspace)] 'backward-kill-word))

;; delete previous character, changing tabs into spaces
(global-set-key [(shift backspace)] 'backward-delete-char-untabify)


;;*** 29.2 (info "(emacs)Sentences")

;; FIXME When changing the ispell language, this should be changed
;; simultaneously.

;; a single space does end a sentence
(setq-default sentence-end-double-space nil)

;; See `sentence-end' and `(sentence-end)'


;;*** 29.5 (info "(emacs)Filling") Text

(defun my-text-mode-hook ()
  "Turn on filling modes in text mode."
  (turn-on-auto-fill)

  ;; adaptative filling
  (when (try-require 'filladapt)
      (setq-default filladapt-mode nil)

      ;; turn on filladapt mode everywhere but in ChangeLog files
      (cond ((equal mode-name "Change Log")
             t)
            (t
             (turn-on-filladapt-mode)))))

;; 2010-05-21 John Wiegley:
;; The Emacs ChangeLog is a file which predates the existence of freely
;; available, project-wide version control. It was a way to see, in one place,
;; the stream of changes occurring in a project -- something which RCS could
;; not do for you.
;; However, in this modern era of project-wide, atomic commits, the ChangeLog
;; is not only an archaism, but is a continuous source of merge conflicts. For
;; example, when I reverted Russell's latest change -- a one-liner that was
;; minor in the extreme -- I had to do with a merge conflict in
;; lisp/ChangeLog.
;; With a system like Git, and properly written commits, you can produce a
;; ChangeLog at any time with "git log". You even see a ChangeLog for just one
;; file, or a directory with "git log --follow PATH". This completes
;; supersedes any need for a ChangeLog file, and has led me to abandon the use
;; of ChangeLogs in all the projects I maintain.

;; 2010-05-21 Ben Finney:
;; It seems worth pointing out explicitly, though: Eliminating a
;; manually-maintained ChangeLog doesn't obviate the need for a ChangeLog (or
;; the equivalent) in the distributed source.
;; This is because the copyright holders license their works under the GPLv2,
;; and Â§2.a of those terms requires the work to include dated notice of all
;; modifications made to the work. This is conventionally understood to be
;; most directly satisfied by a ChangeLog in the distributed source for the
;; work.
;; Generating that file automatically from the VCS commit messages, at the
;; time a source release is packaged, is a good use of the VCS.



;; turn on my text setup
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; fabrication automatique de la typo franÃ§aise avec la ponctuation
;; ajout automatique de l'espace insÃ©cable lÃ  oÃ¹ cela va bien

(defun my-insert-interrogation-mark ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~?")
        (insert "Â ?"))) ; non-breaking space
    (insert "?")))

(defun my-insert-exclamation-mark ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~!")
        (insert "Â !"))) ; non-breaking space
    (insert "!")))

;; FIXME Remove NBSP if two colons are put one after the other (for terms and
;; definitions in Org)

(defun my-insert-colon ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~:")
          (insert "Â :"))) ; non-breaking space
    (insert ":")))

(defun my-insert-semi-colon ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~;")
        (insert "Â ;"))) ; non-breaking space
    (insert ";")))

(defun my-double-keys ()
  "Touches spÃ©cifiques"
  (interactive)
  (local-set-key "?" 'my-insert-interrogation-mark)
  (local-set-key "!" 'my-insert-exclamation-mark)
  (local-set-key ":" 'my-insert-colon)
  (local-set-key ";" 'my-insert-semi-colon))

;; typo auto pour les modes suivants
(add-hook 'text-mode-hook 'my-double-keys)
(add-hook 'message-mode-hook 'my-double-keys)






;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
;; (add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)










(defun insert-one-quote-or-two ()
  (interactive)
  (cond
   ((or (bolp) (not (looking-back "'")))
    ;; insert just one '
    (self-insert-command 1))
   ((save-excursion
      (backward-char)
      ;; Skip symbol backwards.
      (and (not (zerop (skip-syntax-backward "w_")))
           (not (looking-back "`"))
           (or (insert-and-inherit "`") t))))
   (t
    ;; insert `' around following symbol
    (delete-backward-char 1)
    (unless (looking-back "`") (insert-and-inherit "`"))
    (save-excursion
      (skip-syntax-forward "w_")
      (unless (looking-at "'") (insert-and-inherit "'"))))))

(global-set-key [39] 'insert-one-quote-or-two)


;; automatic line-wrapping beyond that column
(setq-default fill-column 78)


;;*** 29.6 (info "(emacs)Case") Conversion Commands

;; enable the use of the commands `downcase-region' and `upcase-region'
;; without confirmation
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;*** 29.8 (info "(emacs)Outline Mode")

;; Outline is line-oriented and does not distinguish end-of-block.

;; `outline-minor-mode.el' is also used to collapse Lisp code (i.e., to see in
;; the buffer just the definition of a function instead of the whole body)

;; See also the library `foldout' and `hs-minor-mode', for instance, in the
;; Emacs manual.

;; outline mode commands for Emacs
(when (try-require 'outline)

    ;; bind the function `open-line' to `M-o' instead of `C-o' (by default)
    (global-set-key (kbd "M-o") 'open-line)

    ;; bind the outline minor mode functions to an easy to remember prefix key
    ;; (more accessible than the horrible prefix `C-c @')
    (setq outline-minor-mode-prefix (kbd "C-o"))

    ;; make other `outline-minor-mode' files (LaTeX, etc.) feel like Org files
    (when (try-require 'outline-magic)
        (add-hook 'outline-minor-mode-hook
                  (lambda ()
                    (define-key outline-minor-mode-map
                      (kbd "<backtab>") 'outline-cycle)

                    (define-key outline-minor-mode-map
                      (kbd "<M-left>") 'outline-promote)
                    (define-key outline-minor-mode-map
                      (kbd "<M-right>") 'outline-demote)
                    (define-key outline-minor-mode-map
                      (kbd "<M-up>") 'outline-move-subtree-up)
                    (define-key outline-minor-mode-map
                      (kbd "<M-down>") 'outline-move-subtree-down))))

    ;; extra support for outline minor mode
    (try-require 'out-xtra)


    ;; Org-style folding for a `.emacs' (and much more)
    (defun my-outline-regexp ()
      "Calculate the outline regexp for the current mode."
      (let ((comment-starter (replace-regexp-in-string
                              "[[:space:]]+" "" comment-start)))
        (when (string= comment-start ";")
          (setq comment-starter ";;"))
     ;; (concat "^" comment-starter "\\*+")))
        (concat "^" comment-starter "[*]+ ")))

    (defun my-outline-minor-mode-hook ()
      (interactive)
      (setq outline-regexp (my-outline-regexp))

      ;; highlight the headings
      ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
      ;; use `M-x customize-apropos-faces' to customize faces
      ;; to find the corresponding face for each outline level, see
      ;; `org-faces.el'

      ;; Added `\n?', after having read the following chunk of code (from org.el):
      ;; `(,(if org-fontify-whole-heading-line
      ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
      ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")

      (let ((org-fontify-whole-heading-line "") ; "\n?")
            (heading-1-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{1\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-2-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{2\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-3-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{3\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-4-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{4,\\} \\(.*" org-fontify-whole-heading-line "\\)")))
        (font-lock-add-keywords
         nil
         `((,heading-1-regexp 1 'org-level-1 t)
           (,heading-2-regexp 1 'org-level-2 t)
           (,heading-3-regexp 1 'org-level-3 t)
           (,heading-4-regexp 1 'org-level-4 t)))))

    (add-hook 'outline-minor-mode-hook
              'my-outline-minor-mode-hook)

    ;; Add the following as the top line of your `.emacs':
    ;;
    ;; ; -*- mode: emacs-lisp; mode: outline-minor; -*-
    ;;
    ;; Now you can add `;;*' and `;;**', etc. as headings in your `.emacs' and
    ;; cycle using `M-tab', `M-left' and `M-right' will collapse or expand all
    ;; headings respectively. I am guessing you mean to make segments such as
    ;; `;;* SHORTCUTS' and `;;* VARIABLES', this will do that, but not too much
    ;; more.
    )


    ;; Explorer-like bindings (`M-left/right/up/down' to navigate outlines)
    (when (locate-library "outline-mode-easy-bindings.el")

      (add-hook 'outline-mode-hook
                '(lambda ()
                   (require 'outline-mode-easy-bindings)))

      (add-hook 'outline-minor-mode-hook
                '(lambda ()
                   (require 'outline-mode-easy-bindings))))


;; I really like the following for outline-node based navigation.  It is
;; similar to the behavior of paredit-mode in lisp files.
;; org-mode hook
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "\C-\M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "\C-\M-p") 'outline-previous-visible-heading)
            (local-set-key (kbd "\C-\M-u") 'outline-up-heading)))



(global-set-key [backtab] 'org-cycle) ; that works (but on level 1+)
;; TODO Look at org-cycle-global and local below, they work better, but still
;; on level 1+
;; TODO Replace it by a function which alternatively does hide-body and
;; show-all

;; XXX 2010-06-21 Conflicts with outline-minor-mode bindings
;; ;;; Use `orgstruct-mode' in `emacs-lisp-mode' buffers
;; (add-hook 'emacs-lisp-mode-hook 'orgstruct-mode)

(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

(global-set-key (kbd "C-M-]") 'org-cycle-global) ; ok on Elisp, not on LaTeX
(global-set-key (kbd "M-]") 'org-cycle-local) ; ok on Elisp, not on LaTeX


;; unified user interface for Emacs folding modes
(GNUEmacs
 (when (try-require 'fold-dwim)
   (global-set-key (kbd "C-c f t") 'fold-dwim-toggle)
   (global-set-key (kbd "C-c f h") 'fold-dwim-hide-all)
   (global-set-key (kbd "C-c f s") 'fold-dwim-show-all)))

(global-set-key (kbd "<S-f6>") 'visible-mode)



;;*** 29.9 (info "(emacs)TeX Mode")

;; - text-mode-hook     =  all text modes
;; - (la)tex-mode-hook  =  default Emacs built-in (La)TeX mode
;; - (La)TeX-mode-hook  =  AUCTeX

;; Note -- AUCTeX aliases tex-mode to TeX-mode
;; Note -- Invoking `(la)tex-mode' also runs `text-mode-hook'


;;**** 2 (info "(auctex)Installation") of AUCTeX

;; support for LaTeX documents
(GNUEmacs
  (message "29.9 TeX mode....AAA.AAA.AAA..............")
  (try-idle-require 'latex-XXX)
  (message "29.9 TeX mode....BBB.BBB.BBB..............")
  (eval-after-load 'latex
    '(progn

    ;; (try-require 'tex-site)
    ;; should not be used anymore with current AUCTeX releases


       ;; ;; LaTeX-sensitive spell checking
       ;; (add-hook 'tex-mode-hook
       ;;           (lambda ()
       ;;             (make-local-variable 'ispell-parser)
       ;;             (setq ispell-parser 'tex)))



;;**** 3 (info "(auctex)Quick Start")

    ;; Press `C-c C-c File RET RET' to run `dvips'
    ;; (note that the command is `File' and not `Dvips' as one might expect)

    ;; Press `C-c C-c Print RET RET' to run `GSview'
    ;; (also somewhat misleading name)

    ;; If you want to print the document, do it from GSview.


;;**** 5 (info "(auctex)Advanced Features")

;;***** 5.2 (info "(auctex)Completion")

    ;; if this is non-nil when AUC TeX is loaded, the TeX escape character `\'
    ;; will be bound to `TeX-electric-macro'
    (setq TeX-electric-escape t)

;;***** 5.4 (info "(auctex)Indenting")

    ;; leave the `tikzpicture' code unfilled when doing `M-q'
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))

    ;; number of spaces to add to the indentation for each `\begin' not
    ;; matched by a `\end'
    (setq LaTeX-indent-level 4)

    ;; number of spaces to add to the indentation for `\item''s in list
    ;; environments
    (setq LaTeX-item-indent 0)  ; -4

    ;; number of spaces to add to the indentation for each `{' not matched
    ;; by a `}'
    (setq TeX-brace-indent-level 0)  ; 4

    ;; auto-indentation (suggested by the AUCTeX manual -- instead of adding a
    ;; local key binding to `RET' in the `LaTeX-mode-hook')
    (setq TeX-newline-function 'newline-and-indent)


;;**** 6 Controlling Screen (info "(auctex)Display")

;;***** 6.1 (info "(auctex)Font Locking")

    ;; (for Org mode) add the `comment' environment to the variable
    ;; `LaTeX-verbatim-environments' so that, if the `#+TBLFM' line contains
    ;; an odd number of dollar characters, this does not cause problems with
    ;; font-lock in latex-mode
    (add-to-list 'LaTeX-verbatim-environments "comment")
;;;     (add-to-list 'LaTeX-verbatim-environments "mcnuweb") ; FIXME Does not work in .nw files


;;**** 7 (info "(auctex)Running TeX and friends") Processors, Viewers and Other Programs

;;***** 7.1 Executing (info "(auctex)Commands")

    ;; use PDF mode by default (instead of DVI)
    (setq-default TeX-PDF-mode t)

;;***** 7.2 (info "(auctex)Viewing") the formatted output

    ;; use a saner PDF viewer (evince, SumatraPDF)
    (setcdr (assoc "^pdf$" TeX-output-view-style)
            (cond (running-ms-windows
                   '("." "\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" %o"))
                         ; under Windows, we could open the PDF file with:
                         ; start "" xxx.pdf
                  (t
                   '("." "evince %o"))))

    ;; A decent viewer reloads the PDF automatically when the file has changed
    ;; while staying on the same page (no need to close & reopen).

    ;; Support for forward search with PDF files was added. That means the
    ;; viewer jumps to the page in the output file corresponding to the
    ;; position in the source file. Currently this only works if you use the
    ;; pdfsync LaTeX package and xpdf or SumatraPDF as your PDF viewer.

;;***** 7.3 (info "(auctex)Debugging") Catching the errors

    ;; don't show output of TeX compilation in other window
    (setq TeX-show-compilation nil)


;;**** 8 (info "(auctex)Multifile") Documents

    ;; AUC TeX will will assume the file is a master file itself
    (setq-default TeX-master t)


;;**** 9 Automatic (info "(auctex)Parsing Files")

    ;; enable parse on load (if no style hook is found for the file)
    (setq TeX-parse-self t)

    ;; enable automatic save of parsed style information when saving
    ;; the buffer
    (setq TeX-auto-save t)


;;**** 11 (info "(auctex)Automatic") Customization

;;***** 11.1 (info "(auctex)Automatic Global") Customization for the Site

    ;; directory containing automatically generated TeX information. Must end
    ;; with a slash
    (setq TeX-auto-global
          "~/.emacs.d/auctex-auto-generated-info/")

;;***** 11.3 (info "(auctex)Automatic Local") Customization for a Directory

    ;; directory containing automatically generated TeX information. Must end
    ;; with a slash
    (setq TeX-auto-local
          "~/.emacs.d/auctex-auto-generated-info/")


    ;; (try-require 'beamer)

    (try-require 'babel)



    ;; minor mode with distinct support for `\label', `\ref', `\cite' and
    ;; `\index' in LaTeX
    (when (try-require 'reftex)
        ;; A Table of Contents of the entire (multifile) document with
        ;; browsing capabilities is available with `C-c ='.
        ;; Hitting `l' there will show all the labels and cites.
        ;;
        ;; Labels can be created with `C-c (' and referenced with `C-c )'.
        ;; When referencing, you get a menu with all labels of a given type
        ;; and context of the label definition. The selected label is
        ;; inserted as a `\ref' macro.
        ;;
        ;; Citations can be made with `C-c [' which will use a regular
        ;; expression to pull out a *formatted* list of articles from your
        ;; BibTeX database. The selected citation is inserted as a `\cite'
        ;; macro.
        ;;
        ;; Index entries can be made with `C-c /' which indexes the word at
        ;; point or the current selection. More general index entries are
        ;; created with `C-c <'. `C-c >' displays the compiled index.
        (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

        ;; turn all plug-ins on
        (setq reftex-plug-into-AUCTeX t)

        ;; use a separate selection buffer for each label type -- so the menu
        ;; generally comes up faster
        (setq reftex-use-multiple-selection-buffers t))


    ;; Remap default face in current buffer (Emacs 23)
;;;     (add-hook 'LaTeX-mode-hook
;;;               '(lambda ()
;;;                  (face-remap-set-base
;;;                   'default
;;;                   '(:family "LMRoman10" :height 100 :background "white"))))


;;*** (info "(preview-latex)Top")

    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
    (autoload 'LaTeX-preview-setup "preview")

    ;; how to call gs for conversion from EPS
    (setq preview-gs-command
          (cond (running-ms-windows
                 "C:/Program Files/gs/gs8.64/bin/gswin32c.exe")
                (t
                 "/usr/bin/gs")))
    (my-file-executable-p preview-gs-command)

    ;; scale factor for included previews
    (setq preview-scale-function 1.2)



    (GNUEmacs
    ;; major mode to edit nuweb files with AUCTex
    (when (try-require 'nuweb)  ;; depends on `TeX-lisp-directory'

        ;; define what's needed to properly call nuweb
        (make-variable-buffer-local 'outline-prefix-char)
        (make-variable-buffer-local 'outline-regexp)
        (make-variable-buffer-local 'outline-level-function)

        ;; our version of nuweb knows about `@%' comments
        (setq nuweb-comment-leader "@%")

        ;; major mode
        (add-to-list 'auto-mode-alist '("\\.w$" . nuweb-mode))

        ;; to get a menu with scraps/files/index entries
        (add-hook 'nuweb-mode-hook
                  (lambda()
                    (imenu-add-to-menubar "Nuweb")))

        ;; recompute all the defs and uses point in the current file
        (add-hook 'nuweb-mode-hook 'nuweb-compute-d-u)

        ;; replace the existing `Web' command in order to use PDF mode by
        ;; default (instead of DVI) -- without writing explicitly the entire
        ;; `TeX-command-list' in the `.emacs' file (as `customize-variable'
        ;; would do):
        (setcdr (assoc "Web" TeX-command-list)
                '("nuweb %s && pdflatex \"\\nonstopmode\\input{%s}\""
                  TeX-run-LaTeX nil t
                  :help "Extract files, create LaTeX document, and run `pdflatex' on it"))))

    ;; Tangle = Extract

    ;; FIXME Noweb -- Problem with multi-mode?
    (try-require 'noweb)

    )))



;;*** (info "(emacs-goodies-el)boxquote")

;; quote text with a semi-box
(when (try-require 'boxquote)

    ;; put spaces before my boxquotes
    (setq boxquote-top-corner    "    ,")
    (setq boxquote-side          "    | ")
    (setq boxquote-bottom-corner "    `")

    (global-set-key (kbd "C-c b r") 'boxquote-region)
    (global-set-key (kbd "C-c b t") 'boxquote-title))

;; --8<---------------cut here---------------start------------->8---
;; In Gnus, you can mark some region with enclosing tags by pressing
;; `C-c M-m' (`message-mark-inserted-region') or by clicking on
;; `<menu-bar> <Message> <Insert Region Marked>'.
;; --8<---------------cut here---------------end--------------->8---

;; interface to the festival speech synthesizer system
(when (locate-library "festival-XXX")
    (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
    (say-minor-mode t)
    (setq auto-mode-alist
          (append '(("\\.festivalrc$" . scheme-mode)) auto-mode-alist))

    (setq festival-program-name "/usr/bin/festival"))

;; phonetic spelling
(try-idle-require 'phonetic)

(message "29 Commands for Human Languages... Done"))

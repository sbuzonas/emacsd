;;** 32 (info "(emacs)Maintaining") Programs

(when section-maintaining (message "32 Maintaining Programs...")

;;*** 32.1 (info "(emacs)Version Control")

;; PCL-CVS
(when (try-require 'pcvs-XXX)

  ;; allow commit on whole directories
  (setq cvs-allow-dir-commit t)

  ;; when to reuse an existing cvs buffer
  (setq cvs-reuse-cvs-buffer 'always)  ;; subdir

  ;; examine
  (global-set-key (kbd "C-x v e") 'cvs-examine)

  ;; examine without asking for a directory
  (global-set-key (kbd "<C-f9>")
                  '(lambda ()
                     (interactive)
                     (cvs-examine (file-name-directory (buffer-file-name))
                                  nil)))

  ;; messages that should be ignored by the parser
  ;; TODO Should only ADD the last one to the default value of cvs-parse-...
  (setq cvs-parse-ignored-messages
        '("Executing ssh-askpass to query the password.*$"
          ".*Remote host denied X11 forwarding.*$"
          ".*-m wrapper option is not supported remotely.*$"))

  ;; change the CVS Id marker to reflect that a source file was edited
  ;; (from Brady Montz)
  (defun my-mark-cvs-modified ()
    "Called when a file has changed. Updates any RCS Id and Header keywords it
    finds to show that the file is modified."
    (let ((buffer-undo-list t))         ; don't let this change get into the
                                        ; undo list because of this, we must
                                        ; ensure that the edit is in-place,
                                        ; and doesn't move any text
      (when (and (buffer-modified-p) (boundp 'vc-mode) vc-mode)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\(\\$\\(?:Id\\|Header\\): "
                          "[^\"'#;$]* \\)\\(Exp \\$\\)")
                  nil t)
            (replace-match "\\1Mod $" t))))))

  (defadvice basic-save-buffer (before my-basic-save-buffer first activate)
    (my-mark-cvs-modified))

  (defun run (command &optional to-buffer)
    "A variation of shell-command.
    With no optional argument this runs the command creating
    a special buffer to put the output in. The buffer is named
    after the first word in the command.

    The optional argument to-buffer allows the target
    buffer to be specified.

    With the interactive-prefix the target buffer is the
    current buffer (as in shell-command)."
    (interactive (list (read-from-minibuffer "Shell command: "
                                             nil nil nil 'shell-command-history)
                       current-prefix-arg))
    (shell-command command (or to-buffer
                               (get-buffer-create
                                (car (split-string command " "))))))

  (defun run-eval (command &optional func)
    "Evaluate the shell command optionally passing results to a function.
    Without the optional func this returns the result of
    running the command, as a string.

    With the function the results of the shell command are passed as
    a string to the function, the value of calling the function is
    returned.

    If you supply func then it must either be a function taking
    one string argument or a string which can be evaluated to a
    function taking one string argument.

    Interactively the prefix argument will cause a function to be
    prompted for."
    (interactive (list (read-from-minibuffer "Shell command: "
                                             nil nil nil 'shell-command-history)
                       (if current-prefix-arg
                           (read-from-minibuffer "Function: "))))
    (with-temp-buffer
      ;; This turns off the open window behavior of shell-command
      (let ((pop-up-windows nil))
        (shell-command command (current-buffer)))
      (let ((str (buffer-substring-no-properties (point-min)
                                                 (- (point-max) 1))))
        (cond
         ((functionp func)
          (funcall func str))
         ((stringp func)
          (funcall (eval (read func)) str))
         ('t
          str)))))

  (defun map-files (thunk filename-list)
    "Read in each file as a buffer and execute thunk on them.
    If any file does not already exist in the buffer list then that
    buffer is destroyed after thunk has been executed.

    If filename-list is a list then it's used directly, if it's
    a string we run string-to-words on it."
    (mapcar (lambda (filename)
              (if (not (get-file-buffer filename))
                  (let ((buf (find-file filename)))
                    (with-current-buffer buf
                      (funcall thunk)
                      (kill-buffer buf)))
                (with-current-buffer (get-buffer filename)
                  (funcall thunk))))
            (if (listp filename-list)
                filename-list
              (split-string filename-list))))

  ;; switch the entire module from one location to another, using the same
  ;; code base when being at different physical sites
  (defun my-cvs-hack ()
    "Toggle the CVS between local and remote"
    (interactive)
    (run-eval "find . -name 'Root'"
              (lambda (list-of-files)
                (map-files (lambda ()
                             (if (re-search-forward ":localhost:" nil 't)
                                 (replace-match ":rawls:")
                               (progn
                                 (re-search-forward ":rawls:" nil 't)
                                 (replace-match ":localhost:")))
                             (save-buffer))
                           list-of-files)))))

;; Unmodified-according-to-VC buffers use "-" as a separator in their VC
;; indicator, and modified buffer have ":" (e.g., "CVS-1.2" vs. "CVS:1.2").
;; The tooltip over the VC indicator also says more explicitly.

(GNUEmacs
 ;; Subversion
 (when (try-require 'psvn)

   ;; `svn-status-property-edit-svn-ignore' (`P TAB') allows user to edit
   ;; list of files ignored by Subversion

   ;; hide unmodified files
   (setq svn-status-hide-unmodified t)

   ;; use longer phrases
   (setq svn-status-short-mod-flag-p nil)

   ;; delete temporary files
   (setq svn-status-ediff-delete-temporary-files t)

   ;; show the diff we are about to commit
   (define-key svn-log-edit-mode-map (kbd "<f6>") 'svn-log-edit-svn-diff)

   ;; examine
   (global-set-key (kbd "C-x v e") 'svn-status)

   ;; examine without asking for a directory
   (global-set-key (kbd "<C-f9>")
                   '(lambda ()
                      (interactive)
                      (svn-status (file-name-directory (buffer-file-name))
                                  nil)))

   (defun my-svn-log-edit-mode-setup ()
     (setq ispell-local-dictionary "en_US")
     (flyspell-mode))

   (add-hook 'svn-log-edit-mode-hook 'my-svn-log-edit-mode-setup)))


(when (try-require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))



;;*** 32.2 (info "(emacs)Change Log")s

;; don't make a new entry, when the last entry was made by you and on the same
;; date
(setq add-log-always-start-new-record nil)

;; adds the file's version number to the change log entry
(setq change-log-version-info-enabled t)


;;*** 32.3 (info "(emacs)Tags") Tables

;; Using tags tables is the most generic approach to setup code navigation.
;; Support for it has been in Emacs for a long time. Any installation of Emacs
;; should also come with the etags program, which supports many different
;; languages, compare:
;;
;; cscope
;;     C, C++
;;
;; global
;;     C, C++, Yacc, Java and PHP4
;;
;; etags (emacs 23)
;;     C, Objective C, C++, Java, Fortran, Ada, Cobol, Erlang,
;;     Forth, HTML, LaTeX, Emacs Lisp/Common Lisp, Lua, Makefile,
;;     Pascal, Perl, PHP, Postscript, Python, Prolog, Scheme and
;;     most assembler-like syntaxes
;;
;; etags (exuberant)
;;     Asm, Asp, Awk, Basic, BETA, C, C++, C#, Cobol, Eiffel,
;;     Erlang, Fortran, HTML, Java, JavaScript, Lisp, Lua, Make,
;;     Pascal, Perl, PHP, Python, REXX, Ruby, Scheme, Sh, SLang,
;;     SML, SQL, Tcl, Vera, Verilog, Vim, YACC
;;
;; It doesn't do fancy stuff, e.g. keeping an index of function
;; references. That's the kind of thing gnu global and cscope can do for
;; you, if you're working with a language that they support.
;;
;; I should look at CEDET again, though. 8-)


;; By default, Emacs TAGS do not record positions where a function is _called_.
;; They record only positions where a function (or variable etc.) is _defined_.


;; First of all, you must build a `TAGS' file (which keeps the symbols from
;; your project, by scanning all the source and header files with the
;; `etags' command).

;; list of file names of tags tables to search
(setq tags-table-list
      '(
        "~/TAGS"
;;;         "/usr/share/texmf-texlive/tex/latex/TAGS"
        ))

;; For example, you can have a "make TAGS" Makefile target to do this for
;; you:
;;
;; TAGS:
;;      rm -f TAGS
;;      find $$(pwd) \( -name \*.el \
;;                   -o -name \*.[chCH] \
;;                   \) -print | etags -
;;
;; You can create a tags file by using `M-x compile RET tags RET'.

;; Alternatively,
(try-require 'sure-tags)
;; will make sure that tags file exists (and builds it if it doesn't),
;; allowing you to first rebuild the tags file or specify a new one when the
;; search fails.

;; After this, you can use a tags table with the command
;; `M-x visit-tags-table RET'.

;; You can search for *definitions* of tags that match your regexp, by using
;; `M-x find-tag' (bound to `M-.').
;; To continue searching for next alternate definition, use `C-u M-.'.
;; To jump back, use `M-*'.

(defun find-next-tag ()
  (interactive)
  (find-tag nil t))

;; select from multiple tags
(when (try-require 'etags-select)

    ;; do a `find-tag-at-point', and display all exact matches
    (global-set-key (kbd "M-?") 'etags-select-find-tag-at-point))

;; find the definition of the Emacs Lisp function or variable near point
(GNUEmacs
    (find-function-setup-keys))

;; You can search for *occurrences* of tags that match you regexp on all
;; files in tags table, by using `M-x tags-search RET'.
;; To continue searching for next match, use `M-,'.

;; There is a cscope interface for emacs. I recommend it. tags are
;; extremely limited imo. cscope does a much better, if slower, job.


;;*** 32.4 Merging Files with (info "(emacs)Emerge")

;; merge file diffs under Emacs control
(try-require 'emerge)

;; `M-x smerge-mode RET'
;; That does not automatically select regions but provides convenient key
;; bindings to navigate between conflicts and to choose the A or B variant

(message "32 Maintaining Programs... Done"))

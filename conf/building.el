;;** 31 (info "(emacs)Building") Compiling and Testing Programs ]

(when section-building (message "31 Compiling and Testing Programs...")

;; >> It's possible to see, while we are programming, if we did a mistake. In
;; >> eclipse, when we do an error, for example, forget a ; , an underline
;; >> appears in the line indicating that something is wrong. It's possible to
;; >> have something like this in Emacs?
;; >
;; > There's a CWarn mode for C and C++, but I don't know about similar
;; > features for Java.  Anyone?

;; flymake can compile in the background and colorize lines with
;; errors/warnings
;; http://flymake.sourceforge.net/
;; http://www.emacswiki.org/cgi-bin/wiki/JdeeFlymake

(when (try-require 'flymake)

    ;; Setting up flymake
    (defun activate-flymake ()
      "Activates flymake when real buffer and you have write access"
      (if (and (buffer-file-name) (file-writable-p buffer-file-name))
          (flymake-mode t)))

    ;; Adding errors to modeline
    ;; With this the error output of othe current line will appear right below
    ;; in the modeline
    (defun my-flymake-show-help ()
      (when (get-char-property (point) 'flymake-overlay)
        (let ((help (get-char-property (point) 'help-echo)))
          (if help (message "%s" help)))))
    (add-hook 'post-command-hook 'my-flymake-show-help))


;; my build command: `cd /path/to/Makefile && make -f Makefile'



;;*** 31.1 Running (info "(emacs)Compilation")s under Emacs

;; http://www.emacswiki.org/emacs-en/eproject
;;
;; It allows to define projects, and in each project to define menu commands
;; and shortcut keys as you like. For example:
;;
;; make (f9)               : `-in src make' OR `make'
;; clean (C-f9)            : `rm -vf src/emacs-23.* etc/DOC* && make clean' OR `make clean'
;; run (f8)                : `src/emacs' OR `./my-program'
;; stop (C-f8)             : `-e kill-compilation'
;; ---
;; configure               : `./configure'
;; install                 : `echo root-pass | sudo -S make install'


;; You don't need a Makefile to perform simple tasks, because Make knows a
;; lot of built in rules out of the box. For example, to compile a `.c'
;; source file `foo.c' into a program `foo', all you need is say
;; "make -k foo", and Make will do it even without a Makefile.

;; invoke a compiler with the same command as in the last invocation of
;; `compile'
(global-set-key (kbd "<f9>") 'recompile)

;; scroll the `*compilation*' buffer window to follow output as it appears
(setq compilation-scroll-output t)

;; number of lines in a compilation window
(setq compilation-window-height (* 2 5))

;; ;; I also don't like that the compilation window sticks around after a
;; ;; successful compile. After all, most of the time, all I care about
;; ;; is that the compile completed cleanly. Here's how I make the
;; ;; compilation window go away, only if there was no compilation
;; ;; errors:
;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (string-match "exited abnormally" str)
;;             ;; there were errors
;;             (message "Compilation errors, press C-x ` to visit")
;;           ;; no errors, make compilation window go away in 0.5 sec
;;           (run-at-time 0.5 nil 'delete-windows-on buf)
;;           (message "NO COMPILATION ERRORS!"))))


(GNUEmacs
    (defun cc-goto-first-error( buffer exit-condition )
      (with-current-buffer buffer
        (goto-char (point-min))
        (compilation-next-error 1)))

    (add-to-list 'compilation-finish-functions 'cc-goto-first-error))


;; (add-hook 'c-mode-hook
;;        (lambda ()
;;          (set (make-local-variable 'compile-command)
;;               (format "make %s"
;;                       (file-name-sans-extension
;;                        (file-name-nondirectory buffer-file-name))))))
;;
;; Just set the CC=gcc and CFLAGs="-Wall -O3" environment variables, and
;; voila!

(defvar make-clean-command "make clean all"
  "*Command used by the `make-clean' function.")

(defun make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (require 'compile) ;; needed for compile-internal
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal make-clean-command "No more errors"))

(global-set-key (kbd "<S-f9>") 'make-clean)


;;*** 31.2 (info "(emacs)Compilation Mode")

;; display the next compiler error message
(global-set-key (kbd "<f10>") 'next-error)

;; display the previous compiler error message
(global-set-key (kbd "<S-f10>") 'previous-error)

;; display the first compiler error message
(global-set-key (kbd "<C-f10>") 'first-error)

;; highlight and parse the whole compilation output as soon as it arrives
(setq compile-auto-highlight t)


;; Some code that will make it so the background color of the lines that gcc
;; found errors on, should be in another color.

(defvar all-overlays ())

(defun delete-this-overlay (overlay is-after begin end &optional len)
  (delete-overlay overlay))

(defun highlight-current-line ()
  (interactive)
  (setq current-point (point))
  (beginning-of-line)
  (setq beg (point))
  (forward-line 1)
  (setq end (point))
  ;; Create and place the overlay
  (setq error-line-overlay (make-overlay 1 1))

  ;; Append to list of all overlays
  (setq all-overlays (cons error-line-overlay all-overlays))

  (overlay-put error-line-overlay
               'face '(background-color . "pink"))
  (overlay-put error-line-overlay
           'modification-hooks (list 'delete-this-overlay))
  (move-overlay error-line-overlay beg end)
  (goto-char current-point))

(defun delete-all-overlays ()
  (while all-overlays
    (delete-overlay (car all-overlays))
    (setq all-overlays (cdr all-overlays))))

(defun highlight-error-lines (compilation-buffer, process-result)
  (interactive)
  (delete-all-overlays)
  (condition-case nil
      (while t
        (next-error)
            (highlight-current-line))
    (error nil)))

(setq compilation-finish-function 'highlight-error-lines)


;;*** 31.4 (info "(emacs)Grep Searching") under Emacs

;; ignore case distinctions in the default grep command
;;(if (my-file-executable-p "~/bin/wcgrep")
(setq grep-command "grep -n -i -e ")

;; grep + emacs 22 + cygwin does not follow file links
;; try adding "-nH" to your grep options.

;; The commands lgrep and rgrep are somehow more user-friendly than the M-x
;; grep command. The word at point can be captured using the command
;; (thing-at-point 'word). So you may try:
;;
;; (defun my-grep ()
;;   "look for word at point in files ending by .cpp and .h
;;    recursively starting from the work directory"
;;   (interactive)
;;   (rgrep (thing-at-point 'word) "*.cpp *.h" "~/work"))
;;
;; (global-set-key [(control shift f)] 'my-grep)


;;*** 31.6 Running (info "(emacs)Debuggers") Under Emacs

;; > Enable debug-on-error via 'M-x toggle-debug-on-error', then start
;; > flyspell-mode again and examine the error. If that does not work, try
;; > edebug. Open the file where flyspell-mode is defined. Reeval the
;; > function with 'C-u C-M-x' and again, start flyspell-mode. Now you are
;; > in edebug-mode. Hit Space till you get the error. Press 'i' to enable
;; > debugging of the called function after point.
;;
;; The cursor has to be inside the flyspell-mode function for this to work.
;; (C-M-x evals the current function , with prefix it also installs the
;;        debug routines.)
;; Alternatively this should enable edebug on all forms in the current buffer:
;; M-x edebug-all-defs
;; M-x eval-buffer


;;**** Debugging Mercury programs

;; 1. Put these lines in your .emacs file:

;; (setq mercury-dir (getenv "MERCURY_DIR"))
;; (load-file (concat mercury-dir "/lib/mercury/elisp/gud.el"))
;; (setq mdb-command-name "bash.exe mdb ./mas_server.exe
;;  -c ../online/mas_server/mas_config_local.xml -d ../data"))

;; 2. To start the debugger, open a file in your build directory,
;;    e.g. build/Makefile

;; 3. Run M-x and then type mdb

;; 4. At the prompt you should see the command from the .emacs file:
;; "bash.exe mdb ./mas_server.exe
;;  -c ../online/mas_server/mas_config_local.xml -d ../data"

;; Change if necessary and hit the `RET' key

;; 5. Find your bugs.

;; Known problems:
;;   - tab completion doesn't work


;;**** Debugging Lisp programs

;; Emacs has the basic debugger/stack trace, but it also has the edebug
;; facility, which is very powerful, for the more complex situation.

;; , (info "(elisp)Edebug") ]
;; | Edebug is a source-level debugger for Emacs Lisp programs with which
;; | you can:
;; |
;; |    * Step through evaluation, stopping before and after each expression.
;; |
;; |    * Set conditional or unconditional breakpoints.
;; |    [...]
;; `----

;; You can cause the debugger to be called at a certain point in your program
;; by writing the expression `(debug)' at that point. To do this, visit the
;; source file, insert the text `(debug)' at the proper place, and type
;; `C-M-x'.

;; `c'  Exit the debugger and continue execution
;; `d'  Continue execution, but enter the debugger the next time any Lisp
;;      function is called.

(define-key emacs-lisp-mode-map (kbd "C-x x") 'edebug-eval-top-level-form)
(define-key emacs-lisp-mode-map (kbd "C-x x") 'edebug-defun) ; other binding onto same key

(autoload 'edebug-eval-top-level-form "edebug")

(setq edebug-global-prefix "\C-xX")

(add-hook 'cl-load-hook
          (lambda ()
            (add-hook 'edebug-setup-hook
                      (lambda ()
                        (load-library "cl-specs")))))

;; toggle whether to enter Lisp debugger when an uncaught error is signaled
;; (global-set-key [(super c) (d)] 'toggle-debug-on-error)


;;*** 31.7 (info "(emacs)Executing Lisp") Expressions

(require 'lisp-mode)

;; nuke and reevaluate an elisp buffer
(try-require 'nukneval)

(add-hook 'emacs-lisp-mode-hook 'my-elisp-extra-keys)
(defun my-elisp-extra-keys ()
  ;; auto-indentation
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "\C-cc" 'nuke-and-eval))


;;*** 31.9 (info "(emacs)Lisp Eval") Expressions

;; enable the use of the command `eval-expression' without confirmation
(put 'eval-expression 'disabled nil)

;; enhanced eval-expression command
(when (try-require 'eval-expr)
    (eval-expr-install))


;;*** 31.10 (info "(emacs)Lisp Interaction") Buffers

;; to evaluate a non-interactive command, simply use IELM!

;; interaction mode for Emacs Lisp
(autoload 'ielm "ielm" "Start an inferior Emacs Lisp session" t)


;;*** 31.11 Running an (info "(emacs)External Lisp")

;; Just as in C, C++, Java, Perl, Python, etc, Lisp code is kept in files. All
;; the normal editing operations are performed on files. In this respect,
;; hacking in Lisp is like hacking in any other language that you are used to.
;; What's different is that what you are hacking is a running Lisp program.
;; When you edit a function definition or add a new one, you compile it into a
;; running program. There is no compile, link, run, debug cycle as you know it
;; from C or Java.
;;
;; Ponder that for a minute.
;;
;; When you fix a bug in a C function, you have to recompile, relink, and
;; reload your program before you can test the fix. You don't do that in Lisp.
;; You make the fix and then go straight to testing it. This process can be
;; even faster than fixing a bug in a scripting language like Perl.

;; see http://svn.peadrop.com/emacs/lisp/lisp-config.el

;; superior Lisp inferior mode extension
(try-idle-require 'slime)
(eval-after-load 'slime
  '(progn

    ;; indentation
    (slime-setup)

    (add-hook 'lisp-mode-hook
              (lambda ()
                (slime-mode t)))
    (add-hook 'inferior-lisp-mode-hook
              (lambda ()
                (inferior-slime-mode t)))

    ;; Gnu CLISP - Inferior Lisp Mode & ILISP (switches for ANSI & no banner)
    ;; TODO Have a look at SBCL
    (defvar clisp-dir
      (cond (running-ms-windows
             "e:/vmware-home/bin/win32/clisp-2.31/full/")
            (t
             "/usr/bin")))

    (defvar clisp-exe
      (cond (running-ms-windows
             (concat clisp-dir "lisp.exe"))
            (t
             (concat clisp-dir "/" "clisp"))))

    ;; optionally, specify the Lisp program you are using. Default is "lisp".
    ;; include the full linking set with `-K full'
    (setq inferior-lisp-program
          (cond (running-ms-windows
                 (concat clisp-exe
                         " -B " clisp-dir
                         " -M " clisp-dir "lispinit.mem"
                         " -ansi -q"))
                (t
                 (concat clisp-exe
                         " -B " clisp-dir
                         " -ansi -q"))))  ;; "clisp -K full"

    ;; connect automatically to my Lisp when opening a Lisp file
    (defun cliki:start-slime ()
      (unless (slime-connected-p)
        (save-excursion (slime))))
    (add-hook 'slime-mode-hook 'cliki:start-slime)

    ;; automatically show documentation for code near the point
    (add-hook 'slime-mode-hook
              (lambda ()
                (slime-autodoc-mode t)))

;;   ;; GNU CLISP - http://clisp.cons.org/
;;   (defun clisp-start ()
;;     (interactive)
;;     (shell-command (concat "c:/bin/clisp-2.32/full/lisp.exe "
;;                            "-B c:/bin/clisp-2.32/full/ "
;;                            "-M c:/bin/clisp-2.32/full/lispinit.mem "
;;                            "-i c:/usr/home/.slime.lisp "
;;                            "-ansi -q&"))))


; Functions and key bindings for getting Emacs to interact with GCL.
; Thomas R. Ioerger, Dept of Computer Science, Texas A&M University
; see http://www.cs.tamu.edu/faculty/ioerger/emacs-gcl.html for more details

(global-set-key "\C-t" '(make-keymap))

(defun run-gcl ()
  (interactive)
  (split-window)
  (other-window 1)
  (inferior-lisp "gcl"))

(defun gcl-debug-quit ()
  (interactive)
  (comint-send-string "*inferior-lisp*" ":q\C-M"))

(defun gcl-quit ()
  (interactive)
  (comint-send-string "*inferior-lisp*" "(bye)\C-M"))

(defun gcl-eval-buffer ()
  (interactive)
  (set-mark 0)
  (goto-char (point-max))
  (lisp-eval-region 1 (point))
  (exchange-point-and-mark))

(global-set-key "\C-tl" 'run-gcl)
(global-set-key "\C-te" 'lisp-eval-defun)
(global-set-key "\C-tw" 'switch-to-lisp) ; split screen!
(global-set-key "\C-tq" 'gcl-debug-quit)
(global-set-key "\C-tb" 'gcl-eval-buffer)
(global-set-key "\C-tx" 'gcl-quit)

; commands (after prefix of control-t)
; l = start lisp
; e = eval current expression
; w = switch to lisp buffer
; q = quit from debugger back to top-level
; b = eval buffer
; x = kill lisp process

))

(message "31 Compiling and Testing Programs... Done"))

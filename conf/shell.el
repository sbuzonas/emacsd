;;** 41 Running (info "(emacs)Shell") Commands from Emacs ]

(when section-shell (message "41 Running Shell Commands from Emacs...")

;;*** 41.1 Single Shell

;; force interactive behavior (to get my handy shell aliases)
;; FIXME Fix for Zsh (zsh:1: command not found: shopt)
;; (defadvice shell-command (before my-shell-command activate)
;;   (ad-set-arg 0
;;               (concat "source ~/.bashrc; shopt -s -q expand_aliases;\n "
;;                       (ad-get-arg 0))))

;; for single shell commands
(setq shell-file-name                   ; must be in the `PATH' (Windows users)
      (if (file-executable-p "/usr/bin/zshXXX")
          "zsh"
        "bash"))

;; use `shell-file-name' as the default shell
(when (try-require 'env)
  (setenv "SHELL" shell-file-name))

;; name of shell used to parse TeX commands
(GNUEmacs
 (setq TeX-shell shell-file-name))
(XEmacs
 ;; for the `preview-latex' package
 (setq TeX-shell "C:/Program Files/Emacs/emacs/bin/cmdproxy.exe"))


;;*** 41.2 Interactive Shell

;; for the interactive (sub)shell
(setq explicit-shell-file-name shell-file-name)

;; args passed to inferior shell by `M-x shell', if the shell is bash
(setq explicit-bash-args '("--noediting" "--login"))
;; FIXME This ensures that /etc/profile gets read (at least for Cygwin). Is
;; this good?


;;*** 41.3 Shell Mode

;; general command interpreter in a window stuff
(when (try-require 'comint)

  ;; `M-s'    `comint-next-matching-input'
  ;; `M-r'    `comint-previous-matching-input'
  ;; `M-n'    `comint-next-input'
  ;; `M-p'    `comint-previous-input'
  ;; `C-up'   `last command'

  ;; regexp to recognize prompts in the inferior process
  ;; (set it for Org-babel sh session to work!)
  (defun set-shell-prompt-regexp ()
    (setq comint-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
  (add-hook 'shell-mode-hook 'set-shell-prompt-regexp)
  ;; FIXME See `shell-prompt-pattern'

  ;; don't add input matching the last on the input ring
  (setq-default comint-input-ignoredups t)

  ;; input to interpreter causes (only) the selected window to scroll
  (setq-default comint-scroll-to-bottom-on-input "this")

  ;; output to interpreter causes (only) the selected window to scroll
  (setq-default comint-scroll-to-bottom-on-output "this")

  ;; show the maximum output when the window is scrolled
  (setq-default comint-scroll-show-maximum-output t)

  ;; ignore short commands as well as duplicates
  (setq comint-min-history-size 5)
  (make-variable-buffer-local 'comint-min-history-size)
  (setq-default comint-input-filter
                (function
                 (lambda (str)
                   (and (not (string-match "\\`\\s *\\'" str))
                        (> (length str) comint-min-history-size)))))

  ;; functions to call after output is inserted into the buffer
  (setq-default comint-output-filter-functions
                ;; go to the end of buffer
                '(comint-postoutput-scroll-to-bottom))

  ;; get rid of the ^M characters
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

  ;; prompt in the minibuffer for password and send without echoing
  ;; (for example, with `su' command)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)

  ;; use the `up' and `down' arrow keys to traverse through the previous
  ;; commands
  (defun my-shell-mode-hook ()
    "Customize my shell-mode."
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input))
  (add-hook 'shell-mode-hook 'my-shell-mode-hook))


;;*** 41.4 Shell Prompts

;;*** 41.5 History

;;*** 41.6 Directory Tracking

;;*** 41.7 Options

;;*** 41.8 Terminal emulator

;;*** 41.9 Term Mode

;;*** 41.10 Paging in Term

;;*** 41.11 Remote Host

;;*** 41.12 Serial Terminal

;; In GNU Emacs 23, there's now support for serial port access. The new
;; command `serial-term' starts an interactive terminal on a serial port.




;; You're debugging bash code? I normally use `mode-compile.el' for
;; this. Basically, it runs bash with lots of debug output.

;; See `w32-settings.el' for more!

;; quote process arguments to ensure correct parsing on Windows
(setq w32-quote-process-args t)

;; switch used to have the shell execute its command line argument
;; (`/c' does not work with XEmacs)
(setq shell-command-switch "-c")

;; shell argument indicating that next argument is the command
(setq TeX-shell-command-option shell-command-switch)

;; regexp to match prompts in the inferior shell
(setq shell-prompt-pattern (concat "^" (system-name) " [^ ]+ \\[[0-9]+\\] "))

;; translate ANSI escape sequences into faces
(GNUEmacs
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


;; Emacs shell is a "dump" terminal which doesn't support (all) terminal
;; control codes. You need to use `M-x term' if you want a proper terminal
;; (but even that is not as good as the good old XTerm).
;;
;; (term "bash")
;; If you do `M-x term', `bash' is offered as the default as well.
;;
;; Need a good terminal emulation for applications that are interactive with
;; your terminal, in the same way `top' is working or `man' ... or `less' ...
;; or `unison'

;; "less" needs a _terminal_. Emacs's shell-mode is not a terminal so "less"
;; doesn't work. If the programs you use needs terminal features then you must
;; use a terminal, such as "M-x term".

;; This "page-at-a-time" feature comes with terminal emulation. There's no
;; need to make "M-x shell" a real terminal because there already is one: "M-x
;; term".

;; Try also to use `M-x ansi-term' that is much better than `term': with M-x
;; term, you can run only one shell; with `ansi-term' you can start more than
;; one (*ansi-term*<2>, *ansi-term*<3>).

;; One weird thing, is that 'M-x term' is a term emulation where less(1)
;; works and it has the 'C-c C-q' one-page-at-a-time thing as well...
;;
;; M-x shell, has neither.

;; >> Or are there some things `M-x term' can't do while `M-x shell' can?

;; > With M-x shell you're using emacs to construct the command to pass to
;; > the shell. This is an advantage if you shell doesn't keep a history or
;; > offers no completion mechanism. Also, you can use isearch to search
;; > through the command output, copy stuff into the kill ring or use the
;; > rectangle functions. Or you might just prefer the emacs keybindings
;; > over the one's your shell offers. 8-)

;; I'd like to point out that term-mode has also the so called "line mode"
;; (C-c C-j) in which user can wander around the buffer pretty much like
;; anywhere else in Emacs. Kill-ring commands, isearch etc. work. Then
;; there is "char mode" (C-c C-k), the default, which is like your normal
;; terminal emulator except the escape key.

;; Actually you can read this in the emacs info pages: "In line mode,
;; Term basically acts like Shell mode".

;; The problem here is the word 'basically' which probably means
;; 'almost'. So they differ in the details.

;; In my case, I started to use `M-x shell', and like it since I almost
;; never have to use applications that need a real term emulation. But if
;; you look at them (M-x term and M-x shell) carefully, then `M-x term'
;; has some weird behaviours when using the shell mode key bindings; try
;; for instance: `C-c C-e', `C-c C-a', `C-c C-o'... but as I said they're
;; details.


;; managing multiple terminal buffers in Emacs
;; (and fixing some troubles of `term-mode': key bindings, etc.)
(try-require 'multi-term)


;; run an inferior shell, with I/O through buffer `*shell*'
(global-set-key [(control !)]
                (cond (running-ms-windows 'shell)
                      (t 'term)))

;; you can switch between term modes. Using `C-c c-j' will put you
;; in line mode where term is behaves like a buffer , then u can use
;; `C-c c-k' to switch back to char mode.


;; run a telnet session from within an Emacs buffer
(when (try-require 'telnet)

  ;; program to run to open a telnet connection
  ;; simple public domain telnet client for Windows console
  ;; (from Igor Milavec)
  (setq telnet-program
        (cond (running-ms-windows
               (concat my-site-lisp-directory "../bin/telnet.exe"))
              (t
               "/usr/bin/telnet")))

  ;; open a network login connection to a host
  (defun telnet (host)
    "Open a network login connection to host named HOST (a string).
    Communication with HOST is recorded in a buffer `*telnet-HOST*'.
    Normally input is edited in Emacs and sent a line at a time."
    (interactive "sOpen telnet connection to host: ")
    (let* ((comint-delimiter-argument-list '(?\  ?\t))
           (name (concat "telnet-" (comint-arguments host 0 nil) ))
           (buffer (get-buffer (concat "*" name "*")))
           process)
      (cond ((string-equal system-type "windows-nt")
             (setq telnet-new-line "\n")))
      (if (and buffer (get-buffer-process buffer))
          (pop-to-buffer (concat "*" name "*"))
        (pop-to-buffer (make-comint name telnet-program nil host))
        (setq process (get-buffer-process (current-buffer)))
        (set-process-filter process 'telnet-initial-filter)
        (accept-process-output process)
        (telnet-mode)
        (setq comint-input-sender 'telnet-simple-send)
        (setq telnet-count telnet-initial-count)))))

(message "41 Running Shell Commands from Emacs... Done"))

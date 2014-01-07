;;** 43 (info "(emacs)Printing") Hard Copies

(when section-printing (message "43 Printing Hard Copies...")

;; print Emacs buffer on line printer
;; for {lpr,print}-{buffer,region}
(when (try-require 'lpr)

    ;; name of program for printing a file
    (setq lpr-command "enscript")  ; TODO Install `enscript'

    ;; list of strings to pass as extra options for the printer program
    (setq lpr-switches (list "--font=Courier8"
                             "--header-font=Courier10"
                             (format "--header=%s" (buffer-name))))

    ;; name of a printer to which data is sent for printing
    (setq printer-name
          (cond (running-ms-windows
                 "//PRINT-SERVER/C510_APS")
                (t
                 t))))

;; 43 Printing Hard Copies...
;; Checking for library `lpr'... Found
;; Checking for library `ps-print'... Found [3 times]
;; Checking for library `filladapt'... Found
;; Tramp: Opening connection for fni@C using ssh...
;; Tramp: Waiting 60s for local shell to come up...
;; Tramp: Sending command `ssh C -l fni  -q -e none && exit || exit'
;; Tramp: Waiting for prompts from remote shell
;; File error: Process died




;; print text from the buffer as PostScript
(when (try-require 'ps-print-XXX)

    (let ((gsprint-program "C:/Program Files/Ghostgum/gsview/gsprint.exe"))
      (my-file-executable-p gsprint-program)

      (if (and gsprint-program
               (file-executable-p gsprint-program))
          (progn
            ;; name of a local printer for printing PostScript files
            ;; adjusted to run Ghostscript
            (setq ps-printer-name t)

            ;; name of program for printing a PostScript file
            ;; tell Emacs where ghostscript print utility is located
            (setq ps-lpr-command gsprint-program)

            ;; list of extra switches to pass to `ps-lpr-command'
            ;; tell Ghostscript to query which printer to use
            (setq ps-lpr-switches '("-query")))

        (progn
          (setq ps-printer-name "//PRINT-SERVER/LexmarkC510")
          (setq ps-lpr-command "")
          (setq ps-lpr-switches '("raw"))))

    ;; size of paper to format for
    (setq ps-paper-type 'a4)

    ;; print in portrait mode
    (setq ps-landscape-mode nil)

    ;; number of columns
    (setq ps-number-of-columns 1)))

;; generate and print a PostScript image of the buffer
(GNUEmacs
    (when running-ms-windows
      (w32-register-hot-key [snapshot]) ; override `Print Screen' globally
                                        ; used as a hotkey by Windows
      (global-set-key (kbd "<snapshot>") 'ps-print-buffer-with-faces)))
(XEmacs
    (setq toolbar-print-function 'ps-print-buffer-with-faces))

(global-set-key (kbd "M-p") 'ps-print-buffer-with-faces)

(message "43 Printing Hard Copies... Done"))

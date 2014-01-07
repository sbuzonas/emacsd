;;** 28 (info "(emacs)Indentation")

;; Just hit C-j -- as in other modes, it runs the command
;; `newline-and-indent'.
;; Usually one binds `RET' to `newline-and-indent'.

(when section-indentation (message "28 Indentation...")

;;*** 28.1 (info "(emacs)Indentation Commands") and Techniques

;; `C-M-\' runs the command `indent-region' (which does the job of
;; the imaginary command `unsuck-html-layout' in `html-mode')

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)))


;;*** 28.3 Tabs vs. (info "(emacs)Just Spaces")

;; indentation can't insert tabs
(setq-default indent-tabs-mode nil)

(message "28 Indentation... Done"))

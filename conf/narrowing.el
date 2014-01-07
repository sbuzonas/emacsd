;;** 48 (info "(emacs)Narrowing")

(when section-narrowing (message "48 Narrowing...")

;; enable the use of the command `narrow-to-region' without confirmation
(put 'narrow-to-region 'disabled nil)

(message "48 Narrowing... Done"))

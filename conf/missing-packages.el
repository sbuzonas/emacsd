;;* Recovery from Problems

;;** Debugging

(when section-debugging (message "99 Debugging...")

;; ;; get the backtrace when uncaught errors occur
;; (setq debug-on-error nil)  ; was set to `t' at beginning of buffer

;; warn that some packages were missing
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))

(message "99 Debugging... Done"))

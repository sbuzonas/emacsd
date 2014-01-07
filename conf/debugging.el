;;** Debugging

(when section-debugging (message "0 Debugging...")

;; ;; get the backtrace when uncaught errors occur
;; (setq debug-on-error t)  ; will be unset at the end

(XEmacs
    (setq stack-trace-on-error t))

;; ;; hit `C-g' while it's frozen to get an ELisp backtrace
;; (setq debug-on-quit t)

(message "0 Debugging... Done"))

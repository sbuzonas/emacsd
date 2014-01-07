;;** 16 (info "(emacs)CUA Bindings")

(when section-cua-bindings (message "16 CUA Bindings...")

;; CUA mode sets up key bindings used in many other applications (`C-x',
;; `C-c', `C-v' and `C-z').
;;
;; The `C-x' and `C-c' keys only do cut and copy when the region is active, so
;; in most cases, they do not conflict with the normal function of these
;; prefix keys.
;;
;; If you really need to perform a command which starts with one of the prefix
;; keys even when the region is active, you have three options:
;;
;; - press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds, or
;; - use the SHIFT key with the prefix key, i.e. `C-S-x' or `C-S-c'.
;;
;; You can customize `cua-enable-cua-keys' to completely disable the CUA
;; bindings, or `cua-prefix-override-inhibit-delay' to change the prefix
;; fallback behavior.

;; CUA mode also provides enhanced rectangle support with visible rectangle
;; highlighting. Check out "Emacs Column Editing" at
;; http://www.vimeo.com/1168225?pg=embed&sec=1168225.
;;
;; `C-RET' runs the command `cua-set-rectangle-mark'
;; `M-n' runs the command `cua-sequence-rectangle'

;; ;; activate CUA mode
;; (cua-mode t)

;; standard Windows behavior
(setq cua-keep-region-after-copy t)

;; fix funny things of cursor moving commands
(add-hook 'cua-mode-hook
          (lambda ()
            (dolist (cmd '(forward-char
                           backward-char
                           previous-line
                           next-line
                           forward-paragraph
                           backward-paragraph
                           beginning-of-buffer
                           end-of-buffer))
              (put cmd 'CUA nil))))

(message "16 CUA Bindings... Done"))

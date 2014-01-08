;;** 21 (info "(emacs)Keyboard Macros")

(when section-keyboard-macros (message "21 Keyboard Macros...")

;;*** 21.1 (info "(emacs)Basic Keyboard Macro") Use

;; Replace default "M-x" functionality
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "S-M-x") 'smex-major-mode-commands)
;; expose the old "M-x"
(global-set-key (kbd "C-c C-c M-x") 'executed-extended-command)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Swap default and regex search bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)



;; If you want to check the result each time before repeating, then
;; `C-x e e e...'.
;; If you want to repeat only N times, then `C-u N C-x e'.
;; If you want to repeat forever or until error, then `C-u 0 C-x e'.

;; <shift>-<F8>  to start recording
;; <shift>-<F8>  again to stop recording
;; <F8>          to call it

(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; start/stop recording a keyboard macro
(global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)

;; execute the most recent keyboard macro
(global-set-key (kbd "<f8>") 'call-last-kbd-macro)

;;*** 21.5 Name and (info "(emacs)Save Keyboard Macro")s

;; assign a name to the last keyboard macro defined
(global-set-key (kbd "<C-f8>") 'name-last-kbd-macro)

(message "21 Keyboard Macros... Done"))

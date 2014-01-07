;;** 21 (info "(emacs)Keyboard Macros")

(when section-keyboard-macros (message "21 Keyboard Macros...")

;;*** 21.1 (info "(emacs)Basic Keyboard Macro") Use

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

(global-set-key (kbd "M-g") 'goto-line)

;; Toggle line numbers
(global-set-key (kbd "C-c n") 'linum-mode)

;; Make C-a jump between start of line and start of indentation
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-p' (or `M-n')
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

;; shortcuts under c-x for buffer start and end
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)

;; Use shell-like backspace C-h, rebind help to F1
;;(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which use to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Buffer manipulations
(global-set-key "\M-\"" 'shell-command-on-buffer)
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Window switching, see also the tmux implementation
(global-set-key [S-left] 'windmove-left)
(global-set-key [S-right] 'windmove-right)
(global-set-key [S-up] 'windmove-up)
(global-set-key [S-down] 'windmove-down)

;; Change next underscore with a camel case
;; (global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
;; (global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Toggle quotes
(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

(defun custom-key-sequences ()
  "Add some escape sequences defined in iterm for keys that aren't normally sent to terminal."
  (interactive)
  (define-key input-decode-map "\e[27;5;9~" [C-tab])
  (define-key input-decode-map "\e[27;6;9~" [C-S-tab]))

(defun fix-arrow-keys ()
  "Some arrow key combinations are mapped incorrectly. This will fix it."
  (interactive)
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;6A" [C-S-up])
  (define-key input-decode-map "\e[1;6B" [C-S-down])
  (define-key input-decode-map "\e[1;6C" [C-S-right])
  (define-key input-decode-map "\e[1;6D" [C-S-left])
  (define-key input-decode-map "\e[1;4A" [M-S-up])
  (define-key input-decode-map "\e[1;4B" [M-S-down])
  (define-key input-decode-map "\e[1;4C" [M-S-right])
  (define-key input-decode-map "\e[1;4D" [M-S-left])
  (define-key input-decode-map "\e[1;7A" [C-M-up])
  (define-key input-decode-map "\e[1;7B" [C-M-down])
  (define-key input-decode-map "\e[1;7C" [C-M-right])
  (define-key input-decode-map "\e[1;7D" [C-M-left]))

(defun fix-function-keys ()
  "Function keys with modifiers are missing configuration. This will fix it."
  (interactive)
  (define-key local-function-key-map "\eOP" [f1])
  (define-key local-function-key-map "\eOQ" [f2])
  (define-key local-function-key-map "\eOR" [f3])
  (define-key local-function-key-map "\eOS" [f4])
  (define-key local-function-key-map "\e[15~" [f5])
  (define-key local-function-key-map "\e[17~" [f6])
  (define-key local-function-key-map "\e[18~" [f7])
  (define-key local-function-key-map "\e[19~" [f8])
  (define-key local-function-key-map "\e[20~" [f9])
  (define-key local-function-key-map "\e[21~" [f10])
  (define-key local-function-key-map "\e[23~" [f11])
  (define-key local-function-key-map "\e[24~" [f12])
  (define-key local-function-key-map "\eO2P" [S-f1])
  (define-key local-function-key-map "\eO2Q" [S-f2])
  (define-key local-function-key-map "\eO2R" [S-f3])
  (define-key local-function-key-map "\eO2S" [S-f4])
  (define-key local-function-key-map "\e[15;2~" [S-f5])
  (define-key local-function-key-map "\e[17;2~" [S-f6])
  (define-key local-function-key-map "\e[18;2~" [S-f7])
  (define-key local-function-key-map "\e[19;2~" [S-f8])
  (define-key local-function-key-map "\e[20;2~" [S-f9])
  (define-key local-function-key-map "\e[21;2~" [S-f10])
  (define-key local-function-key-map "\e[23;2~" [S-f11])
  (define-key local-function-key-map "\e[24;2~" [S-f12])
  (define-key local-function-key-map "\e[11;5~" [C-f1])
  (define-key local-function-key-map "\e[12;5~" [C-f2])
  (define-key local-function-key-map "\e[13;5~" [C-f3])
  (define-key local-function-key-map "\e[14;5~" [C-f4])
  (define-key local-function-key-map "\e[15;5~" [C-f5])
  (define-key local-function-key-map "\e[17;5~" [C-f6])
  (define-key local-function-key-map "\e[18;5~" [C-f7])
  (define-key local-function-key-map "\e[19;5~" [C-f8])
  (define-key local-function-key-map "\e[20;5~" [C-f9])
  (define-key local-function-key-map "\e[21;5~" [C-f10])
  (define-key local-function-key-map "\e[23;5~" [C-f11])
  (define-key local-function-key-map "\e[24;5~" [C-f12])
  (define-key local-function-key-map "\e[11;6~" [C-S-f1])
  (define-key local-function-key-map "\e[12;6~" [C-S-f2])
  (define-key local-function-key-map "\e[13;6~" [C-S-f3])
  (define-key local-function-key-map "\e[14;6~" [C-S-f4])
  (define-key local-function-key-map "\e[15;6~" [C-S-f5])
  (define-key local-function-key-map "\e[17;6~" [C-S-f6])
  (define-key local-function-key-map "\e[18;6~" [C-S-f7])
  (define-key local-function-key-map "\e[19;6~" [C-S-f8])
  (define-key local-function-key-map "\e[20;6~" [C-S-f9])
  (define-key local-function-key-map "\e[21;6~" [C-S-f10])
  (define-key local-function-key-map "\e[23;6~" [C-S-f11])
  (define-key local-function-key-map "\e[24;6~" [C-S-f12]))

;; Map function keys to modifiers
(add-hook 'term-setup-hook (lambda ()
                             (fix-arrow-keys)
			     (fix-function-keys)
			     (custom-key-sequences)))

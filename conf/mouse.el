(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(autoload 'mwheel-install "mwheel" "Enable wheely mouse")
(mwheel-install)

(defun scroll-up-one ()
  (interactive)
  (scroll-up 1))
(defun scroll-down-one ()
  (interactive)
  (scroll-down 1))
(defun scroll-up-slightly ()
  (interactive)
  (scroll-up 5))
(defun scroll-down-slightly ()
  (interactive)
  (scroll-down 5))

(defun fix-mouse-bindings ()
  "Remap input keybindings to actual modifier combinations"
  (interactive)
;;  (define-key key-translation-map [mouse-2] [mouse-3])
;;  (define-key key-translation-map [mouse-3] [mouse-2])
  (define-key key-translation-map [mouse-20] [C-mouse-5])
  (define-key key-translation-map [mouse-21] [C-mouse-4]))

(add-hook 'term-setup-hook (lambda ()
                             (fix-mouse-bindings)))

(defadvice terminal-init-xterm (after fix-mouse-bindings activate)
  (fix-mouse-bindings))

(unless window-system
  (global-set-key [mouse-3] 'mouse-popup-menubar-stuff)
  (global-set-key [mouse-4] 'scroll-down-slightly)
  (global-set-key [mouse-5] 'scroll-up-slightly)
  (global-set-key [C-mouse-4] 'scroll-up-one)
  (global-set-key [C-mouse-5] 'scroll-down-one)
  (global-set-key [S-mouse-1] 'beginning-of-buffer))

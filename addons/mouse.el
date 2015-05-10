(defaddon mouse
  "Provides mouse support in terminal"

  (autoload 'mwheel-install "mwheel" "Enable wheely mouse")

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

  (defun setup-terminal-mouse ()
    (unless window-system
      (xterm-mouse-mode t)
      (defun track-mouse (e))
      (setq mouse-sel-mode t)

      (mwheel-install)))

  (defun nomad/fix-mouse-bindings ()
    "Remap input keybindings to actual modifier combinations"
    (interactive)
    (define-key key-translation-map [mouse-20] [C-mouse-5])
    (define-key key-translation-map [mouse-21] [C-mouse-4]))
  (advice-add 'terminal-init-xterm :after #'nomad/fix-mouse-bindings))

  

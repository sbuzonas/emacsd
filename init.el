(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Load package manager

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Personalization

(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'fancyguy)

(load "~/.emacs.d/conf/keybindings.el")
(load "~/.emacs.d/conf/themes.el")

(message "* --[ Loading Emacs init.el ]--")
(setq emacs-load-start-time (current-time))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; turn on Common Lisp support
;(require 'cl)

(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called."
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path))

(defun load-config (config-name)
  "Loads a configuration file located in .emacs.d/conf/CONFIG-NAME.el"
  (let ((file-relative-path (concat "conf/" config-name ".el")))
    (load (fullpath-relative-to-current-file file-relative-path))))

(add-to-list 'load-path "~/.emacs.d/elisp")

(load-config "sections")

;; Configuration files to load
(defvar my-sections
  '(environment
    loading-libraries
    packages
    mouse
    hooks
    debugging
    screen
    basic
    minibuffer
    help
    mark
    killing
    yanking
    rectangles
    cua-bindings
    registers
    display
    search
    fixit
    keyboard-macros
    buffers
    frames
    international
    major-modes
    indentation
    text
    programs
    building
    maintaining
    abbrevs
    dired
    document-view
    gnus
    shell
    emacs-server
    printing
    sorting
    narrowing
    saving-emacs-sessions
    hyperlinking
    amusements
    customization
    emacs-display
    missing-packages
    extra)
  "A list of configuration files to load.")

(message "* --[ Loading configuration files ]--")
(dolist (s my-sections)
  (load-config (symbol-name s)))


;(require 'fancyguy)
(require 'vagrant)

;(load "~/.emacs.d/conf/settings.el")
;(load "~/.emacs.d/conf/keybindings.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" default)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

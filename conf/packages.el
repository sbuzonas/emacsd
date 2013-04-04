(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My Packages
(defvar my-packages
  '(clojure-mode
    clojurescript-mode
    find-file-in-git-repo
    gist
    magit
    nrepl
    paredit
    rainbow-mode
    smex
    smooth-scroll
    solarized-theme
    starter-kit
    starter-kit-bindings
    starter-kit-eshell
    starter-kit-js
    starter-kit-lisp
    tango-2-theme
    tidy
    undo-tree
    virtualenv
    wget
    yaml-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

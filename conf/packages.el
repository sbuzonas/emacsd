(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My Packages
(defvar my-packages
  '(smex
    idle-require
    unbound
    browse-kill-ring
    bm
    pager
    highlight
    highlight-symbol
    column-marker
    speedbar
    sr-speedbar
    pp-c-l
    csv-mode
    outline-magic
    fold-dwim
    boxquote
    pabbrev
    company
    magit
    etags-select
    yasnippet
    dired+
    dired-single
    multi-term
    tidy
    htmlize
    find-file-in-git-repo
    rainbow-mode
    ruby-mode
    guru-mode
    gitignore-mode
    php-mode
    ssh-config-mode
    syslog-mode
    apache-mode
    tango-2-theme
    solarized-theme
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
(when (not (package-installed-p p))
  (package-install p)))

(defaddon appearance
  "Appearance enhancements and theme interactions."
  (defvar fg/default-theme 'tango-2-theme)
  
  (defvar fg/theme-mode-package-alist '((tango-2-theme . tango-2-theme)
					(solarized-theme . solarized-theme)
					(zenburn-theme . zenburn-theme))
    "Association list of (theme-function . theme-package) cons cells")

  (when fg/default-theme
    (unless (fboundp fg/default-theme)
      (package-install (cdr (assoc fg/default-theme fg/theme-mode-package-alist))))
    (fg/default-theme))

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (setq inhibit-startup-message t)
  ;; Set line numbers when displayed in a gutter to be 4 character wide followed by a line and a space
  (setq linum-format "%4d \u2502 "))

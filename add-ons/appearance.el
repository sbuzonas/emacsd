(defaddon appearance
  "Appearance enhancements and theme interactions."
  (fg/require-packages '(tango-2-theme solarized-theme zenburn-theme))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (setq inhibit-startup-message t)
  ;; Set line numbers when displayed in a gutter to be 4 character wide followed by a line and a space
  (setq linum-format "%4d \u2502 "))

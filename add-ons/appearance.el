(defaddon appearance
  "Appearance enhancements and theme interactions."
  (setq custom-theme-directory theme-dir)
  
  (defvar fg/default-theme 'afternoon
    "Default theme to load at startup")

  (defvar fg/theme-mode-package-alist '((afternoon . afternoon-theme)
					(ample . ample-theme)
					(ample-zen . ample-zen-theme)
					(anti-zenburn . anti-zenburn-theme)
					(assemblage . assemblage-theme)
					(badger . badger-theme)
					(base16-chalk . base16-theme)
					(base16-default . base16-theme)
					(base16-eighties . base16-theme)
					(base16-greenscreen . base16-theme)
					(base16-mocha . base16-theme)
					(base16-monokai . base16-theme)
					(base16-ocean . base16-theme)
					(base16-railscasts . base16-theme)
					(base16-solarized . base16-theme)
					(base16-tomorrow . base16-theme)
					(basic . basic-theme)
					(bliss . bliss-theme)
					(boron . boron-theme)
					(bubbleberry . bubbleberry-theme)
					(busybee . busybee-theme)
					(calmer-forest . calmer-forest-theme)
					(cherry-blossom . cherry-blossom-theme)
					(clues . clues-theme)
					(colonoscopy . colonoscopy-theme)
					(cyberpunk . cyberpunk-theme)
					(tango-2 . tango-2-theme)
					(solarized-dark . solarized-theme)
					(solarized-light . solarized-theme)
					(zenburn . zenburn-theme))
    "Association list of (theme-name . theme-package) cons cells")

  (add-to-list 'fg/theme-mode-package-alist '(birds-of-paradise-plus . birds-of-paradise-plus-theme))

  (defvar fg/blacklisted-themes-list '()
    "Themes that should not be loaded for various reasons.")
  
  (global-set-key (kbd "C-c n") 'fg/cycle-theme)
  (global-set-key (kbd "C-c b") 'fg/blacklist-current-theme)

  (defun fg/available-themes ()
    (delq nil (mapcar #'(lambda (x)
		(if (member (car x) fg/blacklisted-themes-list)
		    nil
		  (car x)))
		      fg/theme-mode-package-alist)))
  
  (defun fg/install-all-available-themes ()
    (let ((available-themes (fg/available-themes)))
      (fg/require-packages
       (delete-dups (mapcar #'cdr
			    (mapcar #'(lambda (x)
					(assoc x fg/theme-mode-package-alist))
				    available-themes))))))

  (fg/install-all-available-themes)
  
  (defun fg/blacklist-current-theme ()
    (interactive)
    (if fg/current-theme
	(let ((old-theme fg/current-theme))
	  (fg/cycle-theme)
	  (add-to-list 'fg/blacklisted-themes-list old-theme))
      (warn "Current theme cannot be blacklisted!")))
  (defalias 'blacklist-current-theme 'fg/blacklist-current-theme)

  (setq fg/current-theme nil)

  ;; Unload all theme settings before loading a new theme
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))
  
  (defun fg/load-theme (theme)
    (message "%S" theme)
    (let ((theme-package (cdr (assoc theme fg/theme-mode-package-alist))))
      (when theme-package
	(fg/require-package theme-package)
	(with-temp-message (concat "Loading theme '" (symbol-name theme) "'...")
		      (load-theme theme))
	(message "Loading theme '%s'...done." theme)
	(setq fg/current-theme theme))))

  (defun fg/cycle-theme ()
    (interactive)
    (let ((current-theme (or fg/current-theme
			     fg/default-theme))
	  (available-themes (fg/available-themes)))
      (if current-theme
	  (let ((theme-count (length available-themes))
		(next-theme-pos (+ 1 (position current-theme available-themes))))
	    (when (>= next-theme-pos theme-count)
	      (setq next-theme-pos 0))
	    (fg/load-theme (nth next-theme-pos available-themes)))
	(warn "Current theme was not found in the current theme alist."))))
  (defalias 'cycle-theme 'fg/cycle-theme)
  
  (when fg/default-theme
    (fg/load-theme fg/default-theme))
  
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (setq inhibit-startup-message t)
  ;; Set line numbers when displayed in a gutter to be 4 character wide followed by a line and a space
  (setq linum-format "%4d \u2502 "))

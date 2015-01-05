(defaddon appearance
  "Appearance enhancements and theme interactions."

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (setq inhibit-startup-message t)
  (setq custom-theme-directory theme-dir)
  ;; Set line numbers when displayed in a gutter to be 4 character wide followed by a line and a space
  (setq linum-format "%4d \u2502 ")
  
  (setq fg/current-theme nil)
  
  ;; Unload all theme settings before loading a new theme
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  (global-set-key (kbd "C-c n") 'fg/cycle-theme)
  (global-set-key (kbd "C-c b") 'fg/blacklist-current-theme)

  (defun fg/allowed-themes ()
    (delq nil (mapcar #'(lambda (x)
		(if (member (car x) fg/blacklisted-themes-list)
		    nil
		  (car x)))
		      fg/theme-mode-package-alist)))

  (defun fg/allowed-theme-packages ()
    (delete-dups (mapcar #'cdr (mapcar #'(lambda (x) (assoc x fg/theme-mode-package-alist))
				       (fg/allowed-themes)))))

  (defun fg/load-theme (theme)
    (let ((theme-package (cdr (assoc theme fg/theme-mode-package-alist)))
	  (loading-message (format "Loading theme '%s'..." theme)))
      (when theme-package
	(when (or (member theme (fg/allowed-themes))
		  (and (not (member theme fg/blacklisted-themes-list))
		       (yes-or-no-p (format "The chosen theme '%s' is not in the allowed themes list. Continue? " theme))))
	  (fg/require-package theme-package)
	  (with-temp-message loading-message
	    (load-theme theme t))
	  (message "%sdone." loading-message)
	  (setq fg/current-theme theme)))))

  (defun fg/cycle-theme ()
    (interactive)
    (let ((current-theme (or fg/current-theme
			     fg/default-theme))
	  (available-themes (fg/allowed-themes)))
      (if current-theme
	  (let ((theme-count (length available-themes))
		(next-theme-pos (+ 1 (position current-theme available-themes))))
	    (when (>= next-theme-pos theme-count)
	      (setq next-theme-pos 0))
	    (fg/load-theme (nth next-theme-pos available-themes)))
	(warn "Current theme was not found in the current theme alist."))))
  (defalias 'cycle-theme 'fg/cycle-theme)

  (defun fg/blacklist-current-theme ()
    (interactive)
    (if fg/current-theme
	(let ((old-theme fg/current-theme))
	  (fg/cycle-theme)
	  (add-to-list 'fg/blacklisted-themes-list old-theme))
      (warn "Current theme is not in the allowed themes list and cannot be blacklisted!")))
  (defalias 'blacklist-current-theme 'fg/blacklist-current-theme)

  (defun fg/random-theme ()
    (nth (random (length (fg/allowed-themes))) (fg/allowed-themes)))

  (defun fg/load-random-theme ()
    (interactive)
    (fg/load-theme (fg/random-theme)))
  (defalias 'load-random-theme 'fg/load-random-theme)
  
  (defun fg/load-default-theme ()
    (interactive)
    (if fg/default-theme
	(fg/load-theme fg/default-theme)
      (fg/load-random-theme)))
  (defalias 'load-default-theme 'fg/load-default-theme)

  (defun fg/install-all-themes ()
    (fg/require-packages (fg/allowed-theme-packages)))

  (add-hook 'after-appearance-addon-hook 'fg/install-all-themes)
  (add-hook 'after-appearance-addon-hook 'fg/load-default-theme))

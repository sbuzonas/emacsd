(defaddon appearance
  "Appearance enhancements and theme interactions."
  (setq custom-theme-directory theme-dir)


  
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

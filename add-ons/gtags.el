(defaddon gtags
  "Provides integration with gtags."
  (fg/require-package 'gtags)

  (defun gtags-create-or-update ()
    "Create or update the GNU global tag file"
    (interactive)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
	(when (y-or-n-p "Tagfile not found. Create it? ")
	  (let ((olddir default-directory)
		(topdir (read-directory-name
			 "gtags: top of source tree:" default-directory)))
	    (cd topdir)
	    (shell-command "gtags && echo 'created tagfile'")
	    (cd olddir))) ; restore
      ;; tagfile already exists; update it
      (shell-command "global -u && echo 'updated tagfile'")))

  (defun fg/gtags-mode-init ()
    (local-set-key [(meta ?.)] 'gtags-find-tag)
    (local-set-key [(meta ?,)] 'gtags-find-rtag))
  (add-hook 'gtags-mode-hook 'fg/gtags-mode-init)

  (defun fg/gtags-select-mode-init ()
    (local-set-key (kbd "RET") 'gtags-select-tag)
    (setq-local hl-line-face 'underline)
    (hl-line-mode 1))
  (add-hook 'gtags-select-mode-hook 'fg/gtags-select-mode-init))

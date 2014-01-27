(defcustom addon-gtags
  nil
  "Enable gtags features"
  :type 'boolean
  :group 'features)

(when addon-gtags
  (progn
    (add-to-list 'my-default-packages 'gtags)
    (defun gtags-create-or-update ()
      "create or update the gnu global tag file"
      (interactive)
      (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
	  (let ((olddir default-directory)
		(topdir (read-directory-name
			 "gtags: top of source tree:" default-directory)))
	    (cd topdir)
	    (shell-command "gtags && echo 'created tagfile'")
	    (cd olddir)) ; restore
	;; tagfile already exists; update it
	(shell-command "global -u && echo 'updated tagfile'")))
    (add-hook 'gtags-mode-hook
      (lambda ()
	(local-set-key (kbd "M-.") 'gtags-find-tag)
	(local-set-key (kbd "M-,") 'gtags-find-rtag)))
    (add-hook 'php-mode-hook
      (lambda ()
	(require 'gtags)
	(gtags-mode t)
	(gtags-create-or-update)))))

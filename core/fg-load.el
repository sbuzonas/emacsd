(defun fg/add-to-load-path (this-directory &optional with-subdirs
					   recursive)
  "Add THID-DIRECTORY at the beginning of the `load-path', if it exists.
Add all its subdirectories not starting with a '.' if the optional argument
WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
	     (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
	   (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with '~')
      (while (not (string= this-directory (expand-file-name this-directory)))
	(setq this-directory (expand-file-name this-directory)))

      (when debug-on-error (message "Adding '%s' to `load-path'." this-directory))
      (add-to-list 'load-path this-directory)

      (when with-subdirs
	(while files
	  (setq dir-or-file (car files))
	  (when (file-directory-p dir-or-file)
	    (unless recursive
	      (setq with-subdirs nil))
	    (fg/add-to-load-path dir-or-file with-subdirs recursive))
	  (setq files (cdr files)))))))

(defun fg/load-directory (directory)
  "Load all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or
			(string= path ".")
			(string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
	(fg/load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
	(load (file-name-sans-extension fullpath)))))))

(provide 'fg-load)

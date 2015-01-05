(defaddon recentf
  "Modifications for the recent file settings"
  ;; Save a list of recent files visited. (open recent file with C-x f)
  (require 'recentf)
  
  (setq recentf-exclude '("~$"))
  (setq recentf-max-saved-items 99) ;; just 20 is too recent

  (defun recentf-save-list ()
    "Save the recent list.
Load the list from the file specified by `recentf-save-file', merge
the changes of your current session, and save it back to the file."
    (interactive)
    (let ((instance-list (copy-list recentf-list)))
      (recentf-load-list)
      (recentf-merge-with-default-list instance-list)
      (recentf-write-list-to-file)))

  (defun recentf-merge-with-default-list (other-list)
    "Add all items from `other-list' to `recentf-list'."
    (dolist (oitem other-list)
      ;; add-to-list already checks for equality
      (add-to-list 'recentf-list oitem)))

  (defun recentf-write-list-to-file ()
    "Write the recent files list to file.
Uset `recentf-list' as the list and `recentf-save-file' as the file
to write to."
    (condition-case error
	(with-temp-buffer
	  (erase-buffer)
	  (set-buffer-file-coding-system recentf-save-file-coding-system)
	  (insert (format recentf-save-file-header (current-time-string)))
	  (recentf-dump-variable 'recentf-list recentf-max-saved-items)
	  (recentf-dump-variable 'recentf-filter-changer-current)
	  (insert "\n \n;;; Local Variables:\n"
		  (format ";;; coding %s\n" recentf-save-file-coding-system)
		  ";;; End:\n")
	  (write-file (expand-file-name recentf-save-file))
	  (when recentf-save-file-modes
	    (set-file-modes recentf-save-file recentf-save-file-modes))
	  nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))
  
  (defsubst file-was-visible-p (file)
    "Return non-nil if FILE's buffer exists and has been displayed."
    (let ((buf (find-buffer-visiting file)))
      (if buf
	  (let ((display-count (buffer-local-value 'buffer-display-count buf)))
	    (if (> display-count 0) display-count nil)))))

  (defsubst keep-default-and-visible-recentf-p (file)
    "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
    (if (recentf-keep-default-predicate file)
	(file-was-visible-p file)))


  (let ((r-list recentf-list))
    (defsubst keep-default-old-and-visible-recentf-p (file)
      "Decide whether to keep file in recentf-list.
Return non-nil if recentf would, by default, keep FILE, and
either FILE name was loaded from recentf file on disk or FILE
has been displayed in this session."
      (if (recentf-keep-default-predicate file)
	  (or (member file r-list)
	      (file-was-visible-p file)))))
  
  (setq recentf-keep '(keep-default-old-and-visible-recentf-p))

  (defun undo-kill-buffer (arg)
    "Re-open the last buffer killed. With ARG, re-open the nth buffer."
    (interactive "p")
    (let ((recently-killed-list (copy-sequence recentf-list))
	  (buffer-files-list
	   (delq nil (mapcar (lambda (buf)
			       (when (buffer-file-name buf)
				 (expand-file-name (buffer-file-name buf))))
			     (buffer-list)))))
      (mapc
       (lambda (buf-file)
	 (setq recently-killed-list
	       (delq buf-file recently-killed-list)))
       buffer-files-list)
      (find-file
       (if arg (nth arg recently-killed-list)
	 (car recently-killed-list)))))
  (recentf-mode 1))

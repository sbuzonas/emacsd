(defaddon history
  "Extension for managing history"

  (nomad/require-package 'undo-tree)
  (global-undo-tree-mode)

  (require 'recentf)

  (global-set-key [(control ?x)(meta ?f)] 'nomad/ido-choose-from-recentf)

  (defun nomad/ido-choose-from-recentf ()
    "Use ido to select a recently opened file from the `recentf-list'"
    (interactive)
    (let ((home (expand-file-name (getenv "HOME"))))
      (find-file
       (ido-completing-read "Recentf open: "
                            (mapcar (lambda (path)
                                      (replace-regexp-in-string home "~" path))
                                    recentf-list)
                            nil t))))

  (defun recentf-save-list ()
    "Save the recent list.
Load the list from the file specified by `recentf-save-file', merge
the changes of your current session, and save it back to the file."
    (interactive)
    (let ((instance-list (mapcar (lambda (path)
                                   (replace-regexp-in-string (getenv "HOME") "~" path))
                                 recentf-list)))
      (recentf-load-list)
      (recentf-merge-with-default-list instance-list)
      (recentf-write-list-to-file)))

  (defun recentf-merge-with-default-list (other-list)
    "Add all items from `other-list' to `recentf-list'."
    (dolist (oitem other-list)
      (add-to-list 'recentf-list oitem)))

  (defun recentf-write-list-to-file ()
    "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the file to write to."
    (condition-case error
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-file-coding-system recentf-save-file-coding-system)
          (insert (format recentf-save-file-header (current-time-string)))
          (recentf-dump-variable 'recentf-list recentf-max-saved-items)
          (recentf-dump-variable 'recentf-filter-changer-current)
          (insert "\n\n;;;Local Variables:\n"
                  (format ";;; coding %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
          (write-file (expand-file-name recentf-save-file))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))
  
  (add-to-list 'recentf-exclude "\\.ido\\.last\\'")
  (add-to-list 'recentf-exclude "\\.recentf\\'")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "~\\'")
  (add-to-list 'recentf-exclude "\\^/tmp")
  (add-to-list 'recentf-exclude "\\^/var/tmp")

  (setq recentf-max-saved-items 99)

  (recentf-mode 1))

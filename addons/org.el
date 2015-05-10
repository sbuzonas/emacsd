(defaddon org
  "Configure org-mode and related task systems."

  (defun jump-to-org-agenda ()
    (interactive)
    (let ((buf (get-buffer "*Org Agenda*"))
	  wind)
      (if buf
	  (if (setq wind (get-buffer-window buf))
	      (select-window wind)
	    (if (called-interactively-p)
		(progn (select-window (display-buffer buf t t))
		       (org-fit-window-to-buffer)
		       (org-agenda-redo))
	      (with-selected-window (display-buffer buf)
		(org-fit-window-to-buffer)
		(org-agenda-redo))))
	(call-interactively 'org-agenda-list)))
    (let ((buf (get-buffer "*Calendar*")))
      (unless (get-buffer-window buf)
	(org-agenda-goto-calendar))))
  (run-with-idle-timer 300 t 'jump-to-org-agenda)
  
  (org-agenda-list 1)
  (org-fit-window-to-buffer))

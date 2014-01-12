(defcustom addon-org
  nil
  "Enable org features"
  :type 'boolean
  :group 'features)

(when addon-org
    (try-require 'org))

(after 'org
  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (setq org-directory "~/Documents/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; TODO: figure out why modifiers are not recognized with function keys on Mac
  (define-key global-map (kbd "M-<f6>") 'org-capture))

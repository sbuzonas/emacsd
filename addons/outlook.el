(defaddon outlook
  "Microsoft Outlook Integration"
  
  (nomad/require-package 'org-outlook)
  (setq org-outlook-location "/Applications/Microsoft Office 2011/Microsoft Outlook.app/Contents/MacOS/Microsoft Outlook")

  (defun org-outlook-open (id)
    (if (and org-outlook-location (file-exists-p org-outlook-location))
        (async-shell-command (concat org-outlook-location " /select \"outlook:" id "\""))
      (message "`org-outlook-location' does not exist"))))

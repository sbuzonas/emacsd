(defcustom addon-gnus
  nil
  "Enable gnus features"
  :type 'boolean
  :group 'features)

(when addon-gnus
  (progn
    (when (file-exists-p "~/.gnus-config.el")
      (load-file "~/.gnus-config.el"))))

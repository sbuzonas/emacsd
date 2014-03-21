;; Define the features group for Custom to allow addons to show their states
(defgroup features nil
  "Group of features allowing to toggle their configuration"
  :group 'emacs)

(load-directory (expand-file-name "add-ons" user-emacs-directory))

(provide 'add-ons)

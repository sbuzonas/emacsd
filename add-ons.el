;; Define the extensions group for Custom to allow extensions to be toggled on and off
(defgroup addons nil
  "Addons the current configuration is aware of and whether or not they are enabled"
  :group 'emacs)

(defmacro defaddon (symbol require &rest forms)
  (progn
    (add-hook 'after-init-hook `(lambda () (when ,symbol
                                        ,@forms)))
    (let ((args '(:type 'boolean :group 'addons))
          req)
      `,(if require
            (progn
              (setq req (list :require require))))
      `(defcustom ,symbol
         nil
         ,(format "Functionality for %s" symbol)
         ,@(append args req)))))

(load-directory (expand-file-name "add-ons" user-emacs-directory))

(provide 'add-ons)

(defcustom addon-guru-mode
  nil
  "Enable guru-mode features"
  :type 'boolean
  :group 'features)

(when addon-guru-mode
  (progn
    (add-to-list 'my-default-packages 'guru-mode)
    (when (try-require 'guru-mode)
      (add-hook 'prog-mode-hook
                '(lambda ()
                   (guru-mode t))))))

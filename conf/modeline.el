(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info)
          header-line-format '((which-func-mode ("" which-func-format " "))))))

;;** 51 (info "(emacs)Saving Emacs Sessions")

(when section-saving-emacs-sessions (message "51 Saving Emacs Sessions...")

(try-idle-require 'saveplace)
(eval-after-load 'saveplace
  '(progn

    ;; automatically save place in each file
    (setq-default save-place t)  ;; default value for all buffers

    ;; name of the file that records `save-place-alist' value
    (setq save-place-file (convert-standard-filename "~/.emacs.d/places.txt"))

    ;; do not make backups of master save-place file
    (setq save-place-version-control "never")))

(message "51 Saving Emacs Sessions... Done"))

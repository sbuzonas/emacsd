(defaddon convenience
  nil
  (require-package 'speedbar)
  (require-package 'sr-speedbar)

  (defun slbmeh-speedbar-set-state ()
    "open or close the speedbar depending on if there is a file for the buffer"
    (interactive)
    (if (and (not (fboundp 'sr-speedbar-exist-p))
             buffer-file-name)
        (sr-speedbar-open)
      (if buffer-file-name
          (unless (or (not (fboundp 'sr-speedbar-exist-p))
                      (sr-speedbar-exist-p))
            (sr-speedbar-open))
        (when (and (fboundp 'sr-speedbar-exist-p)
                   (sr-speedbar-exist-p))
          (sr-speedbar-close)))))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (defadvice switch-to-buffer (after switch-buffer-now activate)
                (slbmeh-speedbar-set-state))

              (defadvice other-window (after other-window-now activate)
                (slbmeh-speedbar-set-state))

              (defadvice other-frame (after other-frame-now activate)
                (slbmeh-speedbar-set-state)))))

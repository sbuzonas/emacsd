(defun my-latex (action)
  (interactive)
  (if (buffer-modified-p) (save-buffer))
  (let ((f1 (current-frame-configuration))
        (retcode (shell-command (concat "~/bin/my-latex " action " " buffer-file-name))))
    (if (= retcode 0) (set-frame-configuration f1))))

(add-hook 'latex-mode-hook
          (lambda ()
            (define-key LaTex-mode-map (kbd "<f8>") '(lambda ()
                                                       (interactive)
                                                       (my-latex "preview")))
            (define-key Latex-mode-map (kbd "<S-f8>") '(lambda ()
                                                         (interactive)
                                                         (my-latex "create")))))

(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list
                  '("%(RubberPDF)"
                    (lambda ()
                      (if
                          (not TeX-PDF-mode)
                          ""
                        "--pdf"))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)))

(add-hook 'TeX-mode-hook
          '(lambda ()
             (define-key TeX-mode-map (kbd "<f6>")
               (lambda ()
                 (interactive)
                 (save-buffer)
                 (TeX-command-menu "Rubber")
                 (TeX-clean)))
             (define-key TeX-mode-map (kdb "<f7>")
               (lambda ()
                 (interactive)
                 (Tex-view)
                 [return]))))

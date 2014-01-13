(defcustom addon-yasnippet
  nil
  "Enable yasnippet features"
  :type 'boolean
  :group 'features)

(when addon-yasnippet
  (progn
    (add-to-list 'my-default-packages 'yasnippet)
    (add-to-list 'my-default-packages 'yasnippet-bundle)
    (add-to-list 'my-default-packages 'angular-snippets)
    (add-to-list 'my-default-packages 'php-auto-yasnippets)
    (try-require 'yasnippet-bundle)))

(after 'yasnippet-bundle
  (setq yas/root-directory (expand-file-name "snippets" user-emacs-directory))
  (yas/load-directory yas/root-directory)

  (try-require 'angular-snippets)
  (try-require 'php-auto-yasnippets)
  (try-require 'yasnippet-bundle)

  (yas/global-mode)

  ;; Jump to end of snippet definition
  (define-key yas/keymap (kbd "<return>") 'yas/exit-all-snippets)

  ;; Be less verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  (defun yas-ido-expand ()
    "Lets you select (and expand) a yasnippet key"
    (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min)))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
      (let* ((init-word (point))
             (word (buffer-substring init-word original-point))
             (list (yas/active-keys)))
        (goto-char original-point)
        (let ((key (remove-if-not
                    (lambda (s) (string-match (concat "^" word) s)) list)))
          (if (= (length key) 1)
              (setq key (pop key))
            (setq key (ido-completing-read "key: " list nil nil word)))
          (delete-char (- init-word original-point))
          (insert key)
          (yas/expand)))))
  (define-key yas/minor-mode-map (kbd "<C-tab>") 'yas-ido-expand))

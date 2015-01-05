(defaddon yasnippet
  nil
  (require-package 'yasnippet)

  (after yasnippet
    (setq yas/trigger-key (kbd "C-c /"))
    (yas/initialize)
    (setq yas/root-directory '("~/.emacs.d/snippets"))
    (mapc 'yas/load-directory yas/root-directory)))

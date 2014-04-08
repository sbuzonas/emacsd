(defaddon php
  nil
  (require-package 'php-mode)
  (require-package 'gtags)

  (require 'fancyguy-tempo)
  (setq tempo-interactive t)

  (defvar php-tempo-tags nil
    "Tempo tags for PHP mode")

  (defvar php-tempo-keys-alist nil
    "")

  (tempo-define-template "php-if"
                         '(> "if (" ~ ") {" n>
                           > n
                           "}" > n>
                           )
                         "if"
                         "Insert an if statement"
                         'php-tempo-tags)

  (tempo-define-template "php-else"
                         '(> "} else {" n>
                           > ~ n
			 )
                         "else"
                         "Insert an else statement"
                         'php-tempo-tags)

  (tempo-define-template "php-else-if"
                         '(> "} else if (" ~ ") {" n>
                           > n
			 )
                         "elif"
                         "Insert an else if statement"
                         'php-tempo-tags)

  (tempo-define-template "php-while"
                         '(> "while (" ~ ") {" n>
                           > n
                           "}" > n>
                         )
                         "while"
                         "Insert a while statement"
                         'php-tempo-tags)

  (tempo-define-template "php-for"
                         '(> "for (" ~ ") {" n>
                           > n
                           "}" > n>
                         )
                         "for"
                         "Insert a for loop"
                         'php-tempo-tags)

  (tempo-define-template "php-fori"
                         '(> "for ($" (p "variable: " var) " = 0; $" (s var)
                           " < " (p "upper bound: " ub)"; $" (s var) "++) {" > n>
                           > r n
                           "}" > n>
                         )
                         "fori"
                         "Insert a for loop: for($i = 0: $i < ..; $i++)"
                         'php-tempo-tags)

  (tempo-define-template "php-foreach"
                         '(> "foreach ($" (p "variable: " var) " as $" (p "as: " as) ") {" n>
                           > ~ n
                           "}" > n>
                         )
                         "fe"
                         "Insert a foreach loop."
                         'php-tempo-tags)

  (tempo-define-template "php-foreachkv"
                         '(> "foreach ($" (p "variable: " var) " as $k => $v) {" n>
                           > ~ n
                           "}" > n>
                         )
                         "fekv"
                         "Insert a foreach loop: foreach ($.. as $k => $v)"
                         'php-tempo-tags)

  (defun gtags-create-or-update ()
    "create or update the gnu global tag file"
    (interactive)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
        (let ((olddir default-directory)
              (topdir (read-directory-name
                       "gtags: top of source tree:" default-directory)))
          (cd topdir)
          (shell-command "gtags && echo 'created tagfile'")
          (cd olddir)) ; restore
      ;; tagfile already exists; update it
      (shell-command "global -u && echo 'updated tagfile'")))

  (add-hook 'php-mode-hook
            (lambda ()
              (require 'gtags)
              (local-set-key (kbd "C-c t") 'tempo-complete-tag)
              (tempo-use-tag-list 'php-tempo-tags)
              (gtags-mode t)
              (gtags-create-or-update)))

  (add-hook 'gtags-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'gtags-find-tag)
              (local-set-key (kbd "M-,") 'gtags-find-rtag)))

  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") 'gtags-select-tag)
              (setq-local hl-line-face 'underline)
              (hl-line-mode 1))))


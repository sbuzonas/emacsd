(defaddon php
  "Provides extensions developing with PHP."
  (fg/require-packages '(php-mode php-extras))

  (defvar php-tempo-tags nil
    "Tempo tags for PHP mode")
  
  (eval-after-load 'autoinsert
    '(define-auto-insert
       '("\\.php\\'" . "PHP Application")
       '(nil
	 "<?php\n"
	 ;; Insert the license when we don't have an `insert-license' preference or it is non nil
	 `(when (or (not (boundp 'insert-license))
		    insert-license)
	    "/**\n"
	    ;; Insert the `project-name' if we have it
	    `(when (and project-name
			(not (string= "" project-name)))
	       (concat " * This file is part of " project-name ".\n *\n"))
	    ;; If we have `auto-insert-copyright' set put it in the header
	    `(when (and auto-insert-copyright
			(not (string= "" auto-insert-copyright)))
	       (concat " * Copyright (c) " (substring (current-time-string) -4) " " auto-insert-copyright "\n *\n"))
	    " * For the full copyright and license information, please view the LICENSE\n"
	    " * file that was distributed with this source code.\n"
	    " */\n"
	    "\n")
	 `(when (not (string= "" (fg/assume-namespace-from-path)))
	    (concat "namespace " (fg/assume-namespace-from-path) ";\n\n"))
	 "class " (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) "\n"
	 "{\n"
	 > _ "\n"
	 "}\n"
	 )))

  (defun fg/assume-namespace-from-path ()
    (let ((working-dir (file-name-directory (buffer-file-name)))
	  (path nil)
	  (namespace nil)
	  (sources (if (boundp 'source-path) source-path "src/"))
	  (tests (if (boundp 'tests-path) tests-path "tests/")))
      (when (string= sources ".")
	(setq sources (fg/dir-locals-dir)))
      (when (string= tests ".")
	(setq tests (fg/dir-locals-dir)))
      (if (string-match sources	working-dir)
	  (setq path sources
		namespace (if (boundp 'source-namespace) source-namespace ""))
	(setq path tests
	      namespace (if (boundp 'tests-namespace) tests-namespace "")))
      (when (string-match path working-dir)
	(setq namespace (replace-regexp-in-string "/" "\\" (replace-regexp-in-string "\\(^/+\\|/\\'\\)" "" (concat namespace "/" (substring working-dir (match-end 0)))) t t))
	namespace)))

  (defun fg/dir-locals-dir ()
    "Return the directory local variables directory.
Code taken from `hack-dir-local-variables'."
    (let ((variables-file (dir-locals-find-file (or (buffer-file-name) default-directory)))
	  (dir-name nil))
      (cond
       ((stringp variables-file)
	(setq dir-name (file-name-directory variables-file)))
       ((consp variables-file)
	(setq dir-name (nth 0 variables-file))))
      dir-name))
  
  (defun fg/php-mode-init ()
    (when (and buffer-file-name
	       (fboundp 'gtags-create-or-update))
      (gtags-mode t)
      (gtags-create-or-update))
    (eldoc-mode))
  (add-hook 'php-mode-hook 'fg/php-mode-init))

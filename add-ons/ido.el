(defcustom addon-ido
  nil
  "Enable ido features"
  :type 'boolean
  :group 'features)

(when addon-ido
  (progn
    (add-to-list 'my-default-packages 'flx-ido)
    (add-to-list 'my-default-packages 'ido-vertical-mode)
    (add-to-list 'my-default-packages 'ido-at-point)
    (add-to-list 'my-default-packages 'ido-ubiquitous)
    (try-require 'ido)))

(after 'ido
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)

  ;; Filters ido-matches setting acronym matches in front of the results
  (defadvice ido-set-matches-1 (after ido-acronym-matches activate)
    (if (> (length ido-text) 1)
	(let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
	      (acronym-matches (list))
	      (remove-regexes '("-menu-")))
	  ;; Creating the list of the results to be set as first
	  (dolist (item items)
	    (if (string-match (concat regex "[^-]*$") item) ;; strict match
		(add-to-list 'acronym-matches item)
	      (if (string-match regex item) ;; appending relaxed match
		  (add-to-list 'acronym-matches item t))))

	  ;; Filtering ad-return-value
	  (dolist (to_remove remove-regexes)
	    (setq ad-return-value
		  (delete-if (lambda (item)
			       (string-match to_remove item))
			     ad-return-value)))

	  ;; Creating resulting list
	  (setq ad-return-value
		(append acronym-matched
			ad-return-value))

	  (delete-dups ad-return-value)
	  (reverse ad-return-value))))

  (try-require 'flx-ido)
  (try-require 'ido-vertical-mode)
  (defun slbmeh/setup-ido ()
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
        (interactive)
        (cond
          ((looking-back "~/") (insert "projects/"))
          ((looking-back "/") (insert "~/"))
          (:else  (call-interactively 'self-insert-command)))))

    ;; Use C-w to go back up a dir to better match normal usage of C-w
    ;; - insert current file name with C-x C-w instead.
    (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
    (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

  (add-hook 'ido-setup-hook 'slbmeh/setup-ido)

  ;; Always rescan buffer for imenu
  (set-default 'imenu-auto-rescan t)
  (try-require 'ido-at-point)
  (try-require 'ido-ubiquitous))

(after 'flx-ido
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(after 'ido-vertical-mode
  (ido-vertical-mode))

(after 'ido-at-point
  (ido-at-point-mode))

(after 'ido-ubiquitous
  (ido-ubiquitous-mode 1)
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))
  (ido-ubiquitous-use-new-completing-read webjump 'webjump)
  (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
  (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet))

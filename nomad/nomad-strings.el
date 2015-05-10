;;;##autoload
(defun nomad/string-starts-with-p (str prefix)
  "Return t if STR starts with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
		     str)
       t))

;;;###autoload
(defun nomad/string-ends-with-p (str suffix)
  "Return t if STR ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
		     str)
       t))

;;;###autoload
(defun nomad/string-reverse (str)
  "Reverse STR."
  (apply #'string
	 (reverse
	  (string-to-list str))))

;;;###autoload
(defun nomad/string-trim (str)
  "Remove leading and trailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

;;(defun toggle-inflection ()
;;  (interactive)
;;  (let (start end word)
;;    (save-excursion
;;      (skip-chars-backward
;;  (let (str (nomad/inflection-get-word-at-point t))
;;    (message "%S" str)
;;    (cond
;;     ((nomad/inflect-underscore-p str)
;;      (nomad/inflect-upper-case str))
;;     (t
;;      (nomad/inflect-underscore str)))))

(defconst nomad/inflection-word-chars "a-zA-Z0-9_-")

(defun nomad/inflection-get-word-at-point (&optional skip)
  "Gets the symbol near the cursor. If SKIP is non-nil, skip non-word characters forward."
  (and skip
       (skip-chars-forward (concat "^" nomad/inflection-word-chars)))
  (let ((start (progn
		 (skip-chars-forward nomad/inflection-word-chars)
		 (point)))
	(end (progn
	       (skip-chars-backward nomad/inflection-word-chars)
	       (point))))
    (buffer-substring-no-properties start end)))

(defun nomad/inflection-delete-word-at-point (&optional skip)
  "Gets the symbol near the cursor and deletes it. If SKIP is non-nil, skip non-word characters forward."
  (and skip
       (skip-chars-forward (concat "^" nomad/inflection-word-chars)))
  (let ((start (progn
		 (skip-chars-forward nomad/inflection-word-chars)
		 (point)))
	(end (progn
	       (skip-chars-backward nomad/inflection-word-chars)
	       (point))))
    (prog1
	(buffer-substring start end)
      (delete-region start end))))

(defun nomad/inflection-beginning-of-word (&optional skip)
  "Moves the point to the beginning of the inflected word. If SKIP is non-nil, skip non-word characters backward."
  (and skip
       (skip-chars-backward (concat "^" nomad/inflection-word-chars)))
  (progn (skip-chars-backward nomad/inflection-word-chars)
	 (point)))

;;;###autoload
(defun nomad/inflect-title-case (str)
  "TitleCase"
  (setq str (split-string (nomad/inflect-underscore str) "_"))
  (mapconcat 'capitalize str ""))

;;;###autoload
(defun inflect-word-at-point-title-case ()
  (interactive)
  (insert (nomad/inflect-title-case (nomad/inflection-delete-current-word t)))
  (nomad/inflection-beginning-of-word t))

;;;###autoload
(defun nomad/inflect-title-case-p (str)
  "is TitleCase?"
  (let ((case-fold-search nil))
    (when (string-match str (nomad/inflect-title-case str))
      t)))

;;;###autoload
(defun nomad/inflect-camel-case (str)
  "camelCase"
  (setq str (split-string (nomad/inflect-underscore str) "_"))
  (concat (downcase (car str))
	  (mapconcat 'capitalize (cdr str) "")))

;;;###autoload
(defun inflect-word-at-point-camel-case ()
  (interactive)
  (save-excursion
    (insert (nomad/inflect-camel-case (nomad/inflection-delete-current-word t)))))
;    (nomad/inflection-beginning-of-word t)))

;;;###autoload
(defun nomad/inflect-camel-case-p (str)
  "is camelCase?"
  (let ((case-fold-search nil))
    (when (string-match str (nomad/inflect-camel-case str))
      t)))

;;;###autoload
(defun nomad/inflect-slug-case (str)
  "slug-case"
  (replace-regexp-in-string "_" "-" (nomad/inflect-underscore str)))

;;;###autoload
(defun inflect-word-at-point-slug-case ()
  (interactive)
  (insert (nomad/inflect-slug-case (nomad/inflection-delete-current-word t)))
  (nomad/inflection-beginning-of-word t))

;;;###autoload
(defun nomad/inflect-slug-case-p (str)
  "is slug-case?"
  (let ((case-fold-search nil))
    (when (string-match str (nomad/inflect-slug-case str))
      t)))

;;;###autoload
(defun nomad/inflect-underscore (str)
  "under_score"
  (let ((case-fold-search nil))
    (while (string-match "\\([A-Z]\\)\\([A-Z]\\)" str)
      (setq str (replace-regexp-in-string "\\([a-zA-Z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str)))
    (setq str (replace-regexp-in-string "-" "_" str))
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

;;;###autoload
(defun inflect-word-at-point-underscore ()
  (interactive)
  (insert (nomad/inflect-underscore (nomad/inflection-delete-current-word t)))
  (nomad/inflection-beginning-of-word t))

;;;###autoload
(defun nomad/inflect-underscore-p (str)
  "is under_score?"
  (let ((case-fold-search nil))
    (when (string-match str (nomad/inflect-underscore str))
      t)))

;;;###autoload
(defun nomad/inflect-upper-case (str)
  "UPPER_CASE"
  (upcase (nomad/inflect-underscore str)))

;;;###autoload
(defun inflect-word-at-point-upper-case ()
  (interactive)
  (insert (nomad/inflect-upper-case (nomad/inflection-delete-current-word t))))

;;;###autoload
(defun nomad/inflect-upper-case-p (str)
  "is UPPER_CASE?"
  (let ((case-fold-search nil))
    (when (string-match str (nomad/inflect-upper-case str))
      t)))

(provide 'nomad-strings)

(require 'tempo)

(defvar tempo-initial-pos nil
  "Initial position in template after expansion")

(defadvice tempo-insert (around tempo-insert-pos act)
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it))

(defadvice tempo-insert-template (around tempo-insert-template-pos act)
  "Set initial position when defined."
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
  "Skip self-insert if template function is called by an abbrev."
  (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

(defun expand-tempo-tag (expand-function)
  "Expand the tempo-tag before point by calling the template."
  (let (match templ)
    (undo-boundary)
    (if (dolist (tags tempo-local-tags)
          (when (setq match (tempo-find-match-string (or (cdr tags)
                                                         tempo-match-finder)))
            (when (setq templ (assoc (car match) (symbol-value (car tags))))
              (delete-region (cdr match) (point))
              (funcall (cdr templ))
              (return t))))
        'expand-tempo-tag-alias
      (funcall expand-function))))

(fset 'expand-tempo-tag-alias 'expand-tempo-tag)
(put 'expand-tempo-tag 'no-self-insert t)

(add-hook 'abbrev-expand-functions 'expand-tempo-tag)

(provide 'fancyguy-tempo)

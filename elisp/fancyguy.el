(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun region-active-p () (and transient-mark-mode mark-active))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwin command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaced default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg))
    (next-line)
    (back-to-indentation-or-beginning))

(provide 'fancyguy)

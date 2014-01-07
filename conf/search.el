;;** 19 (info "(emacs)Search")ing and Replacement

(when section-search (message "19 Searching and Replacement...")

;;*** 19.1 (info "(emacs)Incremental Search")

;; Have a look at the "Standard Isearch Keys" on
;; http://www.emacswiki.org/emacs/IncrementalSearch

;; always exit searches at the beginning of the expression found
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))


;;*** 19.4 (info "(emacs)Regexp Search")

;; You can build regexps with visual feedback by using:
;; - `M-x re-builder' or
;; - `M-x regex-tool' (by John Wiegley, get it from
;;   http://www.newartisans.com/downloads_files/regex-tool.el)

;; Optimize regexps with `regexp-opt.el'

;; list the input line, followed by the first nine substrings matches,
;; to debug regexp searches (in IELM)
;; example: ELISP> (save-excursion (set-buffer "BUFFER")
;;                                 (re-search-forward "REGEXP" nil t)
;;                                 (my-buffer-matched-strings))
(defun my-buffer-matched-strings ()
  (interactive)
  (mapcar 'my-buffer-matched-string-nth '(0 1 2 3 4 5 6 7 8 9)))

(defun my-buffer-matched-string-nth (n)
  "Return the Nth pattern-matched string from the current buffer."
  (if (and (match-beginning n) (match-end n))
      (if (> (match-end n) (match-beginning n))
          (buffer-substring (match-beginning n) (match-end n))
        "")
    nil))


;;*** 19.8 (info "(emacs)Search Case")

;; searches and matches should ignore case
(setq-default case-fold-search t)
(setq default-case-fold-search t)       ; FIXME obsolete since Emacs 23.2, but
                                        ; still needed!?


;;*** 19.9 (info "(emacs)Replace")ment Commands

;; You can force a matched regex text pattern to upper case by entering
;; `C-M-% your_regexp RET \,(upcase \num_of_match)'


;;*** 19.10 (info "(emacs)Other Repeating Search") Commands

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; `M-x flush-lines' deletes each line that contains a match for REGEXP

(message "19 Searching and Replacement... Done"))

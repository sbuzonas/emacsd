(defcustom addon-org
  nil
  "Enable org features"
  :type 'boolean
  :group 'features)

(when addon-org
  (progn
    (add-to-list 'my-default-packages 'org2blog)
    (try-require 'org)))

(after 'org

  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (setq org-directory "~/Documents/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
           "* TODO %?\n %i\n")
          ("l" "Link" plain (file (concat org-directory "/links.org"))
           "- %?\n %x\n")))

  (setq org2blog/wp-blog-alist
        '(("stevebuzonas"
           :url "http://www.stevebuzonas.com/xmlrpc.php"
           :username "slbmeh")))

  ;; Transpose paragraphs instead of lines
  (defun org-transpose-paragraphs (arg)
    (interactive)
    (when (and (not (or (org-at-table-p) (org-on-heading-p) (org-at-item-p)))
               (thing-at-point 'sentence))
      (transpose-paragraphs arg)
      (backward-paragraph)
      (re-search-forward "[[:graph:]]")
      (goto-char (match-beginning 0))
      t))

  (add-to-list 'org-metaup-hook
               (lambda () (interactive) (org-transpose-paragraphs -1)))
  (add-to-list 'org-metadown-hook
               (lambda () (interactive) (org-transpose-paragraphs 1)))

  (defun org-time-string-to-seconds (s)
    "Convert a string HH:MM:SS to a number of seconds."
    (cond
     ((and (stringp s)
           (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
      (let ((hour (string-to-number (match-string 1 s)))
            (min (string-to-number (match-string 2 s)))
            (sec (string-to-number (match-string 3 s))))
        (+ (* hour 3600) (* min 60) sec)))
     ((and (stringp s)
           (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
      (let ((min (string-to-number (match-string 1 s)))
            (sec (string-to-number (match-string 2 s))))
        (+ (* min 60) sec)))
     ((stringp s) (string-to-number s))
     (t s)))

  (defun org-time-seconds-to-string (secs)
    "Convert a number of seconds to a time string."
    (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
          ((>= secs 60) (format-seconds "%m:%.2s" secs))
          (t (format-seconds "%s" secs))))

  (defmacro with-time (time-output-p &rest exprs)
    "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If `time-output-p' then return
the result as a time value."
    (list
     (if time-output-p 'org-time-seconds-to-string 'identity)
     (cons 'progn
           (mapcar
            (lambda (expr)
              `,(cons (car expr)
                      (mapcar
                       (lambda (el)
                         (if (listp el)
                             (list 'with-time nil el)
                           (org-time-string-to-seconds el)))
                       (cdr expr))))
            `,@exprs))))

  (defun org-hex-strip-lead (str)
    (if (and (> (length str) 2) (string= (substring str 0 2) "0x"))
        (substring str 2) str))

  (defun org-hex-to-hex (int)
    (format "0x%x" int))

  (defun org-hex-to-dec (str)
    (cond
     ((and (stringp str)
           (string-match "\\([0-9a-f]+\\)" (setf str (org-hex-strip-lead str))))
      (let ((out 0))
        (mapc
         (lambda (ch)
           (setf out (+ (* out 16)
                        (if (and (>= ch 48) (<= ch 57)) (- ch 48) (-ch 87)))))
         (coerce (match-string 1 str) 'list))
        out))
     ((stringp str) (string-to-number str))
     (t str)))

  (defmacro with-hex (hex-output-p &rest exprs)
    "Evaluate an org-table formula, converting all fields that look
like hexadecimal to decimal integers.  If `hex-output-p' then
return the result as a hex value."
    (list
     (if hex-output-p 'org-hex-to-hex 'identity)
     (cons 'progn
           (mapcar
            (lambda (expr)
              `,(cons (car expr)
                      (mapcar (lambda (el)
                                (if (listp el)
                                    (list 'with-hex nil el)
                                  (org-hex-to-dec el)))
                              (cdr expr))))
            `,@exprs))))

  (defadvice org-archive-subtree (around my-org-archive-subtree activate)
    (let ((org-archive-location
           (if (save-excursion (org-back-to-heading)
                               (> (org-outline-level) 1))
               (concat (car (split-string org-archive-location "::"))
                       "::* "
                       (car (org-get-outline-path)))
             org-archive-location)))
      ad-do-it))

  ;; TODO: figure out why modifiers are not recognized with function keys on Mac
  (define-key global-map (kbd "C-c r") 'org-capture))

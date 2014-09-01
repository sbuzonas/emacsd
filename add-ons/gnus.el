(defaddon gnus
  nil
  (setq-default gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n"
                gnus-summary-line-format "%O%U%R%z %(%&user-date; %) %B%(%[%4L: %-22,22f%]%) %s\n"
                gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z"
                gnus-summary-same-subject ""
                gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
                gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
                gnus-thread-sort-functions '(gnus-thread-sort-by-date)
                gnus-sum-thread-tree-false-root ""
                gnus-sum-thread-tree-indent " "
                gnus-sum-thread-tree-leaf-with-other "|⇒ "
                gnus-sum-thread-tree-root ""
                gnus-sum-thread-tree-single-leaf "⇒ "
                gnus-sum-thread-tree-vertical "| ")

  ;; Store gnus specific files to ~/Dropbox/gnus
  (setq gnus-directory "~/Dropbox/gnus")
  (setq message-directory (concat gnus-directory "/mail")
        nnml-directory (concat gnus-directory "/nnml-mail")
        gnus-article-save-directory (concat gnus-directory "/saved")
        gnus-kill-files-directory (concat gnus-directory "/scores")
        gnus-cache-directory (concat gnus-directory "/cache"))

  (setq slbmeh-gnus-header-list '("From"
                                  "Newsgroups"
                                  "Subject"
                                  "Date"
                                  "Followup-To"
                                  "Reply-To"
                                  "Summary"
                                  "Keywords"
                                  "To"
                                  "Cc"
                                  "BCc"
                                  "GCc"
                                  "GCc"
                                  "Posted-To"
                                  "Mail-Copies-To"
                                  "Mail-Followup-To"
                                  "Apparently-To"
                                  "Gnus-Warning"
                                  "Resent-From"
                                  "X-Sent"
                                  "User-Agent"
                                  "X-Mailer"
                                  "X-Newsreader"))

  ;; Visible headers and header sort order
  (setq gnus-visible-headers (concat "^" (regexp-opt slbmeh-gnus-header-list) ":")
        gnus-sorted-header-list (mapcar (function (lambda (x) (concat "^" x ":"))) slbmeh-gnus-header-list))

  ;; Use different outbox for mail and news
  (setq gnus-message-archive-method "nnmaildir+local:"
        gnus-update-message-archive-method t
        gnus-message-archive-group '((if (message-news-p)
                                         "nnmaildir+local:sent-news"
                                       "nnmaildir+local:sent-mail")))

  ;; Adaptive scoring settings
  (add-hook 'message-sent-hook 'gnus-score-followup-article)
  (add-hook 'message-sent-hook 'gnus-score-followup-thread)
  (setq gnus-use-adaptive-scoring t
        gnus-score-expiry-days 14
        gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                            (gnus-ticked-mark (from 4))
                                            (gnus-dormant-mark (from 5))
                                            (gnus-saved-mark (from 20) (subject 5))
                                            (gnus-del-mark (from -2) (subject -5))
                                            (gnus-read-mark (from 2) (subject 1))
                                            (gnus-killed-mark (from 0) (subject -3)))
        gnus-score-decay-constant 1
        gnus-score-decay-scale 0.03
        gnus-decay-scored t
        gnus-global-score-files '("~/Dropbox/gnus/scores/all.SCORE")
        gnus-summary-expunge-below -999)

  ;; Message settings
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  (setq message-generate-headers-first t
        message-kill-buffer-on-exit t
        message-citation-line-function 'slbmeh-message-citation
        message-cite-function 'message-cite-original-without-signature)

  ;; Thread settings
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

  ;; Formatting
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))

  (defun gnus-user-format-function-thread (dummy)
    (propertize gnus-tmp-thread-tree-header-string 'gnus-face t))

  (defun gnus-user-format-function-G (arg)
    (concat (let ((method (car (cdr gnus-tmp-method)))
                  (group-prefix (gnus-group-find-parameter gnus-tmp-group 'group-prefix)))
              (when method
                (concat method ":"))
              (when group-prefix
                group-prefix))
            (or (gnus-group-find-parameter gnus-tmp-group 'display-name)
                (let ((prefix (assq 'remove-prefix (cddr gnus-tmp-method))))
                  (if (and prefix
                           (string-match (concat "^\\("
                                                 (regexp-quote (cadr prefix))
                                                 "\\)")
                                         gnus-tmp-qualified-group))
                      (substring gnus-tmp-qualified-group (match-end 1))
                    gnus-tmp-qualified-group)))))

  ;; Gnus Control
  (defun slbmeh-gnus ()
    (interactive)
    (let ((bufname (buffer-name)))
      (if (or
           (string-equal "*Group*" bufname)
           (string-equal "*BBDB*" bufname)
           (string-match "\*Summary" bufname)
           (string-match "\*Article" bufname))
          (slbmeh-bury-gnus)
        (if (get-buffer "*Group*")
            (slbmeh-unbury-gnus)
          (gnus-unplugged)))))

  (defun slbmeh-unbury-gnus ()
    (interactive)
    (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
      (set-window-configuration gnus-bury-window-configuration)))

  (defun slbmeh-bury-gnus ()
    (interactive)
    (setq gnus-bury-window-configuration nil)
    (let ((buf nil)
          (bufname nil))
      (dolist (buf (buffer-list))
        (setq bufname (buffer-name buf))
        (when (or
               (string-equal "*Group*" bufname)
               (string-equal "*BBDB*" bufname)
               (string-match "\*Summary" bufname)
               (string-match "\*Article" bufname))
          (unless gnus-bury-window-configuration
            (setq gnus-bury-window-configuration (current-window-configuration)))
          (delete-other-windows)
          (if (eq (current-buffer) buf)
              (bury-buffer)
            (bury-buffer buf))))))

  (defun slbmeh-message-citation ()
    (interactive)
    (when message-reply-headers
      (slbmeh-message-citation-delete)
      (message-goto-body)
      (let* ((parsed-address (mail-header-parse-address (mail-header-from message-reply-headers)))
             (my-bbdb-record (bbdb-message-search (cdr parsed-address) (car parsed-address)))
             (start-pos (point))
             (overlay)
             (full-name
              (or (if my-bbdb-record
                      (bbdb-record-name my-bbdb-record)
                    (cdr parsed-address))
                  "Joe Bloggs ")))
        (funcall slbmeh-message-citation-function full-name)
        (unless (eq start-pos (point))
          (setq overlay (make-overlay start-pos (point)))
          (overlay-put overlay 'slbmeh-message-citation nil)))))

  (defun message-first-name-from-full-name (full-name)
    (if (string-match ".*[,].*" full-name)
        (car (last (split-string full-name)))
      (car (split-string full-name))))

  (defun slbmeh-message-citation-name (full-name)
    (insert (message-first-name-from-full-name full-name) ",\n\n"))

  (defun slbmeh-message-citation-hello (full-name)
    (insert "Hello " (message-first-name-from-full-name full-name) "!\n\n"))

  (defun slbmeh-message-citation-dear (full-name)
    (insert "Dear " (message-first-name-from-full-name full-name) ",\n\n"))

  (defun slbmeh-message-citation-hi (full-name)
    (insert "Hi " (message-first-name-from-full-name full-name) "!\n\n"))

  (defun slbmeh-message-citation-default (full-name)
    (message-insert-citation-line))

  (slbmeh-define-alternatives 'slbmeh-message-citation-function '(slbmeh-message-citation-name
                                                                  slbmeh-message-citation-hello
                                                                  slbmeh-message-citation-dear
                                                                  slbmeh-message-citation-hi
                                                                  slbmeh-message-citation-default))

  (defun slbmeh-message-citation-delete ()
    (interactive)
    (let ((overlay)
          (start-pos))
      (goto-char (point-min))
      (goto-char (next-overlay-change (point)))
      (setq overlay (car-safe (overlays-at (point))))
      (when overlay
        (overlay-get overlay 'slbmeh-message-citation)
        (setq start-pos (point))
        (goto-char (next-overlay-change (point)))
        (delete-region start-pos (point)))))

  (defun slbmeh-message-citation-toggle ()
    (interactive)
    (save-excursion
      (toggle-slbmeh-message-citation-function)
      (slbmeh-message-citation)))

  (defun message-toggle-gcc ()
    "Insert or remove the \"Gcc\" header."
    (interactive)
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (if (message-fetch-field "Gcc")
            (message-remove-header "Gcc")
          (gnus-inews-insert-archive-gcc)))))

  (defun slbmeh-show-nnmail-split-history ()
    (interactive)
    (let ((hi (sort (mapcar 'caar nnmail-split-history) 'string<))
          (elem)
          (count)
          (total))
      (while hi
        (if (string= elem (car hi))
            (setq count (+ count 1))
          (setq elem (car hi))
          (when total
            (setcar total (concat (car total) ": " (number-to-string count))))
          (setq count 1)
          (add-to-list 'total elem))
        (setq hi (cdr hi)))
      (if total
          (setcar total (concat (car total) ": " (number-to-string count)))
        (setq total '("No new Mail")))
      (message (format "%s%s" slbmeh-check-mail-time (nreverse total)))))

  (defun slbmeh-get-new-news-set-time ()
    (setq slbmeh-check-mail-time (format-time-string "[%H:%M] ")))

  (unless (boundp 'slbmeh-check-mail-time)
    (slbmeh-get-new-news-set-time))

  (add-hook 'gnus-get-new-news-hook 'slbmeh-get-new-news-set-time)

  ;; Gnus Keybindings
  (global-set-key [C-f7] 'slbmeh-gnus)
  (after gnus
    (define-key gnus-summary-mode-map [(meta up)] '(lambda () (interactive) (scroll-other-window -1)))
    (define-key gnus-summary-mode-map [(meta down)] '(lambda () (interactive) (scroll-other-window 1)))
    (define-key gnus-summary-mode-map [(control up)] 'gnus-summary-prev-thread)
    (define-key gnus-summary-mode-map [(control down)] 'gnus-summary-next-thread)
    (define-key gnus-group-mode-map [(meta h)] 'slbmeh-show-nnmail-split-history))

  (after message
    (define-key message-mode-map [(control ?c) (control ?f) (control ?g)] 'message-toggle-gcc)
    (define-key message-mode-map [f6] 'slbmeh-message-citation-toggle))


  ;; Use w3m for html rendering
  (require-package 'w3m)
  (setq mm-text-html-renderer 'w3m
        mm-discouraged-alternatives '("text/html" "text/richtext"))

  ;; Fetch only part of the article if we can.
  (setq gnus-read-active-file 'some)

  (setq gnus-fetch-old-headers t)

  ;; Tree view for groups.
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)

  (require 'gnus-registry)
  (gnus-registry-initialize)

  ;; BBDB Integration
  (require-package 'bbdb)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (setq ldap-directory-lookup nil)

  (bbdb-initialize 'gnus 'message)

  (setq bbdb-use-pop-up nil
        bbdb-notice-auto-save-file t
        bbdb-pop-up-target-lines 10
        bbdb-send-mail-style 'message)

  (when (file-exists-p "~/.gnus-config.el")
    (load-file "~/.gnus-config.el"))

  (when (file-exists-p "~/.ldap-config.el")
    (load-file "~/.ldap-config.el"))

  (require 'eudc)
  (require 'eudcb-bbdb)
  (require 'eudcb-mab)

  (eudc-set-server "localhost" 'bbdb t)
  (setq eudc-inline-expansion-servers 'hotlist)
  (add-to-list 'eudc-server-hotlist '("localhost" . bbdb) t)
  (setq eudc-default-return-attributes nil
        eudc-strict-return-matches nil)
  (eudc-protocol-set 'eudc-inline-query-format
                     '((firstname)
                       (lastname)
                       (firstname lastname)
                       (net))
                     'bbdb)
  (eudc-protocol-set 'eudc-inline-expansion-format
                     '("%s %s <%s>" firstname lastname net)
                     'bbdb)
  (MacOSX
   (add-to-list 'eudc-server-hotlist '("localhost" . mab) t)
   (eudc-protocol-set 'eudc-inline-query-format
                      '((name)
                        (email))
                      'mab)
   (eudc-protocol-set 'eudc-inline-expansion-format
                      '("%s <%s>" name email)
                      'mab))

  (when (and (boundp 'ldap-directory-lookup) ldap-directory-lookup)
    (add-to-list 'eudc-server-hotlist '("exchange-server" . ldap) t)
    (eudc-protocol-set 'eudc-inline-query-format
                       '(;(cn)
                         ;(cn cn)
                         ;(cn cn cn)
                         ;(sn)
                         ;(givenname)
                         ;(surname)
                         ;(givenname surname)
                         ;(fullname)
                         ;(uid)
                         ;(name)
                         ;(surname)
                         (mail))
                         ;(mailnickname))
                       'ldap)
    (eudc-protocol-set 'eudc-inline-expansion-format
                       '("%s %s <%s>" givenname surname mail)
                       'ldap))

  (defun slb-eudc-expand-inline()
    (interactive)
    (if (eq eudc-protocol 'ldap)
        (progn (move-end-of-line 1)
               (insert "*")
               (unless (condition-case nil
                           (eudc-expand-inline)
                         (error nil))
                 (backward-delete-char-untabify 1)))
      (eudc-expand-inline)))

  (eval-after-load "message"
    '(define-key message-mode-map (kbd "TAB") 'slb-eudc-expand-inline))
  (eval-after-load "sendmail"
    '(define-key mail-mode-map (kbd "TAB") 'slb-eudc-expand-inline))
  (eval-after-load "post"
    '(define-key post-mode-map (kbd "TAB") 'slb-eudc-expand-inline)))

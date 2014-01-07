;;** 38 The (info "(emacs)Calendar/Diary")

(when section-calendar-diary (message "38 The Calendar and the Diary...")

;; TODO All now being done within Org mode...

;;*** 38.1 (info "(emacs)Calendar Motion")

;; years must be written in full
(setq abbreviated-calendar-year nil)
(setq diary-abbreviated-year-flag nil)

;; ;; interpret the date 1/2/1990 as February 1, 1990
;; (setq european-calendar-style t)  ; obsolete!

;; set the style of calendar and diary dates to ISO
(setq calendar-date-style 'iso)

;; week in the calendar begins on Monday
(setq calendar-week-start-day 1)

;; mark all visible dates that have diary entries
(setq mark-diary-entries-in-calendar t)
;; (add-hook 'initial-calendar-window-hook 'mark-diary-entries)

;; marks the current date, by changing its face
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; bind calendar to `C-c c'
(global-set-key (kbd "C-c c") 'calendar)


;;*** 38.2 (info "(emacs)Scroll Calendar")

;; fix foolish calendar-mode scrolling
(add-hook 'calendar-load-hook
          (lambda ()
            (setq mark-holidays-in-calendar t)
            (define-key calendar-mode-map [(>)] 'scroll-calendar-left)
            (define-key calendar-mode-map [(<)] 'scroll-calendar-right)
            (define-key calendar-mode-map [(control x) (>)]
              'scroll-calendar-left)
            (define-key calendar-mode-map [(control x) (<)]
              'scroll-calendar-right)))


;;*** 38.7 Times of (info "(emacs)Sunrise/Sunset")

(setq calendar-latitude [50 87 north])
(setq calendar-longitude [4 71 east])
(setq calendar-location-name "Leuven, BE")

;; (setq calendar-latitude [43 41 north])
;; (setq calendar-longitude [6 81 east])
;; (setq calendar-location-name "Boulouris, FR")


;;*** 38.10 The (info "(emacs)Diary")

;; The Emacs diary keeps track of appointments or other events on a daily
;; basis, in conjunction with the calendar. To use the diary feature, you
;; must first create a "diary file" containing a list of events and their
;; dates.

(when (try-require 'diary-lib)

    ;; create an empty diary file (if it does not exist yet)
    (unless (file-exists-p diary-file)
      (shell-command (concat "touch " diary-file)))

    ;; copy the diary entries into a special buffer (also display the diary
    ;; when I do `M-x diary')
    (add-hook 'diary-display-hook 'fancy-diary-display)

    ;; TODO Sort each day's diary entries by their time of day?
    (add-hook 'diary-display-hook 'sort-diary-entries)
    (add-hook 'list-diary-entries-hook 'sort-diary-entries t)

    ;; allow `#includes' in `~/diary'
    (add-hook 'list-diary-entries-hook 'include-other-diary-files)

    ;; generate the diary window for 4 days starting with the current date
    (diary 4)

    ;; How do you arrange the entries of diary? Can they be automatically
    ;; arranged according to date and not just according to when they were
    ;; entered into the diary?
)


;;*** 38.11 (info "(emacs)Appointments")

;; enable appointment notification, several minutes beforehand
(add-hook 'diary-hook 'appt-make-list)


(when (try-require 'org-agenda)

    ;; Insinuate appt
    (require 'appt)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)
    ;; When use 'r' (rebuild agenda) reload appt
    (add-hook 'org-agenda-mode-hook (lambda ()
                                      (setq appt-time-msg-list nil)
                                      (org-agenda-to-appt)))
    (setq appt-audible t)
    (setq appt-display-format 'echo)

;;;;
    ;; turn appointment checking on
    (appt-activate 1)

    ;; time in minutes before an appointment that the warning begins
    (setq appt-message-warning-time 15)  ; 12

    ;; number of minutes to wait between checking the appointment list
    (setq appt-display-interval 5)  ; 3

    ;; update appt each time agenda opened
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
;;;;

    (when window-system
      (setq appt-display-format 'window)

      ;; FIXME Check `notify-send' (in `libnotify-bin' Ubuntu package) is installed
      (defun rgr/org-display (min-to-app new-time msg)
        (shell-command
         (concat "notify-send "
                 "-i /usr/share/icons/gnome/32x32/status/appointment-soon.png "
                 "'Appointment' "
                 "'" msg "'")))
      ;; TODO For Windows users: use `todochicku.el' and the snarl notifier

      (setq appt-disp-window-function (function rgr/org-display)))
)


;;*** 38.14 Summing (info "(emacs)Time Intervals")

;; http://emacswiki.org/cgi-bin/wiki/TimeClock

;; check Org's capabilities in this area: (info "(org)Clocking work time")


;;*** 38.15 (info "(emacs)Advanced Calendar/Diary Usage")

;; ;; list of notable days
;; (setq calendar-holidays nil)

;; remove some holidays
(setq holiday-bahai-holidays nil)       ; get rid of Baha'i holidays
(setq holiday-general-holidays nil)     ; get rid of too U.S.-centric holidays
(setq holiday-hebrew-holidays nil)      ; get rid of religious holidays
(setq holiday-islamic-holidays nil)     ; get rid of religious holidays
(setq holiday-oriental-holidays nil)    ; get rid of Oriental holidays
(setq holiday-solar-holidays nil)

;; add some Belgian holidays
(setq holiday-local-holidays
      '(
        (holiday-fixed 01 01 "New Year's Day")
        (holiday-fixed 02 14 "Valentine's Day")
        (holiday-fixed 05 01 "Labor Day")
        (holiday-fixed 07 21 "Independence Day")
        (holiday-fixed 08 15 "Assumption")
        (holiday-fixed 11 01 "Toussaint")
        (holiday-fixed 11 11 "Armistice 1918")

        ;; holidays with variable dates
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 6 0 3 "Father's Day")))

;; user defined holidays
(setq holiday-other-holidays nil)  ; default

;; mark dates of holidays in the calendar
(setq mark-holidays-in-calendar t)


;;*** Add-Ons: Getting Things Done with (info "(org)Top") Mode

;; We should get this for Org mode as well: asking the user if...
;; ;; ask if the user wants to clock out before exiting Emacs
;; (add-hook 'kill-emacs-query-functions 'timeclock-query-out)
;;
;; ;; ask the user if they wish to clock in
;; (timeclock-query-in))


;; After all the configuration has been done, you can easily manage your
;; daily work and tasks with Org mode.
;; Press `C-c a a' to jump you to this week's task page from anywhere.

;; (info "(org)Top") outline-based notes management and organizer
(when (try-require 'org-install)

  (try-require 'org-list)

  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  (setq org-completion-use-ido t)

  (setq org-return-follows-link t)

  (add-to-list 'org-modules 'org-habit)

  ;; enable globally unique ID
  (add-to-list 'org-modules 'org-id)




  ;; (defun bh/insert-inactive-timestamp ()
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (insert "\n")
  ;;     (org-cycle)
  ;;     (org-insert-time-stamp nil t t nil nil nil)))
  ;; (add-hook 'org-insert-heading-hook 'bh/insert-inactive-timestamp)


  ;; Unhiding edited areas
  ;; I like the idea of clustering undo but find it disconcerting
  (setf org-self-insert-cluster-for-undo nil)
  ;; somebody, I think Carsten, suggested this, and it might work for you, but
  ;; for some reason I commented it out. I don't remember what the reason was.
  ;; Maybe speed.
  '(defadvice undo (after org-undo-reveal activate)
     "Make point and context visible after an undo command in Org mode."
     (and (org-mode-p) (org-reveal)))
  ;;(ad-unadvise 'undo)



  (defun my/org-switch-language ()
    "Switch language for Org file, if a `#+LANGUAGE:' meta-tag is
on top 14 lines."
    (save-excursion
      (goto-line 15)
      (if (re-search-backward "#\\+LANGUAGE: +\\([A-Za-z_]*\\)" 1 t)
          (ispell-change-dictionary (match-string 1)))))



  (add-hook 'org-mode-hook
            (lambda ()
              ;; display images in your Org files
              (turn-on-iimage-mode)

              (local-set-key "\M-n" 'outline-next-visible-heading)
              (local-set-key "\M-p" 'outline-previous-visible-heading)

              ;; table
              (local-set-key "\M-\C-w" 'org-table-copy-region)
              (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
              (local-set-key "\M-\C-l" 'org-table-sort-lines)

              ;; fix tab
              (local-set-key "\C-y" 'yank)

              ;; file modification date
              (set (make-local-variable 'time-stamp-format) "%:y-%02m-%02d")
              (set (make-local-variable 'time-stamp-start) "^#\\+DATE: +")
              (set (make-local-variable 'time-stamp-end) "$")

              ;; guess language
              (my/org-switch-language)

              ;; flyspell mode to spell check everywhere
              (flyspell-mode 1)))


    ;; getting started
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (define-key global-map (kbd "C-c l") 'org-store-link)
    (define-key global-map (kbd "C-c a") 'org-agenda)

    (global-set-key (kbd "C-c o a l") 'org-agenda-list)
    (global-set-key (kbd "C-c o a t") 'org-todo-list)


    ;; XXX
    (setq org-global-properties
          '(("Effort_ALL" .
             "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00")))


    ;; 
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-agenda-dim-blocked-tasks t)



    (setq org-special-ctrl-a/e t)

    ;;
    (setq org-show-siblings t)

    (setq org-show-hierarchy-above t)

    (setq org-show-following-heading t)

    ;; don't fontify the whole line for headings
    (setq org-fontify-whole-heading-line nil)


;;**** 1 (info "(org)Introduction")

    ;; 1.3 insert the first line setting Org mode in empty files
    (setq org-insert-mode-line-in-empty-file t)


;;**** 2 (info "(org)Document Structure")

    ;; 2.3 don't switch to OVERVIEW at startup
    (setq org-startup-folded nil)

    ;; 2.4 headlines in the current buffer are offered via completion
    ;; (interface also used by the refile command)
    (setq org-goto-interface 'outline-path-completion)

    ;; 2.5 ellipsis to use in the Org mode outline
    (setq org-ellipsis "â€¦â€¦")
    ; (setq org-ellipsis 'org-column)

    ;; 2.7 don't make TAB cycle visibility on plain list items
    (setq org-cycle-include-plain-lists nil)


;;**** 3 (info "(org)Tables")

    ;; Have a look at "Org as a spreadsheet system: a short introduction"
    ;; (http://orgmode.org/worg/org-tutorials/org-spreadsheet-intro.php)

    ;; default export parameters for `org-table-export'
    (setq org-table-export-default-format "orgtbl-to-csv")

    ;; FIXME Only set calc-internal-prec to 12 (instead of 8 by default)
    ;; 3.5.2
    (setq org-calc-default-modes
          '(calc-internal-prec 12
            calc-float-format  (float 12)
            calc-angle-mode    deg
            calc-prefer-frac   nil
            calc-symbolic-mode nil
            calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm))
            calc-display-working-message t))

    ;; recalculate all tables in a file simultaneously
    (defun org-recalculate-all-tables ()
      (interactive)
      (org-table-map-tables (lambda () (org-table-recalculate t)) t))


;;**** 4 (info "(org)Hyperlinks")

    ;; directory with org files (used by the hooks for `remember.el')
    (setq org-directory
          (if (file-directory-p "~/Personal/")
              "~/Personal/"
            "~/"))

    ;; create web links to Google groups or Gmane (instead of Gnus messages)
    (setq org-gnus-prefer-web-links t)

    (defun org-toggle-link-style ()
      "Toggle between descriptive and literal link styles."
      (interactive)
      (if (member '(org-link) buffer-invisibility-spec)
          ;; descriptive -> literal
          (progn
            (org-remove-from-invisibility-spec '(org-link))
            (message "Showing literal links"))
        ;; literal -> descriptive
        (org-add-to-invisibility-spec '(org-link))
        (message "Showing descriptive links"))
      (org-restart-font-lock))

    ;; 4.3 function and arguments to call for following mailto links
    (setq org-link-mailto-program '(compose-mail "%a" "%s"))

    ;; 
    (setq org-link-frame-setup '((vm   . vm-visit-folder)
                                 (gnus . org-gnus-no-new-news)
                                 (file . find-file-other-window)))


;;**** 5 (info "(org)TODO Items")

    ;; 5.1 select a TODO state and bypass any logging associated with that
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)

    ;; We can use TODO keywords to implement the different task states:
    ;; 5.2.4 list of TODO entry keyword sequences and their interpretation
    (setq org-todo-keywords
          '((sequence "TODO(t)" ; 16
                      ; "SOMEDAY(m!)" ; 8 inactive project
                      "STARTED(s!)" ; 5
                      "WAIT(w@/!)" ; 9
                      "DELEGATED(l)" ; 5
                      "DFRD(f)" ; 2    ;; XXX is DEFERRED a completion state?
                      "|"
                      "DONE(d!/!)" ; 14
                      "CANCELED(c@/!)") ; 9
            (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                      "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
            (sequence "OPENPO(O@)" "|" "CLOSEDPO(C!/!)")))

    ;; 5.2.6 faces for specific TODO keywords
    (GNUEmacs
    (setq org-todo-keyword-faces
          '(("TODO" . org-todo)
            ("SOMEDAY" . fni-org-someday-kwd-face)
            ("STARTED" . fni-org-started-kwd-face)
            ("WAIT" . fni-org-waiting-kwd-face)
            ("DELEGATED" . fni-org-delegated-kwd-face)
            ("DFRD" . fni-org-deferred-kwd-face)
            ("DONE" . org-done)
            ("CANCELED" . fni-org-canceled-kwd-face)

            ("QUOTE" . fni-org-quote-kwd-face)
            ("QUOTED" . fni-org-quoted-kwd-face)
            ("APPROVED" . fni-org-approved-kwd-face)
            ("EXPIRED" . fni-org-expired-kwd-face)
            ("REJECTED" . fni-org-rejected-kwd-face)

            ("OPENPO" . fni-org-openpo-kwd-face)
            ("CLOSEDPO" . fni-org-closedpo-kwd-face))))

    ;; change the face of a headline (as an additional information) if it is
    ;; marked DONE (to face `org-headline-done')
    (setq org-fontify-done-headline t)

    ;; 5.3.1 insert a CLOSED time stamp each time a TODO entry is marked DONE
    (setq org-log-done nil)

    ;; 5.3.2 insert state change notes and time stamps into a drawer
    (setq org-log-into-drawer t)

    ;; 5.3.2 the notes will be ordered according to time
    (setq org-log-states-order-reversed nil)


;;**** 6 (info "(org)Tags")

    ;; Context (place, time or particular resources for doing a task) and
    ;; people are something best implemented with tags.

    ;; 6.2 list of tags ("contexts") allowed in Org mode files
    (setq org-tag-alist '((:startgroup . nil)
                            ("home" . ?h)
                            ("work" . ?w)
                          (:endgroup . nil)
                          ("errands" . ?e)
                          ("computer" . ?c)
                          ("online" . ?o)  ; Internet
                          ("mail" . ?m)  ; (e)mail
                          ("phone" . ?p)
                          ("reading" . ?r)

                          ("note" . ?n)
                          ))

    ;; ("waiting" . ?W)
    ;; ("someday" . ?s)

    ;; morning, midday, afternoon, evening
    ;; monday, tuesday, ...
    ;; weekend

    ;; important

    ;; ("note" . ?n)
    ;; ("crypt" . ?XXX)
    ;; ("appt" . ?a)
    ;; ("next" . ?n)

    ;; ("PROJ" . ?P)


    ;; faces for specific tags
    (setq org-tag-faces
          '(
            ("refile" . (:background "#D4EAFF"))
            ("home" . (:background "pale green" :italic t))
            ("work" . (:italic t :background "#F9E816"))
            ))

    ;; remove redundant tags of headlines (from David Maus)
    (defun my/org-remove-redundant-tags ()
      "Remove redundant tags of headlines in current buffer.
A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
      (interactive)
      (when (eq major-mode 'org-mode)
        (save-excursion
          (org-map-entries
           '(lambda ()
              (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                    local inherited tag)
                (dolist (tag alltags)
                  (if (get-text-property 0 'inherited tag)
                      (push tag inherited) (push tag local)))
                (dolist (tag local)
                  (if (member tag inherited) (org-toggle-tag tag 'off)))))
           t nil))))


;;**** 8 (info "(org)Dates and Times")

    ;; 8.2 number of minutes to round time stamps to
    (setq org-time-stamp-rounding-minutes '(5 5))

    ;; 8.3 no. of days to display before expiration of a deadline
    (setq org-deadline-warning-days 14)

    ;; skip deadline prewarning (up to 7 days before the actual deadline) when
    ;; entry is also scheduled
    (setq org-agenda-skip-deadline-prewarning-if-scheduled 7)

    ;; 8.3 don't show deadlines when the corresponding item is done
    (setq org-agenda-skip-deadline-if-done t)

    ;; 8.3 don't show scheduled items in agenda when they are done
    (setq org-agenda-skip-scheduled-if-done t)

    ;; 8.5 format string for displaying dates in the daily/weekly agenda and
    ;; in the timeline
    (setq org-agenda-format-date
          (concat "\n" "%Y-%m-%d" " %a "
                  (make-string (- (window-width) 15) ?_)))

    ;; faces for showing deadlines in the agenda
    (setq org-agenda-deadline-faces
          '((1.01 . fni-org-deadline-yesterday)
            (0.99 . fni-org-deadline-today)
            (0.49 . fni-org-deadline-tomorrow)
            (0.00 . fni-org-deadline-later)))

    ;; don't select item by timestamp or -range if it is DONE
    (setq org-agenda-skip-timestamp-if-done t)

    ;; show all days between the first and the last date
    (setq org-timeline-show-empty-dates t)

    (GNUEmacs
        ;; the time clocking code for Org mode
        (when (try-require 'org-clock)

            ;; If you have an `Effort' property defined, its value is also
            ;; shown in the mode line, and you can configure `org-clock-sound'
            ;; to get an alert when your planned time for a particular item is
            ;; over.
            (setq org-clock-sound "~/Music/Sounds/alarm.wav")
                                        ; sound that will used for
                                        ; notifications

            ;; remove the clock line when the resulting time is 0:00
            (setq org-clock-out-remove-zero-time-clocks t)

    ;;;         ;; when clocking into a task with a clock entry which has not
    ;;;         ;; been closed, resume the clock from that point
    ;;;         (setq org-clock-in-resume t)

            ;; 8.4 save both the running clock and the entire clock history
            ;; when Emacs is closed, and resume it next time Emacs is started
            ;; up
            (setq org-clock-persist t)

            ;; 8.4 set up hooks for clock persistence
            (org-clock-persistence-insinuate)

            ;; resume clocking task on clock-in if the clock is open
            (setq org-clock-in-resume t)

            ;; ;; 8.5 set task state to STARTED while clocking it
            ;; (setq org-clock-in-switch-to-state "STARTED")

            ;; 8.5 resolve open clocks if the user is idle more than 15
            ;; minutes
            (setq org-clock-idle-time 15)

            ;; clock won't be stopped when the clocked entry is marked DONE
            (setq org-clock-out-when-done nil)

            ;; time included for the modeline clock is all time clocked into
            ;; this task today
            (setq org-clock-modeline-total 'today)))



    ;; "C-c a t" should show all the TODO items.  You may also want to
    ;; take a look at "org-agenda-todo-ignore-deadlines",
    ;; "org-agenda-todo-ignore-scheduled" and
    ;; "org-agenda-todo-ignore-with-date" as well.


    (setq org-agenda-columns-add-appointments-to-effort-sum t)
    (setq org-agenda-default-appointment-duration 60)



    (defun my/org-clock-in-task-by-id (id)
      "Start the clock on the entry with id ID."
      (require 'org-id)
      (save-restriction
        (widen)
        (org-with-point-at (org-id-find id 'marker)
          (org-clock-in nil))))

    (defun my/org-clock-in-task-organization ()
      "Start the clock on the entry entry \"Organization\"."
      (interactive)
      (my/org-clock-in-task-by-id "94fa5272-008f-4a26-b4fc-cbc5304f2e05"))

    (defun my/org-goto-task-organization ()
      "Goto Organization heading."
      (interactive)
      (org-id-goto "94fa5272-008f-4a26-b4fc-cbc5304f2e05"))

    (defun my/org-clock-in-task-emails-and-news ()
      "Start the clock on the entry \"Emails and News\"."
      (interactive)
      (my/org-clock-in-task-by-id "3f14e63e-0cd5-4901-9c00-459bbf658a58"))

    ;; (global-set-key (kbd "<f9> o") 'my/org-clock-in-task-organization)
    ;; (global-set-key (kbd "<f9> m") 'my/org-clock-in-task-emails-and-news)
    (global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

    ;; Maybe `C-u M-x org-clock-in RET' does what you want.
    (global-set-key (kbd "C-c C-x S-C-i")
                    (lambda ()
                      (interactive)
                      (org-clock-in '(4))))

    ;; (add-hook 'remember-mode-hook 'org-clock-in 'append)
    ;; (add-hook 'org-remember-before-finalize-hook 'my/org-clock-in-interrupted-task)

    ;; FIXME What if "No active clock"?

    (defun my/org-clock-in-interrupted-task ()
      "Clock back into the task that has been interrupted, if there is one."
      (interactive)
      (if (and (not org-clock-resolving-clocks-due-to-idleness)
               (marker-buffer org-clock-marker)
               (marker-buffer org-clock-interrupted-task))
          (org-with-point-at org-clock-interrupted-task
            (org-clock-in nil))
        (org-clock-out)))

    (global-set-key (kbd "C-c C-x C-q") 'my/org-clock-in-interrupted-task)
    (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
    (global-set-key (kbd "C-c C-x C-i") 'org-clock-in)



    ;; add an effort estimate on the fly when clocking in
    (defun my/org-mode-ask-effort ()
      "Ask for an effort estimate when clocking in."
      (unless (org-entry-get (point) "Effort")
        (let ((effort
               (completing-read
                "Effort: "
                (org-entry-get-multivalued-property (point) "Effort"))))
          (unless (equal effort "")
            (org-set-property "Effort" effort)))))

    (add-hook 'org-clock-in-prepare-hook
              'my/org-mode-ask-effort)




    ;; number of clock tasks to remember in history
    (setq org-clock-history-length 9)





    ;; get a compact view during follow mode in the agenda
    (defun my-compact-follow ()
      "Make the view compact, then show the necessary minimum."
      (ignore-errors
        (save-excursion
          (while (org-up-heading-safe))
          (hide-subtree)))
      (let ((org-show-siblings nil)
            (org-show-hierarchy-above t))
        (org-reveal))
      (save-excursion
        (org-back-to-heading t)
        (show-children)))

    (add-hook 'org-agenda-after-show-hook 'my-compact-follow)





    (defun sacha/org-calculate-free-time (date start-time end-of-day)
      "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
      (save-window-excursion
        (let ((files org-agenda-files)
              (total-unscheduled 0)
              (total-gap 0)
              file
              rtn
              rtnall
              entry
              (last-timestamp start-time)
              scheduled-entries)
          (while (setq file (car files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
              (setq rtnall (append rtnall rtn)))
            (setq files (cdr files)))
          ;; For each item on the list
          (while (setq entry (car rtnall))
            (let ((time (get-text-property 1 'time entry)))
              (cond
               ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
                (setq scheduled-entries (cons (cons
                                               (save-match-data (appt-convert-time (match-string 1 time)))
                                               (save-match-data (appt-convert-time (match-string 2 time))))
                                              scheduled-entries)))
               ((and time
                     (string-match "\\([^-]+\\)\\.+" time)
                     (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry)))
                (setq scheduled-entries
                      (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
                                        (appt-convert-time (match-string 2 time))))) 
                        (cons (cons start
                                    (and (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
                                         (+ start (string-to-number (match-string 2 (get-text-property 1 'txt entry))))))
                              scheduled-entries))))
               ((string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
                (setq total-unscheduled (+ (string-to-number
                                            (match-string 2 (get-text-property 1 'txt entry)))
                                           total-unscheduled)))))
            (setq rtnall (cdr rtnall)))
          ;; Sort the scheduled entries by time
          (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

          (while scheduled-entries
            (let ((start (car (car scheduled-entries)))
                  (end (cdr (car scheduled-entries))))
              (cond
               ;; are we in the middle of this timeslot?
               ((and (>= last-timestamp start)
                     (<= last-timestamp end))
                ;; move timestamp later, no change to time
                (setq last-timestamp end))
               ;; are we completely before this timeslot?
               ((< last-timestamp start)
                ;; add gap to total, skip to the end
                (setq total-gap (+ (- start last-timestamp) total-gap))
                (setq last-timestamp end)))
              (setq scheduled-entries (cdr scheduled-entries))))
          (if (< last-timestamp end-of-day)
              (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
          (cons total-unscheduled total-gap))))


    (defun sacha/org-show-load ()
      "Show my unscheduled time and free time for the day."
      (interactive)
      (let ((time (sacha/org-calculate-free-time
                   ;; today
                   (calendar-gregorian-from-absolute (time-to-days (current-time)))
                   ;; now
                   (let* ((now (decode-time))
                          (cur-hour (nth 2 now))
                          (cur-min (nth 1 now)))
                     (+ (* cur-hour 60) cur-min))
                   ;; until the last time in my time grid
                   (let ((last (car (last (elt org-agenda-time-grid 2)))))
                     (+ (* (/ last 100) 60) (% last 100))))))
        (message "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
                 (/ (car time) (* .01 (cdr time)))
                 (car time)
                 (cdr time)
                 (- (cdr time) (car time)))))

    (defun sacha/org-agenda-load (match)
      "Can be included in `org-agenda-custom-commands'."
      (let ((inhibit-read-only t)
            (time (sacha/org-calculate-free-time
                   ;; today
                   (calendar-gregorian-from-absolute org-starting-day)
                   ;; now if today, else start of day
                   (if (= org-starting-day
                          (time-to-days (current-time)))
                       (let* ((now (decode-time))
                              (cur-hour (nth 2 now))
                              (cur-min (nth 1 now)))
                         (+ (* cur-hour 60) cur-min))
                     (let ((start (car (elt org-agenda-time-grid 2))))
                       (+ (* (/ start 100) 60) (% start 100))))
                   ;; until the last time in my time grid
                   (let ((last (car (last (elt org-agenda-time-grid 2)))))
                     (+ (* (/ last 100) 60) (% last 100))))))
        (goto-char (point-max))
        (insert (format
                 "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
                 (/ (car time) (* .01 (cdr time)))
                 (car time)
                 (cdr time)
                 (- (cdr time) (car time))))))



    (when (try-require 'org-habit)

      (add-to-list 'org-modules 'org-habit)

      (setq org-habit-show-habits-only-for-today nil))


;;**** 9 (info "(org)Remember")

    ;; (info "(remember)Top")

    ;; a mode for quickly jotting down things to remember
    (when (try-require 'remember-XXX)  ; the ultimate capture tool

      ;; default target for storing notes
      (setq remember-data-file (concat org-directory "refile.org"))

      ;; key bindings
      (global-set-key (kbd "C-c r") 'remember)
        ; save yourself some copying and pasting by marking a region of text
        ; and using `C-u C-c r' (remember): the selected text will be
        ; included in the buffer, so all you have to do is comment on it.

      (defun remember-review-file ()
        "Open `remember-data-file'."
        (interactive)
        (find-file-other-window remember-data-file))

      (global-set-key (kbd "C-c R") 'remember-review-file))


    ;; 9.1.2 default target for storing notes
    (setq org-default-notes-file (concat org-directory "refile.org"))
                                        ; Inbox for collecting

    ;; 9.1.2 templates for the creation of remember buffers
    (setq org-remember-templates
          '(("Buffer" ?b "* %a\n\n%i%?%!" org-default-notes-file "Buffer" nil)
            ("Org" ?o "* %a\n\n%i%?" org-default-notes-file "Org" nil)
            ("Infos" ?i "* %a\n\n%i%?%!" org-default-notes-file "Infos" nil)))
    (setq org-remember-templates
          '(("Tasks" ?t "* TODO %?\n  %i\n  %a" remember-data-file)))
    (setq org-remember-templates
          '(("Task" ?t "* TODO %?\n  %i\n  %a" "~/Personal/todo.org")
            ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/Personal/journal.org")))
    (setq org-remember-templates
          '((?t "* TODO %^{Todo} %^G\n %i\n %a\n %U\n")
            (?T "* TODO %^{Todo} %^G\n %i\n %U\n")
            (?n "* %^{Title}\n %i\n %a\n %U\n")
            (?N "* %^{Title}\n %i\n %U\n")))
    (setq org-remember-templates
          '(("Tasks" ?t "* TODO %?\n  %i\n  %a"            "~/Personal/organizer.org")
            ("RT"    ?R "* [[RT:%^{Number}][%^{Number}/%^{Description}]]" "~/Personal/rt.org")))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %? %^g\n %i\n " "F:/GTD/newgtd.org" "Office")
            ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "L:journal.org")
            ("Book" ?b "\n* %^{Book Title} %t :READING: \n%[l:/booktemp.txt]\n" "L:journal.org")
            ("Private" ?p "\n* %^{topic} %T \n%i%?\n" "F:/gtd/privnotes.org")
            ("Contact" ?c "\n* %^{Name} :CONTACT:\n%[l:/contemp.txt]\n" "F:/gtd/privnotes.org")))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/Personal/newgtd.org" "Tasks")
            ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "~/Personal/journal.org")
            ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "~/Personal/journal.org")
            ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")
            ("Daily Review" ?a "** %t :COACH: \n%[~/.daily_review.txt]\n" "~/Personal/journal.org")
            ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Personal/someday.org")
            ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/Personal/vocab.org")))
    (setq org-remember-templates
          '((?t "* TODO %?\n   %i\n %a" "~/Personal/glyn.org")
            (?n "*Note: %?\n%^T\n%i\n  %a" "~/Personal/notes.org"  )
            (?q "*Quote: %?\n%^T\n%i\n  %a" "~/Personal/barth.org"  )))
    (setq org-remember-templates
          '(("Task" ?t "* %^{Task status|TODO|STARTED|SUBTASK|DONE} %^{Brief
Description} %^G\n %^{subject}p  %^{other-subjects}p  %^{sub-subjects}p 
%^{keywords}p %?\n    Added: %U \n" "~/notes/notes-log-090410.org" "Task")))

    ;; Unsorted
    ;; Temporary
    ;; To Do
    ;; Applications
    ;; Web Quotes
    ;; Documentations
    ;; People
    ;; Projects
    ;; Job Ads

    ;; Family
    ;; Friends
    ;; Future
    ;; Graphics
    ;; Languages
    ;; Linux
    ;; Magical Moments
    ;; Networks
    ;; Projects
    ;; Traveling

    ;; setup `remember.el' for use with Org mode
    (eval-after-load 'remember
      '(org-remember-insinuate))


  ;; fast note taking in Org mode (the ultimate capture tool)
  (when (try-require 'org-capture)
    (define-key global-map "\C-cr" 'org-capture)

    ;;   + %a :: annotation (link)
    ;;   + %i :: initial content (selected text)
    ;;   + %? :: cursor position
    ;;   + %^T :: prompt for a date and time
    ;;   + %^G :: prompt for tags with completion on tags in all agenda files
    ;;   + %t :: time stamp (date only)
    ;;   + %^{prompt} :: prompt the user for a string
    ;;   + %[file] :: insert the contents of the file
    ;;   + %U :: inactive time stamp with date and time
    ;;   + %& :: tell remember to jump to the note after storing it
    (setq org-capture-templates
          '(("t" "Task" entry
             (file+headline "~/Personal/refile.org" "Tasks")
             "* TODO %^{Todo}%?
   :PROPERTIES:
   :Created: %U
   :END:

  %i"                                   ; include a date tag showing when it
                                        ; was added to the TODO file
             :empty-lines 1)

            ("a" "Appt" entry
             (file+headline "~/Personal/refile.org" "Appointments")
             "* %^{Appointment}%?
   %^T
   %i"
             :empty-lines 1)

            ("m" "Mail" entry
             (file+headline "~/Personal/refile.org" "Tasks")
             "* Answer this email....%?
   %^T
   %i"
             :empty-lines 1 :immediate-finish)
            ;; immediate-finish :: immediately store note without further
            ;; prompt (skipping `C-c C-c'), which is very handy for quick
            ;; storing of emails

            ("p" "Phone call" entry
             (file+headline "~/Personal/refile.org" "Phone calls")
             "* %?
   %U
   %i
   From %a"
             :empty-lines 1 :clock-in t :clock-resume t)

            ("n" "Note" entry
             (file+headline "~/Personal/refile.org" "Notes")
             "* %?
   %U
   %i
   From %a"
             :empty-lines 1)

            ("j" "Journal" entry
             (file+datetree "~/Personal/journal.org")
             "* %U  :blog:
   %?"
                                        ; "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x"
            :empty-lines 1)

            ;; notes
            ("N" "Templates adding notes")
            ("Ne" "Emacs" entry
             (file+headline "~/Public/Notes-on-Emacs.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("No" "Org mode" entry
             (file+headline "~/Public/Notes-on-Org.txt" "Notes")
             "* %^{Title}
  :PROPERTIES:
  :Created: %U
  :END:

   %i

   From %a"
             :empty-lines 1)
            ("NL" "Lisp" entry
             (file+headline "~/Public/Notes-on-Lisp.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ng" "Gnus" entry
             (file+headline "~/Public/Notes-on-Gnus.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nl" "LaTeX" entry
             (file+headline "~/Public/Notes-on-LaTeX.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("NT" "TikZ" entry
             (file+headline "~/Public/Notes-on-TikZ.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nb" "Beamer" entry
             (file+headline "~/Public/Notes-on-Beamer.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ns" "StumpWM" entry
             (file+headline "~/Public/Notes-on-StumpWM.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nu" "Linux" entry
             (file+headline "~/Public/Notes-on-Linux.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nc" "Ledger" entry
             (file+headline "~/Public/Notes-on-Ledger.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nr" "RFID" entry
             (file+headline "~/Public/Notes-on-RFID.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ns" "Security" entry
             (file+headline "~/Public/Notes-on-Security.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ;; ("web-clippings" ?w
            ;;  "* %^{Title} %^g \n  :PROPERTIES:\n  :date: %^t\n  :link: %^{link}\n  :END:\n\n %x %?"
            ;;  "~/org/data.org" "Web Clippings")
            ("Nw" "Web" entry
             (file+headline org-default-notes-file "Web Snippets")
             "* %^{Title}

   %?%i
   From %c
   %u"
             :empty-lines 1)



            ("w" "org-protocol" entry
             (file "~/Personal/refile.org")
             "* TODO Review %c
   %U"
             :immediate-finish t :clock-in t :clock-resume t)



            ;; ideas
            ("i" "Idea" entry
             (file+headline "~/Personal/refile.org" "Ideas")
             "* %^{Title}
   %?%i
   From %a"
             :empty-lines 1)
            )))


    ;; "Once a date has been scheduled, use cut and paste to move the task to
    ;; the appropriate category."
    ;;
    ;; I find it much easier to use the refile note command `C-c C-w'. This
    ;; lets me select (with completion) the header under which the entry will
    ;; be placed. By default, this only allows first-level headers to be
    ;; selected. But:

    ;; 9.1.2 headline that should be the default location in the notes file
    (setq org-remember-default-headline "Unfiled")

    ;; 9.1.4 any headline with level <= 2 is a target
    (setq org-refile-targets '((nil :maxlevel . 2)
                                        ; all top-level headlines in the
                                        ; current buffer are used (first) as a
                                        ; refile target
                               (org-agenda-files :maxlevel . 2)))

    ;; 9.1.4 provide refile targets as paths, including the file name (without
    ;; directory) as level 1 of the path
    (setq org-refile-use-outline-path 'file)

    ;; 9.1.4 allow to create new nodes (must be confirmed by the user) as
    ;; refile targets
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; refile only within the current buffer
    (defun my/org-refile-within-current-buffer ()
      "Move the entry at point to another heading in the current buffer."
      (interactive)
      (let ((org-refile-targets '((nil :maxlevel . 5))))
        (org-refile)))

    ;; 9.4 capture from Firefox (to store links and text)
    (try-require 'org-protocol)         ; have a look at
                                        ; http://vimeo.com/5662410

    ;; 9.6.1 subtrees should be archived in the current file
    (setq org-archive-location "::* Archive")


;;**** 10 (info "(org)Agenda Views")


    (defcustom find-recursive-exclude-files '(".*.class$" ".*~$" ".*.elc$")
      "List of regular expressions of files to be excluded when recursively searching for files."
      :type '(repeat (string :tag "File regexp")))

    (defun find-file-recursively (file-regexp directory)
      (interactive "sFile name to search for recursively: \nDIn directory: ")
      (let ((directory (if (equal (substring directory -1) "/")
                           directory
                         (concat directory "/")))
            (matches
             (find-recursive-filter-out
              find-recursive-exclude-files
              (find-recursive-directory-relative-files directory "" file-regexp))))
        (cond ((eq (length matches) 0) (message "No file(s) found!"))
              ((eq (length matches) 1)
               (find-file (concat directory (car matches))))
              (t
               (run-with-timer 0.001 nil
                               (lambda ()
                                 (dispatch-event
                                  (make-event 'key-press '(key tab)))))
               (let ((file (completing-read "Choose file: "
                                            (mapcar 'list matches)
                                            nil t)))
                 (if (or (eq file nil) (equal file ""))
                     (message "No file selected.")
                   (find-file (concat directory file))))))))

    (defun find-recursive-directory-relative-files (directory
                                                    relative-directory
                                                    file-regexp)
      (let* ((full-dir (concat directory "/" relative-directory))
             (matches
              (mapcar
               (function (lambda (x)
                           (concat relative-directory x)))
               (find-recursive-filter-out '(nil)
                                          (directory-files full-dir nil
                                                           file-regexp nil t))))
             (inner
              (mapcar
               (function
                (lambda (dir)
                  (find-recursive-directory-relative-files directory
                                                           (concat relative-directory
                                                                   dir "/")
                                                           file-regexp)))
               (find-recursive-filter-out '(nil "\\." "\\.\\.")
                                          (directory-files full-dir nil ".*"
                                                           nil 'directories)))))
        (mapcar (function (lambda (dir) (setq matches (append matches dir))))
                inner)
        matches))

    (defun find-recursive-filter-out (remove-list list)
      "Remove all the elements in *remove-list* from *list*"
      (if (eq list nil)
          nil
        (let ((elem (car list))
              (rest (cdr list)))
          (if (some
               (lambda (regexp)
                 (if (or (eq elem nil) (eq regexp nil))
                     nil
                   (not (eq (string-match regexp elem) nil))))
               remove-list)
              (find-recursive-filter-out remove-list rest)
            (cons elem (find-recursive-filter-out remove-list rest))))))

    (defvar find-recursive-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

    (if find-recursive-running-xemacs
        nil
      (defadvice directory-files (after
                                  directory-files-xemacs
                                  (dirname &optional full match nosort files-only)
                                  activate)
        "Add an additional argument, FILES-ONLY to the list of arguments
for GNU Emacs. If the symbol is t, then only the files in the
directory will be returned. If FILES-ONLY is nil, then both files and
directories are selected. If FILES-ONLY is not nil and not t, then
only sundirectories are returned."
        (setq ad-return-value
              (cond ((null files-only) ad-return-value)
                    ((eq files-only t)
                     (find-recursive-remove-if (lambda (f)
                                                 (file-directory-p
                                                  (concat dirname "/" f)))
                                               ad-return-value))
                    (t
                     (find-recursive-remove-if (lambda (f)
                                                 (not (file-directory-p
                                                       (concat dirname "/" f))))
                                               ad-return-value)))))

      (defun find-recursive-remove-if (func list)
        "Removes all elements satisfying FUNC from LIST."
        (let ((result nil))
          (while list
            (if (not (funcall func (car list)))
                (setq result (cons (car list) result)))
            (setq list (cdr list)))
          (nreverse result))))





    ;; load-path enhancement
    (defun fni/find-org-files-recursively (this-directory)
      "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
      (when this-directory
        (when (file-directory-p this-directory)
          (let* ((this-directory (expand-file-name this-directory))
                 (files (directory-files this-directory t "^[^\\.]")))

            ;; completely canonicalize the directory name (*may not* begin with `~')
            (while (not (string= this-directory (expand-file-name this-directory)))
              (setq this-directory (expand-file-name this-directory)))

            (message "Searching for Org files in `%s'..." this-directory)
            (add-to-list 'load-path this-directory)

            (while files
              (setq dir-or-file (car files))
              (when (file-directory-p dir-or-file)
                  (fni/find-org-files-recursively dir-or-file))
              (setq files (cdr files)))))))



    ;; always start the overview on the current day!
    (setq org-agenda-start-on-weekday nil)

    ;; 10.1 set which files to search for TODO entries and scheduled items
    ;; (avoiding hidden files)
    (setq org-agenda-files
          (append (directory-files org-directory t "^[^\\.].*\\.org$")
                  (if (file-exists-p "~/Projects/")
                      (directory-files "~/Projects/" t "^[^\\.].*\\.org$")
                    nil)
                  (if (file-exists-p "~/Public/")
                      (directory-files "~/Public/" t "^[^\\.].*\\.txt$")
                    nil)
                  ))                    ; be careful that no
                                        ; `custom-set-variables' (at the end
                                        ; of your `.emacs') overrides this!

    ;; 10.3.1 include entries from the Emacs diary into Org mode's agenda
    (setq org-agenda-include-diary t)

    ;; DEPRECATED
    ;; ;; 10.3.1 weekly/daily agenda will always contain all TODO entries
    ;; (setq org-agenda-include-all-todo t)

    ;; 10.3.2 don't show scheduled entries in the global todo list
    (setq org-agenda-todo-ignore-scheduled t)

    ;; 10.3.2 don't show scheduled entries in the global todo list (until they
    ;; are within the warning period)
    (setq org-agenda-todo-ignore-deadlines t)

    ;; don't show entries with a date in the global todo list
    (setq org-agenda-todo-ignore-with-date t)

    ;; 10.3.2 don't check the sublevels of a TODO entry for TODO entries,
    ;; resulting in potentially much shorter TODO lists
    (setq org-agenda-todo-list-sublevels nil)

    ;; 10.4.2 settings for time grid for agenda display
    (setq org-agenda-time-grid '((daily require-timed)
                                 "________"
                                 (0800 1000 1200 1400 1600 1800 2000 2200)))

    ;; 10.4.3 sorting structure for the agenda items of a single day
    ;; (setq org-agenda-sorting-strategy   ; original value
    ;;       '((agenda habit-down time-up priority-down category-keep)
    ;;         (todo priority-down category-keep)
    ;;         (tags priority-down category-keep)
    ;;         (search category-keep)))

    (setq org-agenda-sorting-strategy   ; custom value
          '((agenda time-up category-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))

    ;; 10.5 number of days to include in overview display
    (setq org-agenda-ndays 7)  ; 1 or 7





;;    (setq org-sort-agenda-notime-is-late nil)

    (setq org-agenda-restore-windows-after-quit t)

    (setq org-agenda-window-frame-fractions '(1.0 . 1.0))



    ;; 10.5 Commands in the agenda buffer
    (defun my/weekday-p ()
      "Check if ..."
      (let ((wday (nth 6 (decode-time))))
        (and (< wday 6) (> wday 0))))

    (defun my/working-p ()
      "Check if ..."
      (let ((hour (nth 2 (decode-time))))
        (and (my/weekday-p) (or (and (>= hour 8) (<= hour 11))
                                (and (>= hour 13) (<= hour 17))))))

    (defun my/online-p ()
      "Check if the Internet is available."
      (= 0 (call-process "/bin/ping" nil nil nil
                         "-c1" "-q" "-t1" "www.gnu.org")))


    ;; I've submitted a feature today which provide contextual auto-exclusion
    ;; for tags in the Agenda view. For example, I use the following tags for
    ;; TODOs:
    ;;
    ;; Net      Needs internet access
    ;; Call     Needs a phone
    ;; Errand   Done in town
    ;; Home     Done at home
    ;;
    ;; Now, it's quite easy for my computer to figure out which of these are
    ;; possible, based on my location:
    ;;
    ;; Net      Can I ping mail.gnu.org?
    ;; Call     Am I outside of normal calling hours?
    ;; Errand   Am I outside of business hours?
    ;; Home     Does my IP address begin with 192.168.9?
    ;;
    ;; With the patch I've submitted, I can now define this function to auto-
    ;; exclude based on this type of context information:
    (defun org-my-auto-exclude-function (tag)
      (and (cond
            ((string= tag "online")
             (/= 0 (call-process "/sbin/ping" nil nil nil
                                 "-c1" "-q" "-t1" "mail.gnu.org")))
            ((string= tag "home")
             (with-temp-buffer
               (call-process "/sbin/ifconfig" nil t nil "en0" "inet")
               (goto-char (point-min))
               (not (re-search-forward "inet 192\\.168\\.9\\." nil t))))
            ((or (string= tag "errands")
                 (string= tag "phone"))
             (let ((hour (nth 2 (decode-time))))
               (or (< hour 8) (> hour 21)))))
           (concat "-" tag)))
    ;;
    ;; All I have to do is type `/ RET' in the agenda view now, and it
    ;; excludes based on my machine's current temporal and physical context.



    ;;! ensure that `:refile:' tags never will be excluded!
    (defun my/org-auto-exclude-function (tag)
      (and (cond
            ((string= tag "home")
             (my/working-p))
            ((string= tag "work")
             (not (my/working-p)))
            ((or (string= tag "errands") (string= tag "phone"))
             (let ((hour (nth 2 (decode-time))))
               (or (< hour 8) (> hour 21)))))
           (concat "-" tag)))

    (setq org-agenda-auto-exclude-function 'my/org-auto-exclude-function)

    ;; faces for specific Priorities (#A, #B and #C)
    (setq org-priority-faces
          '((?A . (:foreground "red" :background "white" :weight bold :underline t))
            (?B . (:foreground "#00BB00" :background "white"))
            (?C . (:foreground "blue" :background "white" :italic t))
            ))

    ;; 10.6.3 custom commands for the agenda
    ;; Or use your agenda and use `/' to limit the view to what you want
    ;; (`C-c a a / TAG').

    ;; See http://tiddlywiki.org/wiki/MGSD/Users_Guide
    ;; and http://tiddlywiki.org/wiki/Getting_started_with_MonkeyGTD_2.1_alpha

    ;; Review from mGSD
    ;; - Projects Dashboard
    ;; - Projects Dashboard by Area
    ;; - Action Dashboard by Project
    ;; - Active Projects With No Next Action
    ;; - Someday Projects With No Tickler
    ;; - Completed Projects
    ;; - Done Actions
    ;; - SomedayMaybe and Future
    ;; - Delegated Tasks Dashboard
    ;; - Cleanup
    ;; - Mismatched Realms
    ;; - Subprojects

    ;; (setq org-agenda-custom-commands
    ;;       '(("f" "Agenda without Items tagged xyz"
    ;;          ((agenda ""))
    ;;          ((org-agenda-show-log nil)
    ;;           (org-agenda-ndays 1)
    ;;           (org-agenda-log-mode-items '(state))
    ;;           (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":XYZ:"))))
    ;;         ;; other commands here
    ;;         ("d" "Agenda only Items tagged xyz"
    ;;          ((agenda ""))
    ;;          ((org-agenda-show-log nil)
    ;;           (org-agenda-ndays 1)
    ;;           (org-agenda-log-mode-items '(state))
    ;;           (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":XYZ:"))))
    ;;         ;; other commands here
    ;;         ("w" todo "WAIT")
    ;;         ("g" todo "STARTED")
    ;;         ))

    (setq org-agenda-custom-commands
          '(
            ("W" "Work agenda"
             ((tags-todo "work")
              (tags-todo "office/!+TODO")
;;;                          ((org-agenda-overriding-header "Office\n-------"))
              (tags-todo "work+phone")
              (tags-todo "work+computer")
              (tags-todo "work+online")))
            ("H" "Home agenda"
             (
              (tags "home")
;;;           (tags-todo "home")
;;;           (tags-todo "phone")
;;;           (tags-todo "computer")
;;;           (tags-todo "online")
             ))

;;;             ("D" "Daily Action List"
;;;              ((agenda ""
;;;                       ((org-agenda-ndays 1)
;;;                        (org-agenda-sorting-strategy
;;;                         '((agenda time-up priority-down tag-up)))
;;;                        (org-deadline-warning-days 0)))))
            ("d" "Daily Agenda"
             ((agenda ""
                      ((org-agenda-todo-keyword-format "")
                       (org-agenda-remove-tags t)))
              (tags "LEVEL=2+goals"
                    ((org-agenda-remove-tags t)
                     (org-agenda-prefix-format "  ")
                     (org-agenda-todo-keyword-format "")))
              (todo "NEXT"
                    ((org-agenda-sorting-strategy '(tag-up))
                     (org-agenda-show-inherited-tags nil)
                     (org-agenda-todo-keyword-format "")))
              (todo "PENDING"
                    ((org-agenda-todo-keyword-format "")))
              (stuck ""
                     ((org-agenda-remove-tags t)))))

            ))

    (setq org-agenda-custom-commands
          '(
            ("p" "Printed agenda"
             ((agenda ""
                      ((org-agenda-ndays 7)
                       (org-agenda-start-on-weekday nil)
                       (org-agenda-time-grid nil)
                       (org-agenda-repeating-timestamp-show-all t)
                       (org-agenda-prefix-format "  -->  %t %s")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy '(time-up tag-up))
                       (org-agenda-todo-keyword-format "[ ]")
                       (org-agenda-scheduled-leaders '("" ""))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                       (org-agenda-prefix-format "%t %T %s")
                       ))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-deadline-warning-days 7)
                       (org-agenda-time-grid nil)
                       (org-agenda-include-diary nil)
                       (org-agenda-todo-keyword-format "[ ]")
                       (org-agenda-scheduled-leaders '("" ""))
                       (org-agenda-overriding-header "Deadlines:")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                       (org-agenda-prefix-format "%t %s")))
              (todo "TODO|STARTED|NOW"
                    ((org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-ignore-with-date t)))
              (todo "WAIT"))
             ((org-agenda-with-colors nil)
              (org-agenda-prefix-format "%T [ ]")
              (org-agenda-todo-keyword-format "")
              (org-agenda-include-all-todo nil)
              (org-agenda-block-separator "---------------\n")
              (org-agenda-remove-tags t)
              (ps-number-of-columns 2)
              (ps-print-header nil)
              (ps-landscape-mode t))
             ("~/storage/agenda/agenda.pdf"))
            ("n" "Now"
             ((todo "NOW|STARTED")
              ((org-agenda-todo-ignore-with-date nil))))
            ("h" "Habits"
             ((agenda ""))
             ((org-agenda-show-log t)
              (org-agenda-include-diary nil)
              (org-agenda-include-all-todo nil)
              (org-agenda-ndays 1)
              (org-agenda-start-on-weekday nil) 
              (org-agenda-log-mode-items '(state))
              (org-agenda-time-grid nil)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
            ;; Today - daily tasks view
            ("d" "Today"
             ((agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-include-all-todo nil)))
              (alltodo "" ((org-agenda-sorting-strategy '(todo-state-up))
                           (org-agenda-todo-ignore-with-date t)))))
            ("y" "Projects"
             ((agenda ""
                      ((org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'notregexp "* PROJECT")))
                      ((org-agenda-include-diary nil)
                       (org-agenda-include-all-todo nil)
                       (org-agenda-time-grid nil)))
              (todo "PROJECT" ((org-agenda-todo-ignore-deadlines-t)
                               (org-agenda-sorting-strategy '(priority-down))))))
            ))


    ;; (key desc type match settings files)
    (setq org-agenda-custom-commands
          '(

            ("R" "Review"
             ((stuck "")
              (agenda ""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))
              (todo "PROJECT"
                    ((org-agenda-sorting-strategy '(todo-state-down priority-down))))
              (todo "NOW|STARTED|TODO"
                    ((org-agenda-sorting-strategy '(todo-state-down priority-down))))
              (todo "WAIT")
              (todo "MAYBE"))
             ((org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-time-grid nil)
              (org-agenda-include-all-todo nil)
              (org-deadline-warning-days 360)))


;;; Calendar style views

            ("d" agenda "X-Agenda 0 days deadline preview"
             ((org-deadline-warning-days 0)))
            ("D" agenda "X-Agenda 1 days deadline preview"
             ((org-deadline-warning-days 1)))
            ("2" agenda "X-Agenda 2 days deadline preview"
             ((org-deadline-warning-days 2)))
            ("3" agenda "X-Agenda 3 days deadline preview"
             ((org-deadline-warning-days 3)))
            ("7" agenda "X-Agenda 7 days deadline preview"
             ((org-deadline-warning-days 7)))

            ;; from Matt Lundin
            ("C" "Daily appointments"
             agenda ""
             ((org-agenda-ndays 1)
              (org-agenda-time-grid nil)
              (org-agenda-prefix-format " %-12:t ")
              (org-agenda-include-all-todo nil)
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("c" "Schedule" agenda ""
             ((org-agenda-ndays 7)
              (org-agenda-start-on-weekday 1)
              (org-agenda-time-grid nil)
              (org-agenda-prefix-format " %12:t ")
              (org-agenda-include-all-todo nil)
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("r" "Weekly appointments"
             agenda ""
             (
              ;; agenda will start in week view
              (org-agenda-ndays 7)

              ;; ensures that repeating events appear on all relevant dates
              (org-agenda-repeating-timestamp-show-all t)

              ;; limits agenda view to timestamped items
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("E" "Week's errands"
             tags "errands&SCHEDULED<=\"<+1w>\"&TODO<>\"DONE\"&TODO<>\"CANCELED\"" nil)

            ;; Past due
            ("H" "Due in the next 3 days"
             agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 3)))

            ;; Toodledo
            ;; Hotlist: The hotlist contains tasks that are due soon as well
            ;; as tasks that have a high priority. This is a convenient way to
            ;; see your most important tasks at a glance.

            ;; Hotlist Settings: At least a priority of '3 Top' or a due date
            ;; in the next 14 days.


            ;; Carsten
            ;; FIXME We don't see "timed" DEADLINE
            ("D" "Due today"
             agenda ""
             ((org-agenda-ndays 1)
              (org-deadline-warning-days 0)
              (org-agenda-skip-scheduled-if-deadline-is-shown t)
              (org-agenda-skip-function
               (lambda ()
                 (let* ((dl (org-entry-get nil "DEADLINE")))
                   (if (or (not dl)
                           (equal dl "")
                           (org-time> dl (org-time-today)))
                       (progn (outline-next-heading) (point))))))))

            ("F" "Upcoming deadlines (6 months)"
             ;; FIXME We don't see DEADLINE with `-1m' (or so) specifications
             ;; (if they are more than 1 m ahead of now)!
             agenda ""
             ((org-agenda-ndays 1)
              (org-deadline-warning-days 180)
              (org-agenda-include-all-todo nil)
              (org-agenda-time-grid nil)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
            ;; Some SCHEDULED are shown

;;; Stuck

            ("u" "Someday"
             alltodo ""
             ((org-agenda-skip-function
               (lambda nil
                 (org-agenda-skip-entry-if 'scheduled 'deadline
                                           'regexp "<[^>\n]+>")))
              (org-agenda-overriding-header "Unscheduled TODO entries: ")))


;;; Priorities

            ;; priority levels
            ("p" . "Priorities")

            ("pa" "A items"
             tags-todo "+PRIORITY=\"A\"")

            ("pb" "B items"
             tags-todo "+PRIORITY=\"B\"")

            ("pc" "C items"
             tags-todo "+PRIORITY=\"C\"")

            ;; list only priority A tasks for the current day
            ("pA" "Agenda of priority A tasks for current day"
             agenda ""
             ((org-agenda-skip-function
               (lambda nil
                 (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
              (org-agenda-ndays 1)
              (org-agenda-overriding-header "Today's Priority #A tasks: ")))

            ;; list only priority A and B tasks for the current day
            ("pB"
             agenda ""
             ((org-agenda-ndays 1)
              (org-agenda-overriding-header "Today's Priority #A and #B tasks: ")
              (org-agenda-skip-function
               (quote (org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]")))))


;;; GTD contexts

            ("g" . "Context lists")

            ("gw" "Work"
             tags-todo "work")

            ("go" "Online"
             tags-todo "online")

            ("gc" "Computer"
             tags-todo "computer")

            ("gp" "Phone"
             tags-todo "phone")

            ("gh" "Home"
             tags-todo "home")

            ("gr" "Reading"
             tags-todo "reading")

            ("ge" "Errands"
             tags-todo "errands")

            ("G" "Group actions by Context"
             ;; contains unscheduled things...!
             ((tags-todo "work")
              (tags-todo "online")
              (tags-todo "computer")
              (tags-todo "phone")
              (tags-todo "home")
              (tags-todo "reading")
              (tags-todo "errands")))

            ;; ("s" "Scorpios tasks"
            ;;  alltodo ""
            ;;  ((org-agenda-files
            ;;    '("~/Personal/Business/Real-Estate/Appartment-Scorpios-Boulouris/TODO-Documents.org"))))


;;; TODO keyword

            ;; list entries with a DELEGATED keyword, in all agenda files
            ("d"
             todo "DELEGATED" nil)

            ;; list entries which are at some kind of completion state (DONE,
            ;; DEFERRED or CANCELED), in all agenda files
            ("c"
             todo "DONE|DFRD|CANCELED" nil)

            ;; FIXME This does not show any (or all?) WAITING task!
            ("W" "Waiting"
             todo "WAIT" nil)


;;; Printed agenda

            ;; `c-x c-w' = write the agenda view to a file
            ("n" "Call list"
             tags-todo "phone"
             ((ps-number-of-columns 1)
              (ps-landscape-mode t)
              (org-agenda-prefix-format " %-20:c [ ] " )
              (Orgae-agenda-with-colors nil)
              (org-agenda-remove-tags t))
             ;; ("~/My Dropbox/calls.ps")
             ("/home/sva/calls.pdf"))


;;; Custom queries

            ("Q" . "Custom queries") ;; gives label to `Q'

            ("Qd" "Notes files"
             search ""
             ((org-agenda-files
               (file-expand-wildcards "~/Public/*.txt"))))

            ("Qw" "Website search"
             search ""
             ((org-agenda-files
               (file-expand-wildcards "~/Public/Websites/Org/source/*.org"))))

            ("Qa" "Artemis search"
             search ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))))

            ("Qx" "Artemis with deadline columns"
             alltodo ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))
              (org-agenda-overriding-columns-format "%40ITEM %DEADLINE")
              (org-agenda-view-columns-initially t)))

            ("QA" "Artemis tags search"
             org-tags-view ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))))

            ("n" . "SNCB+Name tags searches") ; description for `n' prefix

            ("nb"
             tags "+sncb+Be")

            ("no"
             tags "+sncb+Oz")

            ("nf"
             tags "+sncb+FNI")

            ("ni"
             tags "+sncb+IML")

            ("nl"
             tags "+sncb+LLG")

            ("nm"
             tags "+sncb+MVA")

            ("np"
             tags "+sncb+PRO")

            ("nr"
             tags "+sncb+RCO")

            ("nw"
             tags "+sncb+PWA")



            ;; Carsten
            ;; Is it listing only tasks *without* DEADLINE?
            ("x" "With deadline columns"
             alltodo ""
             ((org-agenda-overriding-columns-format "%40ITEM %DEADLINE")
              (org-agenda-view-columns-initially t)))



            ("N" "Notes" tags "note" nil)

            ))


    ;; ;; (setq org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: "))
    ;; (setq org-agenda-deadline-leaders '("Deadl.:  " "In %2dd: "))

    ;; ;; text preceeding scheduled items in the agenda view
    ;; (setq org-agenda-scheduled-leaders '("Sched.: " "S. %2dx: "))
    ;;                                     ; ("âŒš " "Sched.%2dx: "))

    ;; DUPLICATE Obey `eval' variables -- RISKY!
    (setq enable-local-eval t)

    ;; generate -- after all initialization --the agenda buffer for this week
    ;; (add-hook 'after-init-hook 'org-agenda-list)


    (org-agenda-list)
    (delete-other-windows)





    ;; I have the following snippet in my .emacs file, which I find very
    ;; useful. Basically what it does is that if I don't touch my Emacs for 15
    ;; minutes, it displays the current agenda. This keeps my tasks "always in
    ;; mind" whenever I come back to Emacs after doing something else, whereas
    ;; before I had a tendency to forget that it was there.
    (defun jump-to-org-agenda ()
      (interactive)
      (let ((buf (get-buffer "*Org Agenda*"))
            wind)
        (if buf
            (if (setq wind (get-buffer-window buf))
                (select-window wind)
              (if (called-interactively-p)
                  (progn
                    (select-window (display-buffer buf t t))
                    (org-fit-window-to-buffer)
                    ;; (org-agenda-redo)
                    )
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )))
          (call-interactively 'org-agenda-list)))
      ;;(let ((buf (get-buffer "*Calendar*")))
      ;;  (unless (get-buffer-window buf)
      ;;    (org-agenda-goto-calendar)))
      )

    (run-with-idle-timer 900 t 'jump-to-org-agenda)



    (defun my/highlight-line ()
      (hl-line-mode 1))

    (add-hook 'org-agenda-mode-hook 'my/highlight-line)



;;**** 11 (info "(org)Embedded LaTeX")

    ;; 11.4 convert LaTeX fragments to images when exporting to HTML
    (setq org-export-with-LaTeX-fragments t)


;;**** 12 (info "(org)Exporting")

    ;; special syntax for emphasized text
    (setq org-emphasis-alist '(("*" bold "<b>" "</b>")
                               ("/" italic "<i>" "</i>")
                               ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                               ("=" org-code "<code>" "</code>" verbatim)
                               ("~" org-verbatim "<code>" "</code>" verbatim)
                               ("+" (:strike-through t) "<del>" "</del>")
                               ("@" org-warning "<b>" "</b>")))

    ;; alist of LaTeX expressions to convert emphasis fontifiers
    (setq org-export-latex-emphasis-alist '(("*" "\\textbf{%s}" nil)
                                            ("/" "\\emph{%s}" nil)
                                            ("_" "\\underline{%s}" nil)
                                            ("+" "\\st{%s}" nil)
                                            ("=" "\\verb=%s=" nil)
                                            ("~" "\\verb~%s~" t)
                                            ("@" "\\alert{%s}" nil)))



    ;; ASCII export for Org mode
    (try-require 'org-ascii)

    ;; ;; 12.1 add the unbreakable space as allowed character after an emphasis
    ;; ;; string, and modify the maximum number of newlines allowed in an
    ;; ;; emphasis
    ;; (setq org-emphasis-regexp-components [...])

    ;; 12.7.3 LaTeX exporter for Org mode
    (when (try-require 'org-latex)

      ;; update the list of LaTeX classes and associated header (encoding,
      ;; etc.) and structure
      (add-to-list 'org-export-latex-classes
                   '("mcarticle"
                     "\\documentclass{mcarticle}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mccommercial"
                     "\\documentclass{mccommercial}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mcreport"
                     "\\documentclass{mcreport}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mcbook"
                     "\\documentclass{mcbook}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("alta"
                     "\\documentclass{alta}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mccontract"
                     "\\documentclass{mccontract}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\mccarticle{%s}" . "\\mccarticle*{%s}")
                     ("\\mccparagraph{%s}" . "\\mccparagraph*{%s}")))

      ;; XXX 2010-03-25 TEMP Fix for conflict TikZ/hyperref: loading atbegshi
      ;; before document class beamer
      (add-to-list 'org-export-latex-classes
                   '("beamer"
                     "\\RequirePackage{atbegshi}\n\\documentclass{beamer}\n"
                     org-beamer-sectioning))

      (setq org-beamer-frame-default-options "")

      ;; ;; "Org2Beamer" (TODO lookat Goettingen theme)
      ;; \\usetheme{Goettingen}
      ;; \\useoutertheme{infolines}
      ;; \\setbeameroption{show notes}
      ;;
      ;; \\mode<{{{beamermode}}}>
      ;; {
      ;; \\usetheme{{{{beamertheme}}}}
      ;; }
      ;; \\institute{{{{beamerinstitute}}}}
      ;; \\subject{{{{beamersubject}}}}"


      ;; 2010-03-31 From Carsten
      ;; =org-export-latex-classes= no longer should be customized for
      ;; packages
      ;;
      ;; The HEADER part of this variable should now only contain the
      ;; documentclass macro, nothing else - at least normally.  All the
      ;; package calls via usepackage should go into
      ;; org-export-latex-packages-alist.  I moved all the default packages
      ;; that into a new variable org-export-latex-default-packages-alist.
      ;; This will allow me to add more packages (as needed) in the
      ;; future, withour requiring you to erase and then redo your
      ;; configuration of org-export-latex-classes.
      ;;
      ;; So if you have customized this variable, please remove once more
      ;; (hopefully for the last time) your customization, so that it can
      ;; revert to its now much simpler default value.  Put all your
      ;; package definitions into org-export-latex-packages-alist.
      ;; I hope this works, and we will not get conflicts because of the
      ;; sequence in which packages are called.  If there are problems,
      ;; please let me know so that we can find a solution.

      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1" "fontenc" t)
              ("" "fixltx2e" nil)
              ("" "graphicx" t)
              ("" "longtable" nil)
              ("" "float" nil)
              ("" "wrapfig" nil)
              ("" "soul" t)
              ("" "t1enc" t)
              ("" "textcomp" t)
              ("" "marvosym" t)
              ("" "wasysym" t)
              ("" "latexsym" t)
              ("" "amssymb" t)
              ("" "hyperref" nil)
              "\\tolerance=1000"))

      ;; tell org to use listings
      (setq org-export-latex-listings t)

      ;; you must include the `listings' package
      (add-to-list 'org-export-latex-packages-alist '("" "listings"))

      ;; if you want colored source code, then you need to include the
      ;; `xcolor' package
      (add-to-list 'org-export-latex-packages-alist '("" "xcolor"))

      (setq org-export-latex-packages-alist
            '(("" "xcolor")
              ("" "listings")))


      ;; default class
      ;; TODO Put this in a personal settings file
      (setq org-export-latex-default-class "article")


      (setq org-export-copy-to-kill-ring nil)

      )


;;**** 13 (info "(org)Publishing")

    ;; publish related Org mode files as a website
    (try-require 'org-publish)

    ;; 13.1.1 association list to control publishing behavior
    (setq org-publish-project-alist
          '(("project-mygooglest.com-fni"

             :base-directory "~/Public/www.mygooglest.com/source/fni/"
             :base-extension "org"
             :publishing-directory "~/Public/www.mygooglest.com/public_html/fni/"

             :publishing-function org-publish-org-to-html

             :section-numbers nil
             :table-of-contents t
             :style-include-default nil
             :style "
<link rel=\"shortcut icon\" href=\"pic/favicon.ico\"/>
<link rel=\"stylesheet\" href=\"css/common.css\" type=\"text/css\"/>
<!--[if IE]>
    <link rel=\"stylesheet\" href=\"css/common-ie.css\" type=\"text/css\" title=\"IE specific stylesheet\"/>
<![endif]-->"
             :preamble "
<!-- Preamble of Page published by Emacs Org mode begins here -->
    <div id=\"navigation\">
        <h2>Navigation</h2>
        <ul>
            <li><a href=\"index.html\" title=\"Home\" id=\"current-home\">Home</a></li>
            <li>About Me
                <ul>
                    <li><a href=\"curriculum-vitae.html\" title=\"CV\" id=\"current-cv\">CV</a></li>
                    <li><a href=\"pgp-public-key.html\" title=\"PGP Public Key\" id=\"current-pgp-public-key\">PGP Public Key</a></li>
                    <li><a href=\"contact-me.html\" title=\"Contact Me\" id=\"current-contact-me\">Contact Me</a></li>
                </ul>
            </li>
            <li>Resources
                <ul>
                    <li><a href=\"ubuntu.html\" title=\"Ubuntu\" id=\"current-ubuntu\">Ubuntu</a></li>
                    <li><a href=\"dot-emacs.html\" title=\"Emacs\" id=\"current-emacs\">Emacs</a></li>
                    <li><a href=\"emacs-lisp-programming.html\" title=\"Emacs Lisp\" id=\"current-emacs-lisp\">Emacs Lisp</a></li>
                    <li><a href=\"shell-scripting.html\" title=\"Shell Scripting\" id=\"current-shell-scripting\">Shell Scripting</a></li>
                    <li><a href=\"freeware.html\" title=\"Freeware\" id=\"current-freeware\">Freeware</a></li>
                    <li><a href=\"latex.html\" title=\"LaTeX\" id=\"current-latex\">LaTeX</a></li>
                    <li><a href=\"virtualbox.html\" title=\"VirtualBox\" id=\"current-virtualbox\">VirtualBox</a></li>
                    <li><a href=\"google.html\" title=\"Google\" id=\"current-google\">Google</a></li>
                    <li><a href=\"ledger.html\" title=\"Ledger\" id=\"current-ledger\">Ledger</a></li>
                    <li><a href=\"stumpwm.html\" title=\"StumpWM\" id=\"current-stumpwm\">StumpWM</a></li>
                    <li><a href=\"networking.html\" title=\"Networking\" id=\"current-networking\">Networking</a></li>
                    <li><a href=\"electronics.html\" title=\"Electronics\" id=\"current-electronics\">Electronics</a></li>
                </ul>
            </li>
            <li>About this Site
                <ul>
                    <li><a href=\"sitemap.html\" title=\"Site Map\" id=\"current-site-map\">Site Map</a></li>
                </ul>
            </li>
        </ul>
    </div>

    <div id=\"org-content\">
<!-- Preamble of Page published by Emacs Org mode ends here -->
"
             :postamble "
<!-- Postamble of Page published by Emacs Org mode begins here -->
    </div>

    <div id=\"footer\">
        <p>
            Copyright (C) 2007 - 2010 Fabrice Niessen<br/>
            Please <a href=\"contact-me.html\">contact me</a> for corrections, additions and suggestions.<br/>
            Made with <a href=\"http://www.orgmode.org/\">Org Mode</a>.<br/>
            <a href=\"http://validator.w3.org/check?uri=referer\"
            title=\"Check the validity of this site's XHTML\"
            style=\"text-decoration: none;\">
            <img src=\"http://www.w3.org/Icons/valid-xhtml10-blue\"
            alt=\"Valid XHTML 1.0!\" height=\"31\" width=\"88\"/>
            </a> &nbsp; 
            <a href=\"http://jigsaw.w3.org/css-validator/check/referer\"
            title=\"Check the validity of this site's CSS\"
            style=\"text-decoration: none;\">
            <img src=\"http://www.w3.org/Icons/valid-css-blue.png\"
            alt=\"Valid CSS!\" height=\"31\" width=\"88\"/>
            </a> &nbsp;
        </p>
    </div>
<script type=\"text/javascript\">
    var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
    document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
    var pageTracker = _gat._getTracker(\"UA-192135-1\");
    pageTracker._initData();
    pageTracker._trackPageview();
</script>
<!-- Postamble of Page published by Emacs Org mode ends here -->
"
             :auto-preamble t  ; FIXME How to get TITLE before navigation menu?
             :auto-postamble nil

             :auto-sitemap t                  ; Generate index.org automagically...
             :sitemap-filename "sitemap.org"  ; ... call it sitemap.org ...
             :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
             )))

    ;; fontify what is treated specially by the exporters
    (setq org-highlight-latex-fragments-and-specials t)

    ;; 13.1.5 default option for images
    (setq org-export-latex-image-default-option "width=0.9\\linewidth")

    (setq org-export-latex-inputenc-alist '(("utf8" . "utf8x")))

    ;; 13.1.5 don't include the javascript snippets in exported HTML files
    (setq org-export-html-style-include-scripts nil)

    ;; XML declaration for exported HTML files
    (setq org-export-html-xml-declaration
          '(("html" . "")
            ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
            ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

    ;; ;; 13.1.5 export all drawers
    ;; (setq org-export-with-drawers t)

    ;; 13.2 always publish all files (do not use timestamp checking for
    ;; skipping unmodified files)
    (setq org-publish-use-timestamps-flag nil)


    ;; indentation for the content of a source code block
    (setq org-edit-src-content-indentation 4)

    ;; switch from org-exp-blocks to Org-babel!
    ;; ;; pre-process blocks when exporting org files (ditaa, dot, comment, R,
    ;; ;; etc.)
    ;; (try-require 'org-exp-blocks)

    ;;;; FIXME This is responsible of problems when used with Org-babel
    ;;;; (`\LaTeX{}' environments are created around verbatim environments)
    ;; turn Org blocks into LaTeX environments and HTML divs
    ;; (markup in environments in LaTeX export, or giving LaTeX attributes to
    ;; sections in export)
    (try-require 'org-special-blocks)


;;**** 14 Working with source code

    ;; literate programming
    ;; (when (try-require 'org-babel-init)  ; 2010-06-24 now loaded by default
    ;;                                      ; with Org

      ;; control the insertion of comments into tangled code
      (setq org-babel-tangle-w-comments t)

      ;; activate a subset of languages
      (try-require 'ob-sh)
      (try-require 'ob-ditaa)           ; TODO Install this for Ditaa
                                        ; sudo aptitude install openjdk-6-jre
      (try-require 'ob-dot)
      (try-require 'ob-emacs-lisp)
      (try-require 'ob-gnuplot)         ; requires gnuplot-mode
      (try-require 'ob-latex)
      (try-require 'ob-perl)
      (try-require 'ob-python)          ; requires python, and python-mode
      (try-require 'ob-R)               ; requires R and ess-mode
      (try-require 'ob-ruby)            ; requires ruby, irb, ruby-mode, and
                                        ; inf-ruby
      (try-require 'ob-sql)


;;      (require 'ob-identifier) ; ????????

      ;; support for interactive terminals. Mostly shell scripts. Heavily
      ;; inspired by 'eev'
      (try-require 'ob-screen)
                                        ; Eric Schulte believes screen has
                                        ; more of a focus on sustained
                                        ; interaction with an interactive
                                        ; terminal, although to be honest I
                                        ; haven't really used it and

      ;; load the source-code blocks defined in an Org mode file into the
      ;; global `org-babel-library-of-babel' variable
      (when (fboundp 'org-babel-lob-ingest)
        (org-babel-lob-ingest
         "~/Downloads/emacs/site-lisp/org-mode/contrib/babel/library-of-babel.org"))

      ;; I don't want to be prompted on every code block evaluation
      (setq org-confirm-babel-evaluate nil)

      ;; I don't want to execute code blocks with `C-c C-c' (evaluate code
      ;; block with `C-c C-v e')
      (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

      ;; mapping between languages (listings in LaTeX) and their major mode
      ;; (in Emacs)
      (setq org-src-lang-modes
            '(("ocaml" . tuareg)
              ("elisp" . emacs-lisp)
              ;; ("Delphi" . perl)
              ("ditaa" . artist)
              ("asymptote" . asy)
              ("dot" . fundamental)))
      ;; )


;;**** 14 (info "(org)Miscellaneous")

    ;; 14.2 speed keys
    ;; activate single letter commands (for example outline navigation with
    ;; `f', `b', `n', and `p') at beginning of a headline:
    ;; - f :: org-forward-same-level
    ;; - b :: org-backward-same-level
    ;; - n :: outline-next-visible-heading
    ;; - p :: outline-previous-visible-heading
    (setq org-use-speed-commands t)

    ;; show next/prev heading tidily (from Dan Davison)
    (defun my/org-show-next-heading-tidily ()
      "Show next entry, keeping other entries closed."
      (if (save-excursion (end-of-line) (outline-invisible-p))
          (progn (org-show-entry) (show-children))
        (outline-next-heading)
        (unless (and (bolp) (org-on-heading-p))
          (org-up-heading-safe)
          (hide-subtree)
          (error "Boundary reached"))
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)))

    (defun my/org-show-previous-heading-tidily ()
      "Show previous entry, keeping other entries closed."
      (let ((pos (point)))
        (outline-previous-heading)
        (unless (and (< (point) pos) (bolp) (org-on-heading-p))
          (goto-char pos)
          (hide-subtree)
          (error "Boundary reached"))
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)))

    (when (fboundp 'org-speed-commands-user)
      (add-to-list 'org-speed-commands-user
                   '("n" my/org-show-next-heading-tidily))
      (add-to-list 'org-speed-commands-user
                   '("p" my/org-show-previous-heading-tidily)))





    (if window-system (try-require 'org-mouse))



    ;; keep my encrypted data (like account passwords) in my org-mode files
    ;; with a special tag instead
    (when (try-require 'org-crypt)

        ;; encrypt all entries before saving
        (org-crypt-use-before-save-magic)

        ;; To later decrypt an entry that's encrypted, use `M-x
        ;; org-decrypt-entry' or `C-c C-r' (fits nicely with the meaning of
        ;; "reveal").

        ;; which tag is used to mark headings to be encrypted
        (setq org-tags-exclude-from-inheritance '("crypt"))

        ;; GPG key to use for encryption
        (setq org-crypt-key "7F376D89"))



    ;; 14.5 hide the first N-1 stars in a headline
    (setq org-hide-leading-stars t)

    ;; ;; 14.5 skip even levels and only use odd levels for the outline
    ;; (setq org-odd-levels-only t)
    ;;
    (when (try-require 'org-indent)
      (org-indent-mode))

;;    (setq org-startup-indented t)


    ;; 14.7.2 yasnippet (allow yasnippet to do it's thing in Org files)
    ;;! make sure you initialise yasnippet *before* Org mode
    ;; FIXME Make sure yasnippet is found and loaded before executing this
    (when (locate-library "yasnippet.el")
      (add-hook 'org-mode-hook
                (lambda ()
                  (defun yas/org-very-safe-expand ()
                    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

                  ;; yasnippet (using the new org-cycle hooks)
                  ;; (make-variable-buffer-local 'yas/trigger-key)
                  ;; (setq yas/trigger-key (kbd "TAB"))
                  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                  (define-key yas/keymap (kbd "TAB") 'yas/next-field))))


    (defun my/open-work-org ()
      "Open my Work Org file."
      (interactive)
      (find-file "~/Projects/Work.org"))
    (global-set-key (kbd "<S-f2>") 'my/open-work-org)

    (defun my/open-personal-org ()
      "Open my Personal Org file."
      (interactive)
      (find-file "~/Personal/Personal.org"))
    (global-set-key (kbd "<C-f2>") 'my/open-personal-org)



    ;; http://gopher.ruricolist.com/lisp/org-velocity.el
    (when (try-require 'org-velocity)
      (setq org-velocity-bucket (concat org-directory "bucket.org"))
      (global-set-key (kbd "C-c v") 'org-velocity-read))




;;;     Use Org mode together with org-annotate-file.el, this is mentioned at
;;;     http://orgmode.org/worg/org-contrib/index.php.

    )


;; How to create web page by Emacs and Muse?
;; http://www.xshi.org/notes/WebPage.html

;; load generic module
(GNUEmacs
  (when (try-require 'muse)

    ;;     (define-key muse-mode-map (kbd "C-c C-p") 'ywb-muse-publish-project)
    ;;     (define-key muse-mode-map (kbd "C-c C-c") 'ywb-muse-preview-source)
    ;;     (define-key muse-mode-map (kbd "C-c C-j") 'ywb-muse-preview-html)
    ;;     (define-key muse-mode-map (kbd "C-c C-m") 'ywb-muse-preview-with-w3m)

        (defun tidy-do ()
          "Run the HTML Tidy program on the current buffer."
          (interactive)
          (shell-command-on-region (point-min) (point-max)
            (concat "tidy"
                    " -config ~/.tidyrc"
                    " --error-file ./temp-tidy-errors") t)
          (delete-file "./temp-tidy-errors")
          (message "Buffer tidy'ed"))

;;;         ;; TODO Check that tidy is in PATH
;;;         ;; run in the buffer to be published
;;;         (eval-after-load "tidy"
;;;           '(progn (add-hook 'muse-after-publish-hook 'tidy-do)))


    ;; See `org-publish-escript.el' for htmlize of code


    ;; How to preview in a browser?
    ;;
    ;; Press `C-c C-v' (`browse-url-of-buffer'). You can also get a textual
    ;; preview by pressing `C-c TAB' (`sgml-tags-invisible'), which will hide
    ;; all the tags. Press `C-c TAB' again to show tags.


    ;;Auto Publish when muse saved
    ;;(eval-after-load "muse-mode"
    ;;    (add-hook 'after-save-hook
    ;;          (lambda ()
    ;;          (when (planner-derived-mode-p 'muse-mode)
    ;;                           (muse-project-publish nil)))
    ;;))
    ;;         nil t))

))

    ;; You can create and publish a blog with Org mode
    ;; http://dto.freeshell.org/e/org-blog.el

    ;; See `muse-project-copy-files' with `rsync'

    ;; ;; automatically update web copy
    ;; (write-region
    ;;  (point-min) (point-max)
    ;;  "/scorpios@ftp.scorpioscotedazur.com:/public_html/emacs/mydotemacs.el")

    ;; or `sitecopy'

(message "38 The Calendar and the Diary... Done"))


;; FIXME This variable has been reset by something in the previous section.
;; So, put back my value!
(setq ispell-dictionary-alist
      ;; TODO Add es_ES
      '(
        ;; default dictionary (see `ispell-dictionary')
        (nil
         "[a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]"
         "[^a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]"
         "[-']" nil
         ("-d" "fr_FR")  ; for hunspell
         nil utf-8)

        ;; standard French
        ("fr_FR"
         "[a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]"
         "[^a-zÃ Ã¢Ã¤Ã©Ã¨ÃªÃ«Ã®Ã¯Ã´Ã¶Ã¹Ã»Ã¼Ã§A-ZÃ€Ã‚Ã„Ã‰ÃˆÃŠÃ‹ÃŽÃÃ”Ã–Ã™Ã›ÃœÃ‡]"
         "[-']" nil
         ("-d" "fr_FR")  ; for hunspell
         nil utf-8)

        ;; American English
        ("en_US"
         "[A-Za-z]"
         "[^A-Za-z]"
         "[']" nil
         ("-d" "en_US")  ; for hunspell
         nil utf-8)
        ))

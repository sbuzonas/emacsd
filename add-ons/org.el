(defaddon org
  nil
  (require-package 'xml-rpc)
  (require-package 'org2blog)
  (require 'org)

  (after org

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
          '(("t" "Todo" entry (file (concat org-directory "/refile.org"))
             "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
            ("r" "Respond" entry (file (concat org-directory "/refile.org"))
             "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
            ("n" "Note" entry (file (concat org-directory "/refile.org"))
             "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
            ("j" "Journal" entry (file+datetree (concat org-directory "/diary.org"))
             "* %?\n%U\n" :clock-in t :clock-resume t)
            ("w" "org-protocol" entry (file (concat org-directory "/diary.org"))
             "* TODO Review %c\n%U\n" :immediate-finish t)
            ("m" "Meeting" entry (file (concat org-directory "/refile.org"))
             "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
            ("p" "Phone call" entry (file (concat org-directory "/refile.org"))
             "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
            ("h" "Habit" entry (file (concat org-directory "/refile.org"))
             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
            ("l" "Link" plain (file (concat org-directory "/links.org"))
             "- %?\n %x\n")))
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" .t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    (setq org-agenda-dim-blocked-tasks nil)

    (setq org-agenda-compact-blocks t)

    (setq org-agenda-custom-commands
          '(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            ("h" "Habits" tags-todo "STYLE=\"habit\""
             ((org-agenda-overriding-header "Habits")
              (org-agenda-sorting-strategy
               '(todo-state-down effort-up category-keep))))
            (" " "Agenda"
             ((agenda "" nil)
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "-CANCELLED/!"
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-skip-function 'slbmeh/skip-non-stuck-projects)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-HOLD-CANCELLED/!"
                         ((org-agenda-overriding-header "Projects")
                          (org-agenda-skip-function 'slbmeh/skip-non-projects)
                          (org-agenda-match-list-sublevels 'indented)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-CANCELLED/!NEXT"
                         ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                (if slbmeh/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'slbmeh/skip-projects-and-habits-and-single-tasks)
                          (org-tags-match-list-sublevels t)
                          (org-agenda-todo-ignore-scheduled slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-with-data slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-sorting-strategy
                           '(todo-state-down effort-up category-keep))))
              (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                         ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                (if slbmeh/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'slbmeh/skip-non-project-tasks)
                          (org-agenda-todo-ignore-scheduled slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-with-date slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                         ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                (if slbmeh/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'slbmeh/skip-project-tasks)
                          (org-agenda-ignore-scheduled slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-ignore-deadlines slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-ignore-with-date slbmeh/hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-CANCELLED+WAITING|HOLD/!"
                         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                          (org-agenda-skip-function 'slbmeh/skip-stuck-projects)
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-todo-ignore-scheduled t)
                          (org-agenda-todo-ignore-deadlines t)))
              (tags "-REFILE/"
                    ((org-agenda-overriding-header "Tasks to Archive")
                     (org-agenda-skip-function 'slbmeh/skip-non-archivable-tasks)
                     (org-tags-match-list-sublevels nil)))
              nil))))

    (setq org2blog/wp-blog-alist
          '(("stevebuzonas"
             :url "http://www.stevebuzonas.com/xmlrpc.php"
             :username "slbmeh")))

    (defun slbmeh/org-auto-exclude-function (tag)
      "Automatic task exclusion in the agenda with / RET"
      (and (cond
            ((string= tag "hold")
             t)
            ((string= tag "lisa")
             t)
            ((string= tag "home")
             t))
           (concat "-" tag)))

    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Shot lot of clocking history so it's easy to pick items off the list
    (setq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change tasks to NEXT when clocking in
    (setq org-clock-in-switch-to-state 'slbmeh/clock-in-to-next)
    ;; Separate drawers for clocking and logs
    (setq org-drawers '("PROPERTIES" "LOGBOOK"))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Remove clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)

    (setq slbmeh/keep-clock-running nil)

    (setq org-time-stamp-rounding-minutes '(1 1))

    (setq org-agenda-clock-consistency-checks
          '(:max-duration "4:00"
            :min-duration 0
            :max-gap 0
            :gap-ok-around ("4:00")))

    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

    (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

    (setq org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                  ("STYLE_ALL" . "habit")))

    (setq org-agenda-log-mode-items '(closed state))

    (setq org-tag-alist '((:startgroup)
                          ("@errand" . ?e)
                          ("@office" . ?o)
                          ("@home" . ?H)
                          ("@lisa" . ?l)
                          (:endgroup)
                          ("WAITING" . ?w)
                          ("HOLD" . ?h)
                          ("PERSONAL" . ?P)
                          ("WORK" . ?W)
                          ("LISA" . ?l)
                          ("ORG" . ?O)
                          ("FANCYGUY" . ?f)
                          ("crypt" . ?E)
                          ("NOTE" . ?n)
                          ("CANCELLED" . ?c)
                          ("FLAGGED" . ??)))

    (setq org-fast-tag-selection-single-key 'expert)
    (setq org-agenda-tags-todo-honor-ignore-options t)

    (setq org-agenda-span 'day)

    (setq org-stuck-projects '("" nil nil ""))

    (defun slbmeh/is-project-p ()
      "Any task with a todo keyword subtask"
      (save-restriction
        (widen)
        (let ((has-subtask)
              (subtree-end (save-excursion (oer-end-of-subtree t)))
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (forward-line 1)
            (while (and (not has-subtask)
                        (< (point) subtree-end)
                        (re-search-forward "^\*+ " subtree-end t))
              (when (member (org-get-todo-state) org-todo-keywords-1)
                (setq has-subtask t))))
          (and is-a-task has-subtask))))

    (defun slbmeh/is-project-subtree-p ()
      "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
      (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                  (point))))
        (save-excursion
          (slbmeh/find-project-task)
          (if (equal (point) task)
              nil
            t))))

    (defun slbmeh/is-task-p ()
      "Any task with a todo keyword and no subtask"
      (save-restriction
        (widen)
        (let ((has-subtask)
              (subtree-end (save-excursion (org-end-of-subtree t)))
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (forward-line 1)
            (while (and (not has-subtask)
                        (< (point) subtree-end)
                        (re-search-forward "^\*+ " subtree-end t))
              (when (member (org-get-todo-state) org-todo-keywords-1)
                (setq has-subtask t))))
          (and is-a-task (not has-subtask)))))

    (defun slbmeh/is-subproject-p ()
      "Any task which is subtask of another project"
      (let ((is-subproject)
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (while (and (not is-subproject) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq is-subproject t))))
        (and is-a-task is-subproject)))

    (defun slbmeh/list-sublevels-for-projects-indented ()
      "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
      (if (marker-buffer org-agenda-restrict-begin)
          (setq org-tags-match-list-sublevels 'indented)
        (setq org-tags-match-list-sublevels nil))
      nil)

    (defun slbmeh/list-sublevels-for-projects ()
      "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
      (if (marker-buffer org-agenda-restrict-begin)
          (setq org-tags-match-list-sublevels nil))
      nil)

    (defvar slbmeh/hide-scheduled-and-waiting-next-tasks t)

    (defun slbmeh/toggle-next-task-display ()
      (interactive)
      (setq slbmeh/hide-scheduled-and-waiting-next-tasks (not slbmeh/hide-scheduled-and-waiting-next-tasks))
      (when (equal major-mode 'org-agenda-mode)
        (org-agenda-redo))
      (message "%s WAITING and SCHEDULED NEXT Tasks" (if slbmeh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

    (defun slbmeh/skip-stuck-projects ()
      "Skip trees that are not stuck projects"
      (save-restriction
        (widen)
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (if (slbmeh/is-project-p)
              (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                     (has-next ))
                (save-excursion
                  (forward-line 1)
                  (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                    (unless (member "WAITING" (org-get-tags-at))
                      (setq has-next t))))
                (if has-next
                    nil
                  next-headline)) ; a stuck project, has subtasks but no next task
            nil))))

    (defun slbmeh/skip-non-stuck-projects ()
      "Skip trees that are not stuck projects"
      ;; (slbmeh/list-sublevels-for-projects-indented)
      (save-restriction
        (widen)
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (if (slbmeh/is-project-p)
              (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                     (has-next ))
                (save-excursion
                  (forward-line 1)
                  (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                    (unless (member "WAITING" (org-get-tags-at))
                      (setq has-next t))))
                (if has-next
                    next-headline
                  nil)) ; a stuck project, has subtasks but no next task
            next-headling))))

    (defun slbmeh/skip-non-projects ()
      "Skip trees that are not projects"
      ;; (slbmeh/list-sublevels-for-projects-indented)
      (if (save-excursion (slbmeh/skip-non-stuck-projects))
          (save-restriction
            (widen)
            (let ((subtree-end (save-excursion (org-end-of-subtree t))))
              (cond
               ((slbmeh/is-project-p)
                nil)
               ((and (slbmeh/is-project-subtree-p) (not (slbmeh/is-task-p)))
                nil)
               (t
                subtree-end))))
        (save-excursion (org-end-of-subtree t))))

    (defun slbmeh/skip-project-trees-and-habits ()
      "Skip trees that are projects"
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((slbmeh/is-project-p)
            subtree-end)
           ((org-is-habit-p)
            subtree-end)
           (t
            nil)))))

    (defun slbmeh/skip-projects-and-habits-and-single-tasks ()
      "Skip trees that are projects, tasks that are habits, single non-project tasks"
      (save-restriction
        (widen)
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (cond
           ((org-is-habit-p)
            next-headline)
           ((and slbmeh/hide-schedule-and-waiting-next-tasks
                 (member "WAITING" (org-get-tags-at)))
            next-headline)
           ((slbmeh/is-project-p)
            next-headline)
           ((and (slbmeh/is-task-p) (not (slbmeh/is-project-subtree-p)))
            next-headline)
           (t
            nil)))))

    (defun slbmeh/skip-project-tasks-maybe ()
      "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
      (save-restriction
        (widen)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (next-headline (save-excursion (or (outline-next-heading) (point-max))))
               (limit-to-project (marker-buffer org-agenda-restrict-begin)))
          (cond
           ((slbmeh/is-project-p)
            next-headline)
           ((org-is-habit-p)
            subtree-end)
           ((and (not limit-to-project)
                 (slbmeh/is-project-subtree-p))
            subtree-end)
           ((and limit-to-project
                 (slbmeh/is-project-subtree-p)
                 (member (org-get-todo-state) (list "NEXT")))
            subtree-end)
           (t
            nil)))))

    (defun slbmeh/skip-project-tasks ()
      "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
      (save-restriction
        (widen)
        (let* ((subtree-end (save-excursion (org-dne-of-subtree t))))
          (cond
           ((slbmeh/is-project-p)
            subtree-end)
           ((org-is-habit-p)
            subtree-end)
           ((slbmeh/is-project-subtree-p)
            subtree-end)
           (t
            nil)))))

    (defun slbmeh/skip-non-project-tasks ()
      "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
      (save-restriction
        (widen)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (cond
           ((slbmeh/is-project-p)
            next-headline)
           ((org-is-habit-p)
            subtree-end)
           ((and (slbmeh/is-project-subtree-p)
                 (member (org-get-todo-state) (list "NEXT")))
            subtree-end)
           ((not (slbmeh/is-project-subtree-p))
            subtree-end)
           (t
            nil)))))

    (defun slbmeh/skip-projects-and-habits ()
      "Skip trees that are projects and tasks that are habits"
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((slbmeh/is-project-p)
            subtree-end)
           ((org-is-habit-p)
            subtree-end)
           (t
            nil)))))

    (defun slbmeh/skip-non-subprojects ()
      "Skip trees that are not projects"
      (let ((next-headline (save-excursion (outline-next-heading))))
        (if (slbmeh/is-subproject-p)
            nil
          next-headline)))

    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::* Archived Tasks")

    (defun slbmeh/skip-non-archivable-tasks ()
      "Skip trees that are not available for archiving"
      (save-restriction
        (widen)
        ;; Consider only tasks with done todo headings as archivable candidates
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
              (subtree-end (save-excursion (org-end-of-subtree t))))
          (if (member (org-get-todo-state) org-todo-keywords-1)
              (if (member (org-get-todo-state) org-done-keywords)
                  (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                         (a-month-ago (* 60 60 24 (+ daynr 1)))
                         (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                         (this-month (format-time-string "%Y-%m-" (current-time)))
                         (subtree-is-current (save-excursion
                                               (forward-line 1)
                                               (and (< (point) subtree-end)
                                                    (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                    (if subtree-is-current
                        subtree-end ; Has a date in this month or last month, skip it
                      nil)) ; available to archive
                (or subtree-end (point-max)))
	  next-headline))))

    (defun slbmeh/clock-in-to-next (kw)
      "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
      (when (not (and (boundp 'org-capture-mode) org-capture-mode))
        (cond
         ((and (member (org-get-todo-state) (list "TODO"))
               (slbmeh/is-task-p))
          "NEXT")
         ((and (member (org-get-todo-state) (list "NEXT"))
               (slbmeh/is-project-p))
          "TODO"))))

    (defun slbmeh/find-project-task ()
      "Move point to the parent (project) task if any"
      (save-restriction
        (widen)
        (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (goto-char parent-task)
          parent-task)))

    (defun slbmeh/punch-in (arg)
      "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
      (interactive "p")
      (setq slbmeh/keep-clock-running t)
      (if (equal major-mode 'org-agenda-mode)
          ;;
          ;; We're in the agenda
          ;;
          (let* ((marker (org-get-at-bol 'org-hd-marker))
                 (tags (org-with-point-at marker (org-get-tags-at))))
            (if (and (eq arg 4) tags)
                (org-agenda-clock-in '(16))
              (slbmeh/clock-in-organization-task-as-default)))
        ;;
        ;; We are not in the agenda
        ;;
        (save-restriction
          (widen)
                                        ; Find the tags on the current task
          (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
              (org-clock-in '(16))
            (slbmeh/clock-in-organization-task-as-default)))))

    (defun slbmeh/punch-out ()
      (interactive)
      (setq slbmeh/keep-clock-running nil)
      (when (org-clock-is-active)
        (org-clock-out))
      (org-agenda-remove-restriction-lock))

    (defun slbmeh/clock-in-default-task ()
      (save-excursion
        (org-with-point-at org-clock-default-task
                           (org-clock-in))))

    (defun slbmeh/clock-in-parent-task ()
      "Move point to the parent (project) task if any and clock in"
      (let ((parent-task))
        (save-excursion
          (save-restriction
            (widen)
            (while (and (not parent-task) (org-up-heading-safe))
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq parent-task (point))))
            (if parent-task
                (org-with-point-at parent-task
                                   (org-clock-in))
              (when slbmeh/keep-clock-running
                (slbmeh/clock-in-default-task)))))))

    (defvar slbmeh/organization-task-id "carnegielearning")

    (defun slbmeh/clock-in-organization-task-as-default ()
      (interactive)
      (org-with-point-at (org-id-find slbmeh/organization-task-id 'marker)
                         (org-clock-in '(16))))

    (defun slbmeh/clock-out-maybe ()
      (when (and slbmeh/keep-clock-running
                 (not org-clock-clocking-in)
                 (marker-buffer org-clock-default-task)
                 (not org-clock-resolving-clocks-due-to-idleness))
        (slbmeh/clock-in-parent-task)))

    (add-hook 'org-clock-out-hook 'slbmeh/clock-out-maybe 'append)

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

    (defun org-table-sort-column()
      "Sort table coolumns at point."
      (interactive)
      (let* ((thisline (org-current-line))
             (thiscol (org-table-current column))
             (otc org-table-overlay-coordinates)
             beg end bcol ecol tend tbeg column lns pos)
        (when (equal thiscol 0)
          (if (org-called-interactively-p 'any)
              (setq thiscol
                    (string-to-number
                     (read-string "Use column N for sorting: ")))
            (setq thiscol 1))
          (org-table-goto-column thiscol))
        (org-table-check-inside-data-field)
        (if (org-region-active-p)
            (progn
              (setq beg (region-beginning) end (region-end))
              (goto-char beg)
              (setq column (org-table-current-column)
                    beg (point-at-bol))
              (goto-char end)
              (setq end (point-at-bol 2)))
          (setq column (org-table-current-column)
                pos (point)
                tbeg (org-table-begin)
                tend (org-table-end))
          (if (re-search-backward org-table-hline-regexp tbeg t)
              (setq beg (point-at-bol 2))
            (goto-char tbeg)
            (setq beg (point-at-bol 1)))
          (goto-char pos)
          (if (re-search-forward org-table-hline-regexp tend t)
              (setq end (point-at-bol 1))
            (goto-char tend)
            (setq end (point-at-bol))))
        (setq beg (move-marker (make-marker) beg)
              end (move-marker (make-marker) end))
        (untabify beg end)
        (goto-char beg)
        (org-table-goto-column column)
        (skip-chars-backward "^|")
        (setq bcol (point))
        (goto-char end)
        (previous-line)
        (org-table-goto-column column)
        (skip-chars-forward "^|")
        (setq ecol (point))
        (org-table-coy-region bcol ecol nil)
        (setq lns (mapcar (lambda (x) (cons
                                  (org-sort-remove-invisible
                                   (substring-no-properties x))
                                  x))
                          (mapcar 'car org-table-clip)))
        (setq lns (org-do-sort lns "Column"))
        (setq org-table-clip (mapcar 'list (mapcar 'cdr lns)))
        (goto-char beg)
        (org-table-goto-column column)
        (org-table-paste-rectangle)
        (org-goto-line thisline)
        (org-table-goto-column thiscol)
        (when otc (org-table-toggle-coordinate-overlays))
        (message "%d element sorted in column %d" (length lns) column)))

    ;; Open up agenda
    (global-set-key (kbd "<f2>") 'org-agenda)
    ;; Switch org buffer
    (global-set-key (kbd "C-c b") 'org-iswitchb)
    ;; Go to currently clocked item
                                        ;(global-set-key (kbd "<f3>") )
    ;; Capture a task
    (global-set-key (kbd "C-c c") 'org-capture)

    (global-set-key "\C-cr" 'org-capture)
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)))

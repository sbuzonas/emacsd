;;** 23 Using Multiple (info "(emacs)Buffers")

(when section-buffers (message "23 Using Multiple Buffers...")

;;*** 23.2 (info "(emacs)List Buffers")

;; The C (current) column has a `.' for the buffer from which you came.
;; The R (read-only) column has a `%' if the buffer is read-only.
;; The M (modified) column has a `*' if it is modified.

;; rebind `C-x C-b'
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
    ;; `buffer-menu' moves point in the window which lists your buffers
    ;; `electric-buffer-list' pops up a buffer describing the set of buffers

;; operate on buffers like Dired
(when (try-require 'ibuffer)
    ;; completely replaces `list-buffer'
    (defalias 'ibuffer-list-buffers 'list-buffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer)



    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("version control" (or (mode . svn-status-mode)
                                    (mode . svn-log-edit-mode)
                                    (name . "^\\*svn-")
                                    (name . "^\\*vc\\*$")
                                    (name . "^\\*Annotate")
                                    (name . "^\\*git-")
                                    (name . "^\\*vc-")))
             ("emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")
                          (name . "^TAGS\\(<[0-9]+>\\)?$")
                          (name . "^\\*Help\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*Occur\\*$")
                          (name . "^\\*grep\\*$")
                          (name . "^\\*Compile-Log\\*$")
                          (name . "^\\*Backtrace\\*$")
                          (name . "^\\*Process List\\*$")
                          (name . "^\\*gud\\*$")
                          (name . "^\\*Man")
                          (name . "^\\*WoMan")
                          (name . "^\\*Kill Ring\\*$")
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*tramp")
                          (name . "^\\*shell\\*$")
                          (name . "^\\*compilation\\*$")))
             ("emacs source" (or (mode . emacs-lisp-mode)
                                 (filename . "/Applications/Emacs.app")
                                 (filename . "/bin/emacs")))
             ("agenda" (or (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (name . "^\\*Agenda")
                           (name . "^\\*org-")
                           (name . "^\\*Org")
                           (mode . org-mode)
                           (mode . muse-mode)))
             ("latex" (or (mode . latex-mode)
                          (mode . LaTeX-mode)
                          (mode . bibtex-mode)
                          (mode . reftex-mode)))
             ("dired" (or (mode . dired-mode))))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

    ;; Order the groups so the order is : [Default], [agenda], [emacs]
    (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                     activate)
      (setq ad-return-value (nreverse ad-return-value)))
    )

;; customizable buffer-selection with multiple menus
(GNUEmacs
    (when window-system
      (require 'msb)))

;; buffer selection
(GNUEmacs
    (try-require 'ibs))

;; make a menu of buffers so you can manipulate buffers or the buffer list
(global-set-key (kbd "C-x C-b") 'bs-show)

;; `cyclebuffer.el'

;; put the current buffer at the end of the list of all buffers
(global-set-key (kbd "<f12>") 'bury-buffer)








;; ;; Like standard Emacs 22 commands (bound to C-x left/right)
;; (define-key global-map [f11] 'previous-buffer) ;; my-buffer-prev
;; (define-key global-map [f12] 'next-buffer)     ;; my-buffer-next

;; ;; Like standard Emacs 22 commands (bound to M-g n/p)
;; (define-key global-map [(control f11)] 'previous-error)
;; (define-key global-map [(control f12)] 'next-error)
;; (define-key global-map [(control shift f11)] 'compilation-previous-file)
;; (define-key global-map [(control shift f12)] 'compilation-next-file)

















;;*** 23.4 (info "(emacs)Kill Buffer")

;; kill buffer without confirmation (if not modified)
(defun my-kill-this-buffer ()
  "Kill the current buffer without confirmation (if not modified)."
  (interactive)
;;;   (let ((bufname (buffer-name)))
;;;     (if (or
;;;          (string-equal "*Group*" bufname))
;;;         (bury-buffer bufname)
      (kill-buffer nil))
;;;       ))

;; key binding
(global-set-key (kbd "<S-f12>") 'my-kill-this-buffer)


;;*** 23.7 (info "(emacs)Buffer Convenience") and Customization of Buffer Handling

;; unique buffer names dependent on file name
(try-idle-require 'uniquify)
(eval-after-load "uniquify"
  '(progn

    ;; style used for uniquifying buffer names with parts of directory name
    (setq uniquify-buffer-name-style 'forward)))

(message "23 Using Multiple Buffers... Done"))


;;** 24 Multiple (info "(emacs)Windows")

(when section-windows (message "24 Multiple Windows...")

;; TODO Have a look at `ido'

;;*** 24.1 Concepts of Emacs (info "(emacs)Basic Window")s

;; turn off this horrible tab thingy in XEmacs
(XEmacs
    (when (boundp 'default-gutter-visible-p)
      (set-specifier default-gutter-visible-p nil)))


;;*** 24.3 Using (info "(emacs)Other Window")

;; cycle through all windows on current frame
(global-set-key (kbd "<f6>") 'other-window)


;;*** 24.6 Deleting and (info "(emacs)Change Window")

;; delete all windows in the selected frame except the selected window
(global-set-key (kbd "<f5>") 'delete-other-windows)

;; enlarge or shrink windows more easily than with `C-x {' and the like
(global-set-key (kbd "<C-S-up>") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'shrink-window)
(global-set-key (kbd "<C-S-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-right>") 'shrink-window-horizontally)

;; make all visible windows the same height (approximately)
(global-set-key (kbd "<C-f6>") 'balance-windows)

;; swap 2 windows
(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(global-set-key (kbd "C-c ~") 'my-swap-windows)

(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'my-toggle-window-split)


;;  25.7 (info "(emacs)Window Convenience") Features and Customization

;; Use `M-x scroll-all-mode' to scroll all visible windows together in
;; parallel.

;; numbered window shortcuts
;; (It numbers windows and you can switch them easily with `M-<number>').
(when (try-require 'window-numbering)
  (window-numbering-mode 1))


;; >> Is there any way of making a particular window persistent? I have an erc
;; >> window which obviously disappears when I do other things and I'd like to
;; >> keep a small window at the bottom of the screen so I can keep an eye on
;; >> it. Is there a function or hack to do this?
;; >
;; > You can store the window configuration in a register, and jump back to it.
;; >
;; > But if you want to keep an eye on it, the best is to open another
;; > frame, if you're using a window manager.
;; >
;; > C-x 5 2    to create a new frame
;; > C-x 5 o    to switch from one frame to the other.
;;
;; Also, frob special-display-buffer-names: You can make a window dedicated,
;; which does just what you want. You can do it for all windows or windows for
;; buffers whose names match some pattern, and so on.
;;
;; Check the Elisp manual (that's Emacs Lisp), and look for `dedicated'
;; windows. See, in particular, user options `special-display-buffer-names'
;; and `special-display-regexps'.

;; winring

(message "24 Multiple Windows... Done"))

;;** 17 (info "(emacs)Registers")

(when section-registers (message "17 Registers...")



;; ;; Enable position saving through shortcuts.
;; ;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
;; (global-set-key [C-f1] '(lambda () (interactive) (point-to-register ?1)))
;; (global-set-key [C-f2] '(lambda () (interactive) (point-to-register ?2)))
;; (global-set-key [C-f3] '(lambda () (interactive) (point-to-register ?3)))
;; (global-set-key [C-f4] '(lambda () (interactive) (point-to-register ?4)))

;; (defun jump-to-register-other (reg)
;; (other-window 1)
;; (jump-to-register reg)
;; (hilit-recenter (/ (window-height) 2)))

;; (defun jump-to-register-here (reg)
;; (jump-to-register reg)
;; (hilit-recenter (/ (window-height) 2)))

;; ;; Move to saved position with F1 F2 F3 and F4
;; (global-set-key [f1] '(lambda () (interactive) (jump-to-register-here ?1)))
;; (global-set-key [f2] '(lambda () (interactive) (jump-to-register-here ?2)))
;; (global-set-key [f3] '(lambda () (interactive) (jump-to-register-here ?3)))
;; (global-set-key [f4] '(lambda () (interactive) (jump-to-register-here ?4)))




;;*** 17.7 (info "(emacs)Bookmarks")

;; Bookmarks are persistent and they have names; not markers. Bookmarked
;; positions can also be relocated (found) if they move slightly because of
;; text changes.

;; To navigate to a bookmark (linking to a file or directory), just press
;; `C-x r b'. You'll be prompted for the bookmark name, and it will open that
;; file or directory.

;; where to save the bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks.txt")

;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag 1)



;; I have just added a key "O" to filter only these two bookmarks in
;; bookmark-extensions.el
;; http://mercurial.intuxication.org/hg/emacs-bookmark-extension/
;; So when you hit "O" from the bookmark list (C-x r l) you have that:
;; ,----
;; | % Bookmark Last Org Stored
;; | - ------------------------
;; |   org-refile-last-stored
;; |   org-remember-last-stored
;; `----



;; visible bookmarks in buffer
(GNUEmacs
    ;; repository should be restored when loading `bm'
    (setq bm-restore-repository-on-load t)

    (when (try-require 'bm)
        ;; key binding
        (global-set-key (kbd "<M-f2>") 'bm-toggle)

        ;; buffer should be recentered around the bookmark
        (setq bm-recenter t)

        ;; make bookmarks persistent as default
        (setq-default bm-buffer-persistence t)

        ;; loading the repository from file when on start up
        (add-hook' after-init-hook 'bm-repository-load)

        ;; restoring bookmarks when on file find
        (add-hook 'find-file-hooks 'bm-buffer-restore)

        ;; saving bookmark data on killing a buffer
        (add-hook 'kill-buffer-hook 'bm-buffer-save)

        ;; saving the repository to file when on exit
        ;; `kill-buffer-hook' is not called when emacs is killed, so we
        ;; must save all bookmarks first
        (add-hook 'kill-emacs-hook '(lambda nil
                                      (bm-buffer-save-all)
                                      (bm-repository-save)))

        ;; update bookmark repository when saving the file
        (add-hook 'after-save-hook 'bm-buffer-save))

    ;; lists all bookmarks in all buffers
    (try-require 'bm-ext))

(message "17 Registers... Done"))

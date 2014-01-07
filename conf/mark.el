;;* Important Text-Changing Commands

;;** 11 The (info "(emacs)Mark") and the Region

(when section-mark (message "11 The Mark and the Region...")

;;*** 11.7 (info "(emacs)Persistent Mark")s

;; when the mark is active, the *region is highlighted*
;; (enabled by default in Emacs 23)
(GNUEmacs
    (when window-system
      (transient-mark-mode 1)))

(message "11 The Mark and the Region... Done"))

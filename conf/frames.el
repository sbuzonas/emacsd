;;** 25 (info "(emacs)Frames") and Graphical Displays ]

(when section-frames (message "25 Frames and Graphical Displays...")

;;*** 25.1 (info "(emacs)Cut and Paste") on Graphical Displays

;; copy/paste with Gnome desktop
(GNUEmacs
    ;; cutting and pasting uses the clipboard
    (setq x-select-enable-clipboard t)

    ;; make cut, copy and paste (keys and menu bar items) use the clipboard
    (menu-bar-enable-clipboard)

    ;; ;; UNDER-TEST mouse drag copies region to kill-ring
    ;; (setq mouse-drag-copy-region nil)

    ;; ;; UNDER-TEST cutting and pasting does not use the primary selection
    ;; (setq x-select-enable-primary nil)

    ;; ;; UNDER-TEST an active region automatically becomes the window selection
    ;; (setq select-active-regions t)
    )


;;*** 25.5 (info "(emacs)Creating Frames")

;; - resize the frame to the size you want
;; - enter `(frame-parameters)' in the `*scratch*' buffer
;; - evaluate the form: place the cursor after the closing paren, and type
;;   `C-j', so that the output goes right into the `*scratch*' buffer

;; put Emacs exactly where you want it, every time it starts up, by
;; auto-detecting the screen dimensions and computing where it should be
(when window-system
  ;; list of frame parameters for creating the initial frame
  (setq initial-frame-alist '((top . 0) (left . 0)))

  (setq initial-frame-alist
        (append (list
                 '(internal-border-width . 2)
                 '(line-spacing          . 1))
                initial-frame-alist))

  ;; list of default values for frame creation
  (setq default-frame-alist
        (cond ((= (x-display-pixel-height) 1200)
               '((left . 0) (height . 74)))

              ((= (x-display-pixel-height) 1024)
               '((left . 0) (height . 63)))

              ((= (x-display-pixel-height) 800)
               (cond (running-ms-windows
                      '((left . 0) (height . 55)))
                     (running-gnu-linux
                      '((left . 0) (height . 47)
                        (vertical-scroll-bars . right)))))

              ((= (x-display-pixel-height) 768)
               '((left . 0) (height . 46)))))

  (setq default-vertical-scroll-bar 'right))

(XEmacs
 (set-frame-width (buffer-dedicated-frame) 80)
 (set-frame-height (buffer-dedicated-frame) 42)
 (set-frame-position (buffer-dedicated-frame) 0 0))

;; title bar display of visible frames
(setq frame-title-format "Emacs")


;;; From sample .emacs
;;; local Emacs background:  default
;;; remote Emacs background: palegreen1
;;; root Emacs background:   coral2
;;
;;(cond
;; ((and (string-match "XEmacs" emacs-version)
;;       (eq window-system 'x)
;;       (boundp 'emacs-major-version)
;;       (= emacs-major-version 19)
;;       (>= emacs-minor-version 12))
;;  (let* ((root-p (eq 0 (user-uid)))
;;        (dpy (or (getenv "DISPLAY") ""))
;;        (remote-p (not
;;                   (or (string-match "^\\(\\|unix\\|localhost\\):" dpy)
;;                       (let ((s (system-name)))
;;                         (if (string-match "\\.\\(netscape\\|mcom\\)\\.com" s)
;;                             (setq s (substring s 0 (match-beginning 0))))
;;                         (string-match (concat "^" (regexp-quote s)) dpy)))))
;;        (bg (cond (root-p "coral2")
;;                  (remote-p "palegreen1")
;;                  (t nil))))
;;    (cond (bg
;;          (let ((def (color-name (face-background 'default)))
;;                (faces (face-list)))
;;            (while faces
;;              (let ((obg (face-background (car faces))))
;;                (if (and obg (equal def (color-name obg)))
;;                    (set-face-background (car faces) bg)))
;;              (setq faces (cdr faces)))))))))


;;*** 25.6 (info "(emacs)Frame Commands")

(XLaunch
    (defun toggle-full-screen ()
      "Toggle between full screen and partial screen display on X11;
    courtesy of http://www.emacswiki.org/cgi-bin/wiki/FullScreen"
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

    (global-set-key (kbd "C-c z") 'toggle-full-screen))

(GNUEmacs
   (when running-ms-windows
        (defun w32-maximize-frame ()
          "Maximize the current frame."
          (interactive)
          (w32-send-sys-command 61488)
          (global-set-key (kbd "C-c z") 'w32-restore-frame))

        (global-set-key (kbd "C-c z") 'w32-maximize-frame)

        (defun w32-restore-frame ()
          "Restore a minimized frame."
          (interactive)
          (w32-send-sys-command 61728)
          (global-set-key (kbd "C-c z") 'w32-maximize-frame))))


;;*** 25.7 (info "(emacs)Speedbar") Frames

;; everything browser (into individual source files), or Dired on steroids
(when (try-require 'speedbar-XXX)

    ;; number of spaces used for indentation
    (setq speedbar-indentation-width 2)

    ;; expand/collapse LaTeX sections
    (speedbar-add-supported-extension '(".tex" ".bib" ".w" ".nw"))

    ;; jump to speedbar frame
    (global-set-key (kbd "<f4>") 'speedbar-get-focus)

    ;; bind the arrow keys in the speedbar tree
    ;; [http://www.uweb.ucsb.edu/~dreamtheorist/emacs.html]
    (define-key speedbar-key-map (kbd "<right>") 'speedbar-expand-line)
    (define-key speedbar-key-map (kbd "<left>") 'speedbar-contract-line)

    ;; parameters to use when creating the speedbar frame in Emacs
    (setq speedbar-frame-parameters '((width . 30)
                                      (height . 45)
                                      (foreground-color . "blue")
                                      (background-color . "white"))))

;; speedbar frame (vs window)
(when (try-require 'sr-speedbar)
    (global-set-key (kbd "<f4>") 'sr-speedbar-toggle))


;;*** 25.12 Scrolling with (info "(emacs)Wheeled Mice")

;; mouse wheel support
;; (GNUEmacs
;;  (mwheel-install))


;;*** 25.14 (info "(emacs)Menu Bars")

;; turn menus off
(unless window-system
  (menu-bar-mode 0))


;;*** 25.16 Using (info "(emacs)Dialog Boxes")

;; don't use dialog boxes to ask questions
(setq use-dialog-box nil)

;; don't use a file dialog to ask for files
(setq use-file-dialog nil)


;;*** 25.18 (info "(emacs)Mouse Avoidance")

(when window-system
  ;; make mouse pointer stay out of the way of editing
  (when (try-require 'avoid)
    (mouse-avoidance-mode 'jump)))


;; Move the mouse to the screen corner on any keypress.
(when (and (display-mouse-p) (require 'avoid nil t))
  ;; Move the mouse to the lower-right corner instead of default upper-right
  ;; (defun mouse-avoidance-banish-destination ()
  ;;   (cons (+ 3 (frame-width)) (frame-height)))
  (mouse-avoidance-mode 'banish))



(message "25 Frames and Graphical Displays... Done"))

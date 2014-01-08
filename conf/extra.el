;; `C-x *' invokes the GNU Emacs Calculator
;;
;; Try the embedded mode of Calc: `C-x * E' (no need to mark the region) and
;; similar commands (`J' or `W' in place of `E'). You need to type `C-x * E'
;; again to exit the embedded mode.
;;
;; Start the Calc: `C-x * C'
;;
;; Run the Calculator in the minibuffer: `C-x * Q' (`M-x quick-calc')



(defun reverse-words (start end)
  (interactive "r")
  (let ((words (reverse (split-string (buffer-substring start end)))))
    (delete-region start end)
    (dolist (word words)
      (insert word " "))
    (backward-char 1)
    (delete-char 1)))


(defun reverse-region-by-line (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end) (re-search-forward "\\=.*$" end t))
      (replace-match (apply #'string
                            (nreverse (string-to-list (match-string 0)))))
      (forward-line))))


(defun shuffle-vector (vector)
  "Destructively shuffle the contents of VECTOR and return it."
  (loop
   for pos from (1- (length vector)) downto 1
   for swap = (random (1+ pos))
   unless (= pos swap)
   do (rotatef (aref vector pos)
               (aref vector swap)))
  vector)

(defun randomize-region (start end)
  "Randomly re-order the lines in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; narrow to the region
      (narrow-to-region start end)
      (goto-char (point-min))
      (let* ((nlines (line-number-at-pos end))
             (lines (make-vector nlines nil)))
        ;;
        (while (not (eobp))
          (setf (aref lines (decf nlines)) ; if it's random backwards
                is fine
                (delete-and-extract-region (point)
                                           (progn (forward-visible-
                                                   line 1)
                                                  (point)))))
        ;;
        (let ((rlines (shuffle-vector lines)))
          (dotimes (linenum (length rlines))
            (insert (aref rlines linenum))))))))



(message "Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))
(sit-for 1.5)

;; after-save-hook: (lambda () (byte-compile-file (buffer-file-name)))


;; > move-to-window-line, M-r
;; > back-to-indentation, M-m



;; M-x rainbow-mode
(try-require 'rainbow-mode)


(GNUEmacs
    ;; should be part of Emacs 23.1
    ;; see `split-width-threshold' (minimum width for splitting windows
    ;; sensibly)

    (defun th-display-buffer (buffer force-other-window)
      "If BUFFER is visible, select it.

    If it's not visible and there's only one window, split the
    current window and select BUFFER in the new window. If the
    current window (before the split) is more than 160 columns wide,
    split horizontally, else split vertically.

    If the current buffer contains more than one window, select
    BUFFER in the least recently used window.

    This function returns the window which holds BUFFER.

    FORCE-OTHER-WINDOW is ignored."
      (or (get-buffer-window buffer)
          (if (one-window-p)
              (let ((new-win (if (> (window-width) 160)
                                 (split-window-horizontally)
                               (split-window-vertically))))
                (set-window-buffer new-win buffer)
                new-win)
            (let ((new-win (get-lru-window)))
              (set-window-buffer new-win buffer)
              new-win))))

    (setq display-buffer-function 'th-display-buffer))


;; Other IRC for Emacs: rcirc, circe, bitlbee,  liece, riece, zenirc, erc

;; Circe is advised by Tassilo (contributor)

(when (try-require 'circe)

  ;; This defines the password variables below
  (when (file-exists-p "~/.private.el")
    (load-file "~/.private.el"))
    (setq freenode-passwd "")

  (setq circe-default-nick "slbmeh")

  (setq circe-default-realname "slbmeh")

  (setq circe-highlight-nick-type 'all)

  (when (try-require 'circe-highlight-all-nicks)
    (enable-circe-highlight-all-nicks))

  ;; (setq circe-server-coding-system '(latin-1 . undecided))

  (setq circe-format-self-say "<{nick}> {body}")

  (setq circe-server-auto-join-channels
        '(("^freenode$"
           "#emacs"
           "#zsh"
           "##php")))

  (setq circe-nickserv-passwords
        `(("freenode" ,freenode-passwd)))

  (setq lui-flyspell-p t)

  (setq lui-flyspell-alist '(("." "en_US")))

  (setq lui-max-buffer-size 30000)

  ;; (setq lui-fill-column 80)
  (setq lui-highlight-keywords '("[^<]slbmeh" "org" "beamer" "tikz"))

  (eval-after-load "circe"
    '(progn

       ;; add IRC color support to LUI
       (require 'lui-irc-colors)

       (add-to-list 'lui-pre-output-hook 'lui-irc-colors)))

  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "irc.freenode.net" "6667" "freenode")
    ;; (circe "localhost" "6668" "bitlbee")
    )

  ;; (global-set-key (kbd "<f9>")  'irc)

  ;; (irc)
  ;; /whois
  ;; /leave
  )

(when (try-require 'ido)
  (ido-mode 1)

  (ido-everywhere 1)
  (setq ido-confirm-unique-completion t)
  (setq ido-enable-flex-matching t)

  ;; will use ffap-guesser to determine whether file name is at point
  (setq ido-use-filename-at-point 'guess)

  (setq org-completion-use-ido t))

;; SQL-mode
(setq sql-sqlite-program "sqlite3")

(try-require 'ess-site)

;; make Emacs aware of this package
(when (try-require 'command-frequency)

  (command-frequency-table-load)

  ;; load the program
  (command-frequency-mode 1)

  (command-frequency-autosave-mode 1))

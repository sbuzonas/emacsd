(defaddon erc
  "Some basic configuration for ERC mode"
  (require 'erc)
  (require 'erc-log)
  (require 'erc-notify)
  (require 'erc-spelling)
  (require 'erc-autoaway)
  
  (setq erc-interpret-mirc-color t)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-query-display 'buffer)
  
  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				  "324" "329" "332" "333" "353" "477"))
  
  (setq erc-log-channels-directory (expand-file-name "erc-logs" shared-dir))
  (unless (file-exists-p erc-log-channels-directory)
    (make-directory erc-log-channels-directory))

  (setq erc-save-buffer-on-part t)

  (erc-truncate-mode)
  (erc-spelling-mode)

  ;; TODO: Discover why this is an alist
  (defvar erc-autojoin-channels-alist '(("##php"))
    "Alist of channels to join automatically")
  (defvar erc-notify-nick-alist nil
    "Alist of nicks and the last time they tried to trigger a notification")
  (defvar erc-notify-timeout 10
    "Number of seconds that must elapse between notifications from the same persion")
  (defun erc-notify-allowed-p (nick &optional delay)
    "Return non-nil if a notification should be made for NICK. If DELAY is specified,
it will be the minimum time in seconds that can occur between two notifications. The
default is `erc-notify-timeout'."
    (unless delay (setq delay erc-notify-timeout))
    (let ((cur-time (time-to-seconds (current-time)))
	  (cur-assoc (assoc nick erc-notify-nick-alist))
	  (last-time nil))
      (if cur-assoc
	  (progn
	    (setq last-time (cdr cur-assoc))
	    (setcdr cur-assoc cur-time)
	    (> (abs (- cur-time last-time)) delay))
	(push (cons nick cur-time) erc-notify-nick-alist)
	t)))

  (setq erc-auto-discard-away t)
  (setq erc-autoaway-idle-seconde 600)
  (setq erc-autoaway-use-emacs-idle t)

  (setq erc-server-coding-system '(utf-8 . utf-8))
  
  (defun start-irc ()
    "Connect to IRC."
    (interactive)
    (when (y-or-n-p "Do you want to start IRC? ")
      (erc :server "chat.freenode.net" :port 8001 :nick erc-nick)))
  
  (defun filter-server-buffers ()
    (delq nil
	  (mapcar
	   (lambda (x) (and (erc-server-buffer-p x) x))
	   (buffer-list))))

  (defun stop-irc ()
    "Disconnects from all irc servers"
    (interactive)
    (dolist (buffer (filter-server-buffers))
      (message "Server buffer: %s" (buffer-name buffer))
      (with-current-buffer buffer
	(erc-quit-server "Terminated")))))

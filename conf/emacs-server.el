;;** 42 Using (info "(emacs)Emacs Server")

(when section-emacs-server (message "42 Using Emacs as a Server...")

;; use Emacs as a server
;; On GNU/Linux, you should use the `server-start' function and the
;; `emacsclient' program (now as well available under EmacsW32). Emacs 23 has
;; a `--daemon' flag which makes this even more convenient.
(GNUEmacs
;;  (setq server-host "Rabbit")
;;  (setq server-use-tcp t)


;; * How can I use TRAMP to connect to a remote GNU Emacs session?
;;
;; You can configure Emacs Client doing this.  On the remote host,
;; you start the Emacs Server:
;;
;; (require 'server)
;; (setq server-host (system-name)
;;       server-use-tcp t)
;; (server-start)
;;
;; Make sure, that the result of `(system-name)' can be resolved on
;; your local host; otherwise you might use a hard coded IP address.
;;
;; The resulting file `~/.emacs.d/server/server' must be copied to
;; your local host, at the same location.  You can call then the
;; Emacs Client from the command line:
;;
;; emacsclient /ssh:user@host:/file/to/edit
;;
;; `user' and `host' shall be related to your local host.


    ;; start the Emacs server
    (server-start))


;; > I'd like to be able to reconnect to the running Emacs process and have
;; > it display on my X server at home.  Is this possible?
;;
;; In the X11 forwarded ssh shell:
;;
;; $ emacsclient -e "(make-frame-on-display \"$DISPLAY\")"
;; ; Fri Feb  1 13:06:41 2008 - sva   Replace `$DISPLAY' by `:0.0'
;; From VM:
;; > ssh 10.0.2.2 -f emacsclient --eval '"(make-frame-on-display \":0.0\")"'

;; rebind `C-x C-c' to `delete-frame'?

;; (GNUEmacs
;;     (defun my-done ()
;;       (interactive)
;;       (server-edit)
;;       (make-frame-invisible nil t))
;;     (global-set-key (kbd "C-x C-c") 'my-done))

(message "42 Using Emacs as a Server... Done"))

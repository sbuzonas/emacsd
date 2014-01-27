(defcustom addon-circe
  nil
  "Enable circe features"
  :type 'boolean
  :group 'features)

(when addon-circe
  (progn
    (add-to-list 'my-default-packages 'circe)
    (autoload 'circe "circe" "Connect to an IRC server" t)
    (setq freenode-passwd "")
    (when (file-exists-p "~/.private.el")
      (load-file "~/.private.el"))
    (setq circe-default-realname "Steve Buzonas"
	  circe-default-nick "slbmeh"
	  circe-ignore-list nil
	  circe-server-coding-system '(latin-1 . undecided)
	  circe-format-self-say "<{nick}> {body}"
	  circe-server-auto-join-channels
	    '(("^freenode$" "#emacs" "##php" "#css"))
	  circe-nickserv-passwords
	    `(("freenode" ,freenode-passwd)))
    (setq lui-max-buffer-size 30000
	  lui-flyspell-p t
	  lui-flyspell-alist '(("." "american"))
	  lui-highlight-keywords '("[^<]slbmeh"))))

(after 'circe
  (try-require 'lui-irc-colors)
  (when (try-require 'circe-highlight-all-nicke)
    (enable-circe-highlight-all-nicks))
  (defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode")))

(after 'lui-irc-colors
  (add-to-list 'lui-pre-output-hook 'lui-irc-colors))

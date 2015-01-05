(require 'epg)

(if (executable-find epg-gpg-program)
    (progn
      (message "* --[ Loading encrypted variables ]--")
      (if (file-exists-p encrypted-variables-file)
	  (load encrypted-variables-file)
	(message "No secrets found at path: %s" encrypted-variables-file)))
  (warn "Could not find gpg executable %s! Password file could not be loaded." epg-gpg-program))
(provide 'fg-secrets)

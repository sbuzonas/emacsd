(setq shared-dir (expand-file-name "~/Dropbox/emacs"))
(setq encrypted-variables-file (expand-file-name "secrets.el.gpg" shared-dir))

(setq user-full-name "Steve Buzonas")

;; This is here so it gets overridden on local with all other machines not fitting the pattern
(if (fg/string-starts-with system-name "sbuzonas")
    (setq user-mail-address "sbuzonas@carnegielearning.com"
	  user-website "http://www.carnegielearning.com")
  (setq user-mail-address "steve@fancyguy.com"
	user-website "http://www.stevebuzonas.com"))

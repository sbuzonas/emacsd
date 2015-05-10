(defaddon appearance
  "Appearance enhancements and theme interactions."

  (nomad/require-package 'powerline)
  (powerline-default-theme)
  
  (defgroup nomad-appearance nil
    "Nomad Appearance Settings"
    :group 'nomad)
  
  (defcustom theme-dir (expand-file-name "themes" load-emacs-dir)
    "The directory to store custom themes"
    :type 'file
    :group 'nomad-appearance)

  (unless (file-exists-p theme-dir)
    (make-directory theme-dir))

  (defcustom theme-package-alist '((afternoon . afternoon-theme)
				(ample . ample-theme)
				(ample-zen . ample-zen-theme)
				(anti-zenburn . anti-zenburn-theme)
				(badger . badger-theme)
				(base16-chalk . base16-theme)
				(base16-default . base16-theme)
				(base16-eighties . base16-theme)
				(base16-greenscreen . base16-theme)
				(base16-mocha . base16-theme)
				(base16-monokai . base16-theme)
				(base16-ocean . base16-theme)
				(base16-railscasts . base16-theme)
				(base16-solarized . base16-theme)
				(base16-tomorrow . base16-theme)
				(basic . basic-theme)
				(birds-of-paradise-plus . birds-of-paradise-plus-theme)
				(bliss . bliss-theme)
				(boron . boron-theme)
				(brin . sublime-themes)
				(bubbleberry . bubbleberry-theme)
				(busybee . busybee-theme)
				(calmer-forest . calmer-forest-theme)
				(cherry-blossom . cherry-blossom-theme)
				(clues . clues-theme)
				(colonoscopy . colonoscopy-theme)
				(cyberpunk . cyberpunk-theme)
				(dakrone . dakrone-theme)
				(dark-krystal . dark-krystal-theme)
				(darkmine . darkmine-theme)
				(distinguished . distinguished-theme)
				(django . django-theme)
				(dorsey . sublime-themes)
				(espresso . espresso-theme)
				(firebelly . firebelly-theme)
				(firecode . firecode-theme)
				(flatland-black . flatland-black-theme)
				(flatland . flatland-theme)
				(flatui . flatui-theme)
				(fogus . sublime-themes)
				(gandalf . gandalf-theme)
				(gotham . gotham-theme)
				(graham . sublime-themes)
				(granger . sublime-themes)
				(grandshell . grandshell-theme)
				(gruber-darker . gruber-darker-theme)
				(gruvbox . gruvbox-theme)
				(hc-zenburn . hc-zenburn-theme)
				(heroku . heroku-theme)
				(hickey . sublime-themes)
				(hipster . hipster-theme)
				(inkpot . inkpot-theme)
				(ir-black . ir-black-theme)
				(jazz . jazz-theme)
				(junio . sublime-themes)
				(leuven . leuven-theme)
				(light-soap . light-soap-theme)
				(lush . lush-theme)
				(mbo70s . mbo70s-theme)
				(mccarthy . sublime-themes)
				(mellow . mellow-theme)
				(minimal . minimal-theme)
				(molokai . molokai-theme)
				(monochrome . monochrome-theme)
				(monokai . monokai-theme)
				(mustang . mustang-theme)
				(mustard . mustard-theme)
				(naquadah . naquadah-theme)
				(niflheim . niflheim-theme)
				(noctilux . noctilux-theme)
				(obsidian . obsidian-theme)
				(occidental . occidental-theme)
				(odersky . sublime-themes)
				(oldlace . oldlace-theme)
				(organic-green . organic-green-theme)
				(pastels-on-dark . pastels-on-dark-theme)
				(peacock . peacock-theme)
				(phoenix-dark-mono . phoenix-dark-mono-theme)
				(phoenix-dark-pink . phoenix-dark-pink-theme)
				(planet . planet-theme)
				(professional . professional-theme)
				(purple-haze . purple-haze-theme)
				(reverse . reverse-theme)
				(ritchie . sublime-themes)
				(seti . seti-theme)
				(slime . slime-theme)
				(smyx . smyx-theme)
				(soft-charcoal . soft-charcoal-theme)
				(soft-morning . soft-morning-theme)
				(soft-stone . soft-stone-theme)
				(solarized-dark . solarized-theme)
				(solarized-light . solarized-theme)
				(soothe . soothe-theme)
				(spacegray . spacegray-theme)
				(spolsky . sublime-themes)
				(stekene-light . stekene-theme)
				(stekene-dark . stekene-theme)
				(subatomic . subatomic-theme)
				(subatomic256 . subatomic256-theme)
				(sunny-day . sunny-day-theme)
				(tango-2 . tango-2-theme)
				(tango-plus . tango-plus-theme)
				(tangotango . tangotango-theme)
				(wilson . sublime-themes)
				(zenburn . zenburn-theme))
    "Association list of (theme-name . theme-package) cons cells"
    :type 'alist
    :group 'nomad-appearance)

  (defcustom blacklisted-themes-list '()
    "Themes that should not be loaded for various reasons."
    :type 'list
    :group 'nomad-appearance)

  (defcustom default-theme nil
    "Default theme to load at startup. If nil load a random theme that isn't blacklisted."
    :type 'string
    :group 'nomad-appearance)

  (unless (boundp 'appearance-frame-setup)
    (setq appearance-frame-setup '()))
  
  (defun run-frame-setup ()
    (run-hooks 'appearance-frame-setup))
  
  (run-frame-setup)
  (add-hook 'after-make-frame-functions 'run-frame-setup)

  (defun nomad/allowed-themes ()
    (delq nil (mapcar #'(lambda (x)
			  (if (member (car x) blacklisted-themes-list)
			      nil
			    (car x)))
		      theme-package-alist)))

  (defun nomad/random-theme ()
    (nth (random (length (nomad/allowed-themes))) (nomad/allowed-themes)))

  (defun nomad/load-theme (theme)
    (let ((theme-package (cdr (assoc theme theme-package-alist)))
	  (loading-message (format "Loading theme '%s'..." theme)))
      (when theme-package
	(when (or (member theme (nomad/allowed-themes))
		  (and (not (member theme blacklisted-themes-list))
		       (yes-or-no-p (format "The chosen theme '%s' is not in the allowed themes list. Continue? " theme))))
	  (with-temp-message loading-message
	    (nomad/require-package theme-package)
	    (load-theme theme t))
	  (setq current-theme theme)))))

  (defun nomad/load-default-theme ()
    (interactive)
    (if default-theme
	(nomad/load-theme default-theme)
      (nomad/load-random-theme)))
  (defalias 'load-default-theme 'nomad/load-default-theme)

  (defun nomad/load-random-theme ()
    (interactive)
    (nomad/load-theme (nomad/random-theme)))
  (defalias 'load-random-theme 'nomad/load-random-theme)

  (unless (boundp 'current-theme)
    (setq current-theme nil))

  (nomad/load-default-theme)

  (defun nomad/font-lock-lambda ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
	    (0 (progn (compose-region (match-beginning 1) (match-end 1)
				      ,(make-char 'greek-iso8859-7 107))
		      nil))))))

  (add-hook 'prog-mode-hook 'nomad/font-lock-lambda)
  
  ;; Unload all theme settings before loading a new theme
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes)))

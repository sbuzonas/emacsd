(setq terminal-theme (getenv "ITERM_THEME"))

<<<<<<< Updated upstream
(setq fg/default-theme 'mustard)
=======
;(setq fg/default-theme 'calmer-forest)

(setq solarized-dark-blacklisted-themes '(anti-zenburn
					  base16-greenscreen
					  base16-mocha
					  base16-monokai
					  base16-ocean
					  base16-railscasts
					  base16-solarized
					  basic
					  birds-of-paradise-plus
					  brin
					  cherry-blossom
					  colonoscopy
					  dark-krystal
					  django
					  espresso
					  flatui
					  fogus
					  gandalf
					  graham
					  granger
					  heroku
					  hipster
					  inkpot
					  ir-black
					  lavender
					  leuven
					  light-soap
					  mccarthy
					  mellow
					  obsidian
					  occidental
					  odersky
					  oldlace
					  organic-green
					  phoenix-dark-pink
					  planet
					  professional
					  ritchie
					  soft-morning
					  soft-stone
					  solarized-dark
					  solarized-light
					  soothe
					  stekene-light
					  subatomic
					  sunny-day
					  tango-plus))
>>>>>>> Stashed changes

(setq solarized-light-blacklisted-themes '())
(setq tango-dark-blacklisted-themes '())
(setq tango-light-blacklisted-themes '())

(setq solarized-dark-preferred-themes '(afternoon
					ample
					ample-zen
					badger
					base16-chalk
					base16-default
					base16-tomorrow
					bliss
					boron
					busybee
					calmer-forest
					cyberpunk
					dakrone
					darkmine
					distinguished
					firecode
					flatland-black
					flatland
					gotham
					graham
					granshell
					gruber-darker
					gruvbox
					hc-zenburn
					hickey
					jazz
					junio
					lush
					monokai
					mustard
					naquadah
					niflheim
					odersky
					pastels-on-dark
					peacock
					reverse
					seti
					smyx
					spolsky
					soft-charcoal
					subatomic256
					tango-2
					tangotango
					wilson))

(setq fg/blacklisted-themes-list (cond ((equal "solarized-dark" terminal-theme) solarized-dark-blacklisted-themes)
				       ((equal "solarized-light" terminal-theme) solarized-light-blacklisted-themes)
				       ((equal "tango-dark" terminal-theme) tango-dark-blacklisted-themes)
				       ((equal "tango-light" terminal-theme) tango-light-blacklisted-themes)))

(setq fg/blacklisted-themes-list nil)

(defun create-theme-blacklist ()
  (delq nil (mapcar (lambda (x)
		      (unless (member x solarized-dark-preferred-themes)
			x)) (fg/allowed-themes))))

(defun reset-blacklist ()
  (setq fg/blacklisted-themes-list (create-theme-blacklist)))

(add-hook 'after-appearance-addon-hook '(lambda ()
					  (reset-blacklist)
					  (fg/load-default-theme)))

(setq terminal-theme (getenv "ITERM_THEME"))

(setq fg/default-theme 'mustard)

(setq solarized-dark-blacklisted-themes '(anti-zenburn base16-greenscreen base16-mocha base16-monokai base16-ocean base16-railscasts base16-solarized basic birds-of-paradise-plus brin cherry-blossom colonoscopy dark-krystal django espresso flatui fogus gandalf graham granger heroku hipster inkpot ir-black lavender leuven light-soap mccarthy mellow obsidian occidental odersky oldlace organic-green phoenix-dark-pink planet professional ritchie soft-morning soft-stone solarized-dark solarized-light soothe stekene-light subatomic sunny-day tango-plus))
(setq solarized-light-blacklisted-themes '())
(setq tango-dark-blacklisted-themes '())
(setq tango-light-blacklisted-themes '())

(setq fg/blacklisted-themes-list (cond ((equal "solarized-dark" terminal-theme) solarized-dark-blacklisted-themes)
				       ((equal "solarized-light" terminal-theme) solarized-light-blacklisted-themes)
				       ((equal "tango-dark" terminal-theme) tango-dark-blacklisted-themes)
				       ((equal "tango-light" terminal-theme) tango-light-blacklisted-themes)))

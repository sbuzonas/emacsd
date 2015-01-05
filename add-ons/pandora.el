(defvar pianobar-executable "pianobar"
  "Executable name for pianobar Pandora command line client.")

(when (executable-find pianobar-executable)
  (defaddon pandora
    "Pandora radio interface."
    (autoload 'pianobar "pianobar" nil t)

    (global-set-key (kbd "<f9>") 'pianobar-play-or-pause)
    
    (defun fg/start-pianobar ()
      (pianobar))

    (when fg/pandora-at-startup
      (add-hook 'after-pandora-addon-hook 'fg/start-pianobar))))

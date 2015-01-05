(defvar pianobar-executable "pianobar"
  "Executable name for pianobar Pandora command line client.")

(when (executable-find pianobar-executable)
  (defaddon pandora
    "Pandora radio interface."
    (autoload 'pianobar "pianobar" nil t)))
  

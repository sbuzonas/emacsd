(defgroup nomad-pandora nil
  "Pandora settings"
  :group 'nomad)

(defcustom pianobar-executable "pianobar"
  "The name of the pianobar executable including path if it is not in the path"
  :type 'string
  :group 'nomad-pandora)

(defcustom pandora-at-startup nil
  "Start Pandora radio when emacs starts"
  :type 'boolean
  :group 'nomad-pandora)

(when (executable-find pianobar-executable)
  (defaddon pandora
    "Pandora radio interface."
    (autoload 'pianobar "pianobar" nil t)

    (unless (boundp 'before-pandora-hooks)
      (setq before-pandora-hooks '()))
    
    (defun nomad/start-pianobar ()
      (run-hooks 'before-pandora-hooks)
      (pianobar))

    (when pandora-at-startup
      (add-hook 'after-pandora-addon-hook 'nomad/start-pianobar))))

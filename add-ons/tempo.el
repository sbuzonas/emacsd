(defaddon tempo
  "Customizes tempo completion framework."
  (fg/require-package 'tempo)
  
  (defun tempo-complete (prompt completions match-required &optional save-name no-insert)
    "Do whatever `temp-insert-prompt' does, but use completing-read."
    (flet ((read-string (prompt)
			(completing-read prompt completions match-required)))
      (tempo-insert-prompt prompt save-name no-insert))))

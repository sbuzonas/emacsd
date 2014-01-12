(defcustom addon-guide-key
  nil
  "Enable guide-key features"
  :type 'boolean
  :group 'features)

(when addon-guide-key
  (progn
    (add-to-list 'my-default-packages 'guide-key)
    (try-require 'guide-key)))

(after 'guide-key
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

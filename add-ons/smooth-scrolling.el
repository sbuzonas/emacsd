(defcustom addon-smooth-scrolling
  nil
  "Enable smooth-scrolling features"
  :type '(bool)
  :group 'features)

(when addon-smooth-scrolling
  (progn
    (add-to-list my-default-packages 'smooth-scrolling)
    (with-library 'smooth-scrolling nil)))

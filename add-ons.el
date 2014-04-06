;; Define the extensions group for Custom to allow extensions to be toggled on and off
(defgroup addons nil
  "Addons the current configuration is aware of and whether or not they are enabled"
  :group 'emacs)

(defmacro defaddon (symbol doc &optional require &rest forms)
  (if require
      (progn
	`(defcustom ,symbol
	   nil
	   ,doc
           :set (lambda (symbol value)
		  (when value
		    `(funcall `(function ,(lambda () @forms)))))
	   :type 'boolean
	   :group 'addons))
    (progn
      `(defcustom ,symbol
	 nil
	 ,doc
           :set (lambda (symbol value)
		  (when value
		    `(funcall `(function ,(lambda () @forms))))
                  value)
	 :type 'boolean
	 :group 'addons))))

(load-directory (expand-file-name "add-ons" user-emacs-directory))

(provide 'add-ons)

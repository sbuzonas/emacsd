(defgroup addons nil
  "Emacs addons."
  :group 'emacs)

(defun fg/intern-addon-symbols (string-or-symbol)
  (let* ((name (if (symbolp string-or-symbol)
		   (symbol-name string-or-symbol)
		 (string-or-symbol)))
	 (short-name (if (string-suffix-p "-addon" name)
			 (substring name 0 -6)
		       name))
	 (long-name (concat short-name "-addon"))
	 (custom-symbol (intern long-name))
	 (addon-symbol (intern short-name)))
    (cons addon-symbol custom-symbol)))

(defun fg/customize-addon-symbol (string-or-symbol)
  (cdr (fg/intern-addon-symbols string-or-symbol)))

(defun fg/internal-addon-symbol (string-or-symbol)
  (car (fg/intern-addon-symbols string-or-symbol)))

(defun fg/get-addon-status (addon)
  (cdr (assoc (fg/internal-addon-symbol addon) fg-installed-addons)))

(defun fg/set-addon-status (addon status)
  (let* ((symbol (fg/internal-addon-symbol addon))
	 (custom-symbol (fg/customize-addon-symbol addon))
	 (current-status (assoc symbol fg-installed-addons)))
    (delete current-status fg-installed-addons)
    (put custom-symbol 'saved-value (list (custom-quote status)))
    (custom-save-all)
    (add-to-list 'fg-installed-addons (cons symbol status))))

(defun fg/addon-has-setting-p (addon)
  (let ((symbol (fg/customize-addon-symbol addon)))
    (or (get symbol 'customized-value)
	(get symbol 'saved-value))))

(defmacro defaddon (symbol description &rest forms)
  (progn
    (unless (fg/addon-has-setting-p symbol)
      (let ((status (if (y-or-n-p (concat "Enable the '" (symbol-name symbol) "' addon? ")) 1 -1))
	    (custom-symbol (fg/customize-addon-symbol symbol)))
	(fg/set-addon-status symbol status)))
    (add-hook 'after-init-hook `(lambda ()
				    (let ((message (concat "Loading " (symbol-name (fg/internal-addon-symbol ',symbol)) " addon")))
				      (message "%s..." message)
				      (when (eq (fg/get-addon-status ',symbol) 1)
					,@forms)
				      (message "%s...done." message))))
    `(defcustom ,(fg/customize-addon-symbol symbol)
       nil
       ,description
       :set 'fg/set-addon-status
       :get 'fg/get-addon-status
       :initialize 'custom-initialize-reset
       :type '(choice (const :tag "Enabled" 1)
		      (const :tag "Disabled" -1))
       :group 'addons)))

(unless (boundp 'fg-installed-addons)
  (setq fg-installed-addons '()))

(defun fg/installed-addons ()
  (mapcar 'car fg-installed-addons))
(defalias 'installed-addons 'fg/installed-addons)

(defun fg/enabled-addons ()
  (mapcar 'car (delq nil (mapcar (lambda (x) (and (cdr x) (eq (cdr x) 1) x)) fg-installed-addons))))
(defalias 'enabled-addons 'fg/enabled-addons)

(defun fg/disabled-addons ()
  (mapcar 'car (delq nil (mapcar (lambda (x) (and (not (cdr x)) x)) fg-installed-addons))))
(defalias 'disabled-addons 'fg/disabled-addons)

(fg/load-directory addons-dir)

(provide 'fg-addons)

(defgroup addons nil
  "Nomad extensions"
  :group 'nomad)

(defcustom nomad-addons-dir (expand-file-name "addons" load-emacs-dir)
  "The directory with optional extensions that can be toggled"
  :type 'file
  :group 'nomad)

(defcustom nomad-config-dir (expand-file-name "conf" load-emacs-dir)
  "The directory with configuration files for addons"
  :type 'file
  :group 'nomad)

(unless (file-exists-p nomad-addons-dir)
  (make-directory nomad-addons-dir))

(defun nomad/addon-intern-symbols (string-or-symbol)
  (let* ((name (if (symbolp string-or-symbol)
		   (symbol-name string-or-symbol)
		 string-or-symbol))
	 (short-name (if (string-suffix-p "-addon" name)
			 (substring name 0 -6)
		       name))
	 (long-name (concat short-name "-addon"))
	 (custom-symbol (intern long-name))
	 (addon-symbol (intern short-name)))
    (cons addon-symbol custom-symbol)))

(defun nomad/addon-customize-symbol (string-or-symbol)
  (cdr (nomad/addon-intern-symbols string-or-symbol)))

(defun nomad/addon-internal-symbol (string-or-symbol)
  (car (nomad/addon-intern-symbols string-or-symbol)))

(defun nomad/addon-get-status (addon)
  (message "%S" (nomad/addon-internal-symbol addon))
  (cdr (assoc (nomad/addon-internal-symbol addon) nomad-installed-addons)))

(defun nomad/addon-set-status (addon status)
  (let* ((symbols (nomad/addon-intern-symbols addon))
	 (internal-symbol (car symbols))
	 (customize-symbol (cdr symbols))
	 (current-status (assoc symbol nomad-installed-addons)))
    (delete current-status nomad-installed-addons)
    (put customize-symbol 'saved-value (list (custom-quote status)))
    (custom-save-all)
    (add-to-list 'nomad-installed-addons (cons internal-symbol status))))

(defun nomad/addon-has-setting-p (addon)
  (let ((symbol (nomad/addon-customize-symbol addon)))
    (or (get symbol 'customized-value)
	(get symbol 'saved-value))))

(defmacro defaddon (symbol description &rest forms)
  (progn
    (unless (nomad/addon-has-setting-p symbol)
      (let ((status (if (yes-or-no-p (format "Enable the '%s' addon? " (symbol-name symbol))) 1 -1)))
	(nomad/addon-set-status symbol status)))
    (let ((hook-name (intern (format "after-%s-addon-hook" (nomad/addon-internal-symbol symbol)))))
      (set hook-name '())
      (add-hook 'after-init-hook `(lambda ()
				    (let ((loading-message (format "Loading %s addon..." (symbol-name (nomad/addon-internal-symbol ',symbol)))))
				      (message loading-message)
				      (when (eq (nomad/addon-get-status ',symbol) 1)
					(nomad/include (expand-file-name (format "%s.el" ',symbol) nomad-config-dir))
					,@forms
					(run-hooks ',hook-name))))))
    `(defcustom ,(nomad/addon-customize-symbol symbol)
       nil
       ,description
       :set 'nomad/addon-set-status
       :get 'nomad/addon-get-status
       :initialize 'custom-initialize-reset
       :type '(choice (const :tag "Enabled" 1)
		      (const :tag "Disabled" -1))
       :group 'nomad-addons)))

(unless (boundp 'nomad-installed-addons)
  (setq nomad-installed-addons '()))

(defun nomad/installed-addons ()
  (mapcar 'car nomad-installed-addons))
(defalias 'installed-addons 'nomad/installed-addons)

(defun nomad/enabled-addons ()
  (mapcar 'car (delq nil (mapcar
			  (lambda (x)
			    (and
			     (cdr x)
			     (eq (cdr x) 1) x))
			  nomad-installed-addons))))
(defalias 'enabled-addons 'nomad/enabled-addons)

(defun nomad/disabled-addons ()
  (mapcar 'car (delq nil (mapcar
			  (lambda (x)
			    (and
			     (not (cdr x))
			     x))
			  nomad-installed-addons))))
(defalias 'disabled-addons 'nomad/disabled-addons)

(nomad/load-directory nomad-addons-dir)

(provide 'nomad-addons)

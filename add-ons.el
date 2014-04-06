;; Define the extensions group for Custom to allow extensions to be toggled on and off
(defgroup addons nil
  "Addons the current configuration is aware of and whether or not they are enabled"
  :group 'emacs)

(defmacro defaddon (symbol require &rest forms)
  (let ((args '(:type 'boolean :group 'addons))
        req)
    `,(if require
          (progn
            (setq req (list :require require))))
    `(defcustom ,symbol
       nil
       ,(format "Functionality for %s" symbol)
       :set (lambda (symbol value)
              `(funcall (message "hello"))
              ;; Return the new value
              value)
       ,@(append args req))))
;;  (if require
;;      (progn
;;	`(defcustom ,symbol
;;	   nil
;;	   ,doc
;;           :set (lambda (symbol value)
;;		  (when value
;;		    `(funcall `(function ,(lambda () @forms)))))
;;	   :type 'boolean
;;	   :group 'addons))
;;    (progn
;;      `(defcustom ,symbol
;;	 nil
;;	 ,doc
;;           :set (lambda (symbol value)
;;		  (when value
;;		    `(funcall `(function ,(lambda () @forms))))
;;                  value)
;;	 :type 'boolean
;;	 :group 'addons))))

;;(defaddon test
;;  'smex
;;  (message "Hello, world!"))

(load-directory (expand-file-name "add-ons" user-emacs-directory))

(provide 'add-ons)

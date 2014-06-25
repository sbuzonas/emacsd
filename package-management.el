(require 'package)
(setq package-enable-at-startup nil)

;; Add mepla to package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Add marmalade to package repos
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Add legacy elpa
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(defvar my-default-packages '() "A list of packages to ensure are installed at launch.")

(defun package-install-default-packages ()
  (condition-case nil
      (package-install-default-packages-no-fetch)

    (error
     (progn
       (message "Error installing packages, attempting to refresh list.")
       (package-refresh-contents)
       (package-install-default-packages-no-fetch)))))

(defun package-install-default-packages-no-fetch ()
  (dolist (p my-default-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  (dolist (l missing-packages-list)
    (condition-case err
        (progn
          (message "Attempting to load library `%s'..." l)
          (if (stringp l)
              (load-library l)
            (require l))
          (message "Found missing library `%s'..." l)
          (remove l missing-packages-list))
      (file-error
       (message "Could not locate library `%s'!" l)
       nil))))
      

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (add-to-list 'my-default-packages package)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun package-list-non-default-packages ()
  "Like `package-list-packages', but shows only the packages that
are installed and not in `my-default-packages'."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-default-packages))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(provide 'package-management)

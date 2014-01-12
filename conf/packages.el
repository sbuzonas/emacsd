(require 'package)

;; Add mepla to package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Add marmalade to package repos
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Add legacy elpa
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My Default Packages
(defvar my-default-packages
  '(smex
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun packages-install ()
  (dolist (p my-default-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nill, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require 'package)
(setq package-enable-at-startup nil)

(defvar nomad/required-packages '()
  "A list of packages that have been required.")

(defun nomad/require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package nomad/required-packages)
    (add-to-list 'nomad/required-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(setq package-archives
      (nconc
       '(("mepla" . "http://melpa.milkbox.net/packages/"))
;       '(("marmalade" . "http://marmalade-repo.org/packages/"))
       (when (< emacs-major-version 24)
	 '(("gnu" . "http://elpa.gnu.org/packages/")))
       package-archives))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(provide 'nomad-packages)

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      (nconc
       '(("melpa" . "http://melpa.milkbox.net/packages/"))
       '(("marmalade" . "http://marmalade-repo.org/packages/"))
       (when (< emacs-major-version 24)
	 '(("gnu" . "http://elpa.gnu.org/packages/")))
       package-archives))

(defvar fg/default-packages '(cus-edit+)
  "A list of packages to ensure are installed at launch.")

(defvar fg/mode-auto-install-alist
  '((clojure-mode . clojure-mode)
    (coffee-mode . coffee-mode)
    (css-mode . css-mode)
    (d-mode . d-mode)
    (dart-mode . dart-mode)
    (elixr-mode . elixr-mode)
    (erlang-mode . erlang)
    (feature-mode . feature-mode)
    (go-mode . go-mode)
    (groovy-mode . groovy-mode)
    (haml-mode . haml-mode)
    (haskell-mode . haskell-mode)
    (kivy-mode . kivy-mode)
    (LaTeX-mode . auctex)
    (less-css-mode . less-css-mode)
    (lua-mode . lua-mode)
    (markdown-mode . markdown-mode)
    (taureg-mode . taureg)
    (puppet-mode . puppet-mode)
    (php-mode . php-mode)
    (protobuf-mode . protobuf-mode)
    (pkgbuild-mode . pkgbuild-mode)
    (rust-mode . rust-mode)
    (sass-mode . sass-mode)
    (scala-mode . scala-mode2)
    (scss-mode . scss-mode)
    (slim-mode . slim-mode)
    (swift-mode . swift-mode)
    (textile-mode . textile-mode)
    (thrift-mode . thrift)
    (yaml-mode . yaml-mode)
    (dockerfile-mode . dockerfile-mode)))

(defun fg/packages-installed-p ()
  "Check if all packages in `fg/default-packages' are installed."
  (every #'package-installed-p fg/default-packages))

(defun fg/require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package fg/default-packages)
    (add-to-list 'fg/default-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun fg/require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'fg/require-package packages))

(defun fg/install-packages ()
  "Install all packages listed in `fg/default-packages'."
  (unless (fg/packages-installed-p)
    (with-temp-message "Refreshing package database..."
      (package-refresh-contents))
    (with-temp-message "Installing default packages..."
      (fg/require-packages fg/default-packages))))

(defun fg/list-non-default-packages ()
  "Like `package-list-packages', but shows only the packages that
are installed and not in `fg/default-packages'."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x fg-default-packages))
				   (not (package-built-in-p x))
				   (package-installed-p x)))
		  (mapcar 'car package-archive-contents))))

(defun fg/auto-install-mode (orig-fun list-var list &optional append compare-fn)
  (let ((res nil))
    (when (eq 'auto-mode-alist list-var)
      (let* ((replacement-list '())
	     (pattern (car list))
	     (mode (cdr list))
	     (package (cdr (assoc mode fg/mode-auto-install-alist))))
	(when (and package
		   (not (package-installed-p package)))
          (message "old list: %S" list)
	  (setq list (cons pattern `(lambda ()
				      (unless (package-installed-p ',package)
					(package-install ',package))
				      (,mode))))
	  (message "new list: %S" list))))
    (setq res (apply orig-fun list-var list append compare-fn))
    res))
(advice-add 'add-to-list :around #'fg/auto-install-mode)

(setq fg/mode-autoloads-initialized nil)

(defun fg/bootstrap-mode-installers ()
  (unless fg/mode-autoloads-initialized
    (with-temp-message "Bootstrapping predefined autoloads with installers..."
      (let ((fg/autoloads auto-mode-alist))
	(setq auto-mode-alist '())
	(dolist (element fg/autoloads)
	  (add-to-list 'auto-mode-alist element)))
      (message "%s" "Bootstrapping predefined autoloads with installers...done.")
      (setq fg/mode-autoloads-initialized t))))
  
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(fg/bootstrap-mode-installers)
(fg/install-packages)

(provide 'fg-packages)

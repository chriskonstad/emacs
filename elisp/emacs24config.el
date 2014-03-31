;;Setup the package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
	 
;;Sync packages
(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(magit
    auto-complete
    auto-complete-c-headers
    yasnippet)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;;Enable autocomplete if installed via package
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;Enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;Better theme in GUI versions of emacs 24
(when (and (>= emacs-major-version 24) (display-graphic-p))
 (require 'monokai-theme)
 (load-theme 'monokai t))

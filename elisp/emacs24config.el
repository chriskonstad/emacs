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
    auctex
    auto-complete
    auto-complete-c-headers
    auto-complete-clang
    exec-path-from-shell
    elpy
    json-mode
    js2-mode
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
(require 'auto-complete-clang)
(ac-config-default)
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-gtags))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(defun my:ac-webdev-init()
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers))
(add-hook 'html-mode-hook 'my:ac-webdev-init)
(add-hook 'javascript-mode-hook 'my:ac-webdev-init)
(setq ac-use-fuzzy t)

;;Use C-n/C-p to naviage autocomplete
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;;Ignore case
(setq ac-ignore-case t)

;;Enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;Enable AuxTeX
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

;;Better theme in GUI versions of emacs 24
(when (and (>= emacs-major-version 24) (display-graphic-p))
 (require 'monokai-theme)
 (load-theme 'monokai t))

;;Fix the PATH variable for the GUI on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

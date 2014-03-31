;;Add plugin directory
(add-to-list 'load-path "~/elisp")

;;Hide the splash screen
(setq inhibit-splash-screen t)

(when (>= emacs-major-version 24)
  (load-library "emacs24config"))

;;Disable the toolbar
(when (display-graphic-p)
  (tool-bar-mode -1))

;;Smooth scrolling
(global-set-key "\M-n" '"\C-u2\C-v")
(global-set-key "\M-p" '"\C-u2\M-v")
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)

;;Add line numbers
(require 'linum)
(add-hook 'c-mode-common-hook (lambda () (linum-mode 1)))

;;Add the makefile skeleton
(load-library "makefile-skeleton")

;;Speedbar
(require 'sr-speedbar)
;;Use just ASCII, no images
(setq speedbar-use-images nil)
;;Open at startup
;;(sr-speedbar-open)
;;Fix the size even during resize
;;(with-current-buffer sr-speedbar-buffer-name
  ;;(setq window-size-fixed 'width))

;;Show file size
(size-indication-mode)

;;Turns on syntax highlighting
(global-font-lock-mode t)

;;Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;;Max colorization
(setq font-lock-maximum-decoration t)

;;Add auto-pairing
(require 'autopair)
(add-hook 'c-mode-common-hook (lambda () (autopair-mode 1)))

;;Google Nav
(add-to-list 'load-path "~/elisp/emacs-nav-49/")
(require 'nav)
(nav-disable-overeager-window-splitting)
;;Setup a global quick key for nav
(global-set-key [f8] 'nav-toggle)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;;Better multiwindow navigation
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)

;;Open up Header files as C++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;Open up PHP files with HTML mode
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))

;;Google C++ Style guidlines
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;Qt setup
(setenv "QTDIR" "/Users/chris/Qt/5.1.0/clang_64/")
(setq qt-base-directory "/Users/chris/Qt/")
(setenv "PATH" (concat (concat (getenv "QTDIR") "bin" ) ";" (getenv "PATH")))
(setenv "PATH" (concat (concat qt-base-directory "clang_64/bin") ";" (getenv "PATH")))
(setenv "PATH" (concat (concat qt-base-directory "5.1.0/clang_64/lib") ";" (getenv "PATH")))
(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))

;; syntax-highlighting for Qt
;; (based on work by Arndt Gulbrandsen, Troll Tech)
(defun jk/c-mode-common-hook ()
"Set up c-mode and related modes.

Includes support for Qt code (signal, slots and alikes)."

;; base-style
   (c-set-style "google-c-style")
   ;; set auto cr mode
   ;;(c-toggle-auto-hungry-state 1)
 
   ;; qt keywords and stuff ...
   ;; set up indenting correctly for new qt kewords
   (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                  "\\|protected slot\\|private\\|private slot"
                                  "\\)\\>")
         c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                  "\\|public slots\\|protected slots\\|private slots"
                                  "\\)\\>[ \t]*:"))
   (progn
     ;; modify the colour of slots to match public, private, etc ...
     (font-lock-add-keywords 'c++-mode
                             '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
     ;; make new font for rest of qt keywords
     (make-face 'qt-keywords-face)
     (set-face-foreground 'qt-keywords-face "BlueViolet")
     ;; qt keywords
     (font-lock-add-keywords 'c++-mode
                             '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
     (font-lock-add-keywords 'c++-mode
                             '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
     (font-lock-add-keywords 'c++-mode
                             '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
     ))
 (add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)


;; Other things I like are, for example,

 ;; cc-mode
 ;;(require 'cc-mode)
 
 ;; automatic indent on return in cc-mode
 ;;(define-key c-mode-base-map [RET] 'newline-and-indent)
 
 ;; Do not check for old-style (K&R) function declarations;
 ;; this speeds up indenting a lot.
 (setq c-recognize-knr-p nil)
 
 ;; Switch fromm *.<impl> to *.<head> and vice versa
 (defun switch-cc-to-h ()
   (interactive)
   (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
     (let ((name (match-string 1 buffer-file-name))
	     (suffix (match-string 2 buffer-file-name)))
       (cond ((string-match suffix "c\\|cc\\|C\\|cpp")
	           (cond ((file-exists-p (concat name ".h"))
			      (find-file (concat name ".h"))
			         )
			    ((file-exists-p (concat name ".hh"))
			         (find-file (concat name ".hh"))
				    )
			        ))
	         ((string-match suffix "h\\|hh")
		       (cond ((file-exists-p (concat name ".cc"))
			          (find-file (concat name ".cc"))
				     )
			        ((file-exists-p (concat name ".C"))
				     (find-file (concat name ".C"))
				        )
				   ((file-exists-p (concat name ".cpp"))
				        (find-file (concat name ".cpp"))
					   )
				      ((file-exists-p (concat name ".c"))
				           (find-file (concat name ".c"))
					      )))))))


;(setq-default c-basic-offset 4)
;(setq c-default-style "linux" c-basic-offset 4)
;(setq-default c-basic-offset 4
;	      tab-width 4
;	      indent-tabs-mode nil)
;(require 'cc-mode)
;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

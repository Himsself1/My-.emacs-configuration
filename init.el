(setq inhibit-startup-message t ;;Doesn't display startup message
      visible-bell t ;;Doesn't work in terminal
      use-dialog-box nil) ;;Disables graphical windows that may pop up

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (set-fringe-mode 10)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)

(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-pyright origami all-the-icons ess lsp-mode helpful ivy-rich which-key rainbow-delimiters mood-line doom-modeline counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Adds line numbers except in case of eshell
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshel-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package nerd-icons)  

;; Completion of emacs specific tasks
(use-package ivy 
  :diminish
  :bind ("C-s". swiper)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-wrap t
	ivy-count-format "(%d/%d) "
	enable-recursive-minibuffers t))

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil) ;; Doesn't start searches with ^
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-b" . ibuffer-list-buffers))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; M-x menu comes with documentation
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; More helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable]. counsel-describe-variable)
   ([remap describe-key] . helpful-key))
  :init
  (counsel-load-theme-action "misterioso"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Need to install language interpreter for each language
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefic "C-c l")
  :bind
  (("C-c d" . lsp-describe-thing-at-point)
   ("C-c a" . lsp-execute-code-action))
  :hook
  ((python-mode . lsp-deferred)
   (ess-r-mode . lsp-deferred)
   (lsp-mode . efs/lsp-mode-setup))
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :diminish
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp-mode
  :config
  (setq company-idle-delay 0.05
	company-minimum-prefix-length 1)
  (company-keymap--unbind-quick-access company-active-map) ;; Disables M-# from selecting stuff on company minimap
  :hook
  (emacs-lisp-mode . (lambda()
		       (setq-local company-backends '(company-elisp))))
  (emacs-lisp-mode . company-mode)
  (lsp-mode . company-mode)
  :bind(:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common)
	:map company-mode-map
	("C-/" . company-search-filtering))
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package ess
  ;; :load-path "/usr/share/emacs/site-lisp/ess"
  :mode "\\.[rR]\\'"
  :init
  (require 'ess-site)  
  :bind
  (("M--" . " <- "))
  :commands R
  )

;; Change windows intuitively 
(use-package winum
  :config
  (winum-mode)
  :bind
  (("M-1" . 'winum-select-window-1)
   ("M-2" . 'winum-select-window-2)
   ("M-3" . 'winum-select-window-3)
   ("M-4" . 'winum-select-window-4)
   ("M-5" . 'winum-select-window-5)
   ("M-6" . 'winum-select-window-6)
   ("M-7" . 'winum-select-window-7)
   ("M-8" . 'winum-select-window-8)
   ("M-/" . 'winum-select-window-by-number))
  )

(use-package python-mode
  :mode "\\.py\\'" 
  :hook (python-mode . lsp-deferred)
  :config (setq python-shell-interpreter "python3"
		;; python-shell-interpreter-args "console --simple-prompt"
		)
  )

(use-package origami
  :hook (prog-mode . origami-mode)
  )

(use-package lsp-pyright
  :after python-mode
  :config (require 'lsp-pyright))

;; Following setting modifies the fontline
;; This took a good deal of tinkering to set up
;; Steps to reproduce:
;; 1) run nerd-icons-install-fonts in emacs. find the name of font with 'fc-list | grep .local'
;; 2) open dconf-editor and search for "font" in search bar. terminal profiles will pop up
;; 3) copy the name of the font above to the 'font' menu
;; 4) change terminal profile, then change back
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config

  ;; (setq doom-modeline-bar-width 10) ;; Only usefull in gui
  ;; (setq doom-modeline-height 1) ;; Only usefull in gui

  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-line-numbers-style 'relative)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq find-file-visit-truename t))


(use-package magit)

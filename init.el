;;; Remove meny bar and stop message

(setq inhibit-startup-message t ;;Doesn't display startup message
      visible-bell t ;;Doesn't work in terminal
      use-dialog-box nil) ;;Disables graphical windows that may pop up

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; HOW DID I NOT KNOW ABOUT THIS?!?!?!
(xterm-mouse-mode 1)

;; (set-fringe-mode 10)
;;; Melpa and package

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)

(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-pyright origami all-the-icons ess lsp-mode helpful ivy-rich which-key rainbow-delimiters mood-line doom-modeline counsel ivy use-package)))
(custom-set-faces
 '(region ((t :extend nil)))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Configuring use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Adds line numbers except in case of eshell

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshel-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))
(global-hl-line-mode 1)

;;; Auto Update

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;;; Automatically update file that was modified elsewhere

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  )

;;; Install fonts and all-the-icons

(use-package nerd-icons)

(use-package all-the-icons)
;; Also run M-x all-the-icons-install-fonts

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode 1)
  )

;;; Completion of emacs specific tasks

(use-package ivy 
  :diminish
  :bind ("C-s". swiper)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-wrap t
	ivy-count-format "(%d/%d) "
	enable-recursive-minibuffers t))

;;; Functions and utilities integrated with ivy.

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil) ;; Doesn't start searches with ^
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-b" . ibuffer-list-buffers))
  )

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; M-x menu comes with documentation.

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after (ivy-rich-mode))

(use-package all-the-icons-dired
  :after (dired-mode))

;;; Popup that lists all available shortcuts.

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;;; More helpful descriptions.

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
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-region '(accented bg-only)
	modus-themes-paren-match '(bold intense)
	modus-themes-syntax '(yellow-comments)
	modus-themes-headings '((1 . (rainbow extrabold 1.3))
				(2 . (rainbow semibold 1.2))
				(3 . (rainbow 1.1)))
	modus-themes-scale-headings t
	modus-themes-completions '(selection .(rainbow background))
	)
  (counsel-load-theme-action "modus-vivendi"))

;;; Setting up tabs

;; Carefull! Centaur tabs interferes with mouce support on terminal
;; `centaur-tabs-local-mode` toggles the package in current buffer
;; allowing for mouce resizing

(use-package centaur-tabs
  :demand
  :init
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "wave"
	centaur-tabs-set-icons t
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t)
  :bind
  ([C-M-left] . centaur-tabs-backward-tab)
  ([C-M-right] . centaur-tabs-forward-tab)
  ("C-c n" . centaur-tabs--create-new-tab)
  ("C-c t" . centaur-tabs-local-mode)
  )

;;; Auto completion with company

(use-package company
  :config
  (setq company-idle-delay 0.05
	company-minimum-prefix-length 1)
  (company-keymap--unbind-quick-access company-active-map) ;; Disables M-# from selecting stuff on company minimap
  :hook
  (emacs-lisp-mode . (lambda()
		       (setq-local company-backends '(company-elisp))))
  (emacs-lisp-mode . company-mode)
  (lsp-mode . company-mode)
  :bind(:map prog-mode-map
	     ("<tab>" . company-indent-or-complete-common)
	     :map company-mode-map
	     ("C-/" . company-search-filtering))
  )

;; Front end customizations for company-mode
(use-package company-box
  :hook
  (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;;; Configuring Language Server Protocol

;; Adds a line on the top of the screen that tells you where you are
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Need to install language interpreter for each language
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefic "C-c k")
  :bind
  (("C-c d" . lsp-describe-thing-at-point)
   ("C-c a" . lsp-execute-code-action))
  :hook
  ((python-mode . lsp-deferred)
   (ess-mode . lsp)
   (cperl-mode . lsp-deferred)
   (lsp-mode . efs/lsp-mode-setup))
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package tree-sitter
  :config
  (tree-sitter-hl-mode 1)
  :custom
  (treesit-font-lock-level 4)
  )

(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package lsp-ui
  :diminish
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :after lsp)

;;;; Python communication with the language server

(use-package lsp-pyright
  :after python-mode
  :config (require 'lsp-pyright))

;;; Interactive shell for R

(use-package ess
  :load-path "/usr/share/emacs/site-lisp/elpa/ess-18.10.3snapshot"
  :mode(
        ("/R/.*\\.q\\'"       . R-mode)
        ("\\.[rR]\\'"         . R-mode)
        ("\\.[rR]profile\\'"  . R-mode)
        ("NAMESPACE\\'"       . R-mode)
        ("CITATION\\'"        . R-mode)
        ("\\.[Rr]out"         . R-transcript-mode)
        ("\\.Rd\\'"           . Rd-mode)
        )
  :init
  (require 'ess-site)
  ;; :hook (ess-mode . lsp)
  :hook
  (inferior-ess-mode . (lambda()
			 (setq-local ansi-color-for-comint-mode 'filter)))
  :bind
  ("M--" . " <- ")
  :custom
  ((ess-r-backend 'lsp)
   (ess-style 'RStudio)
   (ess-auto-width 'window))
  :commands
  ( R )
  )

(use-package tree-sitter-ess-r
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate))


;;; Change windows intuitively 

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

;;; Interactive shell and other utilities for python

(use-package python-mode
  :mode "\\.py\\'" 
  :hook (python-mode . lsp-deferred)
  :bind (:map python-mode-map
	      ( "C-c C-c" . python-shell-send-paragraph-and-step ))
  :init (setq python-shell-interpreter "ipython3"
	      python-shell-interpreter-args "--simple-prompt -i --pprint --colors=LightBG")
  )

;;; Set up cperl

(use-package cperl-mode
  :mode "\\.(pl|perl)\\'"
  :hook (cperl-mode . lsp-deferred)
  )

;;; Following setting modifies the modeline

;; This took a good deal of tinkering to set up
;; Steps to reproduce:
;; 1) run nerd-icons-install-fonts in emacs. find the name of font with 'fc-list | grep .local'
;; 2) open dconf-editor and search for "font" in search bar. terminal profiles will pop up
;; 3) copy the name of the font above to the 'font' menu
;; 4) change terminal profile, then change back
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-line-numbers-style 'relative)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq find-file-visit-truename t))

;;; Magit

(use-package magit
  :bind("C-c C-m" . magit-status) )

;;; usefull python functions

(use-package python-x
  :after python-mode
  :hook (python-mode-hook . python-x))

;;; Highlight Indentation and other indentation utilities

(defun my-highlight-guides-faces ()
  ;; Custom functiuon that sets indent guides colors so that they match modis-vivendi
  (set-face-background 'highlight-indent-guides-odd-face "darkslateblue")
  (set-face-background 'highlight-indent-guides-even-face "darkslateblue")
  (set-face-background 'highlight-indent-guides-top-odd-face "thistle4")
  (set-face-background 'highlight-indent-guides-top-even-face "thistle4"))

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (highlight-indent-guides-mode . my-highlight-guides-faces)
  :config
  (setq highlight-indent-guides-auto-enables nil
	highlight-indent-guides-method 'column
	;; highlight-indent-guides-character ?\|
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-delay 0
	)
  )

(use-package aggressive-indent
  ;; Doesn't allow you to escape indentation
  :config
  (aggressive-indent-mode 1)
  )

(use-package apheleia
  ;; code reformating on save
  :config
  (apheleia-global-mode 1)
  )

;;; Use outli for headers

;; Headers for non-lisp languages are [comment-start + space + *]
(use-package outli
  :load-path "~/.emacs.d/outli"
  :hook( ((prog-mode text-mode) . outli-mode)
         ((prog-mode text-mode) . outline-minor-mode) )
  :bind( ([M-down] . outline-next-heading)
	 ([M-up] . outline-previous-heading)
	 ))

(use-package imenu-list
  :bind
  (("C-c l" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil
	imenu-list-size 0.25
	imenu-list-position 'left))

;;; Dired functionality

(use-package dired-sidebar
  :bind
  ("C-c s" . dired-sidebar-toggle-sidebar)
  :custom
  ((dired-sidebar-theme 'vscode)
   (dired-sidebar-use-term-integration t))
  :commands (dired-sidebar-toggle-sidebar))

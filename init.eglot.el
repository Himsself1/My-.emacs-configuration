;;; Disable menu bar scroll bar and tool bar

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; Use straight.el as the package manager

;; Disable the default package.el
(setq package-enable-at-startup nil)

;; Bootstrap script from straight.el devs
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Every use-package call will be handled by straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; (package-initialize)

;; (unless package-archive-contents
;;   (package-refresh-contents))

;;; Configuring use-package

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

;;; xterm-mouse-mode toggle

(global-set-key [f4] 'xterm-mouse-mode)

;;; Adds line numbers except in case of eshell

(column-number-mode)

(use-package display-line-numbers
  :custom
  (display-line-numbers-update-width 0)
  :config
  (global-display-line-numbers-mode 1))

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshel-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode -1))))
(global-hl-line-mode 1)


;;; Automatically update file that was modified elsewhere

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  )

;;; Inst'all fonts and all-the-icons

(use-package nerd-icons)

(use-package all-the-icons)
;; Also run M-x all-the-icons-install-fonts

(use-package all-the-icons-completion)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Vertico, Marginalia, Consult, Embark

(use-package vertico
  :custom
  (vertico-count 10) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle 1) ; Go from last to first candidate and first to last (cycle)?
  :init
  (vertico-mode)
  (all-the-icons-completion-mode 1))

(use-package marginalia
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :custom
  (marginalia-align 'left)
  :config
  (marginalia-mode))

(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
	 ("C-s" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("C-x b" . consult-buffer)))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;; (use-package swiper
;;   :bind ( "C-s" . swiper ))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult)

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;;; Corfu and add-ons

(use-package corfu
  :bind
  (:map corfu-map
	("<escape>" . corfu-quit)
	("<return>" . corfu-insert)
	("C-d" . corfu-show-documentation))
  :custom
  (corfu-auto 1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-echo-mode)
  (corfu-min-width 40)
  (corfu-quit-no-match t)
  (corfu-echo-documentation t)
  :config
  (corfu-mode 1)
  )

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(use-package cape
  :after (corfu-mode)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

;;; Company, for when corfu doesn't work

;; (use-package company
;;   :config
;;   (setq company-idle-delay 0.05
;; 	company-minimum-prefix-length 1)
;;   (company-keymap--unbind-quick-access company-active-map) ;; Disables M-# from selecting stuff on company minimap
;;   ;; :hook
;;   ;; (emacs-lisp-mode . (lambda()
;;   ;; 		       (setq-local company-backends '(company-elisp))))
;;   ;; (emacs-lisp-mode . company-mode)
;;   ;; (lsp-mode . company-mode)
;;   :bind(:map company-mode-map
;; 	     ("<tab>" . company-indent-or-complete-common)
;; 	     ("C-/" . company-search-filtering))
;;   )

;; Front end customizations for company-mode
;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode)
;;   :init
;;   (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;;; Popup that lists all available shortcuts.

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))


;;; Modus themes, Ef themes and Doom modeline

(use-package modus-themes
  :init
  (setq ;; modus-themes-mode-line '(accented borderless)
   modus-themes-region '(accented bg-only)
   modus-themes-paren-match '(bold intense)
   modus-themes-syntax '(yellow-comments)
   modus-themes-headings '((1 . (rainbow extrabold 1.3))
			   (2 . (rainbow semibold 1.2))
			   (3 . (rainbow 1.1)))
   modus-themes-scale-headings t
   modus-themes-completions '(selection .(rainbow background))
   modus-operandi-tinted-palette-overrides
   '((bg-main "#efe9e9")
     (bg-dim "#c9c9c9"))
   ))

(use-package ef-themes
  :bind
  ("C-R" . ef-themes-load-random)
  :init
  (consult-theme 'ef-bio)
  ;; (counsel-load-theme-action "modus-operandi")
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
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-height 40)
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
  :straight '(outli
	      :type git
	      :host github
	      :repo "jdtsmith/outli")
  ;; :load-path "~/.emacs.d/outli"
  :hook(
	((prog-mode tex-mode) . outli-mode)
	(outli-mode . global-reveal-mode))
  :bind(
	([M-down] . outline-next-heading)
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
  ((dired-sidebar-theme 'icons)
   (dired-sidebar-use-term-integration t)
   (dired-sidebar-window-fixed 0)
   (dired-sidebar-use-custom-modeline 0)
   ;; (dired-sidebar-display-remote-icons 1)
   )
  ;;  :config
  ;; (all-the-icons-dired-mode 1)
  )

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

;;; Programming Packages

;;; Yasnippet

(use-package yasnippet)

(use-package consult-yasnippet)

;;; Eglot  Tree-Sitter and Tree-Sitter-Auto

(use-package eglot
  :bind(
	:map eglot-mode-map
	("C-c d" . eldoc)
	)
  )

;; Try eglot-x
(use-package eglot-x
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x")
  :after eglot
  :custom
  (eglot-x-enable-files 1)
  (eglot-x-setup)
  )

(use-package tree-sitter
  ;; :after eglot
  :hook
  (python-mode . tree-sitter-hl-mode)
  ;; :custom
  ;; ()
  ;; :config
  ;; (tree-sitter-hl-mode 1)
  ;; :custom
  ;; (treesit-font-lock-level 4)
  )

(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Lsp-bridge

;; (use-package lsp-bridge
;;   :hook
;;   (lsp-bridge-mode . (lambda ()
;; 		       (corfu-mode -1)
;; 		       (company-box-mode -1)
;; 		       (company-mode -1)))
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;; 			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;; 			 :build (:not compile))
;;   )

;;; ESS

(use-package ess
  ;; :load-path "/usr/share/emacs/site-lisp/elpa/ess-18.10.3snapshot"
  :mode(
        ("/R/.*\\.q\\'"       . R-mode)
        ("\\.[rR]\\'"         . R-mode)
        ("\\.[rR]profile\\'"  . R-mode)
        ("NAMESPACE\\'"       . R-mode)
        ("CITATION\\'"        . R-mode)
        ("\\.[Rr]out"         . R-transcript-mode)
        ("\\.Rd\\'"           . Rd-mode)
        )
  :bind
  ("M--" . " <- ")
  :custom
  (;; (ess-r-backend 'lsp)
   (ess-style 'RStudio)
   (ess-auto-width 'window))
  ;; :hook
  ;; (ess-mode . 'eglot-ensure)
  :commands
  ( R )
  )

(use-package tree-sitter-ess-r
  :hook (ess-r-mode . tree-sitter-ess-r-mode-activate))

;;; Python

(use-package python-mode
  :mode "\\.py\\'" 
  ;; :hook (python-mode . 'eglot-ensure)
  :bind (:map python-mode-map
	      ( "C-c C-c" . python-shell-send-paragraph-and-step ))
  :init (setq python-shell-interpreter "ipython3"
	      python-shell-interpreter-args "--simple-prompt -i --pprint --colors=LightBG"
	      ;; major-mode-remap-alist '((python-mode . python-ts-mode))
	      )
  )

;;; Cperl

(use-package cperl-mode
  :mode "\\.(pl|perl)\\'"
  ;; :hook (cperl-mode . 'eglot-ensure)
  )

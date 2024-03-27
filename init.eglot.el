;;; Disable menu bar scroll bar and tool bar

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Configuring use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;;; Vertico, Marginalia, Consult, Embark

(use-package vertico
  :custom
  (vertico-count 10) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle 1) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(use-package marginalia
  :custom
  (marginalia-align 'left)
  :init
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
  (global-corfu-mode 1)
  )

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

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
     (bg-dim "#c9c9c9")
     )
   )
  :config
  (consult-theme 'ef-autumn))

(use-package ef-themes
  :bind
  ("C-R" . ef-themes-load-random)
  :config
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
  ((dired-sidebar-theme 'icons)
   (dired-sidebar-use-term-integration t)
   (dired-sidebar-window-fixed 0)
   (dired-sidebar-use-custom-modeline 0)
   (dired-sidebar-display-remote-icons 1))
  :config
  (all-the-icons-dired-mode 1))

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

;;; Tree-Sitter and Tree-Sitter-Auto

(use-package eglot
  :bind(
	:map eglot-mode-map
	("C-c d" . eldoc)
	)
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
  :hook (cperl-mode . 'eglot-ensure)
  )

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
   '(ess lsp-mode helpful ivy-rich which-key rainbow-delimiters mood-line doom-modeline counsel ivy use-package)))
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
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-b" . ibuffer-list-buffers))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Doesn't start searches with ^

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
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable]. counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :init
  (counsel-load-theme-action "misterioso")
  )

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  (setq lsp-keymap-prefic "C-c l")
  :hook(
	(python-mode . lsp)
	(ess-r-mode . lsp)
	)
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package ess
  ;; :load-path "/usr/share/emacs/site-lisp/ess"
  :init (require 'ess-site)  ;; seems like this is needed to load the minor modes as well keybindings don't work without it
  ;; :mode (ess-r-mode . lsp-deferred)
  :hook (
         ((ess-r-mode inferior-ess-r-mode) . electric-layout-mode)
         ;; (ess-r-post-run . (lambda ()
         ;;    (ess-load-file (make-temp-file nil nil nil
         ;;                                "Sys.setenv(\"DISPLAY\"=\":0.0\")")))
         )
  :commands R
  )
;; Use hydra to make movewindow keybind prefix

;;; ...  -*- lexical-binding: t -*-
;;; Disable menu bar scroll bar and tool bar

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; Use straight.el as the package manager

;; Disable the default package.el
(setq package-enable-at-startup nil)

(setenv "PATH" (concat ":/usr/local/texlive/2025/bin/x86_64-linux:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/texlive/2025/bin/x86_64-linux")

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
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

(setq straight-use-package-by-default t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.org/packages/"))
      )

;; (package-initialize)

;; (unless package-archive-contents
;;   (package-refresh-contents))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Raise Garbage collection limit

(setq gc-cons-threshold 100000000)

;;; Better C-g from Prot's config

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Initial Emacs setup

;; Taken from emacs-kick github

;;;; Functions for caching info that TRAMP calls constantly. 
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      project-current-directory-override
                      default-directory)
                  'project-current-cache orig prompt directory))

(advice-add 'project-current :around #'memoize-project-current)

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)

;; memoize all git candidates in the current project
(defvar $counsel-git-cands-cache nil)
(defun $memoize-counsel-git-cands (orig dir)
  ($memoize-remote (magit-toplevel dir) '$counsel-git-cands-cache orig dir))
(advice-add 'counsel-git-cands :around #'$memoize-counsel-git-cands)

(use-package emacs
  :straight nil
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 80)                      ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  ;; Set up TRAMP configuration.
  ;; Taken from https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#fix-remote-compile
  (setq remote-file-name-inhibit-locks t
		tramp-use-scp-direct-remote-copying t
		remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
		tramp-verbose 2)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (setq magit-tramp-pipe-stty-settings 'pty)
  ;; Remove hooks that cause delays over TRAMP
  (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
  (remove-hook 'find-file-hook 'forge-bug-reference-setup)
  
  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.
  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.
  (global-hl-line-mode 1)      ;; Disable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (window-divider-mode t)
  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)
  )

;;;; Configure tab-line

(defun my/set-tab-theme ()
  (let ((bg (face-attribute 'mode-line :background))
        (fg (face-attribute 'default :foreground))
		(hg (face-attribute 'default :background))
        (base (face-attribute 'mode-line :background))
        (box-width (/ (line-pixel-height) 4)))
    (set-face-attribute 'tab-line nil
						:background base
						:foreground fg
						:height 0.8
						:inherit nil
						:box (list :line-width -1 :color base)
						)
    (set-face-attribute 'tab-line-tab nil
						:foreground fg
						:background bg
						:weight 'normal
						:inherit nil
						:box (list :line-width box-width :color bg))
    (set-face-attribute 'tab-line-tab-inactive nil
						:foreground fg
						:background base
						:weight 'normal
						:inherit nil
						:box (list :line-width box-width :color base))
    (set-face-attribute 'tab-line-highlight nil
						:foreground fg
						:background hg
						:weight 'normal
						:inherit nil
						:box (list :line-width box-width :color hg))
    (set-face-attribute 'tab-line-tab-current nil
						:foreground fg
						:background hg
						:weight 'normal
						:inherit nil
						:box (list :line-width box-width :color hg))))

(defun my/tab-line-name-buffer (buffer &rest _buffers)
  "Create name for tab with padding and truncation.
If buffer name is shorter than `tab-line-tab-max-width' it gets centered
with spaces, otherwise it is truncated, to preserve equal width for all
tabs.  This function also tries to fit as many tabs in window as
possible, so if there are no room for tabs with maximum width, it
calculates new width for each tab and truncates text if needed.  Minimal
width can be set with `tab-line-tab-min-width' variable."
  (with-current-buffer buffer
    (let* ((window-width (window-width (get-buffer-window)))
           (tab-amount (length (tab-line-tabs-window-buffers)))
           (window-max-tab-width (if (>= (* (+ tab-line-tab-max-width 3) tab-amount) window-width)
                                     (/ window-width tab-amount)
                                   tab-line-tab-max-width))
           (tab-width (- (cond ((> window-max-tab-width tab-line-tab-max-width)
                                tab-line-tab-max-width)
                               ((< window-max-tab-width tab-line-tab-min-width)
                                tab-line-tab-min-width)
                               (t window-max-tab-width))
                         3)) ;; compensation for ' x ' button
           (buffer-name (string-trim (buffer-name)))
           (name-width (length buffer-name)))
      (if (>= name-width tab-width)
          (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
        (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
               (buffer-name (concat padding buffer-name)))
          (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

(defun tab-line-close-tab (&optional e)
  "Close the selected tab.
If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing windows, kill
the buffer with `kill-buffer` function.  Lastly, if no tabs left in the
window, it is deleted with `delete-window` function."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
    (with-selected-window window
      (let ((tab-list (tab-line-tabs-window-buffers))
            (buffer-list (flatten-list
                          (seq-reduce (lambda (list window)
                                        (select-window window t)
                                        (cons (tab-line-tabs-window-buffers) list))
                                      (window-list) nil))))
        (select-window window)
        (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
            (progn
              (if (eq buffer (current-buffer))
                  (bury-buffer)
                (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                (set-window-next-buffers window (delq buffer (window-next-buffers))))
              (unless (cdr tab-list)
                (ignore-errors (delete-window window))))
          (and (kill-buffer buffer)
               (unless (cdr tab-list)
                 (ignore-errors (delete-window window)))))))))

(use-package tab-line
  :ensure nil
  :init (global-tab-line-mode 1)
  :config
  (defcustom tab-line-tab-min-width 10
    "Minimum width of a tab in characters."
    :type 'integer
    :group 'tab-line)
  (defcustom tab-line-tab-max-width 30
    "Maximum width of a tab in characters."
    :type 'integer
    :group 'tab-line)
  (setq tab-line-close-button-show t
        tab-line-new-button-show nil
        tab-line-separator ""
        tab-line-tab-name-function #'my/tab-line-name-buffer
        tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                          'keymap tab-line-right-map
                                          'mouse-face 'tab-line-highlight
                                          'help-echo "Click to scroll right")
        tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                         'keymap tab-line-left-map
                                         'mouse-face 'tab-line-highlight
                                         'help-echo "Click to scroll left")
        tab-line-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
                                          'keymap tab-line-tab-close-map
                                          'mouse-face 'tab-line-close-highlight
                                          'help-echo "Click to close tab"))
  (my/set-tab-theme)
  (dolist (mode '(ediff-mode process-menu-mode))
    (add-to-list 'tab-line-exclude-modes mode))
  )


;;; xterm-mouse-mode toggle

(global-set-key [f4] 'xterm-mouse-mode)

;;; Adds line numbers except in case of eshell

(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only 1)
  :config
  (global-display-line-numbers-mode 1)
  )

(dolist (mode '(org-mode-hook
				term-mode-hook
				eshel-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode -1))))


;;; Automatically update file that was modified elsewhere

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  )

;;; Install fonts and all-the-icons

(use-package nerd-icons)

(add-to-list 'default-frame-alist
             '(font . "Iosevka-13"))

;; (use-package all-the-icons)
;; Also run M-x all-the-icons-install-fonts

;; (use-package all-the-icons-completion
;;   :hook
;;   (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode)
;;   )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;;; Vertico, Marginalia, Consult, Embark

(use-package vertico
  :custom
  (vertico-count 10) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle 1) ; Go from last to first candidate and first to last (cycle)?
  :init
  (vertico-mode)
  )

(use-package marginalia
  ;; :hook
  ;; (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :custom
  (marginalia-align 'left)
  :init
  (marginalia-mode)
  )

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
         ("C-x b" . consult-buffer))
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  )

(use-package orderless
  :config
  (setq completion-styles '(basic orderless))
  )

;; (use-package swiper
;;   :bind ( "C-s" . swiper ))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  )

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
          ("C-c C-c" . wgrep-finish-edit))
  )

;;; Corfu and add-ons

(use-package corfu
  :straight t
  :unless
  (display-graphic-p)
  :bind (
		 :map corfu-map
		 ("<tab>" . corfu-complete)
		 ("<return>" . corfu-insert)
		 )
  :custom
  (corfu-auto nil)
  ;; (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
  :init
  (global-corfu-mode)
  )

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :straight t
  :init
  (nerd-icons-completion-mode t)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  )

(use-package cape
  :unless
  (display-graphic-p)
  :hook
  (corfu-mode . add-cape-completions)
  (corfu-terminal-mode . add-cape-completions)
  :init
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless
  (display-graphic-p)
  :config
  (corfu-terminal-mode)
  )


;;; Company, for when corfu doesn't work

(use-package company
  :straight t
  :if (display-graphic-p)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (tab-always-indent 'complete)
  (company-keymap--unbind-quick-access company-active-map) ;; Disables M-# from selecting stuff on company minimap
  ;; :hook
  ;; (prog-mode . company-mode)
  ;; :hook
  ;; (emacs-lisp-mode . (lambda()
  ;; 		       (setq-local company-backends '(company-elisp))))
  ;; (emacs-lisp-mode . company-mode)
  ;; (lsp-mode . company-mode)
  :bind(
		(:map company-active-map
			  ("RET" . company-complete-selection))
		;; (:map company-mode-map
		;;       ("TAB" . company-complete-common-or-show-delayed-tooltip))
		)
  :init
  (global-company-mode 1)
  (set-face-attribute 'company-tooltip-common nil :inherit nil)
  )
;; Front end customizations for company-mode

(use-package company-box
  :hook
  (company-mode . company-box-mode)
  ;; :config
  ;; (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  )

;;; popup that lists all available shortcuts.

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  )


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
   ;; modus-themes-completions '(selection .(rainbow background))
   ;; modus-operandi-tinted-palette-overrides
   ;; '((bg-main "#efe9e9")
   ;;   (bg-dim "#c9c9c9"))
   )
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (consult-theme 'doom-dark+)
  )

;; Prot's functions for agnostic theme loading
(defvar after-enable-theme-hook nil)

(defun run-after-enable-theme-hook (&rest _args)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

(add-hook 'after-enable-theme-hook (lambda () (set-face-attribute 'line-number nil
																  :background (face-background 'tab-bar)
																  :foreground (face-foreground 'warning)
																  )))

(use-package doric-themes
  :straight (doric-themes :type git :host github :repo "protesilaos/doric-themes")
  )

(use-package ef-themes
  :bind
  ([f6] . toggle-ef-themes-light)
  ([f7] . toggle-ef-themes-dark)
  ;; :init
  ;; (consult-theme 'ef-duo-light)
  :init
  (setq custom-safe-themes t) 
  (setq ef-themes-to-toggle-light '(ef-duo-light ef-kassio doric-earth doric-wind))
  (setq ef-themes-to-toggle-dark '(doric-dark ef-dream doric-obsidian catppuccin))
  (defun toggle-ef-themes-dark ()
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (let* ((current-theme (car ef-themes-to-toggle-dark))
           (rotated-themes (append (cdr ef-themes-to-toggle-dark) (list current-theme))))
      (setq ef-themes-to-toggle-dark rotated-themes)
      (load-theme current-theme)))
  (defun toggle-ef-themes-light ()
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (let* ((current-theme (car ef-themes-to-toggle-light))
           (rotated-themes (append (cdr ef-themes-to-toggle-light) (list current-theme))))
      (setq ef-themes-to-toggle-light rotated-themes)
      (load-theme current-theme)))
  (load-theme 'ef-duo-dark)
  )

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
  (setq find-file-visit-truename t)
  )

(use-package spacious-padding
  :bind
  ("C-c C-v" . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
		'( :internal-border-width 20
		   ;; :header-line-width 4
		   ;; :mode-line-width 50
		   :tab-width 4
		   :right-divider-width 10
		   ;; :scroll-bar-width 8
		   :fringe-width 8
		   ))
  ;; (setq spacious-padding-subtle-mode-line
  ;; 	`( :mode-line-active 'default
  ;; 	   :mode-line-inactive vertical-border))
  ;; :init
  ;; (spacious-padding-mode 1)
  )

;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package catppuccin-theme
  :straight t
  :config
  (custom-set-faces
   ;; Set the color for changes in the diff highlighting to blue.
   `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))

  (custom-set-faces
   ;; Set the color for deletions in the diff highlighting to red.
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))

  (custom-set-faces
   ;; Set the color for insertions in the diff highlighting to green.
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))

  ;; Load the Catppuccin theme without prompting for confirmation.
  (load-theme 'catppuccin :no-confirm)
  )

(use-package golden-ratio
  :defer t
  )

;;; Magit

(use-package magit
  :bind("C-c C-m" . magit-status)
  ;; :hook
  ;; (magit-status-mode . (lambda()
  ;; 			 (setq-local company-mode 0
  ;; 				     global-company-mode 0)
  ;; 			 )
  ;; 		     )
  ;; :config
  )

;;; Highlight Indentation and other indentation utilities

;; (defun my-highlight-guides-faces ()
;;   ;; Custom functiuon that sets indent guides colors so that they match modis-vivendi
;;   (set-face-background 'highlight-indent-guides-odd-face "darkslateblue")
;;   (set-face-background 'highlight-indent-guides-even-face "darkslateblue")
;;   (set-face-background 'highlight-indent-guides-top-odd-face "thistle4")
;;   (set-face-background 'highlight-indent-guides-top-even-face "thistle4"))

;; (use-package highlight-indent-guides
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode)
;;   (highlight-indent-guides-mode . my-highlight-guides-faces)
;;   :config
;;   (setq highlight-indent-guides-auto-enables nil
;; 	highlight-indent-guides-method 'column
;; 	;; highlight-indent-guides-character ?\|
;; 	highlight-indent-guides-responsive 'top
;; 	highlight-indent-guides-delay 0
;; 	)
;;   )

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode) ; or whichever modes you prefer
  :custom(
		  (indent-bars-no-descend-lists t)
		  (indent-bars-no-descend-string t)
		  ;; (indent-bars-treesit-support t)
		  (indent-bars-display-on-blank-lines nil)
		  (indent-bars-pattern ".")
		  (indent-bars-width-frac 0.5)
		  (indent-bars-pad-frac 0.01)
		  ;; (indent-bars-color-by-depth nil)
		  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
		  )
  )

(use-package aggressive-indent
  ;; Doesn't allow you to escape indentation
  :config
  (aggressive-indent-mode 1)
  )

;; (use-package apheleia
;;   ;; code reformating on save
;;   :config
;;   (apheleia-global-mode 1)
;;   )

(use-package undo-tree
  :defer t
  :straight t
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

;;; Use outli for headers

;; Headers for non-lisp languages are [comment-start + space + *]

;; (use-package outline-indent
;;   :straight t
;;   :bind
;;   ("C-<tab>" . outline-indent-toggle-fold)
;;   :custom
;;   (outline-indent-ellipsis " ▼ ")
;;   (outline-blank-line t)
;;   :init
;;   (outline-indent-minor-mode))

(use-package outli
  :straight '(outli
			  :type git
			  :host github
			  :repo "jdtsmith/outli")
  ;; :load-path "~/.emacs.d/outli"
  :hook
  (prog-mode . outli-mode)
  ;; :init
  ;; (outli-mode)
  :config
  (global-reveal-mode)
  :bind(
		([M-down] . outline-next-heading)
		([M-up] . outline-previous-heading)
		)
  )

(use-package imenu-list
  :bind
  (("C-c l" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil
		imenu-list-size 0.25
		imenu-list-position 'left)
  )

;;; Dired functionality

;; (use-package all-the-icons-dired
;;   :after (dired-mode)
;;   )

(use-package dired
  :straight nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  )

(use-package dired-subtree
  :straight t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode)
  (dired-sidebar-mode . nerd-icons-dired-mode))

(use-package dired-sidebar
  :bind(
		("C-c s" . dired-sidebar-toggle-sidebar)
		;; :map dired-sidebar-mode-map
		;; ("C-o" . 'casual-dired-tmenu)
		)
  :custom(
		  (dired-sidebar-theme 'none)
		  (dired-sidebar-use-term-integration t)
		  (dired-sidebar-window-fixed 0)
		  (dired-sidebar-use-custom-modeline 0)
		  (dired-sidebar-display-remote-icons 0)
		  )
  )

;; (use-package casual-dired
;;   :straight (casual-dired
;; 	     :type git
;; 	     :host github
;; 	     :repo "kickingvegas/casual-dired")
;;   :bind (
;; 	 :map dired-mode-map
;; 	 ("C-o" . 'casual-dired-tmenu)
;; 	 ("C-<tab>" . dired-subtree-toggle)
;; 	 )
;;   )

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

;;;; Make winum play nice with golden-ratio-mode

(defadvice winum-select-window-by-number
    (after golden-ratio-resize-window)
  (golden-ratio) nil)

(if golden-ratio-mode (progn (ad-activate 'winum-select-window-by-number)))

;;; Programming Packages

(use-package eldoc
  :straight nil                              ;; This is built-in, no need to fetch it.
  :config
  (setq eldoc-idle-delay 0)                  ;; Automatically fetch doc help
  (setq eldoc-echo-area-use-multiline-p nil) ;; We use the "K" floating help instead
  ;; set to t if you want docs on the echo area
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode)
  )

(use-package eldoc-box
  :straight t
  :if (display-graphic-p)
  :defer t)


;;; Yasnippet

(use-package yasnippet)

(use-package consult-yasnippet)

;;; Eglot Tree-Sitter and Tree-Sitter-Auto

(use-package eglot
  :bind
  (:map eglot-mode-map
		("C-c d" . eldoc))
  :custom(
		  (eglot-events-buffer-size 0)
		  (fset #'jsonrpc--log-event #'ignore)
		  (eglot-extend-to-xref 1)
		  (eglot-sync-connect 0)
		  (eldoc-echo-area-use-multiline-p nil)
		  (eglot-connect-timeout nil)
		  )
  )

;; (use-package eglot-booster
;;   :straight (eglot-booster
;; 	     :type git
;; 	     :host github
;; 	     :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-no-remote-boost 1)
;;   (eglot-booster-mode)
;;   )

;;  Still need to download the emacs-lsp-booster binary from:
;; https://github.com/blahgeek/emacs-lsp-booster/releases
;; and put emacs-lsp-booster into $PATH

;; Try eglot-x
;; (use-package eglot-x
;;   :straight (eglot-x
;; 	     :type git
;; 	     :host github
;; 	     :repo "nemethf/eglot-x")
;;   :after eglot
;;   :custom
;;   (eglot-x-enable-files 1)
;;   (eglot-x-setup)
;;   )

;; (use-package tree-sitter
;; :after eglot
;; :hook
;; (python-mode . tree-sitter-hl-mode)
;; :custom
;; ()
;; :hook
;; (prog-mode . tree-sitter-hl-mode)
;; :custom
;; (treesit-font-lock-level 4)
;;  )

(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  )

(use-package lsp-mode
  ;; :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")
  :straight t
  :custom
  (lsp-enable-which-key-integration t)
  (lsp-inlay-hint-enable nil)                           ;; Usage of inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0.5)                                  ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                    ;; Enable text document color.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-eldoc-render-all t)                              ;; Render all ElDoc messages.
  (lsp-headerline-breadcrumb-enable nil)                ;; Disable breadcrumb
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  )

(use-package lsp-ui
  :straight (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
  :diminish
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-mode-map
		([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
		([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (lsp-ui-peak-enable)
  )

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
  :straight t
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
  :custom(
		  (ess-r-backend 'lsp)
		  (ess-style 'RStudio)
		  (ess-auto-width 'window)
		  (ess-toggle_underscore nil)
		  )
  ;; :hook
  ;; (ess-mode . tree-sitter-ess-r-using-r-faces)
  ;; (ess-mode . 'eglot-ensure)
  :commands
  ( R )
  )

(use-package tree-sitter-ess-r
  :after (ess)
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
  :mode "\\\\.\\(p\\([lm]\\)\\)\\'"
  ;; :custom (major-mode-remap-alist)
  ;; :hook (cperl-mode . 'eglot-ensure)
  )

;;; GPTELL

;; In order not to save the gemini api key on the config file
;; I need to tell emacs to read the key from a different file

(defun your-read-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
			 unless (eobp)
			 collect (prog1 (buffer-substring-no-properties
							 (line-beginning-position)
							 (line-end-position))
					   (forward-line 1))))
  )

(use-package gptel
  :config
  (setq gptel-model 'gemini)
  (setq gptel-backend (gptel-make-gemini "Gemini"
						:key (nth 0 (your-read-lines "~/my-emacs-config/gemini.api.txt" 1))
						:stream t))
  :bind(
		:map gptel-mode-map
		("C-c C-c" . gptel-send)
		)
  )

;;; Using org-present for casual presentations + configurations.

(use-package visual-fill-column
  :config
  (setq visual-fill-column-center-text 1)
  )

(defun my/org-present-start ()
  ;; Center presentation and wrap lines on org-present start
  (visual-fill-column-mode 1)
  (setq visual-fill-column-center-text 1)
  (visual-line-mode 1)
  (setq fill-column 85)
  )

(defun my/org-present-end ()
  ;; Center presentation and wrap lines on org-present start
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  )

(use-package org-present
  :straight '(org-present
			  :type git
			  :host github
			  :repo "rlister/org-present")
  :config
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  )

(use-package org-bullets
  :after org)

(use-package org
  :straight nil
  :init
  (visual-line-mode 1)
  (org-bullets-mode 1)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  :hook
  (org-mode . org-bullets-mode)
  :bind(
		:map org-mode-map
		([f5] . org-present)
		)
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.5))))
  (org-level-2 ((t (:inherit outline-2 :height 1.3))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1))))
  )

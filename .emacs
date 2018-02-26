;; Some custom key-bindings
(global-set-key (kbd "C-c k") 'kill-this-buffer)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;(global-set-key (kbd "C-c RET") 'elpy-shell-send-current-statement)

;; Interactive Do Things: Displays different buffers nicely
(require 'ido)
(ido-mode t)

;; Using auto-complete in emacs
(add-to-list 'load-path "/home/himsself/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "/home/himsself/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;; el-get for installing packages
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; (el-get 'sync)

;; Installing melpa and other package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Installing elpy
(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; ;; pymacs
;; (add-to-list 'load-path "~/.emacs.d/Pymacs/")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; ;; ropemacs
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")



(package-initialize)
(elpy-enable)

(setq python-shell-interpreter "/usr/bin/ipython3"
      python-shell-interpreter-args "-i --simple-prompt"
      ;; elpy-rpc-backend "jedi"
      elpy-rpc-python-command "/usr/bin/python3.6")

;; (add-to-list 'load-path "~/.emacs.d/python-mode.el-6.2.3") 
;; (setq py-install-directory "~/.emacs.d/python-mode.el-6.2.3")
;; (require 'python-mode)


;; (defvar myPackages
;;   '(better-defaults
;;     elpy ;; add the elpy package
;;     ein ;; emmacs ipython notebook
;;     material-theme)
;;   )
;; (elpy-enable)

;; (autoload 'jedi:setup "jedi" nil t)
;; ;; Enable jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(add-to-list 'load-path  "/home/himsself/.emacs.d/ess-14.09/lisp/")
(require 'ess-site)

;; (load "ess-site")
(setq-default inferior-S+6-program-name "Splus")
(setq-default inferior-R-program-name "R")
;; automatically get the correct mode 
auto-mode-alist (append (list '("\\.c$" . c-mode)
			      '("\\.tex$" . latex-mode)
			      '("\\.S$" . S-mode)
			      '("\\.s$" . S-mode)
			      '("\\.R$" . R-mode)
			      '("\\.r$" . R-mode)
			      '("\\.html$" . html-mode)
                              '("\\.emacs" . emacs-lisp-mode)
			      )
			auto-mode-alist)
;; comment out the following if you are not using R/S-Plus on ACPUB
;; add a ";" in front of each line 
;; (load "~/.emacs.d/ess-14.09/lisp/ess-site")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-django-always-prompt t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "/usr/bin/python3.6")
 '(elpy-syntax-check-command "")
 '(py-split-window-on-execute (quote just-two)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )





;; Some custom key-bindings
(global-set-key (kbd "C-c k") 'kill-this-buffer)
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
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync)






;; Installing melpa and other package archives

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))


;; pymacs
(add-to-list 'load-path "~/.emacs.d/Pymacs")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Installing elpy
(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(setq python-shell-interpreter "/usr/bin/ipython3")

(autoload 'jedi:setup "jedi" nil t)

;; Enable jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)


(require 'ess-site)
;; (add-to-list 'load-path  "/home/himsself/.emacs.d/ess-14.09/lisp/")
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
 '(py-split-window-on-execute (quote just-two)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

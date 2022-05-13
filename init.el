;; Disable tls due to emacs bug to download from melpa
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;;(package-install-selected-packages)

;; Make sure your required packages are installed on startup.
(defvar packages-required-at-startup
  '(haskell-mode lsp-mode lsp-haskell go-mode lsp-ui use-package flycheck iedit elm-mode tree-sitter tree-sitter-langs psc-ide))

(defun packages-required-at-startup-are-installed-p ()
  (cl-loop for p in packages-required-at-startup
	when (not (package-installed-p p)) do (cl-return nil)
	finally (cl-return t)))

(unless (packages-required-at-startup-are-installed-p)
  (package-refresh-contents)
  (dolist (p packages-required-at-startup)
    (when (not (package-installed-p p))
      (package-install p))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; lsp-mode suggestions:  https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; File matching with major modes
;;(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode

;;use-package settings
(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; lsp-ui settings
;; customization options:  https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :ensure t)

;; flycheck settings
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; tree-sitter settings
(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package tree-sitter-langs
  :after (tree-sitter)
  :ensure t)

;; (use-package tree-sitter-hl
;;   :after (tree-sitter)  
;;   :ensure t
;;   :hook (tree-sitter-after-on))

;; (use-package tree-sitter-debug
;;   :after (tree-sitter)  
;;   :ensure t)

;; (use-package tree-sitter-query
;;   :after (tree-sitter)  
;;   :ensure t)

;; iedit settings
(use-package iedit
  :ensure t)

;; ;; Haskell mode
;; (use-package haskell-mode
;;   :ensure t
;;   :hook (haskell-mode tree-sitter-hl-mode))

(require 'lsp)
(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil)

;; (require 'lsp-haskell)
;; ;; Hooks so haskell and literate haskell major modes trigger LSP setup
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;; Go mode
(require 'go-mode)
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-install-save-hooks)
;; All the available customizations: https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-go.el 
(setq lsp-go-hover-kind "FullDocumentation")
(setq lsp-go-links-in-hover t)
;; (setq lsp-go-use-gofumpt nil)
;;(setq lsp-go-import-shortcut "Both")

;; Elm mode
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
(add-hook 'elm-mode-hook 'elm-indent-simple-mode)

;;(add-hook 'elm-mode-hook 'lsp-deferred-mode)
;;(add-hook 'elm-mode-hook 'elm-format-on-save-mode)
(add-hook 'elm-mode-hook (lambda ()
			   (message "Starting the elm-language-server in the background")
 			   (shell-command "elm-language-server &")))
;;(add-hook 'elm-mode-hook (lambda ()
;; 			   (add-hook 'after-save-hook 'elm-format-on-save-mode)))
(add-hook 'elm-mode-hook (lambda ()
			   (message "Adding the untabify after save hook")
			   (add-hook 'after-save-hook (lambda ()
							(message "Starting to untabify")
							(untabify (point-min) (point-max)) nil t))))
(add-hook 'elm-mode-hook (lambda ()
			   (message "Resetting the C-c C-f keybinding")
			   (local-set-key (kbd "C-c C-f") #'elm-format-buffer)))
;; (add-hook 'elm-mode-hook (lambda ()
;; 			   (message "Adding the elm compile on save hook")
;; 			   (add-hook 'after-save-hook (lambda ()
;; 							(message "Running elm make")
;; 							(elm-compile-buffer)))))
;;(add-hook 'elm-mode-hook 'lsp-mode)
;; (add-hook 'elm-mode-hook (lambda () lsp-mode))
;; (setq                                                                       
;;  lsp-eldoc-render-all t                                                     
;;  lsp-ui-doc-enable nil                                                      
;;  lsp-ui-sideline-enable nil                                                 
;;  lsp-prefer-capf t                                                          
;;  lsp-idle-delay 0.2)
;;(add-hook 'after-save-hook 'untabify)
;; (add-hook 'elm-mode-hook 
;;           (lambda () 
;;             (add-hook 'after-save-hook 'elm-format-on-save-mode nil 't))

;; (use-package elm-mode
;;   :ensure t
;;   ;;:mode ("\\.elm\\'" . elm-mode)
;;   ;; :hook (elm-mode tree-sitter-hl-mode)
;;   :hook (elm-mode lsp-deferred)
;;   :hook (elm-mode elm-format-on-save-mode))
 

;; PureScript mode
(require 'psc-ide)
(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))
(add-hook 'purescript-mode-hook (lambda ()
			   (message "Starting the purescript-language-server")
			   (shell-command "purescript-language-server")))
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (lsp-mode)
    (turn-on-purescript-indentation)))
(customize-set-variable 'psc-ide-rebuild-on-save t)
;;(add-to-list 'company-backends 'company-elm)


;; Packages to consider:
;; treemacs, comint, projectile, magit, ivy, ido, parens, diminish, rainbow-mode, dash-at-point,

;; Need packages to manage:
;; html, web, emacs lisp, dockerfiles
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(psc-ide purescript-mode tree-sitter-query tree-sitter-debug tree-sitter-hl tree-sitter-langs tree-sitter use-package-chords use-package-ensure-system-package iedit flycheck lsp-ui go-mode lsp-haskell lsp-mode use-package haskell-mode elm-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; DAP mode: https://emacs-lsp.github.io/dap-mode/

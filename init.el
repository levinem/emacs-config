;; Disable tls due to emacs bug to download from melpa
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Make sure your required packages are installed on startup.
(defvar packages-required-at-startup
  '(haskell-mode lsp lsp-haskell go-mode lsp-mode use-package))

(defun packages-required-at-startup-are-installed-p ()
  (loop for p in package-required-at-startup
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (packages-required-at-startup-are-installed-p)
  (package-refresh-contents)
  (dolist (p packages-required-at-startup)
    (when (not (package-installed-p p))
      (package-install p))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

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

;; Haskell mode
(require 'haskell-mode)			 
(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; Go mode
(require 'go-mode)
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; All the available customizations: https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-go.el 
(setq lsp-go-hover-kind "FullDocumentation")
(setq lsp-go-links-in-hover t)
;; (setq lsp-go-use-gofumpt nil)
;;(setq lsp-go-import-shortcut "Both")

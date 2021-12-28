;; Disable tls due to emacs bug to download from melpa
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

;; Haskell mode
(require 'haskell-mode)			 
(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; Go mode

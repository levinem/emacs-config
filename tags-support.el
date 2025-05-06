;;; -*- lexical-binding: t -*-

(add-hook 'c++-mode-hook #'ggtags-mode)
(add-hook 'c-mode-hook #'ggtags-mode)

;;(setq ggtags-global-mode 1)

; Allow very large database files
(setq ggtags-oversize-limit 104857600)
(setq ggtags-sort-by-nearness t)

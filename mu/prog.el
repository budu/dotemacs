;;; prog.el --- Programming mode configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; General programming mode configuration and various language modes.
;;
;;; Code:

(setq scheme-program-name "racket")

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package crystal-mode)
(use-package dap-mode)
(use-package dockerfile-mode)
(use-package geiser-racket)
(use-package lua-mode)
(use-package terraform-mode)
(use-package yaml-mode)

(provide 'mu/prog)

;;; prog.el ends here

;;; folding.el --- Folding configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for code folding with origami.
;;
;;; Code:

;; Origami computes this face from `highlight' while loading.  On Emacs 31's
;; initial frame that background can still be `unspecified', which is not a
;; valid box color and prevents packages that load Origami indirectly (such as
;; dap-mode via lsp-mode) from initializing.
(when (version<= "31" emacs-version)
  (custom-set-faces
   '(origami-fold-header-face ((t (:inherit highlight :box nil))))))

;; old but working, might want to look into ts-fold
(use-package origami
  :bind (("C-c v" . origami-recursively-toggle-node)
         ("C-c V" . origami-toggle-all-nodes)
         ("C-c C-v" . origami-toggle-node))
  :hook ((prog-mode . origami-mode)
         (text-mode . origami-mode)))

(provide 'mu/folding)

;;; folding.el ends here

;;; flycheck.el --- Flycheck configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for flycheck syntax checking.
;;
;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c n" . flycheck-next-error))
  :custom ((flycheck-check-syntax-automatically '(save mode-enabled))
           (flycheck-display-errors-delay 0.25)
           (flycheck-emacs-lisp-load-path 'inherit)))

(provide 'mu/flycheck)

;;; flycheck.el ends here

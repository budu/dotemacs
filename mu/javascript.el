;;; javascript.el --- JavaScript configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for JavaScript development.
;;
;;; Code:

;; this needs some npm packages to be installed
;; npm install -g javascript-typescript-langserver
;; npm install typescript-eslint-language-service -D
(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . eglot-ensure))
  :bind (:map js2-mode-map
              ("C-c n" . 'eglot-rename))
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t)
  (js2-strict-missing-semi-warning nil))

(use-package typescript-mode
  :mode ("\\.ts$" . typescript-mode)
  :hook (typescript-mode . eglot-ensure)
  :bind (:map typescript-mode-map
              ("C-c n" . 'eglot-rename)))

(use-package json-mode)

(provide 'mu/javascript)

;;; javascript.el ends here

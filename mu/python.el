;;; package --- Python setup for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; ----------------------------------------
;; LSP Mode (for Python autocompletion, linting, etc.)
;; ----------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; optional, but handy
  :config
  (setq lsp-pylsp-server-command "pylsp")
  ;; If you want LSP UI features:
  (use-package lsp-ui
    :commands lsp-ui-mode))


;; TODO: try this
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; ----------------------------------------
;; Python Mode Configuration
;; ----------------------------------------
(use-package python
  :ensure nil  ;; python.el is built into Emacs
  :hook
  (python-mode . lsp-deferred)
  :config
  (setq-default flycheck-checker 'python-ruff)
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"))

;; ----------------------------------------
;; Blacken (auto-format with Black)
;; ----------------------------------------
;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :config
;;   ;; Optional: Configure line length
;;   (setq blacken-line-length 120))

;; ----------------------------------------
;; OPTIONAL: Virtual environment tools
;; ----------------------------------------
;; If you use pipenv or pyvenv, you can install corresponding packages.

;; (use-package pyvenv
;;   :after python
;;   :config
;;   (setenv "WORKON_HOME" "~/.virtualenvs/"))

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq pipenv-projectile-after-switch-function
;;         #'pipenv-projectile-after-switch-extended))

;; ----------------------------------------
;; Python-pytest
;; ----------------------------------------
(use-package python-pytest
  :after python
  :bind (:map python-mode-map
         ;; Run pytest on the current test function (under cursor)
         ("C-c t f" . python-pytest-function)
         ;; Run pytest on the current file
         ("C-c t F" . python-pytest-file)
         ;; Run the last test command again
         ("C-c t l" . python-pytest-last))
  :config
  ;; Set the executable name if it's not simply "pytest".
  (setq python-pytest-executable "pytest"))

;;; python.el ends here

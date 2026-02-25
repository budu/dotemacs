;;; clojure.el --- Clojure development configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Modern Clojure development setup using clojure-ts-mode (Tree-sitter),
;; CIDER for REPL interaction, and clojure-lsp via eglot for IDE features.
;;
;; External dependencies:
;;   - clojure-lsp: Install via your package manager or from
;;     https://clojure-lsp.io/installation/
;;   - Tree-sitter Clojure grammar: installed automatically by
;;     clojure-ts-mode on first use
;;
;;; Code:

(use-package clojure-ts-mode
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.edn\\'" . clojure-ts-mode))
  :hook ((clojure-ts-mode . eglot-ensure)
         (clojure-ts-clojurescript-mode . eglot-ensure)
         (clojure-ts-clojurec-mode . eglot-ensure)))

(use-package cider
  :hook ((clojure-ts-mode . cider-mode)
         (clojure-ts-clojurescript-mode . cider-mode)
         (clojure-ts-clojurec-mode . cider-mode))
  :bind (:map clojure-ts-mode-map
         ("C-c C-j" . cider-jack-in)
         ("C-c C-s" . cider-jack-in-cljs))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((clojure-ts-mode clojure-ts-clojurescript-mode clojure-ts-clojurec-mode)
                 "clojure-lsp")))

(provide 'mu/clojure)

;;; clojure.el ends here

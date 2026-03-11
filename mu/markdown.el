;;; markdown.el --- Markdown configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for markdown-mode.
;;
;;; Code:

;; fix markdown command is not found
(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("C-M-b" . markdown-backward-paragraph)
              ("C-M-f" . markdown-forward-paragraph)
              ("C-c l" . mu/md-link-commit)
              ("C-c '" . mu/md-backquote))
  :hook
  (markdown-mode . markdown-toggle-markup-hiding)
  (markdown-mode . visual-line-mode)
  :config
  (setq markdown-command "~/bin/pandoc")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "yellow" :height 1.0))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "purple" :height 1.0))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "magenta" :height 1.0))))
   '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "dark cyan" :height 1.0))))))

;; Tempo templates for markdown-mode (like org-tempo's "<r TAB" etc.)
(require 'tempo)

(defvar mu/md-tempo-tags nil
  "Tempo tags for markdown-mode block templates.")

(tempo-define-template "md-ruby"
  '("```ruby" n p n "```" n)
  "<r"
  "Insert a ruby code block"
  'mu/md-tempo-tags)

(tempo-define-template "md-prompt"
  '("```prompt" n p n "```" n)
  "<p"
  "Insert a prompt code block"
  'mu/md-tempo-tags)

(tempo-define-template "md-src"
  '("```" (p "Language: ") n p n "```" n)
  "<s"
  "Insert a code block with language prompt"
  'mu/md-tempo-tags)

(defun mu/md-tempo-complete-tag (&rest _)
  "Expand tempo tag silently, return t if expansion succeeded."
  (not (eq 'fail (tempo-complete-tag 'fail))))

(defun mu/md-tempo-setup ()
  "Set up tempo expansion in markdown-mode."
  (tempo-use-tag-list 'mu/md-tempo-tags)
  (setq-local tempo-match-finder "^ *\\(<[[:word:]]+\\)\\=")
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (unless (mu/md-tempo-complete-tag)
                     (markdown-cycle)))))

(add-hook 'markdown-mode-hook #'mu/md-tempo-setup)

(provide 'mu/markdown)

;;; markdown.el ends here

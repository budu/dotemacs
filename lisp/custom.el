(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-agent-configs
   (list (agent-shell-anthropic-make-claude-code-config) (agent-shell-openai-make-codex-config)
         (agent-shell-google-make-gemini-config)))
 '(package-selected-packages
   '(adoc-mode agent-shell all-the-icons asdf blacken buttercup claude-code coffee-mode
               company-box copilot counsel-projectile crystal-mode csv-mode dap-mode direnv
               dockerfile-mode doom-modeline doom-themes eat ein elisp-lint ement emmet-mode
               emojify exec-path-from-shell expand-region flycheck-posframe
               flyspell-correct-ivy forge geiser-racket golden-ratio gptel grip-mode helpful
               ivy-posframe ivy-rich js2-mode json-mode keyfreq lsp-ivy lsp-ui lua-mode
               mastodon multiple-cursors org-appear org-bullets origami pkg-info
               poly-markdown projectile-rails projectile-ripgrep python-pytest
               quelpa-use-package rails-i18n rainbow-delimiters rainbow-mode restclient robe
               rspec-mode rubocop ruby-electric ruby-end rust-mode rvm screenshot scss-mode
               slim-mode transient-posframe web-mode which-key yaml-mode yari zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121a1e"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "yellow" :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "purple" :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "magenta" :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "dark cyan" :height 1.0))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif" :height 1.4 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "white" :family "Sans Serif"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8 :background "#103b66"))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#444444")))))

(provide 'custom)

;;; custom.el ends here

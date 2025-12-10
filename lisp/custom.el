;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-agent-configs
   (list (agent-shell-anthropic-make-claude-code-config) (agent-shell-openai-make-codex-config)
         (agent-shell-google-make-gemini-config)))
 '(org-agenda-files '("~/cg/reservotron/nb-notes/current.org"))
 '(package-selected-packages
   '(acp adoc-mode agent-shell all-the-icons asdf blacken buttercup claude-code coffee-mode
         company-box copilot counsel-projectile crystal-mode csv-mode dap-mode dired direnv
         dockerfile-mode doom-modeline doom-themes eat ein elisp-lint ement emmet-mode
         emojify exec-path-from-shell expand-region flycheck-posframe flyspell-correct-ivy
         forge geiser-racket golden-ratio gptel grip-mode helpful ivy-posframe ivy-rich
         js2-mode json-mode keyfreq load-relative lsp-ivy lsp-ui lua-mode mastodon
         multiple-cursors org-appear org-bullets origami pkg-info poly-markdown
         projectile-rails projectile-ripgrep python-pytest quelpa-use-package rails-i18n
         rainbow-delimiters rainbow-mode restclient robe rspec-mode rubocop ruby-electric
         ruby-end rust-mode rvm screenshot scss-mode shell-maker slim-mode
         transient-posframe web-mode which-key yaml-mode yari zenburn-theme))
 '(package-vc-selected-packages
   '((agent-shell :url "https://github.com/xenodium/agent-shell")
     (acp :url "https://github.com/xenodium/acp.el")
     (asdf :url "https://github.com/tabfugnic/asdf.el")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")
     (screenshot :url "https://github.com/tecosaur/screenshot")))
 '(safe-local-variable-values
   '((eval defun agent-shell-run-tests-batch nil
           "Run tests in a fresh Emacs batch process (most reliable)." (interactive)
           (let*
               ((project-root (project-root (project-current t)))
                (default-directory project-root)
                (script (expand-file-name "run-tests.sh" project-root)))
             (if (file-exists-p script) (compile (format "bash %s" script))
               (message
                "run-tests.sh not found. Use M-x agent-shell-run-all-tests for in-process testing."))))
     (eval defun agent-shell-run-all-tests nil
           "Run all agent-shell tests with a clean reload." (interactive)
           (let ((project-root (project-root (project-current t))))
             (ert-delete-all-tests) (unload-feature 'agent-shell t)
             (load-file (expand-file-name "agent-shell.el" project-root))
             (let ((test-dir (expand-file-name "tests/" project-root)))
               (dolist (file (directory-files-recursively test-dir "\\.el$"))
                 (unless (string-match-p "/\\." file) (load-file file))))
             (if noninteractive (ert-run-tests-batch-and-exit "^agent-shell")
               (ert "^agent-shell"))))
     (eval defun agent-shell-run-all-tests nil "Run all agent-shell tests in batch mode."
           (interactive)
           (let ((test-dir (expand-file-name "tests/" (project-root (project-current t)))))
             (dolist (file (directory-files-recursively test-dir "\\.el$"))
               (unless (string-match-p "/\\." file) (load file)))
             (if noninteractive (ert-run-tests-batch-and-exit "^agent-shell")
               (ert "^agent-shell"))))))
 '(screenshot-font-size 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121a1e"))))
 '(markdown-header-delimiter-face ((t (:foreground "hot pink" :inherit markdown-header-face))))
 '(markdown-header-face ((t (:foreground "hot pink" :inherit bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "yellow" :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "purple" :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "magenta" :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "dark cyan" :height 1.0))))
 '(markdown-list-face ((t (:foreground "green"))))
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
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-patch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8 :background "#103b66"))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#444444")))))

(provide 'custom)

;;; custom.el ends here

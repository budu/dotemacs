;;; appearance.el --- Appearance and theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration for fonts, themes, icons, and visual appearance including
;;; doom-modeline, doom-themes, rainbow-mode, and emojify.

;;; Code:

(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    :height 80)

; Run these after the package has been installed:
; M-x all-the-icons-install-fonts
; M-x nerd-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-dracula t)
  ;; Emacs 31 makes `gnus-group-news-low' inherit from its "empty"
  ;; counterpart.  Doom themes currently specify the reverse inheritance,
  ;; which creates a cycle as soon as Gnus faces are loaded (for example,
  ;; while Counsel is building the M-x candidate list).
  (when (version<= "31" emacs-version)
    (custom-theme-set-faces
     'doom-dracula
     '(gnus-group-news-low-empty
       ((t (:inherit gnus-group-mail-1-empty :weight normal))))))
  (custom-set-faces
   '(default ((t (:background "#121a1e"))))))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#444444")))))
  )

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-user-emojis
        '((":memo:" . (("name" . "Memo")
                       ("image" . "~/.emacs.d/emojis/1f4dd.png")
                       ("style" . "github")))
          (":broom:" . (("name" . "Broom")
                        ("image" . "~/.emacs.d/emojis/1f9f9.png")
                        ("style" . "github")))
          (":mirror:" . (("name" . "Mirror")
                        ("image" . "~/.emacs.d/emojis/1fa9e.png")
                        ("style" . "github"))))))

(provide 'appearance)

;;; appearance.el ends here

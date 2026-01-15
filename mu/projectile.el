;;; projectile.el --- Projectile configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for projectile and related packages.
;;
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-ripgrep)
         ("C-S-f" . projectile-find-file)
         ("C-M-S-f" . projectile-ripgrep))
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-vc)
  (setq projectile-generic-command "fd . -H -0 --type f")
  (setq projectile-create-missing-test-files t)
  :config
  (setq projectile-project-search-path '("~/cg" "~/projects"))
  (projectile-load-known-projects))

(use-package projectile-ripgrep)

;; TODO: http://localhost:3080/c/b2d69d5c-88d5-4ffb-8c62-b3ad401d18ce
(advice-add 'counsel-rg
            :around
            (lambda (func &rest args)
              (cl-flet ((filter-func (code) (if (= code 2) 0 code)))
                (unwind-protect
                    (progn (advice-add 'process-exit-status :filter-return #'filter-func)
                           (apply func args))
                  (advice-remove 'process-exit-status #'filter-func)))))

(use-package counsel-projectile
  :init (setq counsel-projectile-switch-project-action #'projectile-vc)
  :config (counsel-projectile-mode))

(defun mu/counsel-projectile-find-file-dwim ()
  "Find file in parent project, or current directory with C-u.
With C-u, uses counsel-file-jump for fuzzy matching in current directory."
  (interactive)
  (if current-prefix-arg
      (counsel-file-jump nil default-directory)
    (let ((default-directory (mu/get-project-dir)))
      (call-interactively #'counsel-projectile-find-file))))

(defun mu/counsel-projectile-rg-dwim ()
  "Search in parent project, or current directory with C-u."
  (interactive)
  (if current-prefix-arg
      (counsel-rg nil default-directory)
    (let ((default-directory (mu/get-project-dir)))
      (call-interactively #'counsel-projectile-rg))))

(global-set-key (kbd "C-S-f") #'mu/counsel-projectile-find-file-dwim)
(global-set-key (kbd "C-M-S-f") #'mu/counsel-projectile-rg-dwim)

(provide 'mu/projectile)

;;; projectile.el ends here

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

(defun mu/projectile--with-parent-project (orig-fun &rest args)
  "Advice to run counsel-projectile commands from parent directory when in nb-notes.
ORIG-FUN is the original function being advised, ARGS are its arguments."
  (let ((default-directory (mu/get-project-dir)))
    (apply orig-fun args)))

(advice-add 'counsel-projectile-find-file :around #'mu/projectile--with-parent-project)
(advice-add 'counsel-projectile-rg :around #'mu/projectile--with-parent-project)

(provide 'mu/projectile)

;;; projectile.el ends here

;;; package --- Project related helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mu/project/counsel-projectile-rg-subdir (subdir)
  "Run `counsel-rg' in a project subdirectory using `projectile'.
SUBDIR is the subdirectory within the project to search."
  (interactive "DSubdirectory: ")
  (let* ((project-root (projectile-project-root))
         (search-dir (expand-file-name subdir project-root)))
    (if (file-directory-p search-dir)
        (let ((default-directory search-dir))
          (counsel-rg nil search-dir))
      (error "Directory %s does not exist" search-dir))))

(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "s d") 'mu/project/counsel-projectile-rg-subdir))

(defun mu/project/counsel-rg-pwd-no-ignore ()
  "Run `counsel-rg` in the current directory with `--no-ignore`."
  (interactive)
  (counsel-rg nil default-directory "--no-ignore"))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c s f") 'mu/project/counsel-rg-pwd-no-ignore))

;;; project.el ends here

;;; spelling.el --- Spelling configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for flyspell and related packages.
;;
;;; Code:

(defun mu/flyspell-skip-commit-hashes (beg end _poss)
  "Skip flyspell highlighting for git commit hashes (7-40 hex chars)."
  (string-match-p "^[0-9a-f]\\{7,40\\}$" (buffer-substring-no-properties beg end)))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (agent-shell-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (add-hook 'flyspell-incorrect-hook #'mu/flyspell-skip-commit-hashes)
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(provide 'mu/spelling)

;;; spelling.el ends here

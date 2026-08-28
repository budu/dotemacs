;;; package --- Magit helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'magit
  ;; Common choices:
  ;;   -committerdate : sort by last commit time (descending)
  ;;   -creatordate   : similar, often best for branches/tags
  (setq magit-list-refs-sortby '("-committerdate"))
  (setq magit-show-long-lines-warning nil)

  ;; Open magit-status in the current window
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Add custom keybinding for magit-checkout in magit-status-mode
  (define-key magit-status-mode-map (kbd "`") 'magit-checkout)

  ;; Ivy doesn't support completing-read-multiple, so magit's multi-branch
  ;; reader falls back to a bare minibuffer.  Use the single-branch reader
  ;; instead (only loses octopus merge multi-select).
  (advice-add 'magit-read-other-branches-or-commits :override
              #'magit-read-other-branch-or-commit))

;; TODO: make it opt-in only
(defun mu/magit/quicksave ()
  "Stage everything, commit and push."
  (interactive)
  (let ((default-directory (if (fboundp 'magit-toplevel)
                               (magit-toplevel)
                             default-directory)))
    (if (and default-directory (magit-git-repo-p default-directory))
        (progn
          ;; Stage all changes including untracked files
          (magit-stage-modified t)
          (magit-stage-untracked)

          ;; Create the commit
          (magit-commit-create
           (list "-m" "Quicksave"))

          ;; Push to origin
          (magit-push-current-to-pushremote nil))
      (message "Not in a git repository!"))))

(global-set-key (kbd "C-c q") 'mu/magit/quicksave)

(defun mu/magit/open-parent ()
  "Open magit for the root of the project."
  (interactive)
  (let* ((toplevel (if (fboundp 'magit-toplevel)
                       (magit-toplevel)
                     default-directory))
         (parent-dir (when (and toplevel
                                (string-match-p "/nb-notes/?$" toplevel))
                       (file-name-directory (directory-file-name toplevel)))))
    (if parent-dir
        (magit-status parent-dir)
      (magit-status))))

(global-set-key (kbd "C-x g") 'mu/magit/open-parent)
(global-set-key (kbd "C-x C-g") 'magit-status)

(defun mu/magit/auto-visit-staged-file ()
  "Automatically visit the real file and close blob buffer if viewing staged content.
Checks if buffer name ends with ~{index}~ which indicates a staged blob."
  (when (and (buffer-name)
             (string-match-p "~{index}~\\'" (buffer-name)))
    (let ((blob-buffer (current-buffer)))
      (run-at-time 0 nil
                   `(lambda ()
                      (when (buffer-live-p ,blob-buffer)
                        (with-current-buffer ,blob-buffer
                          (magit-blob-visit-file))
                        (kill-buffer ,blob-buffer)))))))

(add-hook 'magit-blob-mode-hook 'mu/magit/auto-visit-staged-file)

;;; magit.el ends here

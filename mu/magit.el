;;; package --- Magit helpers
;;; Commentary:
;;; Code:

(with-eval-after-load 'magit
  ;; Common choices:
  ;;   -committerdate : sort by last commit time (descending)
  ;;   -creatordate   : similar, often best for branches/tags
  (setq magit-list-refs-sortby '("-committerdate"))

  ;; Open magit-status in the current window
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Add custom keybinding for magit-checkout in magit-status-mode
  (define-key magit-status-mode-map (kbd "`") 'magit-checkout))

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

(defvar mu/magit/index-buffer-keymap
  (let ((map (make-sparse-keymap)))
    ;; Bind common navigation/editing keys
    (define-key map (kbd "RET") 'magit-blob-visit-file)
    (define-key map (kbd "C-j") 'magit-blob-visit-file)
    (define-key map (kbd "SPC") 'magit-blob-visit-file)
    ;; Bind all letters and numbers
    (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
      (dotimes (idx (length chars))
        (define-key map (kbd (char-to-string (aref chars idx)))
          'magit-blob-visit-file)))
    ;; Bind common punctuation
    (dolist (key '("." "," ";" ":" "'" "\"" "/" "\\" "-" "_" "=" "+" "*" "&" "%" "$" "#" "@" "!" "?" "<" ">" "[" "]" "{" "}" "(" ")"))
      (define-key map (kbd key) 'magit-blob-visit-file))
    map)
  "Keymap for magit index buffers that redirects most keys to jump to actual file.")

(defun mu/magit/setup-index-buffer-keymap ()
  "Set up keybindings for magit index buffers to jump to actual file.
This makes most self-insert keys jump to the actual file for editing."
  (when (and (boundp 'magit-buffer-revision)
             magit-buffer-revision
             (string= magit-buffer-revision "{index}"))
    ;; Use minor mode map list to override keys even in read-only buffers
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'magit-blob-mode mu/magit/index-buffer-keymap)
                      minor-mode-overriding-map-alist))))

;; Try using magit-blob-mode-hook instead, which runs after the mode is fully enabled
(add-hook 'magit-blob-mode-hook 'mu/magit/setup-index-buffer-keymap)

(defun mu/magit/auto-visit-staged-file ()
  "Automatically visit the real file and close blob buffer if viewing staged content.
Checks if buffer name ends with ~{index}~ which indicates a staged blob."
  (when (and (buffer-name)
             (string-match-p "~{index}~\\'" (buffer-name)))
    (let ((blob-buffer (current-buffer)))
      (magit-blob-visit-file)
      (kill-buffer blob-buffer))))

(add-hook 'magit-blob-mode-hook 'mu/magit/auto-visit-staged-file)

;;; magit.el ends here

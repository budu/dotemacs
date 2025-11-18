;;; package --- Magit helpers
;;; Commentary:
;;; Code:

(with-eval-after-load 'magit
  ;; Common choices:
  ;;   -committerdate : sort by last commit time (descending)
  ;;   -creatordate   : similar, often best for branches/tags
  (setq magit-list-refs-sortby '("-committerdate")))

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

(defun mu/magit/find-line-by-content (line-text start-line)
  "Find a line matching LINE-TEXT in the current buffer.
First searches forward from START-LINE, then from the beginning if not found.
Returns the position of the beginning of the matching line, or nil if not found."
  (let ((found-pos nil))
    ;; First try: search forward from the original line
    (goto-char (point-min))
    (forward-line (1- start-line))
    (when (search-forward line-text nil t)
      (beginning-of-line)
      (when (string= (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))
                    line-text)
        (setq found-pos (point))))
    ;; If not found forward, try searching from the beginning
    (unless found-pos
      (goto-char (point-min))
      (when (search-forward line-text nil t)
        (beginning-of-line)
        (when (string= (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      line-text)
          (setq found-pos (point)))))
    found-pos))

(defun mu/magit/replay-keystroke-if-valid (key target-col)
  "Replay KEY at the current position if TARGET-COL is valid for the current line.
Only replays if we can successfully move to TARGET-COL on the current line."
  (when (and key (> (length key) 0))
    (let ((moved-col (move-to-column target-col))
          (line-length (- (line-end-position) (line-beginning-position))))
      (when (and (<= target-col line-length)
                 (= moved-col target-col))
        (setq unread-command-events
              (append (listify-key-sequence key) unread-command-events))))))

(defun mu/magit/jump-to-actual-file ()
  "Jump from a magit blob buffer (like ~index~ buffers) to the actual file.
Closes the index buffer and tries to find the same position in the actual file
by matching line content. Only repeats the keystroke if the exact position is found."
  (interactive)
  (if (and (bound-and-true-p magit-buffer-file-name)
           (bound-and-true-p magit-blob-mode))
      (let ((file magit-buffer-file-name)
            (target-col (current-column))
            (key (this-command-keys-vector))
            (index-buffer (current-buffer))
            (current-line-text (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            (start-line (line-number-at-pos)))
        ;; Kill the index buffer
        (kill-buffer index-buffer)
        ;; Open the real file
        (find-file file)
        ;; Try to find the same line content in the actual file
        (let ((found-pos (mu/magit/find-line-by-content current-line-text start-line)))
          (if found-pos
              (progn
                (goto-char found-pos)
                (mu/magit/replay-keystroke-if-valid key target-col))
            ;; Fallback: go to original line number if content search failed
            (goto-char (point-min))
            (forward-line (1- start-line)))))
    (message "Not in a magit blob buffer")))

(defvar mu/magit/index-buffer-keymap
  (let ((map (make-sparse-keymap)))
    ;; Bind RET, C-j, and common editing keys
    (define-key map (kbd "RET") 'mu/magit/jump-to-actual-file)
    (define-key map (kbd "C-j") 'mu/magit/jump-to-actual-file)
    (define-key map (kbd "SPC") 'mu/magit/jump-to-actual-file)
    (define-key map (kbd "e") 'mu/magit/jump-to-actual-file)
    (define-key map (kbd "i") 'mu/magit/jump-to-actual-file)
    ;; Bind all printable characters
    (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
      (dotimes (idx (length chars))
        (define-key map (kbd (char-to-string (aref chars idx)))
          'mu/magit/jump-to-actual-file)))
    ;; Bind some punctuation and special keys
    (dolist (key '("." "," ";" ":" "'" "\"" "/" "\\" "-" "_" "=" "+" "*" "&" "%" "$" "#" "@" "!" "?" "<" ">" "[" "]" "{" "}" "(" ")"))
      (define-key map (kbd key) 'mu/magit/jump-to-actual-file))
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

;;; magit.el ends here

(defun mu/word-with-underscores-at-point-bounds ()
  "Find the bounds of a word at point, including hyphens and underscores."
  (save-excursion
    (skip-chars-backward "a-zA-Z0-9-_")
    (let ((start (point)))
      (skip-chars-forward "a-zA-Z0-9-_")
      (cons start (point)))))

;; Written in collaboration with ChatGPT 3.5, might need improvements
(defun mu/convert-region-to-percent-w-syntax (begin end)
  "Convert a region of text inside square brackets to Ruby's %w syntax and replace it."
  (interactive "r")
  (save-excursion
    (re-search-forward "\\]" nil t)
    (when (re-search-backward "\\[" nil t)
      (let* ((start (point))
             (end (re-search-forward "\\]" nil t))
             (region-text (buffer-substring-no-properties (+ start 1) (- end 1))))
        (delete-region start end)
        (setq region-text (replace-regexp-in-string "[ \t,]+" " " region-text))
        (setq region-text (replace-regexp-in-string "\\(\"\\)\\(.*?\\)\\(\"\\)" "\\2" region-text))
        (insert (concat "%w[" region-text "]"))))))

;; From SO with improvements made using ChatGPT 4
(defun mu/cslist-to-indented-lines-and-back (start end &optional arg)
  (interactive "r\nP")
  (unless (use-region-p)
    (call-interactively 'er/expand-region 0)
    (when (<= start end)
      (call-interactively 'er/mark-inside-pairs 2))
    (setq start (region-beginning))
    (setq end (region-end)))
  (if (string-match-p "\n" (buffer-substring start end))
      (let* ((insertion (mapconcat
                         (lambda (x) (string-trim x))
                         (split-string (buffer-substring start end) "\n") " "))
             (insertion (replace-regexp-in-string " \\." "." insertion)))
        (delete-region start end)
        (insert insertion)
        (indent-region start (point))
        (when arg (forward-char (length insertion))))
    (if (string-match-p "," (buffer-substring start end))
        (let ((insertion
               (mapconcat
                (lambda (x) (string-trim x))
                (split-string (buffer-substring start end) ",") ",\C-j")))
          (delete-region start end)
          (insert insertion)
          (indent-region start (point))
          (when arg (forward-char (length insertion))))
        (let ((insertion
               (mapconcat
                (lambda (x) (string-trim x))
                (split-string (buffer-substring start end) "\\.") "\C-j.")))
          (delete-region start end)
          (insert insertion)
          (indent-region start (point))
          (when arg (forward-char (length insertion)))))
      ))

(defvar mu/personal-notes-target "nb-notes/current.org"
  "Target file for `mu/goto-personal-notes'.
This can be customized to change where F6 navigates to.")

(defun mu/goto-personal-notes ()
  "Go to the personal notes file specified by `mu/personal-notes-target'."
  (interactive)
  (->> mu/personal-notes-target
       (projectile-expand-root)
       (find-file)))

(defun mu/set-personal-notes-target ()
  "Set the current buffer's file as the target for `mu/goto-personal-notes'.
This allows you to dynamically change where F6 takes you.
The path is calculated relative to the project root, excluding nb-notes directory."
  (interactive)
  (if (buffer-file-name)
      (let* ((file-path (buffer-file-name))
             (file-dir (file-name-directory file-path))
             ;; Use same logic as mu/get-project-dir to handle nb-notes
             (in-nb-notes (string-match-p "^/.+/nb-notes\\(/\\|$\\)" file-dir))
             (project-root (if in-nb-notes
                               (file-name-as-directory
                                (car (split-string file-dir "/nb-notes" t)))
                             (or (projectile-project-root)
                                 file-dir)))
             (relative-path (file-relative-name file-path project-root)))
        (setq mu/personal-notes-target relative-path)
        (message "Personal notes target set to: %s (relative to %s)" relative-path project-root))
    (user-error "Current buffer is not visiting a file")))

(defun mu/kmacro-start-or-end-macro (arg)
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro arg)
    (kmacro-start-macro arg)))

(defun mu/file-path-at-point ()
  "Extract @file-path at point, including dots, slashes, underscores, and hyphens."
  (save-excursion
    ;; If we're on '@', move forward one char
    (when (eq (char-after) ?@)
      (forward-char 1))
    ;; First move to end of file path if we're in the middle
    (skip-chars-forward "a-zA-Z0-9-_./")
    (let ((end (point)))
      ;; Then move backwards to find the start
      (skip-chars-backward "a-zA-Z0-9-_./")
      ;; Check if there's an @ just before the start
      (when (and (> (point) (point-min))
                 (eq (char-before) ?@))
        (buffer-substring-no-properties (point) end)))))

(defun mu/open-at-point ()
  "Open the thing at point, if possible.
   If the thing at point looks like a commit hash, open the
   corresponding commit in Magit. If it looks like a URL, open it in
   the browser. If it starts with @, treat it as a project file path."
  (interactive)
  (let ((thing (thing-at-point 'url)))
    (if thing (browse-url-at-point)
      (let* ((file-path (mu/file-path-at-point))
             (thing (thing-at-point 'symbol))
             (toplevel (magit-toplevel))
             (parent-dir (when (and toplevel
                                  (string-match-p "/nb-notes/?$" toplevel))
                          (file-name-directory (directory-file-name toplevel)))))
        (cond (file-path
               ;; Handle @file paths
               (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                            (projectile-project-root))
                                       (and (fboundp 'project-current)
                                            (when-let ((proj (project-current)))
                                              (project-root proj)))
                                       default-directory))
                      (full-path (expand-file-name file-path project-root)))
                 (if (file-exists-p full-path)
                     (find-file full-path)
                   (message "File not found: %s" full-path))))
              ((string-match-p "^[0-9a-f]\\{7,20\\}$" thing)
               (if parent-dir
                   (let ((default-directory parent-dir))
                     (magit-show-commit thing))
                 (magit-show-commit thing)))
              ((string-match-p "^[0-9a-f]\\{21,41\\}$" thing)
               (browse-url (format "https://github.com/codegenome/reservotron/commit/%s" thing)))
              (t (message "Nothing to open at point for: %s" thing)))))))

(defun mu/xdg-open ()
  "Open the file(s) at point with an external application."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path)))
     file-list)))

(defun mu/org-fold-all-done-entries ()
  "Close/fold all entries marked DONE."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (org-entry-is-done-p)
        (hide-entry)))))

(defun mu/select-ruby-block ()
  "Select the current Ruby block."
  (interactive)
  (ruby-end-of-block)
  (move-end-of-line nil)
  (set-mark (point))
  (ruby-beginning-of-block)
  (move-beginning-of-line nil))

(defun mu/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun mu/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun mu/touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))

;; hardcoded for now
(defun mu/search-gems (search-term)
  (interactive (list (read-from-minibuffer "Search gems for: ")))
  (counsel-rg search-term
              (let* ((ruby-version (->> "/opt/asdf-vm/bin/asdf current ruby | tr -s ' ' | cut -d' ' -f2"
                                        (shell-command-to-string)
                                        (string-trim)))
                     (partial-version (replace-regexp-in-string "[0-9]+$" "0" ruby-version))
                     (gems-path "/home/budu/.asdf/installs/ruby/%s/lib/ruby/gems/%s/gems"))
                   (format gems-path ruby-version partial-version))
              "--no-ignore"))

;; wrap region in quotes using Ruby string interpolation
(defun mu/wrap-in-interpolated-quotes ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "}\"")
    (goto-char start)
    (insert "\"#{")
    (goto-char (+ start 1))))

(provide 'functions)

;;; functions.el ends here

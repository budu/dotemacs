;;; package --- Org-mode helpers
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<return>") 'mu/open-at-point)
  (define-key org-mode-map (kbd "M-<return>") 'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-heading-respect-content)
  (define-key org-mode-map (kbd "M-p") 'org-metaup)
  (define-key org-mode-map (kbd "M-n") 'org-metadown)
  (define-key org-mode-map (kbd "M-h") 'er/expand-region)
  (define-key org-mode-map (kbd "C-(") 'org-shiftleft)
  (define-key org-mode-map (kbd "C-)") 'org-shiftright)
  (define-prefix-command 'mu/org-map)
  (define-key org-mode-map (kbd "C-c o") 'mu/org-map)
  (define-key mu/org-map (kbd "p") 'org-priority-up)
  (define-key mu/org-map (kbd "n") 'org-priority-down)
  (define-key mu/org-map (kbd "w") 'mu/org/copy-as-markdown)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-agenda-files '("~/cg/" "~/org/" "~/projects/" "~/.emacs.d"))
  (setq org-hide-emphasis-markers t)
  (setq org-tags-column -20)  ; Adjust the number to control tag position
  (setq org-auto-align-tags t)
  (setq org-tags-case-fold-search t)
  (setq org-todo-keywords
        '((sequence "TODO" "OPEN" "HOLD" "|" "DONE" "FAIL")))
  (setq org-todo-keyword-faces
        '(("OPEN" . (:foreground "cyan" :weight bold))
          ("HOLD" . (:foreground "orange" :weight bold))
          ("FAIL" . (:foreground "red" :weight bold))))
  (setq org-tag-faces
        '(("WIP" . (:foreground "yellow" :weight bold))
          ))
  (when org-tags-special-faces-re
    (setq org-tags-special-faces-re nil))
  (font-lock-add-keywords
   'org-mode '(("^\\*+ "
                ":" nil nil
                (0 (put-text-property (match-beginning 0) (match-end 0) 'display " ")))))
  (setq org-ellipsis "▼")
  ;; from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline           `(:inherit default :weight bold :foreground ,"white")))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple))))
     `(org-level-2 ((t (,@headline ,@variable-tuple))))
     `(org-level-1 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.4 :underline nil))))))
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8
                           :background "#103b66"))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  (setq org-startup-folded t)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("r" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("p" . "quote prompt"))
  ;; Load org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  ;; Optional but recommended: Don't ask for confirmation when executing code blocks
  (setq org-confirm-babel-evaluate nil)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(defun org-get-tag-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (let ((special-tag-face (or (cdr (assoc kwd org-tag-faces))
                              (and (string-match "^@.*" kwd)
                                   (cdr (assoc "@.*" org-tag-faces))))))
    (or (org-face-from-face-or-color 'tag 'org-tag special-tag-face)
        'org-tag)))

(defun mu/org/open-daily-note ()
  "Create and open a daily note for today."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (file-name (concat "daily/" today "-daily.org")))
    (find-file (expand-file-name file-name org-directory))
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Note %s\n\n* Notes\n" today)))))

(defun mu/org/open-daily-note-at-point (orig-fun &rest args)
  "Advice to open daily notes with a custom function.

This function acts as an advice around ORIG-FUN, checking the link at the
current point in an org document.

If the link points to a daily note (`daily/.*-daily\\.org`), it
calls `mu/open-today-daily-note`.  Otherwise, it proceeds by
calling ORIG-FUN with ARGS."
  (let ((link (org-element-property :path (org-element-context))))
    (if (and link (string-match "daily/.*-daily\\.org" link))
        (mu/org/open-daily-note)
      (apply orig-fun args))))

(advice-add 'org-open-at-point
            :around
            #'mu/org/open-daily-note-at-point)

(defun mu/org-insert-example-block ()
  "Insert an example block at point."
  (interactive)
  (let ((start (point)))
    (insert "#+begin_example\n\n#+end_example")
    (goto-char (+ start 16))))

(defun mu/org-insert-src-block ()
  "Insert an example block at point."
  (interactive)
  (let ((start (point)))
    (insert "#+ATTR_LATEX: :options frame=single\n#+begin_src sh\n\n#+end_src")
    (goto-char (+ start 16))))

(add-hook 'org-mode-hook 'mu/org-fold-all-done-entries)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c c") 'mu/org-insert-src-block)
  (define-key org-mode-map (kbd "C-c e") 'mu/org-insert-example-block))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun mu/org/surround-with-tilde ()
  "Surround the symbol at point or region with tildes."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "~")
        (goto-char beg)
        (insert "~"))
    (when (thing-at-point 'symbol)
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
        (goto-char end)
        (insert "~")
        (goto-char beg)
        (insert "~")))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c `") #'mu/org/surround-with-tilde)) ;; replace org-table-edit-field

(defun mu/org/git-hash-to-org-link ()
  "Convert git hash at point into an org link"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (hash (thing-at-point 'word t))
         (default-directory (if (string= (file-name-nondirectory
                                        (directory-file-name default-directory))
                                       "nb-notes")
                              (expand-file-name ".." default-directory)
                            default-directory))
         (repo-url (replace-regexp-in-string
                   "[\n\r]+$" ""
                   (shell-command-to-string "git config --get remote.origin.url")))
         (clean-url (replace-regexp-in-string "\\.git$" "" repo-url))
         (https-url (cond
                    ;; Convert SSH URL format (git@github.com:user/repo)
                    ((string-match "git@\\([^:]+\\):\\(.+\\)" clean-url)
                     (format "https://%s/%s"
                            (match-string 1 clean-url)
                            (match-string 2 clean-url)))
                    ;; Convert git:// format
                    ((string-match "git://\\(.+\\)" clean-url)
                     (format "https://%s" (match-string 1 clean-url)))
                    ;; Already HTTPS or HTTP
                    ((string-match "^https?://" clean-url) clean-url)
                    ;; Default case
                    (t clean-url)))
         (short-hash (substring hash 0 7))
         (org-link (format "[[%s/commit/%s][%s]]" https-url hash short-hash)))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert org-link)))

(global-set-key (kbd "C-c l") 'mu/org/git-hash-to-org-link)

(defun mu/org/copy-as-markdown ()
  (interactive)
  (let ((org-export-with-toc nil)
        (wc (current-window-configuration)))
    (with-current-buffer (org-md-export-as-markdown)
      (clipboard-kill-region (point-min) (point-max))
      (kill-buffer))
    (set-window-configuration wc)
    (message "Copied as Markdown")))

(defun mu/org/auto-link--extract-real-url (ddg-url)
  "Extract the real URL from a DuckDuckGo redirect URL."
  (when (string-match "uddg=\\([^&]+\\)" ddg-url)
    (url-unhex-string (match-string 1 ddg-url))))

(defun mu/org/auto-link-dwim ()
  "Convert text to an org-mode link using DuckDuckGo search.
If region is active, use the region text.
Otherwise, use the word at point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (when bounds
                (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (search-url (when text
                      (concat "https://duckduckgo.com/html/?q="
                             (url-hexify-string text))))
         (url-buffer (url-retrieve-synchronously search-url))
         ddg-url
         url)
    (when (and text url-buffer)
      (with-current-buffer url-buffer
        (goto-char (point-min))
        (when (re-search-forward "class=\"result__url\" href=\"\\([^\"]+\\)\"" nil t)
          (setq ddg-url (match-string 1))
          (when ddg-url
            (setq url (mu/org/auto-link--extract-real-url ddg-url))))
        (kill-buffer))

      ;; Replace the text with org link if we found a URL
      (when (and url bounds)
        (delete-region (car bounds) (cdr bounds))
        (insert (format "[[%s][%s]]" url text))))))

; copy link with shr-maybe-probe-and-copy-url
(with-eval-after-load 'org
  (define-key mu/org-map (kbd "l") 'mu/org/auto-link-dwim))

;; from https://stackoverflow.com/questions/25161792
(defun mu/org/focus ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<tab>") 'mu/org/focus))

(defun mu/org/create-today-todo (title)
  "Create a TODO entry scheduled for today, with TITLE prompted from user."
  (interactive "sTitle of TODO: ")
  (insert (format "* TODO %s\nSCHEDULED: <%s>\n\n#+begin_quote prompt\n\n#+end_quote\n"
                  title
                  (format-time-string "%Y-%m-%d")))
  ;; Move cursor inside the quote block.
  (search-backward "#+end_quote")
  (forward-line -1))

(define-key mu/cg-map (kbd "d") 'mu/org/create-today-todo)

(defun mu/org/copy-block-at-point (&optional element)
  "Copy the current block at point to the kill ring.
If ELEMENT is provided, use it as the starting point for finding the block."
  (interactive)
  (let ((elem (or element (org-element-context))))
    (cond
     ;; Found a src block element
     ((eq (org-element-type elem) 'src-block)
      (let ((content (org-element-property :value elem)))
        (kill-new content)
        (message "Copied src-block to kill ring")))

     ;; Found a block element
     ((string-suffix-p "-block" (symbol-name (org-element-type elem)))
      (let ((begin (org-element-property :contents-begin elem))
            (end (org-element-property :contents-end elem))
            (type-name (symbol-name (org-element-type elem))))
        (kill-ring-save begin end)
        (message "Copied %s to kill ring" type-name)))

     ;; No parent element (reached the top without finding a block)
     ((null (org-element-property :parent elem))
      (message "No org block at point"))

     ;; Try with the parent element
     (t (mu/org/copy-block-at-point (org-element-property :parent elem))))))

(define-key mu/cg-map (kbd "b") 'mu/org/copy-block-at-point)

(defun mu/org--project-files ()
  "Get project files using projectile or project.el."
  (let ((default-directory (mu/get-project-dir)))
    (cond
     ((and (boundp 'projectile-mode)
           projectile-mode
           (projectile-project-p))
      (mapcar (lambda (f)
                (file-relative-name f (projectile-project-root)))
              (projectile-current-project-files)))
     ((fboundp 'project-current)
      (when-let ((proj (project-current)))
        (mapcar (lambda (f) (file-relative-name f (project-root proj)))
                (project-files proj))))
     (t nil))))

(defvar-local mu/org--file-completion-active nil
  "Non-nil when file completion is active.")

(defun mu/org/file-completion-at-point ()
  "Complete project files after @."
  (when-let* ((triggered (eq (char-before) ?@))
              (start (point))
              (end (save-excursion
                     (skip-chars-forward "[:alnum:]/_.-")
                     (point)))
              (prefix (buffer-substring start end))
              (files (mu/org--project-files)))
    (list start end
          (seq-filter
           (lambda (f) (string-prefix-p prefix (file-name-nondirectory f)))
           files)
          :exclusive 'no
          :annotation-function
          (lambda (_candidate) " [file]")
          :exit-function
          (lambda (_status _string)
            (setq mu/org--file-completion-active nil)
            (insert " ")))))

(defun mu/org--trigger-completion-at-point ()
  "Trigger completion when @ is typed."
  (when (eq (char-before) ?@)
    (setq mu/org--file-completion-active t)
    (run-with-idle-timer 0 nil #'completion-at-point)))

(define-minor-mode mu/org-file-completion-mode
  "Toggle file completion with @ prefix in org-mode."
  :lighter " @Compl"
  (if mu/org-file-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'mu/org/file-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'mu/org--trigger-completion-at-point nil t))
    (remove-hook 'completion-at-point-functions #'mu/org/file-completion-at-point t)
    (remove-hook 'post-self-insert-hook #'mu/org--trigger-completion-at-point t)))

(add-hook 'org-mode-hook #'mu/org-file-completion-mode)

(defun mu/org/move-to-done-section ()
  "Move the current task to the DONE section, creating it if necessary.
The DONE section is organized by week (DONE W##) and date (DONE <date>)."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))

  ;; Navigate to the root-level heading
  (while (> (org-outline-level) 1)
    (org-up-heading-safe))

  ;; Change to DONE status if not already
  (unless (string= (org-get-todo-state) "DONE")
    (org-todo "DONE"))

  (let* ((begin (point))
         (end (save-excursion
                (org-end-of-subtree t t)
                (point)))
         (entry-text (buffer-substring-no-properties begin end))
         (week-str (format "W%02d" (string-to-number (format-time-string "%V"))))
         (date-str (format-time-string "<%Y-%m-%d %a>"))
         done-heading
         week-heading
         date-heading)

    ;; Delete the current entry
    (delete-region begin end)

    ;; Go to end of buffer to find or create DONE section
    (goto-char (point-max))

    ;; Look for "* DONE W##" heading
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* ~DONE~ %s$" week-str) nil t)
        (setq week-heading (point))
      ;; Create week heading at end of buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* ~DONE~ %s\n" week-str))
      (setq week-heading (point)))

    ;; Look for "** DONE <date>" under the week heading
    (goto-char week-heading)
    (if (re-search-forward (format "^\\*\\* DONE %s$" (regexp-quote date-str))
                          (save-excursion (org-end-of-subtree t t) (point))
                          t)
        (progn
          ;; Found existing date heading, go to end of its subtree
          (org-back-to-heading t)
          (org-end-of-subtree t t)
          (setq date-heading (point)))
      ;; Create date heading under week heading
      (org-back-to-heading t)
      (org-end-of-subtree t t)
      (insert (format "** DONE %s" date-str))
      (setq date-heading (point)))

    ;; Insert the entry after any existing tasks under the date heading
    ;; Adjust the heading level (add two stars since date heading is level 2)
    (let ((insert-pos (point)))
      (insert "\n" (replace-regexp-in-string "^\\(\\*+\\) " "\\1** "
                                             (string-trim-left entry-text)))

      ;; Collapse the moved task
      (goto-char insert-pos)
      (forward-line 1)
      (org-flag-subtree t))

    (message "Moved task to DONE section")))

(with-eval-after-load 'org
  (define-key mu/org-map (kbd "d") 'mu/org/move-to-done-section))

;;; org.el ends here

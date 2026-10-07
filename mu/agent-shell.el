;;; mu/agent-shell.el --- Agent Shell configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and helper functions for agent-shell integrations

;;; Code:

;;;; ACP (agent-shell)

(use-package shell-maker :vc (:url "https://github.com/xenodium/shell-maker" :rev :newest))
(use-package acp :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))
(use-package agent-shell :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest))

(setopt agent-shell-file-completion-enabled t)

(require 'agent-shell)
(require 'agent-shell-anthropic)
(require 'agent-shell-antigravity)
(require 'agent-shell-openai)

(defcustom mu/agent-shell-session-title-width 100
  "Maximum display width of session titles in the session picker.
Set to nil to show session titles without truncation."
  :type '(choice (integer :tag "Columns")
                 (const :tag "No limit" nil))
  :group 'agent-shell)

(defcustom mu/agent-shell-session-list-worktree-roots
  '("~/cg/reservotron")
  "Main Git checkouts whose worktree sessions should be listed together.

Each entry names the main checkout.  When a session list is requested from
that checkout or from a sibling named PROJECT.NUMBER.BRANCH, all registered
worktrees for the repository are sent to the agent as additional roots."
  :type '(repeat directory)
  :group 'agent-shell)

(defcustom mu/agent-shell-antigravity-default-model-id
  "gemini-3.8-flash-medium"
  "Model to select when starting an Antigravity agent shell.
Set this to nil to let the Antigravity ACP server choose its default."
  :type '(choice (const :tag "Antigravity default" nil)
                 (string :tag "Model ID"))
  :group 'agent-shell)

(defun mu/agent-shell-antigravity--add-default-model (config)
  "Add the configured Antigravity default model to CONFIG."
  (map-put! config :default-model-id
            (lambda () mu/agent-shell-antigravity-default-model-id))
  config)

(unless (advice-member-p #'mu/agent-shell-antigravity--add-default-model
                         'agent-shell-antigravity-make-agent-config)
  (advice-add 'agent-shell-antigravity-make-agent-config :filter-return
              #'mu/agent-shell-antigravity--add-default-model))

(defun mu/agent-shell--session-title (acp-session)
  "Return the display title for ACP-SESSION.
Limit it to `mu/agent-shell-session-title-width' display columns."
  (let ((title (or (map-elt acp-session 'title) "Untitled")))
    (if mu/agent-shell-session-title-width
        (truncate-string-to-width
         title mu/agent-shell-session-title-width nil nil "...")
      title)))

(defun mu/agent-shell--worktree-family-p (directory main-root)
  "Return non-nil when DIRECTORY belongs to MAIN-ROOT's worktree family."
  (let* ((directory (directory-file-name (expand-file-name directory)))
         (main-root (directory-file-name (expand-file-name main-root)))
         (directory-parent (file-name-directory directory))
         (main-parent (file-name-directory main-root))
         (directory-name (file-name-nondirectory directory))
         (main-name (file-name-nondirectory main-root)))
    (and (equal directory-parent main-parent)
         (string-match-p
          (format "\\`%s\\(?:\\.[[:digit:]]+\\..+\\)?\\'"
                  (regexp-quote main-name))
          directory-name))))

(defun mu/agent-shell--configured-worktree-root (directory)
  "Return the configured main checkout matching DIRECTORY, or nil."
  (when-let* ((project-root (locate-dominating-file directory ".git")))
    (seq-find (lambda (main-root)
                (mu/agent-shell--worktree-family-p project-root main-root))
              mu/agent-shell-session-list-worktree-roots)))

(defun mu/agent-shell--git-worktree-roots (directory)
  "Return registered Git worktree roots for DIRECTORY.
Return nil when DIRECTORY is not a Git checkout or Git cannot list its
worktrees."
  (unless (file-remote-p directory)
    (condition-case nil
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (default-directory (file-name-as-directory directory)))
            (when (zerop (process-file
                          "git" nil t nil "-C" directory
                          "worktree" "list" "--porcelain" "-z"))
              (delete-dups
               (delq nil
                     (mapcar
                      (lambda (field)
                        (when (string-prefix-p "worktree " field)
                          (let ((root (directory-file-name
                                       (substring field (length "worktree ")))))
                            (when (file-directory-p root)
                              root))))
                      (split-string (buffer-string) "\0" t)))))))
      (error nil))))

(defun mu/agent-shell--session-list-worktree-roots (cwd)
  "Return additional worktree roots to use for a session list at CWD."
  (when (mu/agent-shell--configured-worktree-root cwd)
    (let ((cwd (directory-file-name (expand-file-name cwd))))
      (seq-remove (lambda (root) (equal root cwd))
                  (mu/agent-shell--git-worktree-roots cwd)))))

(defun mu/agent-shell--decorate-session-list-request (request)
  "Add configured Git worktrees to a session/list ACP REQUEST."
  (if (not (equal (map-elt request :method) "session/list"))
      request
    (let* ((params (map-elt request :params))
           (cwd (map-elt params 'cwd))
           (worktree-roots (and cwd
                                (mu/agent-shell--session-list-worktree-roots
                                 cwd))))
      (if (null worktree-roots)
          request
        (let* ((meta (if (listp (map-elt params '_meta))
                         (copy-tree (map-elt params '_meta))
                       nil))
               (additional-roots
                (delete-dups
                 (append (append (map-elt meta 'additionalRoots) nil)
                         worktree-roots)))
               (meta (cons (cons 'additionalRoots (vconcat additional-roots))
                           (assq-delete-all 'additionalRoots meta)))
               (params (cons (cons '_meta meta)
                             (assq-delete-all '_meta (copy-tree params)))))
          (cons (cons :params params)
                (assq-delete-all :params (copy-tree request))))))))

(setopt agent-shell-outgoing-request-decorator
        #'mu/agent-shell--decorate-session-list-request)

(setopt agent-shell-agent-configs
        (list (agent-shell-anthropic-make-claude-code-config)
              (agent-shell-openai-make-codex-config)
              (agent-shell-antigravity-make-agent-config)
              (agent-shell-opencode-make-agent-config)))

;; (setq agent-shell-anthropic-default-model-id "default")

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setopt agent-shell-anthropic-default-session-mode-id "bypassPermissions")

(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))

(setopt agent-shell-openai-default-session-mode-id "agent-full-access")

(setq agent-shell-antigravity-authentication
      (agent-shell-antigravity-make-authentication :login t))

(setopt agent-shell-antigravity-environment
        (agent-shell-make-environment-variables
         "ANTIGRAVITY_HARNESS_PATH"
         (expand-file-name "~/.local/bin/localharness_external")))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--session-title
              :override #'mu/agent-shell--session-title)
  (setq agent-shell-agent-configs
        (mapcar (lambda (cfg)
                  (when (eq (map-elt cfg :identifier) 'opencode)
                    (map-put! cfg :default-model-id (lambda () "opencode/glm-4.7-free")))
                  cfg)
                agent-shell-agent-configs)))

;;;; Table Faces

;; Zebra rows inherit `lazy-highlight' by default, which is far too loud.
(with-eval-after-load 'agent-shell-markdown
  (set-face-attribute 'agent-shell-markdown-table-zebra nil
                      :inherit 'agent-shell-markdown-table
                      :background "#1a2429")
  (set-face-attribute 'agent-shell-markdown-table-border nil
                      :inherit '(shadow agent-shell-markdown-table)))

(defcustom mu/agent-shell-table-row-spacing 0.3
  "Extra space below each table content row, as `line-spacing' accepts.
A float is a fraction of the frame line height, an integer is pixels.
Set to nil to disable."
  :type '(choice (const :tag "None" nil) number)
  :group 'agent-shell)

(defun mu/agent-shell--mark-table-row-end (render &rest args)
  "Call RENDER with ARGS, tagging the end of content rows.
Header rows are left alone so only data rows get extra spacing."
  (let ((row (apply render args)))
    (when (and (memq (plist-get args :row-face)
                     '(agent-shell-markdown-table agent-shell-markdown-table-zebra))
               (> (length row) 0))
      (put-text-property (1- (length row)) (length row)
                         'mu/agent-shell-table-row-end t row))
    row))

(defun mu/agent-shell--space-table-rows (table)
  "Add `mu/agent-shell-table-row-spacing' below tagged rows of TABLE."
  (dotimes (i (length table))
    (when (and mu/agent-shell-table-row-spacing
               (> i 0)
               (eq (aref table i) ?\n)
               (get-text-property (1- i) 'mu/agent-shell-table-row-end table))
      (put-text-property i (1+ i) 'line-spacing
                         mu/agent-shell-table-row-spacing table)))
  (remove-text-properties 0 (length table)
                          '(mu/agent-shell-table-row-end nil) table)
  table)

(advice-add 'agent-shell-markdown--render-table-data-row :around
            #'mu/agent-shell--mark-table-row-end)
(advice-add 'agent-shell-markdown--render-table-source :filter-return
            #'mu/agent-shell--space-table-rows)

;;;; Link Handling

(defun mu/agent-shell--remote-url-not-local (parse-local-link url)
  "Call PARSE-LOCAL-LINK on URL unless URL is a non-file remote URL.
With `url-handler-mode' on, `file-exists-p' succeeds on http(s) URLs,
so markdown links would open as raw HTML via `find-file' instead of
going through `browse-url'."
  (unless (and (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://" url)
               (not (string-equal-ignore-case (match-string 1 url) "file")))
    (funcall parse-local-link url)))

(with-eval-after-load 'agent-shell-markdown
  (advice-add 'agent-shell-markdown--parse-local-link
              :around #'mu/agent-shell--remote-url-not-local))

;;;; Transcript Scrubbing

(defun mu/agent-shell-scrub-transcript ()
  "Scrub secrets from transcripts when an agent-shell buffer is killed."
  (when (and (derived-mode-p 'agent-shell-mode)
             (bound-and-true-p agent-shell--transcript-file))
    (let ((dir (file-name-directory
                (directory-file-name
                 (file-name-directory agent-shell--transcript-file)))))
      (let ((proc (start-process "scrub-transcripts" nil
                                 "scrub-transcripts" "--apply" "--dir" dir)))
        (set-process-sentinel
         proc (lambda (_proc event)
                (unless (string-match-p "finished" event)
                  (message "scrub-transcripts failed: %s" (string-trim event)))))))))

(add-hook 'kill-buffer-hook #'mu/agent-shell-scrub-transcript)

;;;; Turn Stats in Header

(defvar-local mu/agent-shell--turn-start-time nil
  "Time when the current turn's prompt was submitted.")

(defvar-local mu/agent-shell--last-turn-stats nil
  "Alist with :input-tokens, :output-tokens and :duration of the last turn.")

(defun mu/agent-shell--format-duration (seconds)
  "Format SECONDS as a short duration like 42s, 2m13s or 1h05m."
  (let ((seconds (round seconds)))
    (cond
     ((< seconds 60) (format "%ds" seconds))
     ((< seconds 3600) (format "%dm%02ds" (/ seconds 60) (% seconds 60)))
     (t (format "%dh%02dm" (/ seconds 3600) (/ (% seconds 3600) 60))))))

(defun mu/agent-shell--turn-input-tokens (usage)
  "Return the new input tokens of the turn from USAGE.
Cache reads are excluded as they re-count the whole context on every
API call of the turn."
  (+ (or (map-elt usage :input-tokens) 0)
     (or (map-elt usage :cached-write-tokens) 0)))

(defun mu/agent-shell--on-input-submitted (_event)
  "Record the start time of a new turn."
  (setq mu/agent-shell--turn-start-time (float-time)))

(defun mu/agent-shell--on-turn-complete (event)
  "Save the token usage and duration of the turn completed by EVENT."
  (let ((usage (map-nested-elt event '(:data :usage))))
    (setq mu/agent-shell--last-turn-stats
          `((:input-tokens . ,(mu/agent-shell--turn-input-tokens usage))
            (:output-tokens . ,(or (map-elt usage :output-tokens) 0))
            (:duration . ,(when mu/agent-shell--turn-start-time
                            (- (float-time) mu/agent-shell--turn-start-time))))))
  (setq mu/agent-shell--turn-start-time nil)
  (agent-shell--update-header-and-mode-line))

(defun mu/agent-shell--subscribe-turn-stats ()
  "Track turn stats in the current agent-shell buffer."
  (agent-shell-subscribe-to :shell-buffer (current-buffer)
                            :event 'input-submitted
                            :on-event #'mu/agent-shell--on-input-submitted)
  (agent-shell-subscribe-to :shell-buffer (current-buffer)
                            :event 'turn-complete
                            :on-event #'mu/agent-shell--on-turn-complete))

(add-hook 'agent-shell-mode-hook #'mu/agent-shell--subscribe-turn-stats)

(defun mu/agent-shell--turn-stats-indicator (state)
  "Return the last turn stats of STATE's shell buffer, or nil."
  (when-let* ((shell-buffer (map-elt state :buffer))
              ((buffer-live-p shell-buffer))
              (stats (buffer-local-value 'mu/agent-shell--last-turn-stats
                                         shell-buffer)))
    (let ((input (map-elt stats :input-tokens))
          (output (map-elt stats :output-tokens))
          (duration (map-elt stats :duration)))
      (string-join
       (delq nil (list (when (> (+ input output) 0)
                         (format "%s↑ %s↓"
                                 (agent-shell--format-number-compact input)
                                 (agent-shell--format-number-compact output)))
                       (when duration
                         (mu/agent-shell--format-duration duration))))
       " · "))))

(defun mu/agent-shell--add-turn-stats-to-header (make-header-model state &rest args)
  "Append the last turn stats to the context indicator of the header model.
MAKE-HEADER-MODEL is called with STATE and ARGS."
  (let ((model (apply make-header-model state args))
        (stats (mu/agent-shell--turn-stats-indicator state)))
    (unless (string-empty-p (or stats ""))
      (let ((indicator (map-elt model :context-indicator)))
        (setf (alist-get :context-indicator model)
              (if indicator (concat indicator " ➤ " stats) stats))))
    model))

(advice-add 'agent-shell--make-header-model :around
            #'mu/agent-shell--add-turn-stats-to-header)

;;;; Helper Functions

(defun mu/get-agent-shell-buffer ()
  "Get the most recent agent-shell buffer for the current project.
Falls back to any agent-shell buffer if none match the project."
  (let ((project-buffers (agent-shell-project-buffers)))
    (if project-buffers
        (get-buffer (car project-buffers))
      ;; Fallback: any agent-shell buffer, most recent first
      (seq-find (lambda (buf)
                  (with-current-buffer buf
                    (derived-mode-p 'agent-shell-mode)))
                (buffer-list)))))

(defun mu/agent-shell-send-region-internal (buffer start end)
  "Send region from START to END to agent-shell BUFFER.
Uses agent-shell-add-region internally."
  ;; Activate the region in the current buffer
  (goto-char start)
  (push-mark end nil t)
  (activate-mark)
  (agent-shell-send-region buffer))

;;;; Interactive Commands

(cl-defun agent-shell-send-region (&optional buffer)
  "Send region to agent shell BUFFER prompt and submit immediately.

If BUFFER is nil, use the last accessed shell buffer in project.
The region content is sent as a prompt without any formatting or metadata."
  (interactive)
  (let* ((region (or (agent-shell--get-region :deactivate t)
                     (user-error "No region selected")))
         (content (map-elt region :content)))
    (agent-shell-send-prompt buffer content)))

(cl-defun agent-shell-send-prompt (&optional buffer prompt)
  "Send PROMPT text to agent shell BUFFER and submit immediately.

If BUFFER is nil, use the last accessed shell buffer in project.
The region content is sent as a prompt without any formatting or metadata."
  (interactive)
  (let* ((shell-buffer (or buffer
                           (seq-first (agent-shell-project-buffers))
                           (user-error "No agent shell buffers available for current project"))))
    (with-current-buffer shell-buffer
      (when (shell-maker-busy)
        (user-error "Busy, try later"))
      (goto-char (point-max))
      (insert prompt)
      (shell-maker-submit))
    (agent-shell--display-buffer shell-buffer)))

(defun mu/agent-shell--agent-buffer-p (buffer)
  "Return non-nil when BUFFER is a live `agent-shell-mode' buffer."
  (and buffer
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'agent-shell-mode))))

(defun mu/agent-shell--ensure-agent-buffer (buffer)
  "Return BUFFER when it is a valid agent shell buffer, else nil."
  (when (mu/agent-shell--agent-buffer-p buffer)
    buffer))

(defun mu/agent-shell--resolve-agent-buffer ()
  "Return the most relevant agent shell buffer after launching."
  (or (mu/agent-shell--ensure-agent-buffer
       (when (derived-mode-p 'agent-shell-mode)
         (current-buffer)))
      (mu/agent-shell--ensure-agent-buffer (mu/get-agent-shell-buffer))))

(defun mu/agent-shell--display-buffer (buffer)
  "Display BUFFER in the current frame and return it."
  (when (mu/agent-shell--agent-buffer-p buffer)
    (if-let ((window (get-buffer-window buffer)))
        (select-window window)
      (switch-to-buffer buffer))
    buffer))

(defun mu/agent-shell--select-existing-buffer ()
  "Focus the latest agent shell buffer when available."
  (when-let ((buffer (mu/agent-shell--ensure-agent-buffer (mu/get-agent-shell-buffer))))
    (mu/agent-shell--display-buffer buffer)))

(defun mu/agent-shell--start-interactive-shell (target-dir)
  "Launch a new agent shell via `agent-shell' inside TARGET-DIR."
  (let* ((default-directory target-dir)
         (buffer (agent-shell-start :config (or (agent-shell-select-config
                                                 :prompt "Start new agent: ")
                                                (error "No agent config found")))))
    (mu/agent-shell--display-buffer buffer)))

(defun mu/agent-shell--start-default-shell (target-dir)
  "Launch a new agent shell using the first configured agent in TARGET-DIR."
  (let* ((default-directory target-dir)
         (config (or (car agent-shell-agent-configs)
                     (error "No agent config found")))
         (buffer (agent-shell-start :config config)))
    (mu/agent-shell--display-buffer buffer)))

(defun mu/agent-shell--focus-buffer (buffer)
  "Move point to the most relevant location inside BUFFER."
  (when (mu/agent-shell--agent-buffer-p buffer)
    (with-current-buffer buffer
      (unless (agent-shell-jump-to-latest-permission-button-row)
        (goto-char (point-max))
        (when-let ((window (get-buffer-window buffer)))
          (set-window-point window (point))))))
  buffer)

(defun mu/agent-shell--buffers ()
  "Return live agent shell buffers in most-recently-used order."
  (seq-filter #'mu/agent-shell--agent-buffer-p (buffer-list)))

(defun mu/agent-shell--next-buffer ()
  "Cycle to the next agent-shell buffer.
Buffers are ordered by most recent use, wrapping around at the end."
  (let* ((buffers (mu/agent-shell--buffers))
         (current (current-buffer))
         (tail (cdr (memq current buffers)))
         (next (or (car tail) (car buffers))))
    (when (and next (not (eq next current)))
      next)))

(defun mu/agent-shell--choose-buffer-or-start (buffers target-dir)
  "Choose among agent shell BUFFERS or start a new one in TARGET-DIR."
  (let* ((start-label "Start new agent shell...")
         (buffer-choices
          (mapcar
           (lambda (buffer)
             (let ((status (agent-shell-status :shell-buffer buffer)))
               (cons
                (concat (buffer-name buffer)
                        (pcase status
                          ('busy (propertize " [busy]" 'face 'warning))
                          ('blocked (propertize " [blocked]" 'face 'error))
                          (_ "")))
                buffer)))
           buffers))
         (choices (append (mapcar #'car buffer-choices)
                          (list start-label)))
         (choice
          (minibuffer-with-setup-hook
              (lambda ()
                (use-local-map (copy-keymap (current-local-map)))
                (local-set-key (kbd "<f8>") #'ignore))
            (completing-read "Agent shell: " choices nil t))))
    (if (equal choice start-label)
        (mu/agent-shell--start-interactive-shell target-dir)
      (mu/agent-shell--display-buffer (cdr (assoc choice buffer-choices))))))

(defun mu/agent-shell-smart-switch (&optional arg)
  "Smart agent-shell buffer switching:
- If already in an agent-shell buffer and more than two exist, select one
  from a minibuffer prompt or start a new agent shell
- If already in an agent-shell buffer and at most two exist, cycle to the next
- If agent-shell buffer exists and is displayed, switch to that window
- If agent-shell buffer exists but not displayed, switch to it and go to end
- If no agent-shell buffer exists, create one and go to end
- Never starts agent-shell inside nb-notes directory, always in containing git project

With prefix ARG (such as using `C-u`), always start a new agent shell via
`agent-shell`, allowing you to select the agent."
  (interactive "P")
  (cond
   ;; From an agent shell, choose when there are several or cycle when there
   ;; are only one or two.
   ((and (not arg)
         (derived-mode-p 'agent-shell-mode))
    (let ((buffers (mu/agent-shell--buffers)))
      (if (> (length buffers) 2)
          (mu/agent-shell--focus-buffer
           (mu/agent-shell--choose-buffer-or-start
            buffers (mu/get-project-dir)))
        (if-let ((next (mu/agent-shell--next-buffer)))
            (mu/agent-shell--focus-buffer
             (mu/agent-shell--display-buffer next))
          (message "No other agent-shell buffers")))))
   ;; Otherwise, do the normal smart switch behavior
   (t
    (let* ((force-new arg)
           (target-dir (mu/get-project-dir)))
      (mu/agent-shell--focus-buffer
       (cond
        (force-new (mu/agent-shell--start-interactive-shell target-dir))
        ((mu/agent-shell--select-existing-buffer))
        (t (mu/agent-shell--start-default-shell target-dir))))))))

(defun mu/send-prompt-block-to-agent-shell (&optional arg)
  "Send surrounding prompt block to agent-shell.
Works with both org-mode blocks (#+begin_quote prompt) and markdown code blocks (```).
Does nothing if cursor is not inside such a block.
With prefix ARG, switch to agent-shell buffer after sending."
  (interactive "P")
  (let ((block-start nil)
        (block-end nil)
        (current-pos (point)))
    ;; Try to find surrounding block
    (save-excursion
      (cond
       ;; Check for org-mode #+begin_quote prompt block
       ((save-excursion
          (and (re-search-backward "^#\\+begin_quote[ \t]+prompt[ \t]*$" nil t)
               (let ((begin-pos (line-beginning-position 2)))
                 (when (re-search-forward "^#\\+end_quote[ \t]*$" nil t)
                   (let ((end-pos (1- (line-beginning-position))))
                     (when (and (<= begin-pos current-pos)
                                (>= end-pos current-pos))
                       (setq block-start begin-pos
                             block-end end-pos)
                       t))))))
        ;; Found org block, already set
        )
       ;; Check for markdown ``` block
       ((save-excursion
          (and (re-search-backward "^```" nil t)
               (let ((begin-pos (line-beginning-position 2)))
                 (when (re-search-forward "^```" nil t)
                   (let ((end-pos (1- (line-beginning-position))))
                     (when (and (<= begin-pos current-pos)
                                (>= end-pos current-pos))
                       (setq block-start begin-pos
                             block-end end-pos)
                       t))))))
        ;; Found markdown block, already set
        )))
    ;; Send block if found
    (if (and block-start block-end)
        (let ((agent-buffer (mu/get-agent-shell-buffer)))
          (unless agent-buffer
            (user-error "No agent-shell buffer found. Start one first with M-x agent-shell-anthropic-start-claude-code"))
          (mu/agent-shell-send-region-internal agent-buffer block-start block-end)
          (deactivate-mark)
          (when arg
            (pop-to-buffer agent-buffer)))
      (message "Cursor is not inside a prompt block"))))

(defun mu/agent-shell-send-prompt-from-notes ()
  "Send a prompt from nb-notes/prompts.org to agent-shell.
If prompts.org is not found, fallback to nb-notes/current.org.
Prompts are stored under the '* :robot: Prompts' heading.
Each sub-heading is a prompt - uses the heading title if no content,
or the first #+begin_quote prompt block if present."
  (interactive)
  (let* ((project-dir (mu/get-project-dir))
         (prompts-file (expand-file-name "nb-notes/prompts.org" project-dir))
         (current-file (expand-file-name "nb-notes/current.org" project-dir))
         (notes-file (cond
                      ((file-exists-p prompts-file) prompts-file)
                      ((file-exists-p current-file) current-file)
                      (t nil)))
         (agent-buffer (mu/get-agent-shell-buffer)))
    ;; Check agent buffer exists
    (unless agent-buffer
      (user-error "No agent-shell buffer found. Start one first with M-x agent-shell-anthropic-start-claude-code"))
    ;; Check notes file exists
    (unless notes-file
      (user-error "Notes file not found. Tried: %s and %s" prompts-file current-file))
    ;; Parse prompts from notes file
    (let ((prompts-alist '()))
      (with-temp-buffer
        (insert-file-contents notes-file)
        (org-mode)
        (goto-char (point-min))
        ;; Find the :robot: Prompts heading
        (unless (re-search-forward "^\\*+ +:robot: +Prompts" nil t)
          (user-error "No '* :robot: Prompts' heading found in %s" notes-file))
        (let ((prompts-level (org-current-level)))
          ;; Iterate through sub-headings
          (while (and (outline-next-heading)
                      (> (org-current-level) prompts-level))
            (when (= (org-current-level) (1+ prompts-level))
              (let* ((heading (org-get-heading t t t t))
                     (content-start (save-excursion
                                      (forward-line 1)
                                      (point)))
                     (content-end (save-excursion
                                    (or (outline-next-heading)
                                        (point-max))))
                     (content (buffer-substring-no-properties content-start content-end))
                     (prompt nil))
                ;; Try to extract prompt from #+begin_quote prompt block
                (if (string-match "^[ \t]*#\\+begin_quote[ \t]+prompt[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)[ \t]*#\\+end_quote" content)
                    (setq prompt (string-trim (match-string 1 content)))
                  ;; Use heading as prompt if no content block
                  (setq prompt (string-trim heading)))
                ;; Add to alist
                (push (cons heading prompt) prompts-alist))))))
      ;; Check we found prompts
      (unless prompts-alist
        (user-error "No prompts found under '* :robot: Prompts' heading"))
      ;; Reverse to maintain order from file
      (setq prompts-alist (nreverse prompts-alist))
      ;; Let user select a prompt
      (let* ((selected-heading (completing-read "Select prompt: "
                                                (mapcar #'car prompts-alist)
                                                nil t))
             (selected-prompt (cdr (assoc selected-heading prompts-alist))))
        ;; Send the prompt
        (agent-shell-send-prompt agent-buffer selected-prompt)
        (message "Sent prompt: %s" selected-heading)))))

(provide 'mu/agent-shell)

;;; agent-shell.el ends here

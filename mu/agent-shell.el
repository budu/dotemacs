;;; mu/agent-shell.el --- Agent Shell configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and helper functions for agent-shell (Anthropic Claude integration)

;;; Code:

;;;; ACP (agent-shell)

(use-package shell-maker :vc (:url "https://github.com/xenodium/shell-maker" :rev :newest))
(use-package acp :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))
(use-package agent-shell :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest))

(setopt agent-shell-file-completion-enabled t)

(require 'agent-shell)
(require 'agent-shell-anthropic)
(require 'agent-shell-openai)

(custom-set-variables
 '(agent-shell-agent-configs
   (list (agent-shell-anthropic-make-claude-code-config)
         (agent-shell-openai-make-codex-config)
         (agent-shell-google-make-gemini-config)
         (agent-shell-opencode-make-agent-config)
         )))

(setq agent-shell-anthropic-default-model-id "sonnet")

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))

(setq agent-shell-google-authentication
      (agent-shell-google-make-authentication :login t))

(with-eval-after-load 'agent-shell
  (setq agent-shell-agent-configs
        (mapcar (lambda (cfg)
                  (when (eq (map-elt cfg :identifier) 'opencode)
                    (map-put! cfg :default-model-id (lambda () "opencode/glm-4.7-free")))
                  cfg)
                agent-shell-agent-configs)))

;;;; Helper Functions

(defun mu/get-agent-shell-buffer ()
  "Get the most recent agent-shell buffer, or nil if none exists."
  (let ((agent-buffers (seq-filter (lambda (buf)
                                     (with-current-buffer buf
                                       (derived-mode-p 'agent-shell-mode)))
                                   (buffer-list))))
    (car (last agent-buffers))))

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
  "Launch a new agent shell using the default anthropic command in TARGET-DIR."
  (let ((default-directory target-dir))
    (agent-shell-anthropic-start-claude-code))
  (when-let ((buffer (mu/agent-shell--resolve-agent-buffer)))
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

(defun mu/agent-shell-smart-switch (&optional arg)
  "Smart agent-shell buffer switching:
- If already in agent-shell buffer and this command was just called, cycle session mode
- If agent-shell buffer exists and is displayed, switch to that window
- If agent-shell buffer exists but not displayed, switch to it and go to end
- If no agent-shell buffer exists, create one and go to end
- Never starts agent-shell inside nb-notes directory, always in containing git project

With prefix ARG (such as using `C-u`), always start a new agent shell via
`agent-shell`, allowing you to select the agent."
  (interactive "P")
  (cond
   ;; If we're already in an agent-shell buffer and this command was just called,
   ;; cycle the session mode instead of switching
   ((and (not arg)
         (derived-mode-p 'agent-shell-mode)
         (eq last-command 'mu/agent-shell-smart-switch))
    (agent-shell-cycle-session-mode))
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

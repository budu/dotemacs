;;; copilot.el --- Copilot configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for GitHub Copilot with completion logging.
;;
;;; Code:

;; NOTE: DISABLED and replaced by minuet
(use-package copilot
  :disabled t
  :hook ((prog-mode . copilot-mode)
         (text-mode . copilot-mode)
         (git-commit-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :bind (:map copilot-completion-map
              ("<return>" . copilot-accept-completion)
              ("<M-tab>" . copilot-next-completion))
  :config
  (setq copilot-max-char-warning-disable t)
  (set-face-attribute 'copilot-overlay-face nil :foreground "#585"))

;;;; Copilot Completion Logging

(defgroup copilot-logger nil
  "Logging for Copilot completions."
  :group 'copilot
  :prefix "copilot-logger-")

(defcustom copilot-logger-enabled t
  "Whether to enable Copilot completion logging."
  :type 'boolean
  :group 'copilot-logger)

(defcustom copilot-logger-file (expand-file-name "~/.emacs.d/copilot-completions.log")
  "File path for storing Copilot completion logs."
  :type 'file
  :group 'copilot-logger)

(defcustom copilot-logger-max-entries 5000
  "Maximum number of log entries to keep in memory.
Older entries are removed when this limit is exceeded."
  :type 'integer
  :group 'copilot-logger)

;;; Internal variables

(defvar copilot-logger--entries nil
  "List of completion log entries.
Each entry is a plist with keys:
  :timestamp - Time when completion was requested
  :file - Buffer file name
  :mode - Major mode
  :prompt - Text before point (context)
  :completion - Completion text offered
  :uuid - Unique identifier for this completion
  :request-time - Time when request was made
  :response-time - Time when response was received
  :duration - Request duration in seconds
  :status - 'accepted, 'rejected, or 'pending
  :action-time - Time when user accepted/rejected")

(defvar copilot-logger--pending-requests (make-hash-table :test 'equal)
  "Hash table tracking pending completion requests.
Key is buffer, value is plist with :request-time and :doc.")

;;; Logging functions

(defun copilot-logger--current-context (&optional max-chars)
  "Get current buffer context (text before point).
Limit to MAX-CHARS characters (default 500)."
  (let ((max-chars (or max-chars 500)))
    (save-excursion
      (buffer-substring-no-properties
       (max (point-min) (- (point) max-chars))
       (point)))))

(defun copilot-logger--add-entry (entry)
  "Add ENTRY to the log.
Trims old entries if over limit."
  (when copilot-logger-enabled
    (push entry copilot-logger--entries)
    (when (> (length copilot-logger--entries) copilot-logger-max-entries)
      (setq copilot-logger--entries
            (cl-subseq copilot-logger--entries 0 copilot-logger-max-entries)))
    (copilot-logger--write-all-entries-to-file)))

(defun copilot-logger--write-all-entries-to-file ()
  "Write all entries to log file, replacing existing content."
  (when copilot-logger-file
    (condition-case err
        (with-temp-file copilot-logger-file
          (dolist (entry (reverse copilot-logger--entries))
            (insert (format "%s\n" (prin1-to-string entry)))))
      (error (message "Copilot logger: Failed to write to file: %s" err)))))

(defun copilot-logger--format-entry (entry)
  "Format ENTRY as human-readable string."
  (let-alist entry
    (format "[%s] %s - %s\n  File: %s\n  Prompt: %s\n  Completion: %s\n  Duration: %.3fs\n  Status: %s\n"
            (format-time-string "%Y-%m-%d %H:%M:%S" .timestamp)
            .mode
            (or .file "scratch")
            (or .file "N/A")
            (truncate-string-to-width (or .prompt "") 80 nil nil "...")
            (truncate-string-to-width (or .completion "") 100 nil nil "...")
            (or .duration 0)
            (or .status 'unknown))))

;;; Advice functions

(defun copilot-logger--before-get-completion (orig-fn callback)
  "Advice before `copilot--get-completion' to log request.
ORIG-FN is the original function, CALLBACK is passed through."
  (when copilot-logger-enabled
    (let* ((request-time (current-time))
           (context (copilot-logger--current-context))
           (doc-info (list :request-time request-time
                          :context context
                          :file (buffer-file-name)
                          :mode major-mode)))
      (puthash (current-buffer) doc-info copilot-logger--pending-requests)))
  (funcall orig-fn callback))

(defun copilot-logger--after-show-completion (completion-data)
  "Advice after `copilot--show-completion' to log response.
COMPLETION-DATA contains the completion information."
  (when copilot-logger-enabled
    (let* ((pending (gethash (current-buffer) copilot-logger--pending-requests))
           (response-time (current-time))
           (request-time (plist-get pending :request-time))
           (duration (when request-time
                      (float-time (time-subtract response-time request-time))))
           (text (plist-get completion-data :text))
           (uuid (plist-get completion-data :uuid)))
      (when pending
        (let ((entry (list :timestamp request-time
                          :file (plist-get pending :file)
                          :mode (plist-get pending :mode)
                          :prompt (plist-get pending :context)
                          :completion text
                          :uuid uuid
                          :request-time request-time
                          :response-time response-time
                          :duration duration
                          :status 'pending
                          :action-time nil)))
          (copilot-logger--add-entry entry))
        ;; Keep pending for tracking accept/reject
        (puthash (current-buffer)
                (plist-put pending :uuid uuid)
                copilot-logger--pending-requests)))))

(defun copilot-logger--after-clear-overlay (&optional is-accepted)
  "Advice after `copilot-clear-overlay' to log rejection.
IS-ACCEPTED indicates if completion was accepted."
  (when (and copilot-logger-enabled (not is-accepted))
    (let* ((pending (gethash (current-buffer) copilot-logger--pending-requests))
           (uuid (plist-get pending :uuid)))
      (when uuid
        ;; Update the most recent entry with matching UUID
        (let ((entry (cl-find-if (lambda (e)
                                   (and (equal (plist-get e :uuid) uuid)
                                        (eq (plist-get e :status) 'pending)))
                                 copilot-logger--entries)))
          (when entry
            (plist-put entry :status 'rejected)
            (plist-put entry :action-time (current-time))
            (copilot-logger--write-all-entries-to-file)))
        (remhash (current-buffer) copilot-logger--pending-requests)))))

(defun copilot-logger--after-accept-completion (result)
  "Advice after `copilot-accept-completion' to log acceptance.
RESULT is the return value from the original function."
  (when (and copilot-logger-enabled result)
    (let* ((pending (gethash (current-buffer) copilot-logger--pending-requests))
           (uuid (plist-get pending :uuid)))
      (when uuid
        ;; Update the most recent entry with matching UUID
        (let ((entry (cl-find-if (lambda (e)
                                   (and (equal (plist-get e :uuid) uuid)
                                        (eq (plist-get e :status) 'pending)))
                                 copilot-logger--entries)))
          (when entry
            (plist-put entry :status 'accepted)
            (plist-put entry :action-time (current-time))
            (copilot-logger--write-all-entries-to-file)))
        (remhash (current-buffer) copilot-logger--pending-requests))))
  result)

;;; Copilot inhibition for file completion

(defun mu/copilot--should-inhibit-completion ()
  "Return non-nil if copilot should be inhibited in current context."
  (and (boundp 'mu/org--file-completion-active)
       mu/org--file-completion-active))

(defun mu/copilot--maybe-inhibit-completion (orig-fn callback)
  "Inhibit copilot during file completion in org-mode.
ORIG-FN is the original function, CALLBACK is passed through."
  (if (mu/copilot--should-inhibit-completion)
      nil  ; Don't call copilot when file completion is active
    (funcall orig-fn callback)))

;;; Install advice

(with-eval-after-load 'copilot
  (advice-add 'copilot--get-completion :around #'mu/copilot--maybe-inhibit-completion)
  (advice-add 'copilot--get-completion :around #'copilot-logger--before-get-completion)
  (advice-add 'copilot--show-completion :after #'copilot-logger--after-show-completion)
  (advice-add 'copilot-clear-overlay :after #'copilot-logger--after-clear-overlay)
  (advice-add 'copilot-accept-completion :filter-return #'copilot-logger--after-accept-completion))

;;; User commands

(defun copilot-logger-view-log ()
  "View recent Copilot completion logs in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Copilot Completion Log*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Copilot Completion Log\n")
        (insert "======================\n\n")
        (if copilot-logger--entries
            (dolist (entry (reverse copilot-logger--entries))
              (insert (copilot-logger--format-entry entry))
              (insert "\n"))
          (insert "No completions logged yet.\n"))
        (goto-char (point-min))
        (view-mode)))
    (switch-to-buffer buf)))

(defun copilot-logger-clear-log ()
  "Clear all completion logs."
  (interactive)
  (when (yes-or-no-p "Clear all Copilot completion logs?")
    (setq copilot-logger--entries nil)
    (clrhash copilot-logger--pending-requests)
    (message "Copilot completion log cleared")))

(defun copilot-logger--count-tokens (text)
  "Estimate token count for TEXT.
Uses a simple heuristic: ~4 characters per token on average."
  (if (stringp text)
      (ceiling (/ (length text) 4.0))
    0))

(defun copilot-logger--csv-escape (str)
  "Escape STR for CSV format."
  (if (not str)
      ""
    (let ((str (format "%s" str)))
      (if (or (string-match-p "[,\"\n]" str))
          (concat "\"" (replace-regexp-in-string "\"" "\"\"" str) "\"")
        str))))

(defun copilot-logger-export-csv (file)
  "Export completion logs to CSV FILE."
  (interactive "FExport logs to CSV file: ")
  (with-temp-file file
    (insert "timestamp,file,mode,prompt_tokens,completion_tokens,duration_sec,status\n")
    (dolist (entry (reverse copilot-logger--entries))
      (let* ((timestamp (plist-get entry :timestamp))
             (file-name (plist-get entry :file))
             (mode (plist-get entry :mode))
             (prompt (plist-get entry :prompt))
             (completion (plist-get entry :completion))
             (duration (plist-get entry :duration))
             (status (plist-get entry :status))
             (prompt-tokens (copilot-logger--count-tokens prompt))
             (completion-tokens (copilot-logger--count-tokens completion)))
        (insert (format "%s,%s,%s,%d,%d,%.3f,%s\n"
                       (copilot-logger--csv-escape (format-time-string "%Y-%m-%d %H:%M:%S" timestamp))
                       (copilot-logger--csv-escape file-name)
                       (copilot-logger--csv-escape mode)
                       prompt-tokens
                       completion-tokens
                       (or duration 0)
                       (copilot-logger--csv-escape status))))))
  (message "Exported %d entries to %s" (length copilot-logger--entries) file))

(defun copilot-logger-statistics ()
  "Show statistics about Copilot completions."
  (interactive)
  (let* ((total (length copilot-logger--entries))
         (accepted (cl-count-if (lambda (e) (eq (plist-get e :status) 'accepted))
                               copilot-logger--entries))
         (rejected (cl-count-if (lambda (e) (eq (plist-get e :status) 'rejected))
                               copilot-logger--entries))
         (pending (cl-count-if (lambda (e) (eq (plist-get e :status) 'pending))
                              copilot-logger--entries))
         (durations (cl-remove-if #'null
                                 (mapcar (lambda (e) (plist-get e :duration))
                                        copilot-logger--entries)))
         (avg-duration (if durations
                          (/ (apply #'+ durations) (float (length durations)))
                        0))
         (acceptance-rate (if (> total 0)
                             (* 100.0 (/ (float accepted) total))
                           0)))
    (message "Copilot Stats: %d total | %d accepted (%.1f%%) | %d rejected | %d pending | Avg duration: %.3fs"
             total accepted acceptance-rate rejected pending avg-duration)))

(provide 'mu/copilot)

;;; copilot.el ends here

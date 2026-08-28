;;; package --- Auto-clean buffers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mu/auto-clean-buffers/buffer-old-p (buffer)
  "Return t if BUFFER hasn't been accessed in the last 24 hours."
  (let ((time (current-time)))
    (when-let ((buffer-time (buffer-local-value 'buffer-display-time buffer)))
      (> (float-time (time-subtract time buffer-time))
         (* 48 60 60)))))

(defun mu/auto-clean-buffers/get-old-buffers ()
  "Return a list of buffers that haven't been accessed in 24 hours."
  (seq-filter
   (lambda (buf)
     (and (mu/auto-clean-buffers/buffer-old-p buf)
          (not (buffer-modified-p buf))     ; Skip modified buffers
          (not (string-match-p "\\`[ *]" (buffer-name buf))) ; Skip special buffers
          (not (get-buffer-process buf))))  ; Skip buffers with active processes
   (buffer-list)))

(defun mu/auto-clean-buffers/get-file-buffers ()
  "Return a list of buffers that visit files (excluding special buffers)."
  (seq-filter
   (lambda (buf)
     (and (buffer-file-name buf)
          (not (string-match-p "\\`[ *]" (buffer-name buf)))))
   (buffer-list)))

(defun mu/auto-clean-buffers/cleanup-old-buffers ()
  "Close buffers that haven't been accessed in 24 hours, keeping at least 12 file buffers open."
  (interactive)
  (let* ((old-buffers (mu/auto-clean-buffers/get-old-buffers))
         (file-buffers (mu/auto-clean-buffers/get-file-buffers))
         (num-old (length old-buffers))
         (total-file-buffers (length file-buffers))
         (min-buffers 12) ; Minimum number of file buffers to keep open
         (num-to-close (max 0 (min num-old (- total-file-buffers min-buffers)))))
    (when (> num-to-close 0)
      (let ((buffers-to-close (seq-take old-buffers num-to-close)))
        (message "Closing %d unused buffer(s) (keeping at least %d file buffers)"
                 num-to-close min-buffers)
        (mapc 'kill-buffer buffers-to-close)))))

(defun mu/auto-clean-buffers/setup-automatic-buffer-cleanup ()
  "Set up automatic buffer cleanup to run every hour."
  (interactive)
  (run-with-timer 0 3600 'mu/auto-clean-buffers/cleanup-old-buffers))  ; Run every hour (3600 seconds)

;; Enable automatic cleanup
(mu/auto-clean-buffers/setup-automatic-buffer-cleanup)

;;; auto-clean-buffers.el ends here

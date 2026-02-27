;;; package --- CG job-related helpers
;;; Commentary:
;;; Code:

(require 'org)

(defcustom mu/cg/appsignal-site-id "59165566221393096f71ec84"
  "AppSignal site ID used for building incident URLs.
Can be set per-project via .dir-locals.el."
  :type 'string
  :group 'mu-cg
  :safe #'stringp)

(defun mu/cg/create-ticket-todo (ticket-number)
  "Create a TODO entry for the specified TICKET-NUMBER."
  (interactive "nTicket number: ")
  (let ((today (format-time-string "%Y-%m-%d"))
        (template-file (expand-file-name "templates/ticket.org")))
    (if (file-exists-p template-file)
        (progn
          (find-file template-file)
          (let ((template-content (buffer-string)))
            (kill-buffer)
            (org-insert-heading)
            (insert (format "TODO [[https://hub.reservotron.com/tickets/%d][Ticket #%d]] :ticket:\n"
                          ticket-number ticket-number))
            (org-schedule nil today)
            (insert "\n")
            (let ((template-start (point)))
              (insert template-content)
              (save-excursion
                (save-restriction
                  (narrow-to-region template-start (point))
                  (goto-char template-start)
                  (while (search-forward "NUMBER" nil t)
                    (replace-match (number-to-string ticket-number) t t)))))))
      (message "Template file not found: %s" template-file))))

(define-key mu/cg-map (kbd "t") 'mu/cg/create-ticket-todo)

(defun mu/cg/create-incident-todo (incident-number)
  "Create a TODO entry for the specified INCIDENT-NUMBER."
  (interactive "nIncident number: ")
  (let ((today (format-time-string "%Y-%m-%d"))
        (template-file (expand-file-name "templates/incident.org")))
    (if (file-exists-p template-file)
        (progn
          (find-file template-file)
          (let ((template-content (buffer-string)))
            (kill-buffer)
            (org-insert-heading)
            (insert (format "TODO [[https://appsignal.com/code-genome/sites/%s/exceptions/incidents/%d/samples/last][Incident #%d]] :incident:\n"
                          mu/cg/appsignal-site-id incident-number incident-number))
            (org-schedule nil today)
            (insert "\n")
            (let ((template-start (point)))
              (insert template-content)
              (save-excursion
                (save-restriction
                  (narrow-to-region template-start (point))
                  (goto-char template-start)
                  (while (search-forward "NUMBER" nil t)
                    (replace-match (number-to-string incident-number) t t)))))))
      (message "Template file not found: %s" template-file))))

(define-key mu/cg-map (kbd "i") 'mu/cg/create-incident-todo)

(defun mu/cg/insert-ticket-link (ticket-number)
  "Insert an org-mode link to the specified TICKET-NUMBER at point."
  (interactive "nTicket number: ")
  (insert (format "[[https://hub.reservotron.com/tickets/%d][Ticket #%d]]"
                  ticket-number ticket-number)))

(define-key mu/cg-map (kbd "T") 'mu/cg/insert-ticket-link)

(defun mu/cg/insert-incident-link (incident-number)
  "Insert an org-mode link to the specified INCIDENT-NUMBER at point."
  (interactive "nIncident number: ")
  (insert (format "[[https://appsignal.com/code-genome/sites/%s/exceptions/incidents/%d/samples/last][Incident #%d]]"
                  mu/cg/appsignal-site-id incident-number incident-number)))

(define-key mu/cg-map (kbd "I") 'mu/cg/insert-incident-link)

(defun mu/cg/insert-pr-link (pr-number)
  "Insert an org-mode link to the specified PR-NUMBER at point."
  (interactive "nPR number: ")
  (insert (format "[[https://github.com/codegenome/reservotron/pull/%d][PR #%d]]"
                  pr-number pr-number)))

(define-key mu/cg-map (kbd "l") 'mu/cg/insert-pr-link)

(defun mu/cg/remove-examples-file ()
  "Remove the file spec/examples.txt at the project root using Projectile."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (examples-file (when project-root
                          (expand-file-name "spec/examples.txt" project-root))))
    (if (and examples-file (file-exists-p examples-file))
        (progn
          (delete-file examples-file)
          (message "Deleted: %s" examples-file))
      (message "File not found or not in a project: %s" examples-file))))

(define-key mu/cg-map (kbd "y") 'mu/cg/remove-examples-file)

(defun mu/cg/copy-relative-path ()
  "Copy the current file path relative to the project root to the clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file")
    (let* ((project-root (and (fboundp 'projectile-project-root)
                             (projectile-project-p)
                             (projectile-project-root)))
           (base-dir (or project-root default-directory))
           (file-path (file-relative-name buffer-file-name base-dir)))
      (kill-new file-path)
      (message "Copied: %s%s"
               file-path
               (if project-root "" " (not in a project)")))))

(define-key mu/cg-map (kbd "p") 'mu/cg/copy-relative-path)

(defun mu/cg/copy-absolute-path ()
  "Copy the absolute path of the current file to the clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file")
    (let ((file-path buffer-file-name))
      (kill-new file-path)
      (message "Copied: %s" file-path))))

(define-key mu/cg-map (kbd "P") 'mu/cg/copy-absolute-path)

;;; cg.el ends here

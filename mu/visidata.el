;;; visidata.el --- VisiData CSV viewer integration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Integration of VisiData with eat-mode for viewing and exploring CSV files.
;; VisiData is a powerful terminal-based data exploration tool.
;;
;;; Code:

(require 'eat)

(defcustom mu/visidata-program "visidata"
  "Path to the visidata executable."
  :type 'string
  :group 'mu-visidata)

(defcustom mu/visidata-args nil
  "Additional arguments to pass to visidata."
  :type '(repeat string)
  :group 'mu-visidata)

(defcustom mu/visidata-monochrome nil
  "When non-nil, disable colors in visidata.
This sets colors to monochrome (white on black) for a cleaner display."
  :type 'boolean
  :group 'mu-visidata)

(defcustom mu/visidata-auto-open-csv nil
  "When non-nil, automatically prompt to open CSV files with visidata."
  :type 'boolean
  :group 'mu-visidata)

(defun mu/visidata-open-file (file)
  "Open FILE with visidata in an eat terminal buffer."
  (interactive
   (list (read-file-name "Open file with visidata: "
                         nil
                         (when (and buffer-file-name
                                   (mu/visidata--csv-file-p buffer-file-name))
                           buffer-file-name)
                         t)))
  (unless (executable-find mu/visidata-program)
    (error "VisiData not found. Install it with: pip install visidata"))
  (let* ((file-path (expand-file-name file))
         (buffer-name (format "*visidata: %s*" (file-name-nondirectory file-path)))
         (command-prefix (if mu/visidata-monochrome "TERM=ansi NO_COLOR=1 " ""))
         (command (concat command-prefix
                         (mapconcat #'shell-quote-argument
                                   (append (list mu/visidata-program)
                                          mu/visidata-args
                                          (list file-path))
                                   " "))))
    ;; Kill existing buffer if it exists
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;; Create new eat buffer with visidata
    (let ((eat-buffer (eat command t)))
      (with-current-buffer eat-buffer
        (rename-buffer buffer-name t)
        (setq show-trailing-whitespace nil)
        ;; Kill buffer automatically when visidata exits
        (let ((proc (get-buffer-process (current-buffer))))
          (when proc
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (memq (process-status process) '(exit signal))
                 (kill-buffer (process-buffer process))))))))
      (switch-to-buffer eat-buffer))))

(defun mu/visidata-open-current-file ()
  "Open the current file with visidata in an eat terminal buffer."
  (interactive)
  (if buffer-file-name
      (mu/visidata-open-file buffer-file-name)
    (error "No file associated with current buffer")))

(defun mu/visidata--csv-file-p (file)
  "Return non-nil if FILE appears to be a CSV file."
  (and file
       (stringp file)
       (string-match-p "\\.\\(csv\\|tsv\\|tab\\)\\'" file)))

(defun mu/visidata--maybe-auto-open ()
  "Prompt to open CSV file with visidata if auto-open is enabled."
  (when (and mu/visidata-auto-open-csv
             buffer-file-name
             (mu/visidata--csv-file-p buffer-file-name)
             (y-or-n-p "Open this CSV file with visidata? "))
    (mu/visidata-open-current-file)))

;; Automatically open CSV/TSV files with visidata
(defun mu/visidata--auto-open-handler ()
  "Automatically open CSV/TSV files with visidata."
  (when (and buffer-file-name
             (mu/visidata--csv-file-p buffer-file-name)
             (not (eq major-mode 'eat-mode)))
    (let ((file buffer-file-name)
          (csv-buffer (current-buffer)))
      (mu/visidata-open-file file)
      ;; Kill the CSV buffer since we're viewing in visidata
      (when (buffer-live-p csv-buffer)
        (with-current-buffer csv-buffer
          (set-buffer-modified-p nil)
          (kill-buffer csv-buffer))))))

(add-hook 'find-file-hook 'mu/visidata--auto-open-handler)

;; Key bindings for csv-mode (if user manually switches to it)
(with-eval-after-load 'csv-mode
  (define-key csv-mode-map (kbd "C-c v") 'mu/visidata-open-current-file))

(provide 'mu/visidata)

;;; visidata.el ends here

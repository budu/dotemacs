;;; package --- Helpers for text manipulation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mu/text/camelize (s)
  "Convert string S to camel case.  Works with both hyphens and underscores."
  (let ((words (split-string s "[-_]")))
    (concat (downcase (car words))
            (mapconcat 'capitalize (cdr words) ""))))

(defun mu/text/un-camelize (s)
  "Convert camelCase string S to snake_case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([A-Z]\\)" "_\\1"
      (replace-regexp-in-string
       "\\([A-Z]\\)\\([A-Z][a-z]\\)" "\\1_\\2" s)))))

(defun mu/text/is-camel-case-p (s)
  "Return t if string S is in camelCase, nil otherwise."
  (and (string-match-p "[A-Z]" s)          ; Contains at least one uppercase letter
       (not (string-match-p "[_-]" s))     ; Contains no underscores or hyphens
       (eq (aref s 0) (downcase (aref s 0))))) ; First letter is lowercase

(defun mu/text/toggle-case (s)
  "Toggle string S between camelCase and snake_case."
  (if (mu/text/is-camel-case-p s)
      (mu/text/un-camelize s)
    (mu/text/camelize s)))

(defun mu/text/toggle-case-region (begin end)
  "Toggle the region from BEGIN to END between camelCase and snake_case."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties begin end))
         (toggled (mu/text/toggle-case text)))
    (delete-region begin end)
    (insert toggled)))

(defun mu/text/toggle-case-dwim ()
  "Toggle case at point or region between camel-case and snake-case.
If region is active, toggle the region.  Otherwise, toggle the word at point."
  (interactive)
  (if (use-region-p)
      (mu/text/toggle-case-region (region-beginning) (region-end))
    (let ((bounds (mu/word-with-underscores-at-point-bounds)))
      (when bounds
        (mu/text/toggle-case-region (car bounds) (cdr bounds))))))

(global-set-key (kbd "C-c m") 'mu/text/toggle-case-dwim)

(defun remove-zero-width-spaces ()
  "Remove zero-width spaces from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\u200B" nil t)
      (replace-match "" nil t))))

(global-set-key (kbd "C-c z") 'remove-zero-width-spaces)

;;; text.el ends here

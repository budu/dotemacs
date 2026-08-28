;;; package --- Helpers for I18n -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mu/i18n/translation-file ()
  (projectile-expand-root "config/locales/extracted_locales.en.yml"))

(defun mu/i18n/goto-translation-file ()
  (interactive)
  (find-file (mu/i18n/translation-file)))

(defun mu/i18n/to-key (text)
  "Produce a key from the given TEXT.

The key is a snake case representation of the first 5 words of
the text.  If the text contains non-alphanumeric characters, they
are replaced by underscores."
  (mapconcat 'identity
             (-> (replace-regexp-in-string "[^[:alnum:]]" "_" text)
                 (downcase)
                 (split-string "_")
                 (seq-take 5))
             "_"))

(defun mu/i18n/insert-entry (translation-file i18n-path translated-text)
  "Use the yq command to insert the new translation."
  (let ((command (format "yq -i \".%s = \\\"%s\\\" | sort_keys(..)\" %s"
                         (mapconcat 'identity i18n-path ".")
                         translated-text
                         translation-file)))
    (shell-command command)))

(defun mu/i18n/get-text ()
  "Get the text to translate."
  (if (ruby-string-at-point-p)
      (let* ((region (ruby--string-region))
             (min (nth 0 region))
             (max (nth 1 region))
             (string-quote (ruby--inverse-string-quote (buffer-substring-no-properties min (1+ min))))
             (content (buffer-substring-no-properties (1+ min) (1- max))))
        (delete-region min max)
        content)
    (read-from-minibuffer "Text to translate: ")))

(defun mu/i18n/extract-translation ()
  ""
  (interactive)
  (let* ((text (mu/i18n/get-text))
         (key (mu/i18n/to-key text)))
    ;; TODO: prompt for key with key as placeholder
    (insert (format "t('.%s')" key))
    (save-buffer)
    (let* ((language "en")
           ;; TODO: handle multiple extensions
           (current-path (->> (buffer-file-name)
                              (file-name-sans-extension)
                              (replace-regexp-in-string "_controller" "" )
                              (replace-regexp-in-string "app/views/components" "app/views")))
           (relative-path (replace-regexp-in-string (projectile-project-root) "" current-path))
           (i18n-path (append (->> (split-string relative-path "/" t)
                                   (mapcar (lambda (x) (string-trim x "_")))
                                   (cdr))
                              (list key)))
           (i18n-path (cons language (cdr i18n-path)))
           (translation-file (mu/i18n/translation-file)))
      (mu/i18n/insert-entry translation-file i18n-path text))))

;;; i18n.el ends here

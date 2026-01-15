;;; package --- minuet package configuration
;;; Commentary:
;;;   https://github.com/milanglacier/minuet-ai.el
;;;   AI-powered code completion with multiple LLM backend support
;;; Code:

(defconst mu/minuet/openai-api-key (getenv "CHATGPT_EMACS_KEY"))

(defun mu/minuet/not-after-trigger-char-p ()
  "Return non-nil if cursor is not on a space, newline, or closing paren/bracket.
This blocks auto-suggestions unless the cursor is positioned on one of these trigger characters."
  (let ((char-at-point (char-after)))
    (not (or (null char-at-point)  ; end of buffer
             (memq char-at-point '(?\s ?\t ?\n ?\) ?\] ?\}))))))

(use-package minuet
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer)
   ("M-i" . #'minuet-show-suggestion)
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("M-A" . #'minuet-accept-suggestion)
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion)
   ("<return>" . #'minuet-accept-suggestion)
   ("RET" . #'minuet-accept-suggestion))
  :hook ((prog-mode . minuet-auto-suggestion-mode)
         (text-mode . minuet-auto-suggestion-mode)
         (git-commit-mode . minuet-auto-suggestion-mode))
  :config
  ;; Set the API key for OpenAI
  (setenv "OPENAI_API_KEY" mu/minuet/openai-api-key)

  ;; Configure OpenAI backend with gpt-4o model
  (setq minuet-provider 'openai-fim-compatible)
  (plist-put minuet-openai-options :model "gpt-4.1-mini")
  (minuet-set-optional-options minuet-openai-options :max_completion_tokens 128)

  ;; Customize completion behavior
  (setq
   minuet-context-window 16000              ; Maximum context characters to send
   minuet-context-ratio 0.75                ; 75% context before cursor, 25% after
   minuet-request-timeout 3                 ; Timeout in seconds
   minuet-n-completions 3                   ; Number of suggestions to generate
   minuet-auto-suggestion-debounce-delay 0.3  ; Wait 0.4s after typing stops
   minuet-auto-suggestion-throttle-delay 1.0  ; Minimum 1s between requests
   minuet-auto-suggestion-block-predicates '(minuet-evil-not-insert-state-p
                                             mu/minuet/not-after-trigger-char-p))

  ;; Customize the appearance of suggestions
  (custom-set-faces
   '(minuet-suggestion-face ((t (:background "#2a3a4a" :foreground "#8be9fd" :slant italic))))))

(provide 'mu-minuet)
;;; minuet.el ends here

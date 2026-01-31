;;; package --- minuet package configuration
;;; Commentary:
;;;   https://github.com/milanglacier/minuet-ai.el
;;;   AI-powered code completion with multiple LLM backend support
;;; Code:

(defun mu/minuet/should-block-completion-p ()
  "Return non-nil if auto-completion should be blocked at point."
  (let ((char-at-point (char-after))
        (char-before-point (char-before)))
    (not (or ;; Condition 1: End of buffer - allow completion
             (null char-at-point)

             ;; Condition 2: Cursor ON closing delimiters - allow completion
             ;; Triggers when current char is: ), ], }
             (memq char-at-point '(?\) ?\] ?\}))

             ;; Condition 3: Cursor AFTER whitespace - allow completion
             ;; Triggers when previous char is: space, tab, newline
             ;; Exceptions:
             ;;   - at EOL after tab, block completion
             ;;   - after newline when current char is not newline, block completion
             (and (memq char-before-point '(?\s ?\t ?\n))
                  (not (and (eq char-before-point ?\n)
                            (not (eq char-at-point ?\n))))
                  (or (not (eolp))
                      (memq char-before-point '(?\s ?\n))))))))

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
  ;; Configure OpenAI backend with gpt-4o model
  (setq minuet-provider 'openai)
  ; (plist-put minuet-openai-options :model "gpt-4.1-mini")
  (plist-put minuet-openai-options :model "gpt-5-mini")
  (plist-put minuet-openai-options :api-key "CHATGPT_EMACS_KEY")
  (minuet-set-optional-options minuet-openai-options :max_completion_tokens 32)
  (minuet-set-optional-options minuet-openai-options :reasoning_effort "minimal")

  ;; Customize completion behavior
  (setq
   ;; NOTE: copilot sent about 500 chars so around 128 tokens
   ;; here we'll try using double that
   minuet-context-window 1000               ; Maximum context characters to send
   minuet-context-ratio 1.0                 ; Percentage of context before cursor
   minuet-request-timeout 3                 ; Timeout in seconds
   minuet-n-completions 1                   ; Number of suggestions to generate
   minuet-auto-suggestion-debounce-delay 0.3  ; Wait 0.4s after typing stops
   minuet-auto-suggestion-throttle-delay 1.0  ; Minimum 1s between requests
   minuet-auto-suggestion-block-predicates '(minuet-evil-not-insert-state-p
                                             mu/minuet/should-block-completion-p))

  ;; Customize the appearance of suggestions
  (custom-set-faces
   '(minuet-suggestion-face ((t (:background "#2a3a4a" :foreground "#8be9fd" :slant italic))))))

(provide 'mu-minuet)
;;; minuet.el ends here

;;; package --- gptel package configuration and helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;;   https://github.com/karthink/gptel
;;; Code:

(defconst chatgpt-api-key (getenv "CHATGPT_EMACS_KEY"))
(defconst claude-api-key (getenv "CLAUDE_EMACS_KEY"))

(use-package gptel
  :bind ((:map gptel-mode-map
          ("<C-M-return>" . gptel-send)))
  :hook ((gptel-post-response-functions gptel-end-of-response))
  :config
  (setq
   gptel-api-key chatgpt-api-key
   gptel-backend gptel--openai
   gptel-model   "gpt-4o"))
   ;; gptel-backend (gptel-make-anthropic "Claude"
   ;;                 :stream t :key claude-api-key)
   ;; gptel-model 'claude-3-5-sonnet-20241022))

;; Error running timer: (wrong-type-argument number-or-marker-p nil)

;;; Customization

;; Ollama

(require 'gptel-ollama)

(defconst mu/gptel/ollama-backend
  (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :stream t
                     :models '("llama3.1")))

; TODO: are input/output-cost relevant here
(defconst mu/gptel/ollama-models
  '((llama3.1
     :description "LLaMA 3.1 model for advanced language tasks"
     :capabilities (tool)
     :context-window 128
     :input-cost 1.00
     :output-cost 5.00)))

(define-key mu/cg-map (kbd "g") 'gptel-menu)

(defun mu/gptel/rewrite-region (beg end)
  "Rewrite the region between BEG and END using the LLM.
Does nothing if no region is selected."
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (gptel-request
          (format "Rewrite this text: %s" text)
        :position beg
        :in-place t))))

(define-key mu/cg-map (kbd "r") 'mu/gptel/rewrite-region)

(defun mu/gptel/convert-to-markdown (beg end)
  "Rewrite the region between BEG and END using the LLM.
Does nothing if no region is selected."
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (gptel-request
          (format "Convert to markdown: %s" text)
        :position beg
        :stream t
        :in-place t))))

(define-key mu/cg-map (kbd "m") 'mu/gptel/convert-to-markdown)

;; TODO:
;; - analyze a region or whole buffer
;; - summarize a region or whole buffer
;; - ask something and get the response in a new buffer

;;; gptel.el ends here

;;; eat.el --- Eat terminal emulator configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for eat terminal emulator.
;;
;;; Code:

(use-package eat
  :config
  ;; override these keys explicitly to prevent eat-mode from shadowing global keys
  (with-eval-after-load 'eat
    (define-key eat-semi-char-mode-map (kbd "M-j") #'previous-window-any-frame)
    (define-key eat-semi-char-mode-map (kbd "M-k") #'next-window-any-frame)
    (define-key eat-char-mode-map (kbd "M-j") #'previous-window-any-frame)
    (define-key eat-char-mode-map (kbd "M-k") #'next-window-any-frame))
  ;; Disable trailing whitespace highlighting in eat buffers
  :hook (eat-mode . (lambda () (setq show-trailing-whitespace nil))))

(provide 'mu/eat)

;;; eat.el ends here

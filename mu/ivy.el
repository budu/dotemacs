;;; ivy.el --- Ivy configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for ivy completion framework.
;;
;;; Code:

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-a" . counsel-apropos) ; displace beginning-of-defun
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-w" . ivy-yank-word)
         ("C-SPC" . ivy-occur)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-height 15)
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide)
              " " (mode 16 16 :left :elide)
              " " filename-and-process)))

(defun mu/counsel-ibuffer-get-mode (candidate)
  "Get the major-mode name for a counsel-ibuffer CANDIDATE."
  (let ((buf (cdr (assoc candidate (ivy-state-collection ivy-last)))))
    (when buf
      (replace-regexp-in-string
       "-mode$" ""
       (symbol-name (buffer-local-value 'major-mode buf))))))

(defun mu/counsel-ibuffer-transformer (candidate)
  "Color counsel-ibuffer candidates based on buffer type."
  (let* ((mode (or (mu/counsel-ibuffer-get-mode candidate) ""))
         (color (cond
                 ((string-match-p "claude" mode) "magenta")
                 ((string-match-p "hub" mode) "violet")
                 ((string-match-p "magit" mode) "purple")
                 ((string-match-p "dired" mode) "blue")
                 ((string-match-p "org" mode) "cyan")
                 ((string-match-p "\\(shell\\|eat\\|term\\|vterm\\)" mode) "green")
                 ((string-match-p "\\(compilation\\|rspec\\)" mode) "orange")
                 ((string-match-p "\\(emacs-lisp\\|ruby\\|slim\\|python\\|javascript\\|clojure\\)" mode) "yellow")
                 ((string-match-p "^  \\*" candidate) "gray")
                 (t nil))))
    (if color
        (propertize candidate 'face `(:foreground ,color))
      candidate)))

(ivy-configure 'counsel-ibuffer
  :display-transformer-fn #'mu/counsel-ibuffer-transformer)

(use-package ivy-posframe
  :after ivy
  :config
  (require 'ivy-overlay nil t)
  (setq ivy-posframe-display-functions-alist '((swiper . ivy-display-function-fallback)
                                                (t . ivy-posframe-display)))
  (setq ivy-posframe-height nil)
  (setq ivy-posframe-width 120)
  (setq ivy-truncate-lines nil)
  (ivy-posframe-mode 1)
  (set-face-attribute 'ivy-posframe nil
                      :foreground "white"
                      :background "#126"))

(provide 'mu/ivy)

;;; ivy.el ends here

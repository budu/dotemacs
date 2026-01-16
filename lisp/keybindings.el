;;; keybindings.el --- Global key bindings -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Global key bindings configuration.
;;
;;; Code:

(define-key input-decode-map [?\C--] [C--])
(define-key input-decode-map [?\M-i] [M-i])

(global-set-key (kbd "M-<mouse-2>") #'mouse-yank-at-click)
(global-set-key (kbd "C-M-y")   #'mouse-yank-primary)

(global-set-key [C-tab]    'mu/cslist-to-indented-lines-and-back)
(global-set-key [C--]      'text-scale-decrease)     ; displace negative-argument
(global-set-key "\C-c1"    'sort-lines)
(global-set-key "\C-c2"    'mu/sort-words)
(global-set-key "\C-c3"    'mu/rubocop-autocorrect-silent)
(global-set-key "\C-c4"    (lambda () (interactive) (kill-new (buffer-file-name))))
(global-set-key "\C-c5"    'mu/convert-region-to-percent-w-syntax)
(global-set-key "\C-c0"    'mu/goto-personal-notes)
(global-set-key "\C-cb"    'magit-blame)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-xl"    'magit-log-buffer-file)
(global-set-key "\C-h"     'delete-backward-char)         ; displace help-command
(global-set-key "\C-w"     'backward-kill-word)           ; displace kill-region
(global-set-key "\C-x\C-b" 'mu/switch-to-last-buffer)     ; displace list-buffers
(global-set-key "\C-x\C-c" 'kill-region)                  ; displace save-buffers-kill-terminal
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key "\M-j"     'previous-window-any-frame)    ; displace default-indent-new-line
(global-set-key "\M-k"     'next-window-any-frame)        ; displace kill-sentence
(global-set-key "\C-\M-h"  'mark-paragraph)               ; displace mark-defun
(global-set-key "\C-\M-g"  'mu/search-gems)

(global-set-key (kbd "C-=")     'text-scale-increase)
(global-set-key (kbd "C-'")     'mu/touch)
(global-set-key (kbd "C-:")     "&:")
(global-set-key (kbd "C-M-S-h") 'mark-defun)
(global-set-key (kbd "C-M-S-k") 'kill-this-buffer)
(global-set-key (kbd "<M-i>")   "<% %>\C-b\C-b\C-b") ; displace tab-to-tab-stop function

(global-set-key (kbd "M-s M-s") 'avy-goto-char-timer)

(global-set-key (kbd "<f2>")    'mastodon-toot)
(global-set-key (kbd "<f5>")    (lambda () (interactive) (find-file "~/org/index.org")))
(global-set-key (kbd "C-<f5>")  'my/org/open-daily-note)
(global-set-key (kbd "<f6>")    'mu/goto-personal-notes)
(global-set-key (kbd "C-<f6>")  'mu/set-personal-notes-target)
(global-set-key (kbd "<f7>")    'mu/agent-shell-send-prompt-from-notes)
(global-set-key (kbd "<f8>")    'mu/agent-shell-smart-switch)
(global-set-key (kbd "<f10>")   (lambda () (interactive) (find-file "~/.bashrc")))
(global-set-key (kbd "<f11>")   (lambda () (interactive) (find-file "~/dotfiles/awesome/.config/awesome/rc.lua")))
(global-set-key (kbd "<f12>")   (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c SPC")    'rspec-toggle-spec-and-target)
(global-set-key (kbd "C-<return>") 'mu/open-at-point)
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-<return>") 'mu/open-at-point))

(global-set-key (kbd "C-M-q")
  (lambda () (interactive)
    (setq fill-column (- (window-width) 7))))

;; key binding to wrap region in quote
(global-set-key (kbd "C-\"") 'mu/wrap-in-interpolated-quotes)

(global-set-key "\C-cw"   "%w[")
(global-set-key "\C-ci"   "%i[")
(global-set-key "\C-z"    "()\C-b") ; displace suspend-frame
(global-set-key "\M-z"    "[]\C-b") ; displace zap-to-char
(global-set-key "\C-\M-z" "{}\C-b")
(global-set-key "\C-\M-d" 'mu/kill-parens) ; displace down-list

(defun mu/org-goto-root-heading ()
  "Go to the root heading of the current Org mode buffer."
  (interactive)
  (condition-case nil
      (while (org-up-heading-safe))
    (error (message "Already at root heading."))))

(global-set-key (kbd "C-_") 'mu/org-goto-root-heading) ; displace undo

(add-hook 'sh-mode-hook
 (lambda ()
   (define-key sh-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)))

(add-hook 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)))

(define-key mu/cg-map (kbd "f") 'mu/send-prompt-block-to-agent-shell)
(define-key mu/cg-map (kbd "r") 'magit-commit-reword)

(define-key mu/cg-map (kbd "w")
  (lambda () (interactive)
    (eww-display-html 'utf-8 (buffer-name) nil (point-min) (current-buffer))
    (setq show-trailing-whitespace nil)))

(provide 'keybindings)

;;; keybindings.el ends here

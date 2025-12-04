;;; package --- My Emacs init file -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   This configuration is based on David Wilson's excellent Emacs From Scratch
;;;   series: https://github.com/daviwil/emacs-from-scratch
;;;
;;;   I'm using the "mu/" prefix as namespace for custom variables/functions.
;;;
;;;   One guiding principle of this configuration is to make key binding as close as my
;;;   bash/readline configuration.
;;;
;;;   TODO: check https://github.com/patrickt/emacs
;;;
;;; Update:
;;;   package-refresh-contents
;;;   package-upgrade-all or package-upgrade for individual package
;;;   package-vc-upgrade-all or package-vc-upgrade for individual package
;;;
;;; Code:

;;;; packages

(define-prefix-command 'mu/cg-map)
(global-set-key (kbd "C-c g") 'mu/cg-map)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-libyaml/")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;; performance

(setq gc-cons-threshold (* 100 1024 1024) ;; GC sometime after allocating 100Mb
      ;; jit-lock-defer-time nil
      ;; jit-lock-stealth-time 1.25
      ;; jit-lock-stealth-nice 0.5
      jit-lock-chunk-size 4096
      read-process-output-max (* 1024 1024)
      package-native-compile t)
(run-with-idle-timer 2 t (lambda () (garbage-collect))) ;; Trigger a GC after 5s of idle time

;;;; shims

(use-package asdf
  :vc (:url "https://github.com/tabfugnic/asdf.el")
  :config
  (setq asdf-binary "/opt/asdf-vm/bin/asdf")
  (asdf-enable))

;;;; env vars

(defun load-env-vars ()
  "Load environment variables from the ~/.env file."
  (let ((env-file (expand-file-name "~/.env")))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(load-env-vars)

;;;; functions & macros

(defun mu/get-project-dir ()
  "Get the project root directory.
If in nb-notes subdirectory, first navigate to parent before finding project root."
  (let* ((original-dir default-directory)
         (in-nb-notes (string-match-p "^/.+/nb-notes\\(/\\|$\\)" default-directory))
         (search-dir (if in-nb-notes
                         (file-name-as-directory
                          (car (split-string original-dir "/nb-notes" t)))
                       original-dir))
         (default-directory search-dir))
    (or (projectile-project-root)
        search-dir)))

(require 'macros)
(require 'functions)

;; List all files in the mu/ directory
(let ((mu-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "mu/")))
  (dolist (file (directory-files mu-dir t "\\.el$"))
    (load file)))

;;;; vars

; set paragraph-start to handle various lists style
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*[0-9]+\\. ")
; handle fill-paragraph list indentation
(setq adaptive-fill-regexp "[ \t]*\\([-+*] \\|\\([0-9]+\\.\\)[ \t]+\\|[ \t]*[-+*][ \t]+\\|[ \t]*[0-9]+\\.\\)[ \t]*")

;;;; local vars overrides

(let ((filename "~/.emacs.d/lisp/local-vars.el"))
  (when (file-exists-p (expand-file-name filename))
    (require 'local-vars)))

;;;; minimalism

(setq inhibit-startup-message t)

(column-number-mode)                 ; show column in mode line
(global-display-line-numbers-mode t) ; show line numbers
(menu-bar-mode -1)                   ; disable the menu bar
(scroll-bar-mode -1)                 ; disable visible scrollbar
(set-fringe-mode 10)                 ; give some breathing room
(tool-bar-mode -1)                   ; disable the toolbar
(tooltip-mode -1)                    ; disable tooltips

;;;; personal preference

(setq custom-file (expand-file-name "~/.emacs.d/lisp/custom.el"))
(require 'custom)

(setq-default
 fill-column 92
 indent-tabs-mode nil
 show-trailing-whitespace t
 tab-width 2
 css-indent-offset 2
 lua-indent-level 2
 js-indent-level 2
 typescript-indent-level 2
 truncate-lines 0)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun my-compilation-mode-hook ()
  (face-remap-add-relative 'default '(:height 0.7)))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun bugfix-display-line-numbers--turn-on (fun &rest args)
  "Avoid `display-line-numbers-mode' in `image-mode' and related.
Around advice for FUN with ARGS."
  (unless (derived-mode-p 'image-mode 'docview-mode 'pdf-view-mode)
    (apply fun args)))

(advice-add 'display-line-numbers--turn-on :around #'bugfix-display-line-numbers--turn-on)

; list directories first in dired
(setq dired-listing-switches "-alh --group-directories-first")

;;;; advice

; don't ask when saving buffers
(defadvice basic-save-buffer (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

; don't use tabs in align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(delete-selection-mode 1)

; use aggressive auto-revert
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-use-notify t)
(setq auto-revert-notify-watch-descriptor-list t)
(setq auto-revert-interval 0.1)

;;;; Open ~/org/index.org by default on load

(find-file "~/org/index.org")

;;;; backups

(setq
 backup-by-copying t    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)     ; use versioned backups

;;;; simple use-package

(use-package csv-mode)
(use-package polymode)
(use-package transient)

;;;; global hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook 'goto-address-mode)

(dolist (hook '(special-mode-hook
                term-mode-hook
                calendar-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;;;; global key bindings

(require 'keybindings)

;;; init.el ends here

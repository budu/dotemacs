;;; screenshot.el --- Screenshot configuration -*- lexical-binding: t; -*-
;;
;; Author: budu
;;
;;; Commentary:
;;
;; Configuration for screenshot package.
;;
;;; Code:

(defvar mu/cg-map)

(use-package screenshot
  :vc (:url "https://github.com/tecosaur/screenshot")
  :bind (:map mu/cg-map ("s" . screenshot)))

(provide 'mu/screenshot)

;;; screenshot.el ends here

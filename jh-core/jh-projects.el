;;; jh-projects.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:
(message "project")

;;; Project

(use-package project
  :straight (:type built-in)
  :demand t
  :custom ('project-list-file (no-littering-expand-var-file-name "projects")))

(message "eind project")
(provide 'jh-projects)
;;; jh-projects.el ends here

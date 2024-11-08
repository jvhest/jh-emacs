;;; jh-agenda.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Settings
(defvar my/agenda-projects (expand-file-name "projects.org" my/org-dir-local)
  "Inbox file project related tasks.")

(defvar my/agenda-notes (expand-file-name "notes.org" my/org-dir-local)
  "Notes file.")

(defvar my/agenda-archive (expand-file-name "archive.org" my/org-dir-local)
  "Archive file for DONE TODOs.")

(defvar my/agenda-index (expand-file-name "index.org" my/org-dir-sync)
  "Inbox file for todo's.")

(defvar my/agenda-inbox (expand-file-name "inbox.org" my/org-dir-sync) \
  "Personal tasks, reminders and so on.")

;;; Org-Agenda

(use-package org-agenda
  :straight (:type built-in)
  :after org
  :preface
  (defun my/mark-done-and-archive()
    "Mark the state of an `org-mode' item as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))
  :init
  (add-to-list 'org-modules 'org-habit t)
  :config
  (setq org-agenda-files (list my/agenda-inbox
                               my/agenda-notes)

        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))

        org-agenda-skip-schedule-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-log-done 'time
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-agenda-start-on-weekday 1 ;; Begin weeks today, not on the last Monday.
        org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i ")
                                   (tags . " %i ")
                                   (search . " %i "))
        org-habit-graph-column 60))

;; TODO: make keymap !
;; (my-leader 'org-mode-map
;;   "a" '(:ignore t :wk "Org-agenda")
;;   "a v" '(org-agenda-list :wk "view")
;;   "a o" '(org-agenda :wk "open")
;;   "a a" '(my/mark-done-and-archive :wk "archive")
;;   "a d" '(org-deadline :wk "deadline")
;;   "a s" '(org-schedule :wk "schedule"))

(provide 'jh-agenda)
;;; jh-agenda.el ends here

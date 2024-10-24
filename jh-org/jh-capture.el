;;; jh-capture.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Settings
(defvar my/capture-bookmarks (expand-file-name "bookmarks.org" my/org-dir-local)
  "Captured weblinks.")

;;; Org-Capture

(use-package org-capture
  :straight (:type built-in)
  :after org
  :init
  (setq org-capture-templates
        '(("t" "Personal TODO" entry
           (file+headline my/agenda-inbox "Inbox")
           "** Todo %?\n  %t\n")
          ("n" "Personal note" entry
           (file+headline my/agenda-notes "Notes")
           "* %?\n  %u\n  %a")
          ("o" "Weblinks" entry
           (file+headline my/capture-bookmarks "Weblinks")
           "* %:annotation\n %?\n %u\n %i\n" :empty-lines-before 1))))

;; (setq counsel-projectile-org-capture-templates
;;       '(("pt" "[${name}] TODO" entry
;;          (file+headline my/agenda-projects "${name}")
;;          "* TODO %? %u\n")
;;         ("pl" "[${name}] TODO" entry
;;          (file+headline my/agenda-projects "${name}")
;;          "* TODO %? %u\n%a")
;;         ("pf" "[${name}] FIXME" entry
;;          (file+headline my/agenda-projects "${name}")
;;          "* FIXME %? %t\n")))))

(provide 'jh-capture)
;;; jh-capture.el ends here

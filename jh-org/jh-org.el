;;; jh-org.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

(defvar my/org-dir-local "~/org-files"
  "Base directory for org related file.")

(defvar my/org-dir-sync "~/Dropbox/org"
  "Base directory for org related file that need sync to android app.")


(use-package org
  :commands org-mode
  :custom
  ;; Return or left-click with mouse follows link
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  ;; Display links as the description provided
  (org-descriptive-links t)
  ;; Hide markup markers
  (org-hide-emphasis-markers t)
  :hook
  (org-mode . org-appear-mode)
  ;; (org-mode . visual-line-mode)
  ;; (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . org-bullets-mode)

  :config
  (setq org-src-fontify-natively t                ;; Always use syntax highlighting of code blocks
        org-src-tab-acts-natively t               ;; TAB in src blocks act same as lang’s major mode
        org-startup-with-inline-images t          ;; Always show images
        org-startup-indented t                    ;; Indent text according to the current header
        org-indent-mode-turns-on-hiding-stars t
        org-hide-emphasis-markers t               ;; Hides the symbols that makes text bold, italics
        org-src-window-setup 'current-window      ;; editing code snippet in current window
        org-catch-invisible-edits 'smart          ;; Smart editing of hidden regions
        org-highlight-latex-and-related '(latex)  ;; Highlight LaTeX fragments, snippets etc
        org-pretty-entities t                     ;; Show entities as UTF8-characters when possible
        org-list-allow-alphabetical t             ;; Allow lists to be a) etc
        org-confirm-babel-evaluate nil            ;; Don't bug about executing code all the time
        org-ellipsis "⤵"
        org-edit-src-content-indentation 0
        org-babel-python-command "python3"        ;; Newer is always better
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")))

  (use-package org-bullets)
  (use-package org-appear)

  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (add-to-list 'org-modules 'org-tempo t)

  (cl-pushnew '("sh" . "src shell") org-structure-template-alist)
  (cl-pushnew '("el" . "src emacs-lisp") org-structure-template-alist)
  (cl-pushnew '("py" . "src python") org-structure-template-alist)
  (cl-pushnew '("cl" . "src lisp") org-structure-template-alist)
  (cl-pushnew '("rkt" . "src racket") org-structure-template-alist)

  ;; Configure which languages we can use in Org Babel code blocks
  ;; NOTE: This slows down the startup of Org-mode a little bit
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (python .t)
     (sql . t))))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq-default visual-fill-column-width my/fill-width)
  (setq visual-fill-column-center-text nil))

(use-package toc-org
  :commands toc-org-enable
  :hook ((markdown-mode . toc-org-mode)
         (org-mode . toc-org-enable)))

(with-no-warnings
  (pretty-hydra-define org-hydra
    (:title "ORG-BINDINGS" :color pink :quit-key ("q" "C-g"))
    ("Headings"
     (("h d" org-demote-subtree "demote subtree")
      ("h p" org-promote-subtree "promote subtree"))
     "Table"
     (("t a" org-table-align "align table")
      ("t c" org-table-create "create table")
      ("t C" org-table-create-or-convert-from-region "create from region"))
     "Tabel-insert"
     (("t i c" org-table-insert-column "insert column")
      ("t i h" org-table-insert-hline "insert hline")
      ("t i r" org-table-insert-row "insert row"))
     "Table-delete"
     (("t r c" org-table-delete-column "delete column")
      ("t r r" org-table-kill-row "delete row"))
     "Org-mode"
     (("m e" org-export-dispatch "Export dispatch")
      ("m i" org-toggle-item "Toggle item")
      ("m t" org-todo "Todo")
      ("m B" org-babel-tangle "Babel tangle")
      ("m T" org-todo-list "Todo list")))))

;;; Load org config files

;; (require 'jh-agenda)
;; (require 'jh-capture)

(provide 'jh-org)
;;; jh-org.el ends here

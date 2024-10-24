;;; jh-git.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Magit

(use-package magit
  :commands magit-add-section-hook)

;; Quickly move between hunks in your document.
(defhydra hydra-git (:color pink)
  "git"
  ("k" diff-hl-previous-hunk "prev hunk")
  ("j" diff-hl-next-hunk "next hunk")
  ("q" nil "quit" :color blue))

(use-package diff-hl
  :commands (diff-hl-magit-post-refresh global-diff-hl-mode)
  :functions (diff-hl-flydiff-mode diff-hl-margin-mode)
  :defines diff-hl-margin-symbols-alist
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (setq diff-hl-margin-symbols-alist
        '((insert . "+") (delete . "-") (change . "~")
          (unknown . "?") (ignored . "i")))
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

(provide 'jh-git)
;;; jh-git.el ends here

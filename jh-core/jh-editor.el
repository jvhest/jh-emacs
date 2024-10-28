;;; jh-editor.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


(defvar my/fill-width 100
  "The default width at which to wrap text.")

(defvar my/tab-width 2
  "The default width for indentation, in spaces.")


(use-package emacs
  :hook
  (text-mode . auto-fill-mode)
  :init
  ;; No backup files
  (setq make-backup-files nil
        backup-inhibited nil
        create-lockfiles nil)

  (setq-default indent-tabs-mode nil
                tab-width my/tab-width
                ;; Automatically wrap lines after this point
                fill-column my/fill-width
                ;; Stop at the first error in compilation log
                compilation-scroll-output 'first-error
                word-wrap t
                require-final-newline t
                sentence-end-double-space nil
                save-interprogram-paste-before-kill t
                ;; Better support for files with long lines
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)

  (set-default-coding-systems 'utf-8)
  (global-superword-mode 1)    ;; e.g. this-is-a-symbol is one word
  (global-auto-revert-mode 1)  ;; Revert buffers when the underlying file has changed
  (global-so-long-mode 1))     ;; Better support for files with long lines


;;; Recentf

(use-package recentf
  :hook ((prog-mode text-mode) . recentf-mode)
  :custom (recentf-save-file
           (no-littering-expand-var-file-name "recentf"))
  :init
  (setq recentf-max-saved-items 1000 ;; Total amount of saved recent files
        recentf-auto-cleanup 'never) ;;  Never clean the history but append and remove the last
  (recentf-mode))

;;; Saveplace

(use-package saveplace
  :init (save-place-mode))

;;; Paren

(use-package paren
  :straight (:type built-in)
  :commands (show-paren-mode)
  :hook (prog-mode . show-paren-mode)
  :config
  (show-paren-mode +1)
  (setq show-paren-highlight-openparen t        ;; Always show the matching parenthesis.
        show-paren-delay 0
        show-paren-when-point-inside-paren t))  ;; Show parenthesis when inside a block.

;;; Electric-Pair-Mode

(use-package elec-pair
  :straight (:type built-in)
  :config 
  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")
                              (?\< . ?\>)
                              ))
  (electric-pair-mode t))

;;; Multiple-Cursors

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)

  ("C-\"" . mc/skip-to-next-like-this)
  ("C-:" . mc/skip-to-previous-like-this))

;;; Nerd-Commenter

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;; HL-TODO

(use-package hl-todo
  :init (global-hl-todo-mode))

;;; Aggressive-Indent

(use-package aggressive-indent
  :defer 5
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

;; Ws-Butler

(use-package ws-butler
  :hook (prog-mode . ws-butler-global-mode))

;;; Yasnippet

(use-package yasnippet
  :diminish yas-global-mode

  :commands (yas-global-mode)

  :hook ((prog-mode text-mode) . yas-global-mode)

  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'jh-editor)
;;; jh-editor.el ends here

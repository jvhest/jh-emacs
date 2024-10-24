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
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

;;; Electric-Pair-Mode

(use-package elec-pair
  :straight (:type built-in)
  :config
  (setq electric-pair-inhibit-predicate
        `(lambda (c)
           (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")
                              (?\< . ?\>)
                              )))

;;; Smartparens

(use-package smartparens
  :hook ((lisp-interaction-mode
          scheme-mode)
         . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("M-["           . sp-backward-slurp-sexp)
        ("M-]"           . sp-forward-slurp-sexp)
        ("M-{"           . sp-backward-barf-sexp)
        ("M-}"           . sp-forward-barf-sexp)
        ("M-U"           . sp-raise-sexp)
        ("M-R"           . raise-sexp)
        ("M-C"           . sp-convolute-sexp)
        ("M-D"           . my/sp-duplicate-sexp)
        ("M-J"           . sp-join-sexp)
        ("M-S"           . sp-split-sexp)
        ("C-M-<up>"      . sp-raise-sexp)
        ("C-<right>"     . sp-forward-slurp-sexp)
        ("C-<left>"      . sp-backward-slurp-sexp)
        ("M-<right>"     . sp-forward-barf-sexp)
        ("M-<left>"      . sp-backward-barf-sexp)
        ("M-K"           . sp-kill-hybrid-sexp)
        ("C-x C-t"       . sp-transpose-hybrid-sexp)
        ("C-M-n"         . sp-next-sexp)
        ("C-M-p"         . sp-previous-sexp)
        ("C-<backspace>" . sp-backward-kill-word))
  :init
  (add-hook 'smartparens-enabled-hook
            (lambda ()
              "Disable \\[electric-pair-mode] when \[[smartparens-mode]] is enabled."
              (electric-pair-local-mode -1)))
  (add-hook 'smartparens-disabled-hook
            (lambda ()
              "Enable \\[electric-pair-mode] when \[[smartparens-mode]] is disabled."
              (electric-pair-local-mode +1)))
  :config
  ;; Repeat actions
  ;; Scratch buffer for: emacs-lisp-mode

  (defun my/sp-duplicate-sexp (&optional arg)
    (interactive "p")
    (insert (buffer-substring
             (save-excursion
               (backward-sexp)
               (point))
             (point))))

  (defvar lisp-navigation-map
    (let ((map (make-sparse-keymap)))
      (pcase-dolist (`(,k . ,f)
                     '(("u" . backward-up-list)
                       ("f" . forward-sexp)
                       ("b" . backward-sexp)
                       ("d" . down-list)
                       ("n" . sp-next-sexp)
                       ("p" . sp-previous-sexp)
                       ("k" . sp-kill-sexp)
                       ("K" . sp-kill-hybrid-sexp)
                       ("]" . sp-forward-slurp-sexp)
                       ("[" . sp-backward-slurp-sexp)
                       ("}" . sp-forward-barf-sexp)
                       ("{" . sp-backward-barf-sexp)
                       ("r" . raise-sexp)
                       (";" . sp-comment)
                       ("C" . sp-convolute-sexp)
                       ("D" . my/sp-duplicate-sexp)
                       ("J" . sp-join-sexp)
                       ("S" . sp-split-sexp)
                       ("R" . sp-raise-sexp)
                       ("\\" . indent-region)
                       ("t" . transpose-sexps)
                       ("x" . eval-defun)
                       ("e" . eval-last-sexp)))
        (define-key map (kbd k) f))
      map))

  (map-keymap
   (lambda (_ cmd)
     (put cmd 'repeat-map 'lisp-navigation-map))
   lisp-navigation-map)
  (put 'kill-sexp 'repeat-map 'lisp-navigation-map)

  ;; (require 'smartparens-config)
  (sp-with-modes sp-lisp-modes
    ;; disable ', it's the quote character!
    ;; (sp-local-pair "`" "'")
    (sp-local-pair "'" nil :actions nil)))

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

(use-package aggressive-indent)

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

(defvar-keymap jh-snippet-map
  :doc "Snippets related commands"
  "c" (cons "Create" 'yas-new-snippet)
  "e" (cons "Expand" 'yas-expand)
  "i" (cons "Insert" 'yas-insert-snippet)
  "r" (cons "Reload" 'yas-reload-all)
  "v" (cons "Edit" 'yas-visit-snippet-file))

(provide 'jh-editor)
;;; jh-editor.el ends here

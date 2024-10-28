;;; jh-minibuffer.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Isearch

;; (use-package isearch
;;   :straight (:type built-in)
;;   :config
;;   (progn
;;     (setq search-whitespace-regexp ".*?"
;;           isearch-lax-whitespace t
;;           isearch-regexp-lax-whitespace nil
;;           isearch-lazy-highlight t
;;           search-lazy-count t
;;           lazy-count-prefix-format "(%s/%s) "
;;           lazy-count-suffix-format nil)))
;;
;;; Vertico

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("M-h" . vertico-directory-up))
  :init
  (vertico-mode 1))

;;; Marginalia

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :init
  (marginalia-mode))

;;; Orderless

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;; Consult

(use-package consult
  :demand t
  :after vertico
  :bind (;; Rebind C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)
         ("C-c T"   . consult-theme)

         ("C-c c f" . describe-face)
         ("C-c c t" . consult-theme)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; Rebind C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Rebind Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)

         ;; Rebind Other custom bindings
         ("M-y"     . consult-yank-pop)            ;; orig. yank-pop

         ;; Rebind M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)             ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-headingq
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                   ;; orig. next-matching-history-element
         ("M-r" . consult-history)))                 ;; orig. previous-matching-history-element

;;  ("M-s M-a" . consult-org-agenda)

;; :config
;; (setq completion-in-region-function #'consult-completion-in-region))

(use-package consult-flycheck)
;; :bind (("M-g f" . consult-flycheck)))      ; find next flycheck error

(use-package consult-dir
  :bind (("C-x F" . consult-dir)      ; find file in directory
         :map vertico-map
         ("C-x F" . consult-dir)))

;;; Embark

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)

  :config
  (global-set-key [remap describe-bindings] #'embark-bindings))

(use-package embark-consult
  :demand t
  :after (:all embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'jh-minibuffer)
;;; jh-minibuffer.el ends here

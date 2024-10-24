;;; jh-modules.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:
(message "module")

;;; Eldoc

(use-package eldoc
  :straight (:type built-in)
  :diminish
  :hook ((emacs-lisp-mode python-mode) . eldoc-mode)
  :config
  (use-package eldoc-box
    :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
    :custom
    (eldoc-box-lighter nil)
    (eldoc-box-only-multi-line t)
    (eldoc-box-clear-with-C-g t)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    (eldoc-box-body ((t (:inherit tooltip))))
    :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
    :config
    ;; Prettify `eldoc-box' frame
    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;;; Eglot

(use-package eglot
  :commands (eglot eglot-ensure)

  :preface
  ;; Setup eldoc the way I like it for emacs
  (defun my/setup-eldoc-for-eglot ()
    "Make sure Eldoc will show us all of the feedback at point."
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose))

  :hook (eglot-managed-mode . my/setup-eldoc-for-eglot)

  :bind (:map eglot-diagnostics-map
              ("M-RET" . eglot-code-actions)))

(use-package consult-eglot
  :after eglot
  :bind ("M-s s" . consult-eglot-symbols))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

;;; Flycheck

(use-package flycheck
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))

  :custom-face
  (flycheck-error   ((t (:inherit error :underline t))))
  (flycheck-warning ((t (:inherit warning :underline t))))

  :bind
  (:map flycheck-mode-map
        ("M-g f" . consult-flycheck))

  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.1)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil))

(use-package sideline
  :hook ((flycheck-mode . sideline-mode))
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t              ; display the backend name
        sideline-backends-right '((sideline-flycheck . down))))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

;;; Treesitter-Grammars

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(defun install-treesit-grammars ()
  "Installs a list of treesitter-grammars."
  (interactive)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;;; Load modules config files

(require 'jh-python)
(require 'jh-lisp)
(require 'jh-markdown)
;; (require 'jh-terminal)
(require 'jh-dape)
(require 'jh-reader)

(provide 'jh-modules)
;;; jh-modules.el ends here

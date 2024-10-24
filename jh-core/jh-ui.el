;;; jh-ui.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Settings

(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'light
                    :height 110)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'light
                    :height 110)
(set-face-attribute 'variable-pitch nil
                    :font "JetBrainsMono Nerd Font"   ;; "Iosevka Comfy Motion Duo"
                    :weight 'light
                    :height 110)
(set-face-attribute 'mode-line nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'semilight
                    :height 145)

(defvar my/frame-transparency 90
  "Frame transparency.")

(defvar my/display-line-numbers-enable
  '(prog-mode-hook conf-mode-hook)
  "Modes with line-numbers.")


(use-package emacs
  :preface
  (defun toggle-transparency ()
    "Toggle theme's transparency."
    (interactive)
    (let ((frame-alpha (frame-parameter nil 'alpha)))
      (if (or (not frame-alpha)
              (= (cadr frame-alpha) 100))
          (set-frame-parameter nil 'alpha
                               `(,my/frame-transparency
                                 ,my/frame-transparency))
        (set-frame-parameter nil 'alpha '(100 100)))))

  :hook
  (text-mode . variable-pitch-mode)

  :custom
  ;; better scrolling experience
  (scroll-margin 0)
  (fast-but-imprecise-scrolling t)  ;; Make scrolling less stuttered
  (scroll-conservatively 101)       ;; > 100
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)

  :config
  ;; line-numbers
  (dolist (mode my/display-line-numbers-enable)
    (add-hook mode 'display-line-numbers-mode))
  (setq-default display-line-numbers-width 3    ;; Enough space for big files
                display-line-numbers-widen t)   ;; Enable dynamic sizing of line number width

  ;; general ui settings
  (global-prettify-symbols-mode t)   ;; Enables us to use ligatures in Emacs.

  ;; frame settings
  (set-frame-parameter nil 'alpha '(100 100))
  (setq frame-title-format '("%b")
        frame-resize-pixelwise t
        frame-inhibit-implied-resize t)

  (setq ring-bell-function #'ignore
        visible-bell nil
        use-short-answers t                ;; Use "y" and "n" answers
        auto-window-vscroll nil)            ;; Make scrolling less stuttered

  (blink-cursor-mode -1)
  (tooltip-mode -1))


;;;  Hydra interface

(use-package posframe)

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (setq hydra-hint-display-type 'posframe)

  (with-eval-after-load 'posframe
    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(:left-fringe 8
                           :right-fringe 8
                           :internal-border-width 2
                           ;; :internal-border-color ,(face-background 'posframe-border nil t)
                           :background-color ,(face-background 'tooltip nil t)
                           :foreground-color ,(face-foreground 'tooltip nil t)
                           :lines-truncate t
                           :poshandler posframe-poshandler-frame-bottom-center)))
    (hydra-set-posframe-show-params)
    (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t)))

(use-package pretty-hydra
  :custom (pretty-hydra-default-title-body-format-spec " %s%s"))

;;; Theme

(setq custom-safe-themes t)

(use-package ef-themes
  :demand t
  :bind ("<f9>" . ef-themes-toggle)
  :init
  (setq ef-themes-to-toggle '(ef-owl ef-deuteranopia-light))   ;;  '(ef-trio-dark ef-trio-light))
  :config
  (setq ef-themes-headings
        '((0 . (1.35))
          (1 . (1.28))
          (2 . (1.22))
          (3 . (1.17))
          (4 . (1.14))
          (t . (1.1)))))
;; Disable all other themes to avoid awkward blending:
;; (mapc #'disable-theme custom-enabled-themes)
(load-theme (car ef-themes-to-toggle) t)

(use-package doom-modeline
  ;; Start up the modeline after initialization is finished
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 17)
  (doom-modeline-bar-width 6)
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project))

;;; Savehist

(use-package savehist
  :init (savehist-mode 1)
  :custom (savehist-file
           (no-littering-expand-var-file-name "history"))
  :config
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(register-alist kill-ring)))

;;; Hide-Mode-Line

(use-package hide-mode-line
  :hook ((shell-mode . hide-mode-line-mode)))

;;; Hl-Line

(use-package hl-line
  :preface
  (defvar global-hi-line-sticky-flag nil)
  :init
  (global-hl-line-mode))

;;; Helpful

(use-package helpful
  :bind
  (([remap describe-command] . helpful-command)
   ([remap describe-function] . helpful-callable)
   ([remap describe-key] . helpful-key)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h K" . describe-keymap)
   :map helpful-mode-map
   ([remap revert-buffer] . helpful-update)))

;;; Which-Key

(use-package which-key
  :demand t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-separator " → " )
  (setq which-key-unicode-correction 3)
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)
  (setq which-key-sort-order 'which-key-key-order-alpha))

;;; Pulse

(use-package pulse
  :preface
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  :config
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))

(provide 'jh-ui)
;;; jh-ui.el ends here

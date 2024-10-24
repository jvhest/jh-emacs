;;; jh-terminal.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Vterm

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  :preface
  (defun ian/new-vterm-instance ()
    (interactive)
    (vterm t))
  :config
  (setq vterm-disable-bold t)
  (setq vterm-timer-delay 0.01)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  (define-key vterm-mode-map (kbd "C-l") #'(lambda ()
                                             (interactive)
                                             (vterm-clear)
                                             (vterm-clear-scrollback))))


(use-package vterm-toggle
  :after (projectile vterm evil)
  :bind ("C-`" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname
                                 (or (equal major-mode 'vterm-mode)
                                     (string-prefix-p vterm-buffer-name bufname))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.5))))

(provide 'jh-terminal)
;;; jh-terminal.el ends here

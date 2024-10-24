;;; jh-python.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Python

(use-package python-mode
  :hook ((python-mode . eglot-ensure)
         (python-mode . corfu-mode)
         (python-mode . eldoc-mode)
         (inferior-python-mode . hide-mode-line-mode))
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))


(use-package auto-virtualenv
  :hook ((python-mode . auto-virtualenv-set-virtualenv)
         (projectile-after-switch-project . auto-virtualenv-set-virtualenv))
  :init
  (use-package pyvenv
    :config
    (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))))

(use-package blacken
  :diminish blacken-mode
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  (blacken-only-if-project-is-blackened t)
  (blacken-line-length 88)
  (black-fast-unsafe t))

(provide 'jh-python)
;;; jh-python.el ends here

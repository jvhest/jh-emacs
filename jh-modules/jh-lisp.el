;;; jh-lisp.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


;; (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
;; (add-hook 'lisp-mode-hook 'smartparens-mode)


;;; Lisp

(use-package lisp-mode
  :straight (:type built-in)
  :hook (aggressive-indent-mode smartparens-mode)
  :config
  (cl-pushnew  "/usr/bin" exec-path)
  (setq inferior-lisp-program "sbcl"))


;;; Emacs-Lisp

(use-package elisp-mode
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . eldoc-mode))


;;; Slime

(use-package slime
  :config
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl)))


;;; Racket

(use-package racket-mode
  :config
  (cl-pushnew  "~/.local/racket/bin" exec-path)
  (setf racket-program "racket"))

(provide 'jh-lisp)
;;; jh-lisp.el ends here

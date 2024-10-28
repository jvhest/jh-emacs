;;; jh-lisp.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


;;; Racket-Mode

(use-package racket-mode
  :config
  (cl-pushnew  "~/.local/racket/bin" exec-path)
  (setf racket-program "racket"))

;;; Lisp

(use-package lisp-mode
  :straight (:type built-in)
  :config
  (cl-pushnew  "/usr/bin" exec-path)
  (setq inferior-lisp-program "sbcl"))

(use-package slime
  :config
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl)))

(provide 'jh-lisp)
;;; jh-lisp.el ends here

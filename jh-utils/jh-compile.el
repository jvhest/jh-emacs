;;; jh-compile.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


(require 'compile-funcs)

(defvar my/compile-tasks `((,core-source-dir)
                           (,modules-source-dir)
                           (,org-source-dir)
                           (,utils-source-dir)
                           (,lisp-source-dir)
                           (,user-emacs-directory "early-init" "init")))

;;;;; Modules
(add-hook 'emacs-startup-hook ;; or kill-emacs-hook?
          (lambda ()
            (when my/compile-on-startup
              (my/compile-modules))))

;;;;; On save files
(add-hook 'after-save-hook
          (lambda ()
            (when (and my/compile-on-save
                       (string-equal major-mode "emacs-lisp-mode"))
              (my/compile-buffer))))

(provide 'jh-compile)
;;; jh-compile.el ends here

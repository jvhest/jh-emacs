;;; jh-core.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

(defvar my/themes-dir (expand-file-name "themes/" user-emacs-directory)
  "Location of themes files.")
(cl-pushnew my/themes-dir load-path)

(defvar var-dir (expand-file-name "var" user-emacs-directory))

(defvar my/compile-on-save nil
  "When non-nil, compile the file after saving it.")

(defvar my/compile-on-startup nil
  "When non-nil, compile EMACS LISP sources for the modules on startup.")


(use-package emacs
  :demand t
  :hook
  ;; Make shebang (#!) file executable when saved
  (after-save . executable-make-buffer-file-executable-if-script-p)

  :custom
  (large-file-warning-threshold 100000000)
  (global-auto-revert-non-file-buffers t)
  (kill-do-not-save-duplicates t)

  :config
  (setq-default  max-lisp-eval-depth 50000
                 max-specpdl-size 10000
                 ;; Use the most recent byte code ops
                 byte-compile--use-old-handlers nil
                 ;; Always follow symbolic links
                 vc-follow-symlinks t
                 ;; Save paste history when killing Emacs
                 save-interprogram-paste-before-kill t
                 repeat-mode 1))

;;; Source all config modules

;;; Core source directories for init.el
(defvar core-source-dir (expand-file-name "jh-core/" user-emacs-directory))
(defvar modules-source-dir (expand-file-name "jh-modules/" user-emacs-directory))
(defvar org-source-dir (expand-file-name "jh-org/" user-emacs-directory))
(defvar utils-source-dir (expand-file-name "jh-utils/" user-emacs-directory))
(defvar lisp-source-dir (expand-file-name "site-lisp/" user-emacs-directory))
(defvar themes-dir (expand-file-name "themes/" user-emacs-directory))

;;; Create list for source dirs
(defvar source-dirs (list core-source-dir
                          modules-source-dir
                          org-source-dir
                          utils-source-dir
                          lisp-source-dir))

;;; Add all source and theme dirs to load-path
;; (eval-and-compile
  (dolist (dir source-dirs)
    (add-to-list 'load-path dir t))
  (add-to-list 'custom-theme-load-path my/themes-dir)

;;; Load core configuration files
(require 'jh-ui)
(require 'jh-editor)
(require 'jh-windows)
(require 'jh-dired)
(require 'jh-projects)
(require 'jh-minibuffer)
(require 'jh-completion)
(require 'jh-bindings)

;;;; Load modules configuration files
(require 'jh-modules)

;;;; Load org configuration files
(require 'jh-org)

;;;; Load utils configuration files
(require 'jh-utils)

(provide 'jh-core)
;;; jh-core.el ends here

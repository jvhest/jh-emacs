;;; jh-reader.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Nov

(use-package nov
  :defer 10
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :preface
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    ;; (visual-line-mode 1)
    (display-line-numbers-mode -1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.0)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors t)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))

;;; PDF

(use-package pdf-view
  :disabled t
  :when (display-graphic-p)
  ;; :ensure pdf-tools
  :diminish (pdf-view-themed-minor-mode
             pdf-view-midnight-minor-mode
             pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init (setq pdf-view-use-scaling t
              pdf-view-use-imagemagick nil
              pdf-annot-activate-created-annotations t)
  :config
  ;; Activate the package
  (pdf-tools-install t nil t nil)

  ;; Recover last viewed position
  (use-package saveplace-pdf-view
    :when (ignore-errors (pdf-info-check-epdfinfo) t)
    :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
    :init
    (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
    (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice)))

(provide 'jh-reader)
;;; jh-reader.el ends here

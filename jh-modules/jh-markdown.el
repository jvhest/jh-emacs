;;; jh-markdown.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Markdown

(use-package markdown-mode
  :hook (smartparens-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Use pandoc to convert documents from markdown to HTML
  (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax")
  (setq markdown-enable-wiki-links t)               ;; Syntax highlighting for wiki links
  (setq markdown-italic-underscore t)               ;; Use underscores for italic text
  (setq markdown-make-gfm-checkboxes-buttons t)     ;; Make checkboxes into buttons you can interact with
  (setq markdown-gfm-additional-languages '("sh"))  ;; Add `sh' as a language to convert
  (setq markdown-fontify-code-blocks-natively t))   ;; Highlight code using the languages major mode

(provide 'jh-markdown)
;;; jh-markdown.el ends here

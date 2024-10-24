;;; jh-completion.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:

;;; Corfu
(message "completion")

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-count 8)
  (corfu-min-width 25)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.8)
  (corfu-echo-documentation 0.25) ; Echo docs for current completion option
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-on-exact-match nil)       ; Don't auto expand tempel snippets

  :hook (corfu-popupinfo-mode)
  ;; Optionally use TAB for cycling, default is `corfu-complete'.

  :bind (:map corfu-map
              ("RET" . nil)
              ("SPC" . corfu-insert-separator)
              ("M-p" . corfu-doc-scroll-down)
              ("M-n" . corfu-doc-scroll-up)
              ("M-d" . corfu-doc-toggle))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info

;;; Cape

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)

  :init
  ;; Add useful defaults completion sources from cape
  (cl-pushnew #'cape-dabbrev completion-at-point-functions)
  (cl-pushnew #'cape-file completion-at-point-functions)
  (cl-pushnew #'cape-elisp-block completion-at-point-functions)
  (cl-pushnew #'cape-keyword completion-at-point-functions)
  (cl-pushnew #'cape-abbrev completion-at-point-functions)

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'jh-completion)
;;; jh-completion.el ends here

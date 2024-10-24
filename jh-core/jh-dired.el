;;; jh-dired.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Modules for init.el.

;;; Code:

(use-package dired
  :straight (:type built-in)
  :defer 5
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("F" . (lambda ()
                  (interactive)
                  (mapc #'find-file (reverse (dired-get-marked-files))))))
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :custom (dired-listing-switches "-alh --group-directories-first")
  :hook (dired-hide-details-mode
         dired-omit-mode
         auto-revert-mode
         hl-line-mode)
  :config
  (setq ls-lisp-dirs-first t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; (setq dired-hide-details-mode 1)
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq-default dired-dwim-target t))

(use-package dired-x
:straight (:type built-in)
:after dired
;; :bind ("H-d" . dired-jump)
:config
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-clean-confirm-killing-deleted-buffers nil)
(require 'ls-lisp)
(setq directory-free-space-program nil)
(setq dired-x-hands-off-my-keys t)
(setq dired-omit-verbose nil
      dired-omit-files
      (concat dired-omit-files
              "\\|^.DS_Store\\'"
              "\\|^.project\\(?:ile\\)?\\'"
              "\\|^.\\(svn\\|git\\)\\'"
              "\\|^.ccls-cache\\'"
              "\\|\\(?:\\.js\\)?\\.meta\\'"
              "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))

(setq dired-guess-shell-alist-user  ;; those are the suggestions for ! and & in Dired
'(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" "xdg-open")
  ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "feh" "xdg-open")
  ("\\.\\(?:xcf\\)\\'" "xdg-open")
  ("\\.csv\\'" "xdg-open")
  ("\\.tex\\'" "xdg-open")
  ("\\.\\(?:mp[4]\\|m4a\\|ogg\\|webm\\|mkv\\)" "mpv" "xdg-open")
  ("\\.\\(?:mp4\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "xdg-open")
  ("\\.\\(?:mp3\\|flac\\)\\'" "mpv" "xdg-open")
  ("\\.html?\\'" "xdg-open")
  ("\\.md\\'" "xdg-open"))))

(use-package dired-subtree
  :bind ( :map dired-mode-map
          (("<tab>" . dired-subtree-toggle)
           ("<backtab>" . dired-subtree-remove))) ; S-TAB
  :config
  (setq dired-subtree-use-backgrounds nil))

(provide 'jh-dired)
;;; jh-dired.el ends here

;;; jh-bindings.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:
(message "keys")

;;; Keybindings

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; editing related bindings
(require 'edit-funcs)

(bind-key "s-v" 'select-line)
(bind-key "s-o" 'open-line-below)
(bind-key "s-O" 'open-line-above)
(bind-key "M-w" 'kill-ring-save-whole-line-or-region)
(bind-key "s-y" 'kill-ring-save-whole-line-or-region)
(bind-key "C-w" 'kill-whole-line-or-region)
(bind-key "s-d" 'kill-whole-line-or-region)
(bind-key "s-x" 'extend-region-by-word)
(bind-key "s-P" 'yank-above)
(bind-key "s-p" 'yank-below)
(bind-key "s-c" 'duplicate-region-or-line)
(bind-key "s-<down>" 'move-region-or-line-down)
(bind-key "s-<up>" 'move-region-or-line-up)
(bind-key "s-`" 'toggle-case)
(bind-key "C-^" 'top-join-line)
(bind-key "M-z" 'zap-up-to-char)
(bind-key "C-6" 'switch-to-windows-last-buffer)

(bind-key "M-u" 'upcase-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-c" 'capitalize-dwim)

(defvar-keymap jh-buffer-map
  :doc "Buffer related commands."
  :prefix 'ctl-z-b
  "c" (cons "" #'clone-indirect-buffer-other-window)
  "f" (cons "Fit to window" #'fit-window-to-buffer)
  "g" (cons "Revert buffer" #'revert-buffer-quick)
  "n" (cons "Next buffer" #'next-buffer)
  "p" (cons "Prev buffer" #'previous-buffer))
;; (put 'next-buffer 'repeat-map 'jh-buffer-map)
;; (put 'previous-buffer 'repeat-map 'jh-buffer-map)

(defvar-keymap jh-goto-map
  :doc "Keymap for goto."
  :prefix 'ctl-z-g
  "e" (cons "Compile errors" #'consult-compile-error)
  "f" (cons "Flycheck errors" #'consult-flycheck)
  "g" (cons "Goto linenumber" #'consult-goto-line)
  "o" (cons "Org headings" #'consult-outline)
  "m" (cons "Mark" #'consult-mark)
  "k" (cons "Global mark" #'consult-global-mark)
  "i" (cons "Imenu" #'consult-imenu)
  "I" (cons "Imenu multi" #'consult-imenu-multi))

(defvar-keymap jh-search-map
  :doc "Keymap for search."
  :prefix 'ctl-z-s
  "a" (cons "Agenda" #'consult-org-agenda)
  "g" (cons "Grep" #'consult-grep)
  "r" (cons "Ripgrep" #'consult-ripgrep)
  "f" (cons "Filenames" #'consult-find)
  "o" (cons "Outline" #'consult-outline)
  "l" (cons "Search-buffer" #'consult-line)
  "d" (cons "Directory" #'consult-dir)
  "h" (cons "History" #'consult-history))

(defvar-keymap jh-sexp-eval-map
  :doc "Evaluate s-expression."
  :prefix 'ctl-z-e
  "b" (cons "Eval buffer" #'eval-buffer)
  "d" (cons "Eval defun" #'eval-defun)
  "r" (cons "Eval region" #'eval-region)
  "s" (cons "Eshell" #'eshell))

(with-no-warnings
  (pretty-hydra-define sexp-motion-hydra
    (:title "Sexp Motions" :color pink :quit-key ("q" "C-g"))
    ("Sexpr"
     (("b" sp-beginning-of-sexp "Beginning")
      ("e" sp-end-of-sexp "End")
      ("d" sp-down-sexp "Down")
      ("u" sp-up-sexp "Up")
      ("U" sp-backward-up-sexp "Up")
      ("D" sp-backward-down-sexp "Down")
      ("f" sp-forward-sexp "Forward")
      ("p" sp-backward-sexp "Backward")
      ("t" sp-transpose-sexp "Transpose")
      ("k" sp-kill-sexp "Kill" :color blue)))))

(defvar-keymap jh-sexp-motion-map
  :doc "Structured movement s-expressions."
  :prefix 'ctl-z-x
  "a" (cons "Start defun" #'beginning-of-defun)
  "e" (cons "End defun" #'end-of-defun)
  "n" (cons "List ->" #'forward-list)
  "p" (cons "List <-" #'backward-list)
  "d" (cons "List up" #'up-list)
  "u" (cons "List backw-up" #'backward-up-list)
  "z" (cons "Motion hydra" #'sexp-motion-hydra/body))

(defvar-keymap jh-file-map
  :doc "File related commands."
  :prefix 'ctl-z-f
  "b" (cons "Bookmark jump" #'bookmark-jump)
  "l" (cons "Find library" #'find-library)
  "m" (cons "Manual" #'consult-man))

(with-no-warnings
  (pretty-hydra-define toggles-hydra
    (:title "Toggles" :color amaranth :quit-key ("q" "C-g"))
    ("Basic"
     (("n" (cond ((fboundp 'display-line-numbers-mode)
                  (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                 ((fboundp 'gblobal-linum-mode)
                  (global-linum-mode (if global-linum-mode -1 1))))
       "line number"
       :toggle (or (bound-and-true-p display-line-numbers-mode)
                   (bound-and-true-p global-linum-mode)))
      ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
      ("d" global-hungry-delete-mode "hungry delete" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("s" prettify-symbols-mode "pretty symbol" :toggle t)
      ("l" global-page-break-lines-mode "page break lines" :toggle t)
      ("m" doom-modeline-mode "modern mode-line" :toggle t)
      ("t" toggle-truncate-lines "truncate lines" :toggle t)
      ("v" variable-pitch-mode "variable pitch" :toglle t))
     "Highlight"
     (("h l" global-hl-line-mode "line" :toggle t)
      ("h p" show-paren-mode "paren" :toggle t)
      ("h s" symbol-overlay-mode "symbol" :toggle t)
      ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("h t" global-hl-todo-mode "todo" :toggle t))
     "Program"
     (("f" flycheck "flycheck" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
      ("v" global-diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("M" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t)))))

;; `prefix-ctrl-z-map'
(defvar-keymap jh-ctrl-z-map
  :doc "Prefix keymap with multiple subkeymaps."
  :prefix 'ctrl-z
  "b" (cons "Buffer" jh-buffer-map)
  "c" (cons "Clock" #'world-clock)
  "e" (cons "Sexp eval" jh-sexp-eval-map)
  "f" (cons "Files" jh-file-map)
  "g" (cons "Goto" jh-goto-map)
  "m" (cons "Magit" #'magit-status)
  "n" (cons "Narrow" narrow-map)
  "o" (cons "Org-hydra" #'org-hydra/body)
  "r" (cons "Registers" ctl-x-r-map)
  "s" (cons "Search" jh-search-map)
  "t" (cons "Toggles" #'toggles-hydra/body)
  "u" (cons "Universal-arg" #'universal-argument)
  "v" (cons "Version Control" 'vc-prefix-map)
  "w" (cons "Window" jh-window-map)
  "x" (cons "Sexp motions" jh-sexp-motion-map)
  "y" (cons "Snippet" jh-snippet-map))

(keymap-set global-map "C-z" jh-ctrl-z-map)

(provide 'jh-bindings)
;;; jh-bindings.el ends here

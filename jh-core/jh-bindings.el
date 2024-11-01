;;; jh-bindings.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


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


;;; s-z keymap

(defvar-keymap jh-buffer-map
  :doc "Buffer related commands."
  :prefix 's-z-b
  "c" (cons "" #'clone-indirect-buffer-other-window)
  "f" (cons "Fit to window" #'fit-window-to-buffer)
  "g" (cons "Revert buffer" #'revert-buffer-quick)
  "n" (cons "Next buffer" #'next-buffer)
  "p" (cons "Prev buffer" #'previous-buffer))
;; (put 'next-buffer 'repeat-map 'jh-buffer-map)
;; (put 'previous-buffer 'repeat-map 'jh-buffer-map)

(defvar-keymap jh-window-map
  :doc "Window related commands"
  :prefix 's-z-w
  "u" (cons "Undo" 'winner-undo)
  "r" (cons "Redo" 'winner-redo)
  "b" (cons "Balans =|=" 'balance-windows-area)
  "d" (cons "Toggle dedicated" 'toggle-window-dedicated)
  "!" (cons "Delete other vert" 'delete-other-windows-vertically)
  "@" (cons "Split root below" 'split-root-window-below)
  "#" (cons "Split root right" 'split-root-window-right)
  "^" (cons "Tear off" 'tear-off-window)
  "h" (cons "Move <-" 'windmove-left)
  "j" (cons "Move v" 'windmove-down)
  "k" (cons "Move ^" 'windmove-up)
  "l" (cons "Move ->" 'windmove-right)
  "H" (cons "Swap <-" 'windmove-swap-states-left)
  "J" (cons "Swap v" 'windmove-swap-states-down)
  "K" (cons "Swap ^" 'windmove-swap-states-up)
  "L" (cons "Swap ->" 'windmove-swap-states-right)
  "z" (cons "Window hydra" 'window-hydra/body))

(defvar-keymap jh-goto-map
  :doc "Keymap for goto."
  :prefix 's-z-g
  "e" (cons "Compile errors [M-g e]" #'consult-compile-error)
  "f" (cons "Flycheck errors [M-g f]" #'consult-flycheck)
  "g" (cons "Goto linenumber [M-g l]" #'consult-goto-line)
  "o" (cons "Org headings [M-g o]" #'consult-outline)
  "m" (cons "Mark [M-g m]" #'consult-mark)
  "k" (cons "Global mark [M-g k]" #'consult-global-mark)
  "i" (cons "Imenu [M-g i]" #'consult-imenu)
  "I" (cons "Imenu multi [M-g I]" #'consult-imenu-multi))

(defvar-keymap jh-search-map
  :doc "Keymap for search."
  :prefix 's-z-s
  "a" (cons "Agenda [M-s M-a]" #'consult-org-agenda)
  "g" (cons "Grep [M-s g]" #'consult-grep)
  "r" (cons "Ripgrep [M-s r]" #'consult-ripgrep)
  "f" (cons "Filenames [M-s d]" #'consult-find)
  "l" (cons "Isearch-buffer [M-s l]" #'consult-line)
  "L" (cons "Isearch-buffer multi [M-s L]" #'consult-line-multi)
  "d" (cons "Directory [C-x F]" #'consult-dir)
  "h" (cons "History [C-c h]" #'consult-history)
  "i" (cons "Isearch history [M-s e]" #'consult-isearch-history))

(defvar-keymap jh-sexp-eval-map
  :doc "Evaluate s-expression."
  :prefix 's-z-e
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
  :prefix 's-z-x
  "a" (cons "Start defun" #'beginning-of-defun)
  "e" (cons "End defun" #'end-of-defun)
  "n" (cons "List ->" #'forward-list)
  "p" (cons "List <-" #'backward-list)
  "d" (cons "List up" #'up-list)
  "u" (cons "List backw-up" #'backward-up-list)
  "z" (cons "Motion hydra" #'sexp-motion-hydra/body))

(defvar-keymap jh-file-map
  :doc "File related commands."
  :prefix 's-z-f
  "b" (cons "Bookmark jump" #'bookmark-jump)
  "l" (cons "Find library" #'find-library)
  "m" (cons "Manual" #'consult-man))

(defvar-keymap jh-snippet-map
  :doc "Snippets related commands"
  "c" (cons "Create" 'yas-new-snippet)
  "e" (cons "Expand" 'yas-expand)
  "i" (cons "Insert" 'yas-insert-snippet)
  "r" (cons "Reload" 'yas-reload-all)
  "v" (cons "Edit" 'yas-visit-snippet-file))

;; `prefix-s-z-map'
(defvar-keymap jh-z-map
  :doc "Prefix keymap with multiple subkeymaps."
  :prefix 's-z
  "b" (cons "Buffer" jh-buffer-map)
  "c" (cons "Clock" #'world-clock)
  "e" (cons "Sexp eval" jh-sexp-eval-map)
  "f" (cons "Files" jh-file-map)
  "g" (cons "Goto" jh-goto-map)
  "m" (cons "Magit" #'magit-status)
  "n" (cons "Narrow" narrow-map)
  "r" (cons "Registers" ctl-x-r-map)
  "s" (cons "Search" jh-search-map)
  "u" (cons "Universal-arg" #'universal-argument)
  "v" (cons "Version Control" 'vc-prefix-map)
  "w" (cons "Window" jh-window-map)
  "x" (cons "Sexp motions" jh-sexp-motion-map)
  "y" (cons "Snippet" jh-snippet-map))

(keymap-set global-map "s-z" jh-z-map)

(provide 'jh-bindings)
;;; jh-bindings.el ends here

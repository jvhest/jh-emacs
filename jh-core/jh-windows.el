;;; jh-windows.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:


(setq switch-to-buffer-obey-display-actions t)

;; see doc buffer-math-p

(defun get-buffer-major-mode ()
  "Get major-mode name of current buffer."
  (buffer-local-value 'major-mode (current-buffer)))

;; display on right-side.
(add-to-list 'display-buffer-alist
             '((or (major-mode . Info-mode)
                   (major-mode . helpful-mode)
                   (major-mode . help-mode)
                   (major-mode . occur-mode))
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.45)
               (inhibit-same-window . t)
               (window-parameters
                (no-other-windows . t))))

;; display at top
(add-to-list 'display-buffer-alist
             '((or "\\*Flycheck.*")
               (display-buffer-in-side-window)
               (side . top)
               (slot . 0)
               (window-height . 0.20)
               (inhibit-same-window . t)
               (window-parameters
                (no-other-windows . t))))

;; display at bottom
(add-to-list 'display-buffer-alist
             '((or (major-mode . dired-mode)
                   "\\*Messages.*"
                   "^\\(\\*e?shell\\|vterm\\).*"
                   "\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
                   "\\*\\(Output\\|Register Preview\\).*"
                   ".*\\*\\(Completions\\|Embark Live Occur\\).*"
                   "\\*Embark Occur.*")
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.30)
               (inhibit-same-window . t)
               (window-parameters
                (no-other-windows . t))))


;;; Popper

(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
              ("C-h z"       . popper-toggle)
              ("C-<tab>"     . popper-cycle)
              ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-echo-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode
          dired-mode
          occur-mode

          flycheck-error-list-mode
          flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode
          "^\\*gt-result\\*$" "^\\*gt-log\\*$"

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (propertize " POP " 'face face)))))
  :config
  (setq popper-display-control nil)

  ;; group by project.el project root, with fall back to default-directory
  (setq popper-group-function #'popper-group-by-directory)

  (with-no-warnings
    ;; (defun my-popper-fit-window-height (win)
    ;;   "Determine the height of popup window WIN by fitting it to the buffer's content."
    ;;   (fit-window-to-buffer
    ;;    win
    ;;    (floor (frame-height) 3)
    ;;    (floor (frame-height) 3)))
    ;; (setq popper-window-height #'my-popper-fit-window-height)


    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and ;(called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
        (when-let ((window (caar popper-open-popup-alist))
                   (buffer (cdar popper-open-popup-alist)))
          (when (and (with-current-buffer buffer
                       (not (derived-mode-p 'eshell-mode
                                            'shell-mode
                                            'term-mode
                                            'vterm-mode)))
                     (window-live-p window))
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


;;; Winner-Mode

(winner-mode 1)

;;; Window-Hydra

(use-package emacs
  :straight (:type built-in)
  :bind ("S-<f9>" . winodw-hydra/body)
  :config
  (pretty-hydra-define window-hydra
    (:title "Window" :color pink :quit-key ("q" "C-g"))
    ("Resize"
     (("h" shrink-window-horizontally "←")
      ("j" enlarge-window "↓")
      ("k" shrink-window "↑")
      ("l" enlarge-window-horizontally "→")
      ("|" balance-windows "balance"))
     "Zoom"
     (("+" text-scale-increase "in")
      ("=" text-scale-increase "in")
      ("-" text-scale-decrease "out")
      ("0" (text-scale-increase 0) "reset")))))

(provide 'jh-windows)
;;; jh-windows.el ends here

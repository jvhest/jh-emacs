;;; jh-windows.el --- Emacs configuration      -*- lexical-binding: t; -*-

;; Maintainer: Jan van Hest
;; Email: jan.vanhest@gmail.com

;;; Commentary:

;; Modules for init.el.

;;; Code:



(defun display-right-side (&rest re-list)
    "Add windows to `display-buffer-alist'.
Windows based on RE-LIST regexs position to right-side."
  (dolist (re re-list)
    (add-to-list 'display-buffer-alist
                 (cons re '((display-buffer-in-side-window)
                            (side . right)
                            (slot . 0)
                            (window-width . 0.45)
                            (inhibit-same-window . t)
                            (window-parameters
                             (no-other-windows . t)))))))

(defun display-top-side (&rest re-list)
    "Add windows to `display-buffer-alist'.
Windows based on RE-LIST regexs position to top-side."
  (dolist (re re-list)
    (add-to-list 'display-buffer-alist
                 (cons re '((display-buffer-in-side-window)
                            (side . top)
                            (slot . 0)
                            (window-height . 0.20)
                            (window-parameters
                             (no-other-window . t)))))))

(defun display-bottom-side (&rest re-list)
    "Add windows to `display-buffer-alist'.
Windows based on RE-LIST regexs position to bottom-side."
  (dolist (re re-list)
    (add-to-list 'display-buffer-alist
                 (cons re '((display-buffer-in-side-window)
                            (side . bottom)
                            (slot . 0)
                            (window-height . 0.20)
                            (window-parameters
                             (no-other-window . t)))))))


;;; Assign windows to specific position

(display-right-side "\\*info.*"
                    "\\*Help.*"
                    "\\*Custom.*"
                    "\\*Python\\*"
                    "\\*Racket REPL.*")

(display-top-side "\\*Flycheck.*")

(display-bottom-side "\\*Messages.*"
                     "^\\(\\*e?shell\\|vterm\\).*"
                     "\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
                     "\\*\\(Output\\|Register Preview\\).*"
                     ".*\\*\\(Completions\\|Embark Live Occur\\).*"
                     "\\*Embark Occur.*")


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
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

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


(provide 'jh-windows)
;;; jh-windows.el ends here

;;; init.el --- start setup emacs      -*- lexical-binding: t; -*-

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

;; Config file for EMACS.

;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(require 'jh-core (expand-file-name "jh-core/jh-core.el" user-emacs-directory))

(let ((jh-info-dir (expand-file-name "docs/dir" user-emacs-directory)))
  (when (file-exists-p jh-info-dir)
    (require 'info)
    (info-initialize)
    (cl-pushnew (file-name-directory jh-info-dir) Info-directory-list)))

(provide 'init)
;;; init.el ends here

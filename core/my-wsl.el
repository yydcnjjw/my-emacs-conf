;;; my-wsl.el --- wsl -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: ()


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'my-const)

(when my/wsl-p
  (defun my/browse-url-generic (url &optional _new-window)
    "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
    (interactive (browse-url-interactive-arg "URL: "))
    (if (not browse-url-generic-program)
        (error "No browser defined (`browse-url-generic-program')"))
    (apply 'call-process browse-url-generic-program nil
	       0 nil
	       (append browse-url-generic-args
                   (list (format "start %s"
                                 (replace-regexp-in-string "&" "^&" url))))))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c")
   browse-url-browser-function #'my/browse-url-generic))

(provide 'my-wsl)

;;; my-wsl.el ends here

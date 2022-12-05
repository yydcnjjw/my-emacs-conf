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
  (defun my/browse-url-wsl (url &optional _new-window)
    "browse url wsl"
    (interactive (browse-url-interactive-arg "URL: "))
    (if (not browse-url-wsl-program)
        (error "No browser defined (`browse-url-wsl-program')"))
    (apply #'start-process (concat "wsl-browse " url) nil
           browse-url-wsl-program
	       (append browse-url-wsl-args
                   (list (format "start %s" url)))))
  (setq
   browse-url-wsl-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-wsl-args     '("/c")
   browse-url-handlers '(("\\`https?:" . my/browse-url-wsl)))

  (defun my/fetch-wsl-host-address ()
    "WSL proxy address."
    (use-package f)
    (let ((text (f-read-text "/etc/resolv.conf")))
      (string-match "^nameserver[ ]+\\(.*\\)$" text)
      (match-string 1 text)))

  (defconst my/wsl-host-address (my/fetch-wsl-host-address)))

(provide 'my-wsl)

;;; my-wsl.el ends here

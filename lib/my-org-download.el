;;; my-org-download.el --- org-download -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

(require 'my-platform)

(require 'org-download)

(defun my/setup-org-download-screenshot-method ()
  "Org download screenshot method."
  (when my/wsl-p
    (let ((powershell-path "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))
      ;; NOTE: use `setq' instead of `setopt' for suppress warning
      (setq org-download-screenshot-method
            (format "%s -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %%s)')\"" powershell-path)))
    ))

(provide 'my-org-download)

;;; my-org-download.el ends here

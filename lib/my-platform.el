;;; my-platform.el --- paltform -*- lexical-binding: t -*-

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

(defconst my/emacs-28+ (> emacs-major-version 27))

(defconst my/emacs-29+ (> emacs-major-version 28))

;; TODO: obsolete
(defconst my/wsl-p (string-match-p "WSL" operating-system-release))

(defconst my/linux-p (eq system-type 'gnu/linux))

(defconst my/windows-p (eq system-type 'windows-nt))

(defun my/browse-url-wsl-open (url &rest _args)
  "Pass the specified URL to the \"wsl-open\" command.
wsl-open is a desktop utility that calls your preferred web browser.
The optional argument ARGS is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "wsl-open" nil 0 nil url))

(provide 'my-platform)

;;; my-platform.el ends here

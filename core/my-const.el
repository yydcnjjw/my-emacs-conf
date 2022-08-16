;;; my-const.el --- my-const -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: ()


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(defconst my/emacs-28+ (> emacs-major-version 27))

(defconst my/emacs-29+ (> emacs-major-version 28))

(defconst my/wsl-p (string-match-p "WSL" operating-system-release))

(defconst my/linux-p (eq system-type 'gnu/linux))

(defconst my/windows-p (eq system-type 'windows-nt))

(provide 'my-const)

;;; my-const.el ends here

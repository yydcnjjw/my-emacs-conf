;;; my-corfu.el --- corfu -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (corfu)


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

(use-package popon
  :straight (popon
             :type git
             :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(use-package corfu-doc
  :ensure t
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-separator ?\s)
  :init
  (global-corfu-mode)
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)
    (corfu-doc-terminal-mode +1)))

(provide 'my-corfu)

;;; my-corfu.el ends here

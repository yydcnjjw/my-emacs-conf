;;; my-pyim.el --- pyim -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (pyim)


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

(use-package popup)

(use-package liberime
  :defer t
  :custom
  (liberime-auto-build t))

(use-package pyim
  :defer t
  :init
  (setq default-input-method "pyim")
  :config
  (if (and (boundp 'posframe-workable-p) (posframe-workable-p))
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))
  (require 'pyim-liberime)
  (liberime-try-select-schema "luna_pinyin_simp")
  (pyim-default-scheme 'rime-quanpin))

(provide 'my-pyim)

;;; my-pyim.el ends here



(provide 'my-pyim)

;;; my-pyim.el ends here

;;; my-spell.el --- spell -*- lexical-binding: t -*-

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

(use-package flyspell-correct
  :defer t
  :ensure-system-package
  (hunspell)
  :bind
  (("C-c ;" . flyspell-correct-wrapper))
  :hook
  ((text-mode . flyspell-mode)
   ((c-mode
     c++-mode
     rust-mode
     typescript-mode
     js-mode)
    . flyspell-prog-mode)))

(provide 'my-spell)

;;; my-spell.el ends here

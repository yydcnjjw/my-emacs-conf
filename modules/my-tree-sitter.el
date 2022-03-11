;;; my-tree-sitter.el --- tree-sitter -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (tree-sitter tree-sitter-langs)


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

(use-package tree-sitter
  :defer t
  :hook
  ((rust-mode
    rustic-mode
    python-mode
    typescript-mode
    javascript-mode
    js2-mode
    c-mode
    c++-mode
    sh-mode
    css-mode
    html-mode
    mhtml-mode
    go-mode)
   . tree-sitter-mode))

(use-package tree-sitter-langs
  :defer t)

(provide 'my-tree-sitter)

;;; my-tree-sitter.el ends here

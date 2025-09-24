;;; my-thing-edit.el --- thing-edit -*- lexical-binding: t -*-

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

(require 'transient)
(require 'thing-edit)

(transient-define-prefix my/thing-edit-main-menu ()
  "Main Menu."
  ["Copy"
   [("w" "Word" thing-copy-word)
    ("s" "Symbol" thing-copy-symbol)
    ("m" "Email" thing-copy-email)
    ("f" "Filename" thing-copy-filename)
    ("u" "URL" thing-copy-url)
    ("x" "Sexp" thing-copy-sexp)
    ("g" "Page" thing-copy-page)
    ("t" "Sentence" thing-copy-sentence)]
   [("o" "White-space" thing-copy-whitespace)
    ("i" "List" thing-copy-list)
    ("c" "Comment" thing-copy-comment)
    ("h" "Function" thing-copy-defun)
    ("p" "Parentheses" thing-copy-parentheses)
    ("l" "Line" thing-copy-line)
    ("a" "To line begin" thing-copy-to-line-beginning)
    ("e" "To line end" thing-copy-to-line-end)]]
  ["Cut"
   [("W" "Word" thing-cut-word)
    ("S" "Symbol" thing-cut-symbol)
    ("M" "Email" thing-cut-email)
    ("F" "Filename" thing-cut-filename)
    ("U" "URL" thing-cut-url)
    ("X" "Sexp" thing-cut-sexp)
    ("G" "Page" thing-cut-page)
    ("T" "Sentence" thing-cut-sentence)]
   [("O" "White-space" thing-cut-whitespace)
    ("I" "List" thing-cut-list)
    ("C" "Comment" thing-cut-comment)
    ("H" "Function" thing-cut-defun)
    ("P" "Parentheses" thing-cut-parentheses)
    ("L" "Line" thing-cut-line)
    ("A" "To line begin" thing-cut-to-line-beginning)
    ("E" "To line end" thing-cut-to-line-end)]]
  (interactive)
  (transient-setup 'my/thing-edit-main-menu))

(provide 'my-thing-edit)

;;; my-thing-edit.el ends here

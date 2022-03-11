;;; my-grammatical-edit.el --- grammatical-edit -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (grammatical-edit)


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

(use-package grammatical-edit
  :straight (grammatical-edit
             :host github
             :repo "manateelazycat/grammatical-edit")
  :defer t
  :hook
  ((c-mode-common
    c-mode
    c++-mode
    emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    sh-mode
    makefile-gmake-mode
    python-mode
    js-mode
    go-mode
    css-mode
    rust-mode
    minibuffer-inactive-mode
    typescript-mode)
   . grammatical-edit-mode)
  :bind (
         :map grammatical-edit-mode-map
         ("(" . grammatical-edit-open-round)
         ("[" . grammatical-edit-open-bracket)
         ("{" . grammatical-edit-open-curly)
         (")" . grammatical-edit-close-round)
         ("]" . grammatical-edit-close-bracket)
         ("}" . grammatical-edit-close-curly)
         ("=" . grammatical-edit-equal)

         ("%" . grammatical-edit-match-paren)
         ("\"" . grammatical-edit-double-quote)
         ("'" . grammatical-edit-single-quote)

         ("SPC" . grammatical-edit-space)
         ("RET" . grammatical-edit-newline)

         ("M-o" . grammatical-edit-backward-delete)
         ("C-d" . grammatical-edit-forward-delete)
         ("C-k" . grammatical-edit-kill)

         ("M-\"" . grammatical-edit-wrap-double-quote)
         ("M-'" . grammatical-edit-wrap-single-quote)
         ("M-[" . grammatical-edit-wrap-bracket)
         ("M-{" . grammatical-edit-wrap-curly)
         ("M-(" . grammatical-edit-wrap-round)
         ("M-)" . grammatical-edit-unwrap)

         ("M-p" . grammatical-edit-jump-right)
         ("M-n" . grammatical-edit-jump-left)
         ("M-:" . grammatical-edit-jump-out-pair-and-newline))
  )

(provide 'my-grammatical-edit)

;;; my-grammatical-edit.el ends here

;;; my-fingertip.el --- summary -*- lexical-binding: t -*-

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

(require 'fingertip)
(require 'my-treesit)

(defun my/setup-fingertip-default-bindings ()
  "Fingertip key bind."
  (bind-keys :map fingertip-mode-map
             ("(" . fingertip-open-round)
             ("[" . fingertip-open-bracket)
             ("{" . fingertip-open-curly)
             (")" . fingertip-close-round)
             ("]" . fingertip-close-bracket)
             ("}" . fingertip-close-curly)
             ("=" . fingertip-equal)
             ("（" . fingertip-open-chinese-round)
             ("「" . fingertip-open-chinese-bracket)
             ("【" . fingertip-open-chinese-curly)
             ("）" . fingertip-close-chinese-round)
             ("」" . fingertip-close-chinese-bracket)
             ("】" . fingertip-close-chinese-curly)
             ("%" . fingertip-match-paren)
             ("\"" . fingertip-double-quote)
             ("'" . fingertip-single-quote)
             ("SPC" . fingertip-space)
             ("RET" . fingertip-newline)
             ("M-o" . fingertip-backward-delete)
             ("C-d" . fingertip-forward-delete)
             ("C-k" . fingertip-kill)
             ("M-\"" . fingertip-wrap-double-quote)
             ("M-'" . fingertip-wrap-single-quote)
             ("M-[" . fingertip-wrap-bracket)
             ("M-{" . fingertip-wrap-curly)
             ("M-(" . fingertip-wrap-round)
             ("M-)" . fingertip-unwrap)
             ("M-p" . fingertip-jump-right)
             ("M-n" . fingertip-jump-left)
             ("M-:" . fingertip-jump-out-pair-and-newline)
             ("C-j" . fingertip-jump-up))
  )

(defun my/fingertip-mode ()
  "Fingertip mode."
  (when (my/treesitp major-mode)
    (fingertip-mode)))

(defun my/fingertip-setup-prog-mode ()
  "Fingertip setup used prog mode."
  (dolist (hook (list
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'sh-mode-hook
                 'markdown-mode-hook
                 ))
    (add-hook hook #'(lambda () (fingertip-mode 1)))))

(provide 'my-fingertip)

;;; my-fingertip.el ends here

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

;; (use-package tree-sitter
;;   :defer t
;;   :hook
;;   ((rust-mode
;;     rustic-mode
;;     python-mode
;;     typescript-mode
;;     javascript-mode
;;     json-mode
;;     js2-mode
;;     c-mode
;;     c++-mode
;;     sh-mode
;;     css-mode
;;     html-mode
;;     mhtml-mode
;;     go-mode
;;     emacs-lisp-mode
;;     lisp-interaction-mode
;;     lisp-mode
;;     )
;;    . tree-sitter-mode)
;;   :config
;;   (dolist (item '((emacs-lisp-mode . elisp)
;;                   (lisp-mode . elisp)
;;                   (lisp-interaction-mode . elisp)))
;;     (add-to-list
;;      'tree-sitter-major-mode-language-alist
;;      item)
;;     )
;;   )

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   ;; activate tree-sitter on any buffer containing code for which it has a parser available
;;   (global-tree-sitter-mode)
;;   ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
;;   ;; by switching on and off
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :defer t
;;   :after tree-sitter)

(provide 'my-tree-sitter)

;;; my-tree-sitter.el ends here

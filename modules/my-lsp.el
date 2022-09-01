;;; my-lsp.el --- lsp -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (lsp-mode lsp-ui dap-mode)


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

(use-package emacs
  :custom
  (read-process-output-max (* 1024 1024)))

(use-package lsp-mode
  :defer t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-auto-execute-action nil)
  (lsp-keep-workspace-alive nil)
  :init
  (defun my/lsp-mode ()
    (if (featurep 'orderless)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless)))
    (lsp-enable-which-key-integration))
  :hook
  ((lsp-completion-mode . my/lsp-mode))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol)

;; for debugger
;; (use-package dap-mode
;;   :defer t)

(provide 'my-lsp)

;;; my-lsp.el ends here

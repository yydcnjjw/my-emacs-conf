;;; init-rust.el --- rust -*- lexical-binding: t -*-

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

(use-package emacs
  :mode (("\\.rs\\'" . rust-ts-mode))
  :functions my/lsp-register-major-mode
  :commands
  (my/lsp-rust-analyzer-set-target)
  :init
  (my/lsp-register-major-mode 'rust-ts-mode)
  :config
  (require 'my-rust)
  (setopt rust-indent-offset 4
          lsp-rust-analyzer-diagnostics-enable-experimental t
          lsp-rust-analyzer-experimental-proc-attr-macros t
          lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro" "unresolved-macro-call"])
  ;; (with-eval-after-load 'lsp
  ;;   (let ((client (gethash 'rust-analyzer lsp-clients)))
  ;;     (setf (lsp--client-major-modes client) '(rust-mode rust-ts-mode)))
  ;;   (add-to-list 'lsp-language-id-configuration '(rust-ts-mode . "rust")))
  )

(use-package emacs
  :after treesit
  :functions my/treesit-register
  :init
  (my/treesit-register
   '(:lang rust
           :source ("https://github.com/tree-sitter/tree-sitter-rust")
           :mode (rust-ts-mode))))

(provide 'init-rust)

;;; init-rust.el ends here

;;; my-rust.el --- my-rust -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (rust-mode)


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

(use-package rust-mode
  :init
  (setq rust-indent-offset 4
        lsp-rust-analyzer-diagnostics-enable-experimental t
        lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro" "unresolved-macro-call"])

  (defun my/rust-list-all-installed-target()
    "Rust List all installed target."
    (split-string (shell-command-to-string "rustup target list --installed")))

  (defun my/lsp-rust-analyzer-set-target ()
    ""
    (interactive)
    (let ((target (ivy-read "Target: " (my/rust-list-all-installed-target))))
      (setq lsp-rust-analyzer-cargo-target target)
      (lsp-workspace-restart (lsp--read-workspace)))
    )
  (defun my/lsp ()
    (when (derived-mode-p 'rust-mode) (lsp)))

  :config
  (with-eval-after-load 'lsp
    (let ((client (gethash 'rust-analyzer lsp-clients)))
      (setf (lsp--client-major-modes client) '(rust-mode rust-ts-mode)))
    (add-to-list 'lsp-language-id-configuration '(rust-ts-mode . "rust")))
  :hook
  ((hack-local-variables . my/lsp))
  )

(provide 'my-rust)

;;; my-rust.el ends here

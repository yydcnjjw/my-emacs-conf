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

(use-package rustic
  :defer t
  :init
  (setq lsp-rust-analyzer-proc-macro-enable t))

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

(provide 'my-rust)

;;; my-rust.el ends here
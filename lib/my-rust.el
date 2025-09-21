;;; my-rust.el --- rust -*- lexical-binding: t -*-

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

(defun my/rust-list-all-installed-target()
  "Rust List all installed target."
  (split-string (shell-command-to-string "rustup target list --installed")))

(require 'lsp-mode)

(defun my/lsp-rust-analyzer-set-target ()
  "Lsp rust analyzer set target."
  (interactive)
  (let ((target (completing-read "Target: " (my/rust-list-all-installed-target))))
    (setopt lsp-rust-analyzer-cargo-target target)
    (lsp-workspace-restart (lsp--read-workspace))))

(provide 'my-rust)

;;; my-rust.el ends here

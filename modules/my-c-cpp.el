;;; my-c-cpp.el --- my-c-cpp -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (dependencies)

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

(require 'my-lib)

(defun my/c-cpp ()
  "C/C++."
  ;; (require 'ccls)
  (setq-local lsp-lens-enable nil)
  (lsp)
  )

;; (use-package ccls
;;   :defer t
;;   :hook
;;   ((c-mode c++-mode objc-mode cuda-mode) . my/c-cpp))

(add-hooks-pair '(c-mode c++-mode objc-mode) #'my/c-cpp)

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(use-package cmake-mode
  :defer t
  :hook
  (cmake-mode . lsp))

(provide 'my-c-cpp)

;;; my-c-cpp.el ends here

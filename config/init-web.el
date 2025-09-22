;;; init-web.el --- web -*- lexical-binding: t -*-

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

(defun my/web-mode ()
  "WEB mode."
  (lsp))

(use-package web-mode
  :defer t
  :mode "\\.html?\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  :hook
  (web-mode . my/web-mode))

(use-package css-mode
  :defer t
  :hook
  (css-mode . lsp))

(use-package web-beautify
  :defer t
  :init
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
  )

(defun my/js-mode ()
  "JS mode."
  (lsp))

(use-package js2-mode
  :defer t
  :custom
  (js-indent-level 2)
  :hook
  ((js-mode js2-mode) . my/js-mode)
  )

(defun my/typescript-mode ()
  "Typescript."
  (lsp))

(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 4)
  (typescript-ts-mode-indent-offset 4)
  :hook
  ((typesciprt-mode tsx-ts-mode) . my/typescript-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
  )

(use-package emmet-mode
  :defer t
  :hook
  ((web-mode css-mode typescript-mode) . emmet-mode))

(provide 'init-web)

;;; init-web.el ends here

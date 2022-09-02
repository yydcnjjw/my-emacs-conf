;;; my-orderless.el --- orderless -*- lexical-binding: t -*-

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

(use-package orderless
  :config
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-literal))
  :init
  ;; (defun basic-remote-try-completion (string table pred point)
  ;;   (and (vertico--remote-p string)
  ;;        (completion-basic-try-completion string table pred point)))
  ;; (defun basic-remote-all-completions (string table pred point)
  ;;   (and (vertico--remote-p string)
  ;;        (completion-basic-all-completions string table pred point)))
  ;; (add-to-list
  ;;  'completion-styles-alist
  ;;  '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  ;; (setq completion-category-defaults nil
  ;;       completion-category-overrides '((file (styles basic-remote partial-completion))))
  )

(provide 'my-orderless)

;;; my-orderless.el ends here

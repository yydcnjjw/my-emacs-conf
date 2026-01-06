;;; init-syntax-check.el --- syntax-check -*- lexical-binding: t -*-

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

(use-package flycheck
  :init
  ;; NOTE: Avoid loading `package'
  (setq flycheck-emacs-lisp-initialize-packages nil)
  :config
  (setopt flycheck-emacs-lisp-load-path 'inherit)
  :hook
  ((emacs-lisp-mode . flycheck-mode)))

(use-package consult-flycheck
  :bind
  (("M-g f" . consult-flycheck)))

(use-package wucuo
  :config
  (setopt ispell-extra-args '("--lang=en_US"))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . wucuo-start)))

(use-package flyspell-correct
  :bind
  (("C-c s" . flyspell-correct-wrapper)))

(provide 'init-syntax-check)

;;; init-syntax-check.el ends here

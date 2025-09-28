;;; my-treesit.el --- treesit -*- lexical-binding: t -*-

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

(require 'treesit)

(defvar my/treesit-mode-table (make-hash-table :test 'eq))
(defun my/treesit-register (&rest recipes)
  "Register treesit RECIPES."
  (dolist (recipe recipes)
    (let ((lang (plist-get recipe :lang))
          (source (plist-get recipe :source))
          (modes (plist-get recipe :mode)))
      (add-to-list 'treesit-language-source-alist `(,lang . ,source))
      (dolist (mode modes)
        (puthash mode `(,lang . ,source) my/treesit-mode-table)))))

(defun my/treesit-setup ()
  "Setup treesit."
  (advice-add 'set-auto-mode-0 :before
              #'(lambda (mode &rest _)
                  (let ((lang (car (gethash mode my/treesit-mode-table))))
                    (when lang
                      (unless (treesit-language-available-p lang)
                        (treesit-install-language-grammar lang)))))))

(provide 'my-treesit)

;;; my-treesit.el ends here

;;; my-python.el --- my-python -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (ob-ipython)


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

(use-package ob-ipython
  :defer t
  :ensure-system-package
  ((ipython)
   (jupyter))
  :after org
  :config
  (my/push-load-org-babel-language 'python)
  (my/push-load-org-babel-language 'ipython))

(use-package lsp-mode
  :defer t
  :custom
  (lsp-pylsp-plugins-autopep8-enabled t)
  :init
  (defun my/python ()
    (lsp)
    )
  :hook
  (python-mode . my/python))

(provide 'my-python)

;;; my-python.el ends here

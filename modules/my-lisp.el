;;; my-lisp.el --- my-lisp -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (flycheck)


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

(use-package slime
  :commands slime
  :config
  (use-package slime-company
    :custom
    (slime-company-completon 'fuzzy)
    (slime-company-display-arglist t)
    :hook
    ((slime-repl-mode . company-mode)))
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy
                         slime-company
                         slime-asdf
                         ))
  (slime-setup))

(provide 'my-lisp)

;;; my-lisp.el ends here

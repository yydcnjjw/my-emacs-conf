;;; my-hl-todo.el --- hl-todo -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (hl-todo)


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

(use-package hl-todo
  :defer t
  :hook
  ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#28ABE3")
          ("FIXME" . "#DB3340")
          ("BUG"   . "#DB3340")
          )))

(provide 'my-hl-todo)

;;; my-hl-todo.el ends here

;;; my-org.el --- org -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: ()


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

(use-package org
  :defer t
  :ensure-system-package
  ((xelatex)
   (latexmk))
  :init
  (progn
    (setq
     ;; org ui
     org-startup-indented t
     ;; org todo
     org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
                         (sequence "FIXME(f)" "|" "ABORT(a@/!)"))
     ;; latex pdf export
     org-latex-compiler "xelatex"
     org-latex-pdf-process
     '("latexmk -g -pdf -pdflatex=\"%latex -shell-escape\" -outdir=%o %f")
     ;; 
     ))
  )

(provide 'my-org)

;;; my-org.el ends here

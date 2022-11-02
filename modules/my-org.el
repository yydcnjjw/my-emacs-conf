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
  ((xelatex . texlive-most)
   (latexmk . texlive-most))
  :custom
  ;; ui
  (org-startup-folded 'showall)
  ;; (org-startup-with-inline-images t)
  ;; (org-startup-with-latex-preview t)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ...")

  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
                       (sequence "FIXME(f)" "|" "ABORT(a@/!)")))

  ;; latex preview
  (org-latex-preview-ltxpng-directory ".cache/ltximg/")
  (org-preview-latex-process-alist
   '((dvisvgm
      :programs
      ("xelatex" "dvisvgm")
      :description "xdv > svg"
      :message "you need to install the programs: xelatex and dvisvgm."
      :use-xcolor t
      :image-input-type "xdv"
      :image-output-type "svg"
      :image-size-adjust
      (1.7 . 1.5)
      :latex-compiler
      ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick
      :programs
      ("xelatex" "convert")
      :description "pdf > svg"
      :message "you need to install the programs: xelatex and imagemagick."
      :use-xcolor t
      :image-input-type "pdf"
      :imagne-output-type "svg"
      :image-size-adjust
      (1.0 . 1.0)
      :latex-compiler
      ("xelatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("convert -density %D -trim -antialias %f -quality 100 %O"))))
  (org-format-latex-options
   '(:scale 1.0
            :html-scale 1.0
            :foreground default
            :background "Transparent"
            :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   '("latexmk -g -pdf -pdflatex=\"%latex -shell-escape\" -outdir=%o %f"))

  :init
  (defun my/org-mode ()
    ""
    (setq-local truncate-lines nil))

  (defun my/org-ruby-export (link description format)
    "Export ruby link from org files."
    (let ((desc (or description link)))
      (pcase format
        (`html (format "<ruby> %s <rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" desc link))
        (`latex (format "\\ruby{%s}{%s}" desc link))
        (_ link))))

  (defun my/org-ruby-follow (path)
    path)

  (defun my/push-load-org-babel-language (language)
    (push (cons language t) org-babel-load-languages)
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    )

  (defun my/insert-zero-width-space ()
    (interactive)
    (insert-char #x200b))

  :hook (org-mode . my/org-mode)
  :bind (:map org-mode-map
              ("C-x 8 0" . my/insert-zero-width-space))
  :config
  (defface org-ruby-face
    `((t (:inherit underline)))
    "org ruby face")

  (org-link-set-parameters "ruby"
                           :follow #'my/org-ruby-follow
                           :export #'my/org-ruby-export
                           :face 'org-ruby-face
                           )
  ;; ditaa
  (my/push-load-org-babel-language 'ditaa)
  ;; dot
  (my/push-load-org-babel-language 'dot))

(use-package org-contrib
  :defer t
  :after org)

;; for ui
(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

;; for editor
(use-package org-appear
  :defer t
  :custom
  (org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package valign
  :defer t
  :init
  (defun my/valign-mode ()
    (when (display-graphic-p)
      (valign-mode)))
  :hook (org-mode . my/valign-mode))

;; for export
(use-package htmlize
  :defer t)

;; for cite
(use-package citar
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  :custom
  (citar-at-point-function 'embark-act))

(use-package citeproc
  :defer t)


(provide 'my-org)

;;; my-org.el ends here

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
  (org-latex-preview-ltxpng-directory ".cache/ltximg/")
  :config
  (defun my/org-latex-export/preview ()
    ""
    (setq org-preview-latex-process-alist
          '((dvisvgm :programs
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
            (imagemagick :programs
                         ("xelatex" "convert")
                         :description "pdf > png"
                         :message "you need to install the programs: xelatex and imagemagick."
                         :use-xcolor t
                         :image-input-type "pdf"
                         :imagne-output-type "png"
                         :image-size-adjust
                         (1.0 . 1.0)
                         :latex-compiler
                         ("xelatex -interaction nonstopmode -output-directory %o %f")
                         :image-converter
                         ("convert -density %D -trim -antialias %f -quality 100 %O"))))
    (setq org-format-latex-options
          '(
            :scale 1.4
            :html-scale 1.0
            :foreground default
            :background "Transparent"
            :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
          org-preview-latex-default-process 'dvisvgm
          org-latex-compiler "xelatex"
          org-latex-pdf-process
          '("latexmk -g -pdf -pdflatex=\"%latex -shell-escape\" -outdir=%o %f"))
    )

  (defun my/org-ruby-export (link description format)
    "Export ruby link from org files."
    (let ((desc (or description link)))
      (pcase format
        (`html (format "<ruby> %s <rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" desc link))
        (`latex (format "\\ruby{%s}{%s}" desc link))
        (_ link))))

  (defun my/org-ruby-follow (path)
    path)
  
  (defface org-ruby-face
    `((t (:inherit underline)))
    "org ruby face")

  (org-link-set-parameters "ruby"
			               :follow #'my/org-ruby-follow
			               :export #'my/org-ruby-export
			               :face 'org-ruby-face
			               )

  (defun my/push-load-org-babel-language (language)
    (push (cons language t) org-babel-load-languages)
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    )
  
  (setq
   ;; org ui
   org-startup-indented t
   ;; org todo
   org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
                       (sequence "FIXME(f)" "|" "ABORT(a@/!)"))
   ;; 
   )
  (set-face-attribute 'org-table nil
                      :family "Noto Sans Mono CJK SC")
  (my/org-latex-export/preview)

  ;; ditaa
  (my/push-load-org-babel-language 'ditaa)
  )

(use-package org-contrib
  :defer t
  :after org)

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))
                                                             
(provide 'my-org)

;;; my-org.el ends here

;;; my-org.el --- org -*- lexical-binding: t -*-

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

(require 'org)

(defface org-ruby-face
  `((t (:inherit underline)))
  "Org ruby face.")

(defun my/org-ruby-export (link description format)
  "Export ruby LINK with DESCRIPTION and FORMAT from org files."
  (let ((desc (or description link)))
    (pcase format
      (`html (format "<ruby> %s <rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" desc link))
      (`latex (format "\\ruby{%s}{%s}" desc link))
      (_ link))))

(defun my/org-ruby-follow (path)
  "Org ruby follow PATH."
  path)

(defun my/setup-ruby-org-link ()
  "Setup ruby org link."
  (org-link-set-parameters "ruby"
                           :follow #'my/org-ruby-follow
                           :export #'my/org-ruby-export
                           :face 'org-ruby-face))

(defun my/setup-org-latex-preview ()
  "Setup org latex preview."
  (setopt
   org-preview-latex-image-directory ".cache/image/"
   org-preview-latex-process-alist
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
      ("convert -density %D -trim -antialias %f -quality 100 %O")))
   org-format-latex-options
   '(:scale 1.0
            :html-scale 1.0
            :foreground default
            :background "Transparent"
            :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-preview-latex-default-process 'dvisvgm
   org-latex-compiler "xelatex"
   org-latex-pdf-process
   '("latexmk -g -pdf -pdflatex=\"%latex -shell-escape\" -outdir=%o %f"))
  )

(defun my/push-load-org-babel-language (language)
  "Add LANGUAGE to `org-babel-load-languages'."
  (push (cons language t) org-babel-load-languages)
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

(defun my/insert-zero-width-space ()
  "Insert zero width space."
  (interactive)
  (insert-char #x200b))

(provide 'my-org)

;;; my-org.el ends here

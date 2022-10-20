;;; my-anki.el --- my-anki -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (anki-editor)


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

(use-package anki-editor
  :straight (:host github :repo "orgtre/anki-editor")
  :commands anki-editor-mode
  :custom
  (anki-editor-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://rgb-24bit.github.io/org-html-theme-list/org/style/main.css\"/>")
  (anki-editor-org-tags-as-anki-tags nil)
  :init
  (defun my/anki-editor--build-fields ()
  "Build a list of fields from subheadings of current heading.

Return a list of cons of (FIELD-NAME . FIELD-CONTENT)."  
  (save-excursion
    (cl-loop with inhibit-message = t ; suppress echo message from `org-babel-exp-src-block'
             initially (unless (org-goto-first-child)
                         (cl-return))
             for last-pt = (point)
             for element = (org-element-at-point)
             for heading = (substring-no-properties
                            (org-element-property :raw-value element))
             for format = (anki-editor-entry-format)
             ;; contents-begin includes drawers and scheduling data,
             ;; which we'd like to ignore, here we skip these
             ;; elements and reset contents-begin.
             for begin = (if (org-element-property :contents-begin element)
                             (cl-loop for eoh = (org-element-property :contents-begin element)
                                  then (org-element-property :end subelem)
                                  for subelem = (progn
                                                  (goto-char eoh)
                                                  (org-element-context))
                                  while (memq (org-element-type subelem)
                                              '(drawer planning property-drawer))
                                  finally return (org-element-property :begin subelem))
                           nil)
             for end = (org-element-property :contents-end element)
             for raw = (or (and begin
                                end
                                (buffer-substring-no-properties
                                 begin
                                 ;; in case the buffer is narrowed,
                                 ;; e.g. by `org-map-entries' when
                                 ;; scope is `tree'
                                 (min (point-max) end)))
                           "")
             for content = (anki-editor--export-string raw format)
             collect (cons heading content)
             ;; proceed to next field entry and check last-pt to
             ;; see if it's already the last entry
             do (org-forward-heading-same-level nil t)
             until (= last-pt (point)))))
  (advice-add #'anki-editor--build-fields :override #'my/anki-editor--build-fields))

(provide 'my-anki)

;;; my-anki.el ends here

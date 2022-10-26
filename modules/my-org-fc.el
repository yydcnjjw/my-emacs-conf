;;; my-org-fc.el --- org-fc -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (org-fc)


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

(use-package org-fc
  :defer t
  :straight
  (org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org"))
  :custom
  (org-fc-directories (list my/im-dir))
  :bind
  ("C-c f d" . org-fc-dashboard)
  :config
  (require 'org-fc-keymap-hint)
  (require 'org-fc-type-vocab)
  )

(provide 'my-org-fc)

;;; my-org-fc.el ends here

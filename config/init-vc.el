;;; init-vc.el --- vc -*- lexical-binding: t -*-

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

(require 'my-path)

(use-package magit
  :defer t
  :config
  (require 'my-magit))

(use-package forge
  :after magit
  :init
  (setopt forge-database-file
          (expand-file-name "forge-database.sqlite" my/emacs-cache-dir)))

(use-package emacs
  :after org-capture
  :defines org-capture-templates
  :init
  (add-to-list 'org-capture-templates
               '("f" "Forge"
                 entry (file my/agenda-inbox-file)
                 (function my/gtd-capture-forge-topic-template)
                 :kill-buffer t)))

(use-package vc-msg
  :defer t)

(provide 'init-vc)

;;; init-vc.el ends here

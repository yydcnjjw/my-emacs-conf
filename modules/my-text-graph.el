;;; my-text-graph.el --- text graph -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: 
;; Package-Requires: (d2 plantuml)
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

(use-package d2-mode
  :defer t
  :ensure-system-package d2
  ;; :config
  ;; (my/push-load-org-babel-language 'd2)
  )

(use-package plantuml-mode
  :custom
  ((plantuml-default-exec-mode 'executable))
  :defer t
  :after org
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (my/push-load-org-babel-language 'plantuml))

(provide 'my-text-graph)

;;; my-text-graph.el ends here

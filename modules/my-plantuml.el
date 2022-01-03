;;; my-plantuml.el --- plantuml -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (plantuml-mode)


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

(use-package plantuml-mode
  :custom
  ((plantuml-default-exec-mode 'executable))
  :defer t
  :after org
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (my/push-load-org-babel-language 'plantuml)
  )

(provide 'my-plantuml)

;;; my-plantuml.el ends here

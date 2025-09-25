;;; init-org.el --- org -*- lexical-binding: t -*-

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

(require 'my-loading)

(use-package org
  :defer t
  :straight (:type built-in)
  :ensure-system-package
  ((xelatex . texlive)
   (latexmk . texlive))
  :bind (:map org-mode-map
              ("C-x 8 0" . my/insert-zero-width-space))
  :config
  (require 'my-org)
  (my/setup-org-latex-preview)
  (my/setup-ruby-org-link)

  ;; `org-babel'
  ;; ditaa
  ;; (my/push-load-org-babel-language 'ditaa)
  ;; dot
  ;; (my/push-load-org-babel-language 'dot)
  )

(use-package org-contrib
  :defer t
  :after org
  :straight (:repo "https://github.com/emacsmirror/org-contrib"))

;; for export
(use-package htmlize
  :defer t)

;; image
(use-package org-download
  :after org
  :functions my/setup-org-download-screenshot-method
  :config
  (require 'my-org-download)
  (my/setup-org-download-screenshot-method))

;; TODO for cite
;; (use-package citar
;;   :no-require
;;   :custom
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   (citar-bibliography org-cite-global-bibliography))

;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode)
;;   :custom
;;   (citar-at-point-function 'embark-act))

;; (use-package citeproc
;;   :defer t)

(my/require-modules
 init-org-theme)

(provide 'init-org)

;;; init-org.el ends here

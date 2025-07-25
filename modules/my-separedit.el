;;; my-separedit.el --- separedit -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (separedit)


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

(use-package separedit
  :defer t
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-continue-fill-column t)
  (separedit-remove-trailing-spaces-in-comment t)
  :bind
  ("C-c '" . separedit)
)

(provide 'my-separedit)

;;; my-separedit.el ends here

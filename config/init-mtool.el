;;; init-mtool.el --- mtool -*- lexical-binding: t -*-

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

(require 'my-mtool)

(use-package emacs
  :hook
  (after-init . my/mtool-run))

(use-package emacs
  :after org-clock
  :init
  (advice-add 'org-user-idle-seconds :override 'my/mtool-user-idle))

(use-package emacs
  :after alert
  :functions alert-define-style
  :init
  (alert-define-style 'mtool :title "Mtool"
                      :notifier #'my/mtool-alert)
  (setopt alert-default-style 'mtool))

(provide 'init-mtool)

;;; init-mtool.el ends here

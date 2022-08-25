;;; my-org-wild-notifier.el --- org-wild-notifier -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (org-wild-notifier)

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

(use-package org-wild-notifier
  :if (daemonp)
  :custom
  (org-wild-notifier-alert-time '(10 1))
  (org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
  :config
  (org-wild-notifier-mode))

(provide 'my-org-wild-notifier)

;;; my-org-wild-notifier.el ends here

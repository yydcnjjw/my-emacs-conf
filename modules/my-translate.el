;;; my-translate.el --- translate -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (google-translate)


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

(use-package insert-translated-name
  :quelpa (insert-translated-name
           :fetcher github
           :repo "manateelazycat/insert-translated-name")
  :config
  (setq insert-translated-name-translate-engine "google"))

(use-package go-translate
  :custom
  (go-translate-base-url "https://translate.google.cn")
  (go-translate-local-language "zh-CN")
  (go-translate-token-current (cons 430675 2721866130))
  :defer t
  :bind
  (("C-c t" . go-translate)
   ("C-c T" . go-translate-popup-current)))

(provide 'my-translate)

;;; my-translate.el ends here

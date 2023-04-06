;;; my-rime.el --- rime -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (rime popup posframe)


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

(use-package popup
  :defer t)

(use-package posframe
  :defer t)

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-in-code-string-p
     rime-predicate-space-after-cc-p)))

(provide 'my-rime)

;;; my-rime.el ends here

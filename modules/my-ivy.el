;;; my-ivy.el --- ivy -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (ivy)


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

(use-package ivy
  :ensure counsel
  :init
  (progn
    (ivy-mode 1)
    (counsel-mode 1))
  :config
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d)"))
  :bind
  (([remap isearch-forward] . swiper)))

(provide 'my-ivy)

;;; my-ivy.el ends here

;;; my-projectile.el --- projectile -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (projectile ibuffer-projectile ag)


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

(use-package projectile
  :defer t
  :ensure-system-package
  (rg . ripgrep)
  :config
  (progn
    (setq projectile-indexing-method 'hybrid
          projectile-enable-caching t)
    (projectile-mode))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (([remap projectile-ag] . projectile-ripgrep)))

(use-package ripgrep
  :defer t)

(use-package ibuffer-projectile
  :defer t
  :hook
  (ibuffer . (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(provide 'my-projectile)

;;; my-projectile.el ends here

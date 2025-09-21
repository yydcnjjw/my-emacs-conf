;;; init-project.el --- project -*- lexical-binding: t -*-

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

(require 'my-path)

(use-package ripgrep
  :defer t)

(use-package projectile
  :defer t
  :ensure-system-package
  (rg . ripgrep)
  :commands
  (projectile-mode)
  :init
  (setopt
   projectile-indexing-method 'alien
   projectile-enable-caching t
   projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my/emacs-cache-dir))
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (([remap projectile-ag] . projectile-ripgrep))
  ;; :custom
  ;; (projectile-globally-unignored-directories '("~/.cargo"))
  )

(use-package ibuffer-projectile
  :defer t
  :commands
  (ibuffer-projectile-set-filter-groups)
  :hook
  (ibuffer . (lambda ()
	       (ibuffer-projectile-set-filter-groups)
	       (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-project)

;;; init-project.el ends here

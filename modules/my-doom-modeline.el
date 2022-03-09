;;; my-doom-modeline.el --- doom-modeline -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (doom-modeline)


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

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-minor-modes t))

(provide 'my-doom-modeline)

;;; my-doom-modeline.el ends here

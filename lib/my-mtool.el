;;; my-mtool.el --- mtool -*- lexical-binding: t -*-

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

(defcustom my/mtool-module-load-path nil
  "Mtool module path."
  :group 'my
  :type 'string)

(defcustom my/mtool-module-name 'mtool
  "Mtool module path."
  :group 'my
  :type 'string)

(defcustom my/mtool-config-dir (expand-file-name "~/.mtool")
  "Mtool config directory."
  :type 'string
  :group 'my)

(declare-function mtool-entry-start "mtool")
(declare-function mtool-user-idle "mtool")

(defvar my/mtool-emacs-context nil)

(defun my/mtool-entry-start ()
  "Mtool entry start."
  (condition-case err
      (unless my/mtool-emacs-context
        (setq my/mtool-emacs-context (mtool-entry-start)))
    (rust-panic
     (message "%s" err)
     nil)))

(defun my/mtool-user-idle ()
  "Mtool user idle."
  (mtool-user-idle my/mtool-emacs-context))

(defun my/mtool-run ()
  "Mtool run."
  (unless (featurep 'mtool)
    (when my/mtool-module-load-path
      (add-to-list 'load-path my/mtool-module-load-path)
      (require my/mtool-module-name)
      (my/mtool-entry-start))))

(provide 'my-mtool)

;;; my-mtool.el ends here

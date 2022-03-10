;;; my-ts.el --- ts -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (typescript-mode)

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

(defun my/typescript ()
  "Typescript."
  (lsp))

(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 2)
  :hook
  (typescript-mode . my/typescript)
  :config
  (defun my/yarn-global-dir()
    "Yarn global dir."
    (string-trim (shell-command-to-string "yarn global dir")))
  (setq lsp-clients-angular-language-server-command
        (let ((node-modules-path (my/yarn-global-dir)))
          (list
           "ngserver"
           "--ngProbeLocations"
           node-modules-path
           "--tsProbeLocations"
           node-modules-path
           "--stdio"))
        lsp-eslint-enable nil))

(provide 'my-ts)

;;; my-ts.el ends here

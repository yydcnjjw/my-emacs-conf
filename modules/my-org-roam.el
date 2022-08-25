;;; my-org-roam.el --- org-roam -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (org-roam org-roam-ui)


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

(use-package org-roam
  :straight (org-roam
             :host github
             :repo "org-roam/org-roam")
  :defer t
  :init
  ;; (when my/emacs-29+
  ;;   (use-package emacsql-sqlite-builtin
  ;;     :defer t)
  ;;   (setq org-roam-database-connector 'sqlite-builtin))
  :custom
  ((org-roam-directory my/im-dir)
   (org-roam-db-location (expand-file-name "org-roam.db" my/im-cache-dir))
   (org-roam-db-update-on-save t)
   (org-roam-node-display-template
    (concat "${title:*} "
            (propertize "${tags:40}" 'face 'org-tag))))
  :bind (("C-c C-n /" . org-roam-node-find)
         ("C-c C-n r" . org-roam-db-sync)
         ("C-c C-n c" . org-roam-capture)
         :map org-mode-map
         (("C-c C-n i" . org-roam-node-insert)
          ("C-c C-n t" . org-roam-tag-add)
          ("C-c C-n a" . org-roam-alias-add)
          ("C-c C-n b" . org-roam-buffer-toggle)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight
  (
   :host github
   :repo "org-roam/org-roam-ui"
   :branch "main"
   :files ("*.el" "out"))
  :if (daemonp)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :config
  (org-roam-ui-mode))

(provide 'my-org-roam)

;;; my-org-roam.el ends here

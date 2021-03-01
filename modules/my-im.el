;;; my-im.el --- my individual management -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (org-roam org-roam-server)


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

;; (use-package org-drill
;;   :defer t
;;   :commands org-drill)

;; (use-package org-download
;;   :defer t
;;   :after org
;;   :commands (org-download-clipboard)
;;   :config
;;   (setq org-download-image-dir "./image"
;;         org-download-heading-lvl 1
;;         ))

(defconst my/im-dir "~/workspace/my-im")
(defconst my/im-db-dir (expand-file-name ".db" my/im-dir))

(defun run-server ()
  "Run server."
  (require 'server)
  (unless (server-running-p)
    (server-start))
  )

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  (org-roam-mode . run-server)
  :custom
  ((org-roam-directory my/im-dir)
   (org-roam-db-location (expand-file-name "org-roam.db" my/im-db-dir))
   (org-roam-db-update-method 'idle-timer)
   (org-roam-db-update-idle-seconds 10)
   )
  :config
  (progn
    (require 'org-roam-protocol)
    )
  :bind (:map org-roam-mode-map
              (("C-c C-n l" . org-roam)
               ("C-c C-n f" . org-roam-find-file)
               ("C-c C-n g" . org-roam-graph)
               ("C-c C-n c" . org-roam-capture)
               ("C-c C-n r" . org-roam-db-build-cache)
               )
              :map org-mode-map
              (("C-c C-n i" . org-roam-insert))
              (("C-c C-n I" . org-roam-insert-immediate)
               ("C-c C-n t" . org-roam-tag-add)
               ))
  )

(with-eval-after-load 'org
  (define-key global-map "\C-cc" #'org-capture)
  (define-key global-map "\C-ca" #'org-agenda)
  (defconst my/agenda-file (expand-file-name "agenda.org" my/im-dir))
  (setq org-default-notes-file my/agenda-file
        ;; org-capture-templates nil
        org-capture-templates
        '(("t" "Todo" entry (file+headline my/agenda-file "Todos")
           "** TODO %?\n"))
        org-agenda-files (list my/agenda-file)))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


(provide 'my-im)

;;; my-im.el ends here

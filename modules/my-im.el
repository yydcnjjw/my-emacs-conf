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

(use-package org-roam
  :defer t
  :custom
  ((org-roam-directory my/im-dir)
   (org-roam-db-location (expand-file-name "org-roam.db" my/im-db-dir))
   (org-roam-db-update-method 'idle-timer)
   (org-roam-db-update-idle-seconds 10)
   )
  :bind (("C-c C-n f" . org-roam-find-file)
         :map org-roam-mode-map
              (("C-c C-n l" . org-roam)
               ("C-c C-n f" . org-roam-find-file)
               ("C-c C-n g" . org-roam-graph)
               ("C-c C-n c" . org-roam-capture)
               ("C-c C-n r" . org-roam-db-build-cache))
         :map org-mode-map
              (("C-c C-n i" . org-roam-insert)
               ("C-c C-n I" . org-roam-insert-immediate)
               ("C-c C-n t" . org-roam-tag-add))))

(use-package org-roam-server
  :if (daemonp)
  :config
  (require 'org-roam-protocol)
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
        org-roam-server-network-label-wrap-length 20)
  (org-roam-server-mode))

(bind-keys*
 ("C-c c" . org-capture)
 ("C-c a" . org-agenda)
 )

(use-package org-gtd
  :after org
  :pin melpa-stable
  :demand t
  :custom
  (org-gtd-directory "~/workspace/GTD/")
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (org-edna-use-inheritance t)
  :config
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d a" . org-agenda-list)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   ("C-c d f" . org-gtd-clarify-finalize)))

(use-package org-agenda
  :ensure nil
  :after org-gtd
  :config
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (setq org-agenda-files `(,org-gtd-directory)
        org-tags-exclude-from-inheritance '("repeat"))
  ;; a useful view to see what can be accomplished today
  (setq org-agenda-custom-commands
        '(("g" "Scheduled today and all NEXT/TODO items"
           ((agenda "" ((org-agenda-span 'day)))
            (alltodo "" ((org-agenda-tag-filter-preset '("-repeat"))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))))))

(use-package org-capture
  :ensure nil
  :after org-gtd
  :config
  (use-package org-ql)
  (defun my/project-location-list ()
    ""
    (org-ql-select (org-agenda-files)
      '(and (category "Projects")
            (level 2))
      :action #'(lambda ()
                  (cons
                   (org-element-property
                    :raw-value (org-element-headline-parser (line-end-position)))
                   (list
                    :path (buffer-file-name)
                    :point (point))))))

  (defun my/complete-project-location (project-location-list)
    ""
    (ivy-read "Project: " (mapcar #'(lambda (location)
                                      (car location))
                                  project-location-list)))

  (defun my/project-capture-location-function ()
    (let* ((project-location-list (my/project-location-list))
           (project-location (assoc (my/complete-project-location
                                     project-location-list)
                                    project-location-list))
           (project-location-prop (cdr project-location))
           (point (plist-get project-location-prop ':point))
           (path (plist-get project-location-prop ':path)))
      (set-buffer (org-capture-target-buffer path))
      (goto-char point)))
  
  ;; use as-is if you don't have an existing set of org-capture templates
  ;; otherwise add to existing setup
  ;; you can of course change the letters, too
  (setq org-capture-templates `(("i" "Inbox"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "Todo with link"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i\n  %a"
                                 :kill-buffer t)
                                ("p" "Projects"
                                 entry (function my/project-capture-location-function)
                                 "* TODO %?\n%U\n\n  %i"
                                 :kill-buffer t))))

(use-package org-wild-notifier
  :if (daemonp)
  :custom
  (org-wild-notifier-alert-time '(10 1))
  (org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
  (alert-default-style 'libnotify)
  :config
  (org-wild-notifier-mode))

(provide 'my-im)

;;; my-im.el ends here

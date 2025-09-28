;;; init-im.el --- information management -*- lexical-binding: t -*-

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

(use-package org
  :defer t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :init
  (setopt org-id-locations-file (expand-file-name ".org-id-locations" my/emacs-cache-dir)))

(use-package org-agenda
  :straight nil
  :defer t
  :after org
  :init
  (setopt org-agenda-tags-column -100
          org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CANCELED(c@)"))
          org-log-into-drawer t
          org-log-redeadline 'time
          org-log-reschedule 'time
          org-log-repeat 'time
          org-log-done 'time
          org-agenda-hide-tags-regexp "todo\\|habit\\|inbox"
          org-habit-show-all-today t
          org-archive-location ".org_archive::* From %s"
          org-agenda-custom-commands
          '(("d" "today view"
             ((agenda "" ((org-agenda-span 'day)
                          (org-agenda-show-log t)
                          (org-agenda-log-mode-add-notes nil)
                          (org-agenda-log-mode-items '(closed clock state))
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :habit t)))))))
            ("i" "inbox view"
             ((alltodo "" ((org-agenda-files (list my/agenda-inbox-file))))))
            ("p" "project group view"
             ((alltodo "" ((org-agenda-files (append (list my/agenda-inbox-file) (my/agenda-project-files)))
                           (org-super-agenda-groups
                            '((:name "Inbox"
                                     :tag "inbox"
                                     :order 1)
                              (:name "Working"
                                     :tag "working"
                                     :order 2)
                              (:order-multi (3 (:auto-category t)))))))))))
  :config
  (require 'my-org-theme)
  (require 'my-im)
  (setopt org-agenda-files (append (list my/gtd-dir) (my/agenda-project-files)))
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'ol-man)

  :init
  (when (daemonp)
    (require 'my-im)
    (my/async-agenda-sync-remote))

  (defun my/org-refile-target-verify-function ()
    (and (member "todo" (org-get-tags)) (not (org-get-todo-state))))
  (setopt org-refile-targets '((my/agenda-project-files . (:maxlevel . 3)))
          org-refile-target-verify-function 'my/org-refile-target-verify-function)

  (defun my/init-when-org-agenda-finalize()
    (setq-local olivetti-body-width 120)
    (olivetti-mode))
  :hook
  ((org-agenda-finalize . my/init-when-org-agenda-finalize)
   (after-save . my/agenda-sync-after-save-hook-func)))

(use-package org-ql
  :defer t
  :after org)

(use-package emacs
  :after org-capture
  :defines org-capture-templates
  :config
  (add-to-list 'org-capture-templates '("t" "Todo heading"
                                        entry (function my/gtd-capture-todo-heading-function)
                                        "* TODO %:description%?\n%U\n\n  %i"
                                        :kill-buffer t))
  (add-to-list 'org-capture-templates '("i" "Inbox"
                                        entry (file my/agenda-inbox-file)
                                        "* TODO %:description%?\n%U\n\n  %i"
                                        :kill-buffer t)))

(use-package org-super-agenda
  :defer t
  :after org
  :hook
  (org-agenda-mode . org-super-agenda-mode))

(use-package org-roam
  :straight (org-roam
             :host github
             :repo "org-roam/org-roam"
             :branch "main")
  :defer t
  :bind (("C-c C-n /" . org-roam-node-find)
         ("C-c C-n r" . org-roam-db-sync)
         ("C-c C-n c" . org-roam-capture)
         ("C-c C-n d" . org-roam-dailies-goto-today)
         :map org-mode-map
         (("C-c C-n i" . org-roam-node-insert)
          ("C-c C-n t" . org-roam-tag-add)
          ("C-c C-n a" . org-roam-alias-add)
          ("C-c C-n b" . org-roam-buffer-toggle)))
  :config
  (require 'my-org-roam)
  (setopt org-roam-directory my/roam-dir
          org-roam-dailies-directory my/daily-dir
          org-roam-db-location (expand-file-name "org-roam.db" my/im-cache-dir)
          org-roam-db-update-on-save t
          org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:40}" 'face 'org-tag))
          org-roam-mode-sections
          '(my/org-roam-backlinks-section org-roam-reflinks-section))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight
  (
   :host github
   :repo "org-roam/org-roam-ui"
   :branch "main"
   :files ("*.el" "out"))
  :after org-roam
  :if (daemonp)
  :init
  (setopt org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil)
  :config
  (org-roam-ui-mode))

(use-package org-wild-notifier
  :if (daemonp)
  :commands
  (org-wild-notifier-mode)
  :init
  (setopt org-wild-notifier-alert-time '(10 1)
          org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
  :config
  (org-wild-notifier-mode))

(use-package ox-hugo
  :defer t
  :after ox)

(use-package nov
  :defer t
  :init
  (setopt nov-save-place-file (expand-file-name "nov-places" my/emacs-cache-dir))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; TODO
;; (use-package org-transclusion
;;   :after org
;;   :defer t
;;   :hook
;;   (org-mode . org-transclusion-mode)
;;   :bind
;;   (("<f12>" . org-transclusion-mode)))

(provide 'init-im)

;;; init-im.el ends here

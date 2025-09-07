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

(require 'my-const)

(defcustom my/im-dir "~/workspace/my-im"
  "IM directory."
  :type 'directory
  :group 'my)

(defcustom my/im-cache-dir (expand-file-name ".cache" my/im-dir)
  "IM db directory."
  :type 'directory
  :group 'my)

(defcustom my/gtd-dir (expand-file-name "gtd" my/im-dir)
  "GTD directory."
  :type '(list directory)
  :group 'my)

(defcustom my/agenda-inbox-file (expand-file-name "inbox.org" my/gtd-dir)
  "GTD directory."
  :type 'file
  :group 'my)

(defcustom my/agenda-project-dir (expand-file-name "project" my/gtd-dir)
  "GTD project directory."
  :type 'file
  :group 'my)

(defcustom my/agenda-sync-interval 60
  "GTD agenda sync interval."
  :type 'integer
  :group 'my)

(defcustom my/agenda-rclone-remote "gtd"
  "GTD rclone remote."
  :type 'string
  :group 'my)

(defcustom my/agenda-rclone-remote-gtd-dir "gtd"
  "GTD rclone remote directory."
  :type 'string
  :group 'my)

(use-package org
  :defer t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)))

(use-package org-agenda
  :straight nil
  :defer t
  :after org
  :custom
  (org-agenda-tags-column -100)
  (org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)" "CANCELED(c@)")))
  (org-log-into-drawer t)
  (org-log-redeadline t)
  (org-log-reschedule t)
  (org-log-repeat 'time)
  (org-log-done 'time)
  (org-agenda-hide-tags-regexp "todo\\|habit")
  (org-habit-show-all-today t)
  (org-archive-location ".org_archive::* From %s")
  (org-agenda-custom-commands
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
      ((alltodo "" ((org-agenda-files (my/agenda-project-files))
                    (org-super-agenda-groups
                     '((:auto-category)))))))))
  :config
  (defun my/agenda-project-files ()
    (directory-files my/agenda-project-dir t org-agenda-file-regexp))
  (setq org-agenda-files (append (list my/gtd-dir) (my/agenda-project-files)))

  :init
  (defvar my/agenda-sync-buffer-name "*agenda-sync*")
  (defvar my/agenda-sync-timer nil)

  (defun my/agenda-sync-cmd (source dest)
    (format "rclone sync -P %s %s" source dest))

  (defun my/agenda-sync-remote-path ()
      (format "%s:%s" my/agenda-rclone-remote my/agenda-rclone-remote-gtd-dir))

  (defun my/run-agenda-sync-timer ()
    (unless my/agenda-sync-timer
      (message (format "[agenda] Starting automatically sync %s to %s"
                       (my/agenda-sync-remote-path) my/gtd-dir))
      (setq my/agenda-sync-timer
            (run-with-timer
             my/agenda-sync-interval nil
             (lambda ()
               (cancel-timer my/agenda-sync-timer)
               (setq my/agenda-sync-timer nil)
               (my/async-agenda-sync-remote)
               )))))

  (defun my/agenda-sync-sentinel (proc event)
    (my/run-agenda-sync-timer))
  
  (defun my/async-agenda-sync-remote ()
    (interactive)
    (let ((proc (start-process-shell-command
                 my/agenda-sync-buffer-name
                 my/agenda-sync-buffer-name
                 (my/agenda-sync-cmd (my/agenda-sync-remote-path) my/gtd-dir))))
      (set-process-sentinel proc #'my/agenda-sync-sentinel)))

  (defun my/agenda-sync-local ()
    (interactive)
    (message (format "[agenda] sync %s to %s" my/gtd-dir (my/agenda-sync-remote-path)))
    (call-process-shell-command (my/agenda-sync-cmd
                                 my/gtd-dir (my/agenda-sync-remote-path))
                                nil
                                my/agenda-sync-buffer-name
                                t))

  (defun my/agenda-sync-after-save-hook-func ()
    (when (and (eq major-mode 'org-mode)
               (string-prefix-p my/gtd-dir buffer-file-name))
      (my/agenda-sync-local)))

  (when (daemonp)
    (my/async-agenda-sync-remote))

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'ol-man)


  (defun my/org-refile-target-verify-function ()
    (and (member "todo" (org-get-tags)) (not (org-get-todo-state))))
  (setq org-refile-targets '((my/agenda-project-files . (:maxlevel . 3)))
        org-refile-target-verify-function 'my/org-refile-target-verify-function)

  :hook
  (after-save . my/agenda-sync-after-save-hook-func))

(use-package org-capture
  :straight nil
  :defer t
  :after org-agenda
  :custom
  (org-capture-templates
   `(("p" "Project"
      entry (function my/gtd-capture-groups-function)
      "* TODO %:description%?\n%U\n\n  %i"
      :kill-buffer t)
     ("i" "Inbox"
      entry (file my/agenda-inbox-file)
      "* TODO %:description%?\n%U\n\n  %i"
      :kill-buffer t)))
  :config
  (use-package org-ql)
  
  (defun my/gtd-todo-heading-list ()
    (org-ql-select (my/agenda-project-files)
      '(and (tags "todo") (not (todo)))
      :action #'(lambda ()
                  (cons
                   (substring-no-properties (format "%s %s" (org-get-title) (org-get-heading t t t t)))
                   (list
                    :path (buffer-file-name)
                    :point (point)))
                  )))

  (defun my/gtd-complete-group (todo-heading-list)
    (ivy-read "Todo: " (mapcar #'(lambda (location)
                                   (car location))
                               todo-heading-list)))

  (defun my/gtd-capture-groups-function ()
    (let* ((todo-heading-list (my/gtd-todo-heading-list))
           (todo-heading (assoc (my/gtd-complete-group
                            todo-heading-list)
                           todo-heading-list))
           (todo-heading-prop (cdr todo-heading))
           (point (plist-get todo-heading-prop ':point))
           (path (plist-get todo-heading-prop ':path)))
      (set-buffer (org-capture-target-buffer path))
      (goto-char point))))

(my/require-modules
 '(org-roam
   deft
   org-super-agenda
   org-wild-notifier
   ;; org-fc
   )
 )

;; (use-package org-download
;;   :defer t
;;   :after org
;;   :commands (org-download-clipboard)
;;   :config
;;   (setq org-download-image-dir "./image"
;;         org-download-heading-lvl 1
;;         ))

(provide 'my-im)

;;; my-im.el ends here

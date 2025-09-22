;;; my-im.el --- im -*- lexical-binding: t -*-

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

(require 'org-agenda)
(require 'org-capture)
(require 'org-ql)
(require 'elfeed-db)

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

(defcustom my/roam-dir (expand-file-name "roam" my/im-dir)
  "GTD directory."
  :type 'directory
  :group 'my)

(defcustom my/daily-dir (expand-file-name "daily" my/im-dir)
  "GTD directory."
  :type 'directory
  :group 'my)

(defun my/agenda-project-files ()
  "Agenda project files."
  (directory-files my/agenda-project-dir t org-agenda-file-regexp))

(defvar my/agenda-sync-buffer-name "*agenda-sync*")
(defvar my/agenda-sync-timer nil)

(defun my/agenda-sync-cmd (source dest)
  "Agenda sync command with SOURCE DEST."
  (format "rclone sync -P %s %s" source dest))

(defun my/agenda-sync-remote-path ()
  "Agenda sync remote path."
  (format "%s:%s" my/agenda-rclone-remote my/agenda-rclone-remote-gtd-dir))

(defun my/run-agenda-sync-timer ()
  "Run agenda sync timer."
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

(defun my/agenda-sync-sentinel (_proc _event)
  "Agenda sync sentinel."
  (my/run-agenda-sync-timer))

(defun my/async-agenda-sync-remote ()
  "Start sync remote."
  (interactive)
  (let ((proc (start-process-shell-command
               my/agenda-sync-buffer-name
               my/agenda-sync-buffer-name
               (my/agenda-sync-cmd (my/agenda-sync-remote-path) my/gtd-dir))))
    (set-process-sentinel proc #'my/agenda-sync-sentinel)))

(defun my/agenda-sync-local ()
  "Start sync local."
  (interactive)
  (message (format "[agenda] sync %s to %s" my/gtd-dir (my/agenda-sync-remote-path)))
  (call-process-shell-command (my/agenda-sync-cmd
                               my/gtd-dir (my/agenda-sync-remote-path))
                              nil
                              my/agenda-sync-buffer-name
                              t))

(defun my/agenda-sync-after-save-hook-func ()
  "Sync after save hook."
  (when (and (eq major-mode 'org-mode)
             (string-prefix-p my/gtd-dir buffer-file-name))
    (my/agenda-sync-local)))

(defun my/gtd-todo-heading-list ()
  "GTD todo heading list."
  (org-ql-select (my/agenda-project-files)
    '(and (tags "todo") (not (or (todo) (done))))
    :action #'(lambda ()
                (cons
                 (substring-no-properties (format "%s %s" (org-get-title) (org-get-heading t t t t)))
                 (list
                  :path (buffer-file-name)
                  :point (point)))
                )))

(defun my/gtd-complete-group (todo-heading-list)
  "GTD complete group with TODO-HEADING-LIST."
  (completing-read "Todo: " (mapcar #'(lambda (location)
                                        (car location))
                                    todo-heading-list)))

(defun my/gtd-capture-todo-heading-function ()
  "GTD capture todo heading function."
  (let* ((todo-heading-list (my/gtd-todo-heading-list))
         (todo-heading (assoc (my/gtd-complete-group
                               todo-heading-list)
                              todo-heading-list))
         (todo-heading-prop (cdr todo-heading))
         (point (plist-get todo-heading-prop ':point))
         (path (plist-get todo-heading-prop ':path)))
    (set-buffer (org-capture-target-buffer path))
    (goto-char point)))

(defun my/gtd-capture-elfeed-template ()
  "GTD capture elfeed template."
  ;; TODO: warn
  (let* ((entry elfeed-show-entry)
         (title (elfeed-entry-title entry))
         (link (elfeed-entry-link entry)))
    (format "* TODO [[%s][%s]] :reading:" link title))
  )

(provide 'my-im)

;;; my-im.el ends here

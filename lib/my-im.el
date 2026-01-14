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

(defcustom my/roam-dir (expand-file-name "roam" my/im-dir)
  "GTD directory."
  :type 'directory
  :group 'my)

(defcustom my/daily-dir (expand-file-name "daily" my/roam-dir)
  "GTD directory."
  :type 'directory
  :group 'my)

(defun my/agenda-project-files ()
  "Agenda project files."
  (directory-files my/agenda-project-dir t org-agenda-file-regexp))

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

(defun my/org-property-habit-get-allowed-values (property)
  "The `habit' PROPERTY get allowed values."
  (when (string-equal-ignore-case property "HABIT")
    (org-ql-select (my/agenda-project-files)
      '(and (habit) (property "ID"))
      :action #'(lambda ()
                  (propertize
                   (org-id-get)
                   'display
                   (substring-no-properties (org-get-heading t t t t)))))))

(defun my/org-insert-timestamp-log-item ()
  "Insert timestamp log item."
  (interactive)
  (if (org-at-item-p)
      (progn
        (end-of-line)
        (org-insert-item)
        (insert (format "%s :: " (format-time-string "%H:%M"))))
    (insert (format "+ %s :: " (format-time-string "%H:%M")))))

(defun my/im-org-metareturn-hook ()
  "Org metareturn hook for timestamp log item."
  (when (and (eq major-mode 'org-mode)
             (org-at-item-p))
    (let ((is-ts-log-item (save-excursion
                            (beginning-of-line)
                            (looking-at-p "^[ \t]*\\+ [0-9][0-9]:[0-9][0-9] ::"))))
      (when is-ts-log-item
        (end-of-line)
        (org-insert-item)
        (insert (format-time-string "%H:%M"))
        (end-of-line)
        t))))

(provide 'my-im)

;;; my-im.el ends here

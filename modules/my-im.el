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

;; (use-package org-download
;;   :defer t
;;   :after org
;;   :commands (org-download-clipboard)
;;   :config
;;   (setq org-download-image-dir "./image"
;;         org-download-heading-lvl 1
;;         ))

(defcustom my/im-dir "~/workspace/my-im"
  "IM directory."
  :type 'directory
  :group 'my)

(defcustom my/im-cache-dir (expand-file-name ".cache" my/im-dir)
  "IM db directory."
  :type 'directory
  :group 'my)

(defcustom my/gtd-dir (list "~/workspace/GTD")
  "GTD directory."
  :type '(list directory)
  :group 'my)

(use-package org
  :defer t
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda))

(use-package org-capture
  :straight nil
  :defer t
  :after org
  :custom
  (org-capture-templates
   `(("g" "Groups"
      entry (function my/gtd-capture-groups-function)
      "* TODO %?\n%U\n\n  %i"
      :kill-buffer t)))
  :config
  (use-package org-ql)
  (defun my/gtd-group-list ()
    (org-ql-select (org-agenda-files)
      '(and (level 1))
      :action #'(lambda ()
                  (cons
                   (org-element-property
                    :raw-value (org-element-headline-parser (line-end-position)))
                   (list
                    :path (buffer-file-name)
                    :point (point))))))

  (defun my/gtd-complete-group (group-list)
    (ivy-read "Groups: " (mapcar #'(lambda (location)
                                     (car location))
                                 group-list)))

  (defun my/gtd-capture-groups-function ()
    (let* ((group-list (my/gtd-group-list))
           (group (assoc (my/gtd-complete-group
                          group-list)
                         group-list))
           (group-prop (cdr group))
           (point (plist-get group-prop ':point))
           (path (plist-get group-prop ':path)))
      (set-buffer (org-capture-target-buffer path))
      (goto-char point)))
  )

(use-package org-agenda
  :straight nil
  :defer t
  :after org
  :config
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (setq org-agenda-files my/gtd-dir
        ;; org-tags-exclude-from-inheritance '("repeat")
        org-agenda-tags-column -100)
  ;; a useful view to see what can be accomplished today
  (setq org-agenda-custom-commands
        '(("g" "group view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :scheduled today)))))
            (alltodo "" ((org-super-agenda-groups
                          '((:auto-parent))))))))))

(my/require-modules
 '(org-roam
   deft
   org-super-agenda
   org-wild-notifier
   ;; org-fc
   )
 )

(provide 'my-im)

;;; my-im.el ends here

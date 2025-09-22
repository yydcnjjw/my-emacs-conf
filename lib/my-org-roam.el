;;; my-org-roam.el --- org-roam -*- lexical-binding: t -*-

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

(require 'org-roam)

(defun my/org-roam-backlinks-groupby (backlink)
  "Org roam backlinks groupby with BACKLINK."
  (cons
   (org-roam-node-id (org-roam-backlink-source-node backlink))
   (plist-get (org-roam-backlink-properties backlink) :outline)))

(defun my/org-roam-backlinks-map (groupby)
  "Org roam backlinks map with GROUPBY."
  (car (cdr groupby)))

(cl-defun my/org-roam-backlinks-section (node)
  "The backlinks section for NODE.

When UNIQUE is nil, show all positions where references are found.
When UNIQUE is t, limit to unique sources."
  (when-let* ((backlinks (seq-sort #'org-roam-backlinks-sort
                                   (seq-map #'my/org-roam-backlinks-map
                                            (seq-group-by #'my/org-roam-backlinks-groupby
                                                          (org-roam-backlinks-get node))))))
    (magit-insert-section (org-roam-backlinks)
                          (magit-insert-heading "Backlinks:")
                          (dolist (backlink backlinks)
                            (org-roam-node-insert-section
                             :source-node (org-roam-backlink-source-node backlink)
                             :point (org-roam-backlink-point backlink)
                             :properties (org-roam-backlink-properties backlink)))
                          (insert ?\n))))

(provide 'my-org-roam)

;;; my-org-roam.el ends here

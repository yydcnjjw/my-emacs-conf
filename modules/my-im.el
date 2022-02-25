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

(defconst my/im-dir "~/workspace/my-im")
(defconst my/im-db-dir (expand-file-name ".db" my/im-dir))

(use-package org-roam
  :straight (org-roam
             :host github
             :repo "org-roam/org-roam")
  :defer t
  :custom
  ((org-roam-directory my/im-dir)
   (org-roam-db-location (expand-file-name "org-roam.db" my/im-db-dir))
   (org-roam-db-update-method 'idle-timer)
   (org-roam-db-update-idle-seconds 10)
   )
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c C-n /" . org-roam-node-find)
         ("C-c C-n r" . org-roam-db-sync)
         ("C-c C-n c" . org-roam-capture)
         :map org-mode-map
         (("C-c C-n i" . org-roam-node-insert)
          ("C-c C-n t" . org-roam-tag-add)
          ("C-c C-n b" . org-roam-buffer-toggle)))
  )

(use-package deft
  :after org
  :bind
  ("C-c C-n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  :config
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
	  (if begin
	      (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	    (deft-base-filename file))))
  
  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)
  
  (setq deft-strip-summary-regexp
	    (concat "\\("
		        "[\n\t]" ;; blank
		        "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		        "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		        "\\)")))

(use-package org-roam-ui
  :straight
  (
   :host github
   :repo "org-roam/org-roam-ui"
   :branch "main"
   :files ("*.el" "out"))
  :if (daemonp)
  :config
  (org-roam-ui-mode))

(bind-keys*
 ("C-c c" . org-capture)
 ("C-c a" . org-agenda)
 )

(use-package org-gtd
  :after org
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
  :straight nil
  :defer t
  :after org-gtd
  :config
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (setq org-agenda-files `(,org-gtd-directory)
        org-tags-exclude-from-inheritance '("repeat")
        org-agenda-tags-column -100)
  ;; a useful view to see what can be accomplished today
  (setq org-agenda-custom-commands
        '(("g" "Scheduled today and all NEXT/TODO items"
           ((agenda "" ((org-agenda-span 'day)))
            (alltodo "" ((org-agenda-tag-filter-preset '("-repeat"))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))))))

(use-package org-capture
  :straight nil
  :defer t
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
                                 entry (file ,(org-gtd--inbox-file))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "Todo with link"
                                 entry (file ,(org-gtd--inbox-file))
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
  :config
  (org-wild-notifier-mode))

(provide 'my-im)

;;; my-im.el ends here

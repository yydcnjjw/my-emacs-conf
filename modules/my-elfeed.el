;;; my-elfeed.el --- elfeed -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (elfeed)


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

(defcustom my/elfeed-org-files nil
  "Elfeed org files."
  :type 'list
  :group 'my)

(defcustom my/elfeed-auto-update-interval (* 10 60)
  "Elfeed auto update interface."
  :type 'integer
  :group 'my)

(defcustom my/elfeed-unjam-threshold (* 3 my/elfeed-auto-update-interval)
  "Elfeed elfeed unjam threshold if pending count > 3"
  :type 'integer
  :group 'my)

(use-package elfeed-org
  :after elfeed
  :config
  (if my/elfeed-org-files
      (setq rmh-elfeed-org-files my/elfeed-org-files))
  (elfeed-org)
  ;; update elfeed list after loading
  (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))

(use-package elfeed
  :if (daemonp)
  :demand
  :bind
  (("C-c e" . elfeed))
  :config
  ;; FIXME: workaround https://github.com/skeeto/elfeed/issues/258
  (defun my/elfeed-xml-parse-region (&optional beg end buffer parse-dtd _parse-ns)
    "Decode (if needed) and parse XML file. Uses coding system from
XML encoding declaration."
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))
    (setf (point) beg)
    (when (re-search-forward
           "<\\?xml.*?encoding=[\"']\\([^\"']+\\)[\"'].*?\\?>" nil t)
      (let ((coding-system (intern-soft (downcase (match-string 1)))))
        (when (ignore-errors (check-coding-system coding-system))
          (let ((mark-beg (make-marker))
                (mark-end (make-marker)))
            ;; Region changes with encoding, so use markers to track it.
            (set-marker mark-beg beg)
            (set-marker mark-end end)
            (set-buffer-multibyte t)
            (recode-region mark-beg mark-end coding-system 'raw-text)
            (setf beg (marker-position mark-beg)
                  end (marker-position mark-end))))))
    (let ((xml-default-ns ()))
      (xml-parse-region beg end buffer parse-dtd nil)))
  
  (advice-add 'elfeed-xml-parse-region :override #'my/elfeed-xml-parse-region)

  (defun my/elfeed-query-count-and-first-entry (query)
    "Return the number of feeds and first entry returned by the QUERY."
    (let* ((count 0)
           (first-entry nil)
           (filter (elfeed-search-parse-filter query))
           (func (byte-compile (elfeed-search-compile-filter filter))))
      (with-elfeed-db-visit (entry feed)
        (when (funcall func entry feed count)
          (when (null first-entry)
            (setq first-entry entry))
          (setf count (1+ count))))
      (cons count first-entry)))

  (defun my/elfeed-query-count (query)
    "Return the number of feeds returned by the QUERY."
    (car (my/elfeed-query-count-and-first-entry query)))

  (defvar my/elfeed-update-timer nil)
  (defvar my/elfeed-unread-count nil)
  (defvar my/elfeed-pending-count 0)
  (defvar my/elfeed-auto-update-timer nil)

  (defun my/elfeed-reset-auto-update-timer ()
    (cancel-timer my/elfeed-update-timer)
    (setq my/elfeed-update-timer nil))

  (defun my/elfeed-update ()
    "elfeed update"
    (elfeed-log 'info "starting automatically update")
    (unless my/elfeed-update-timer
      (setq my/elfeed-unread-count (my/elfeed-query-count "+unread"))
      (elfeed-update)
      (setq my/elfeed-update-timer
            (run-with-timer
             1 1
             (lambda ()
               (if (> (elfeed-queue-count-total) 0)
                   (if (< my/elfeed-pending-count my/elfeed-unjam-threshold)
                       (progn
                         (setq my/elfeed-pending-count (+ my/elfeed-pending-count 1))
                         (elfeed-log 'debug "%d jobs pending" (elfeed-queue-count-total)))
                     (setq my/elfeed-pending-count 0)
                     (elfeed-unjam)
                     (my/elfeed-reset-auto-update-timer))
                 (setq my/elfeed-pending-count 0)
                 (my/elfeed-reset-auto-update-timer)
                 (elfeed-log 'info "Automatic update has been completed")
                 (let* ((count-and-first-entry (my/elfeed-query-count-and-first-entry "+unread"))
                        (newcnt (car count-and-first-entry))
                        (entry (cdr count-and-first-entry)))
                   (when (> newcnt my/elfeed-unread-count)
                     (elfeed-log 'info (format "alert %s" (elfeed-entry-title entry)))
                     (alert (elfeed-entry-title entry)
                            :title (format "elfeed updated %d" (- newcnt my/elfeed-unread-count)))))
                 ))))
      ))
  (setq my/elfeed-auto-update-timer
        (run-at-time nil my/elfeed-auto-update-interval #'my/elfeed-update)))

(provide 'my-elfeed)

;;; my-elfeed.el ends here

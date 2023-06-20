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
  :custom
  (shr-inhibit-images t)
  :bind
  (("C-c e" . elfeed))
  :bind (:map elfeed-search-mode-map
              ("B" . my/elfeed-search-webkit-browse-url))
  :config
  (defun my/elfeed-search-webkit-browse-url (entry)
    "Display the currently selected item in xwidget-webkit-browser."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (require 'elfeed-show)
    (when (elfeed-entry-p entry)
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))
      (let ((link (elfeed-entry-link entry)))
        (xwidget-webkit-browse-url link))))
  
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

  (defun my/elfeed-reset-update-timer ()
    (cancel-timer my/elfeed-update-timer)
    (setq my/elfeed-update-timer nil))

  (defun my/elfeed-alert-update-unread ()
    (let* ((count-and-first-entry (my/elfeed-query-count-and-first-entry "+unread"))
           (newcnt (car count-and-first-entry))
           (entry (cdr count-and-first-entry)))
      (when (> newcnt my/elfeed-unread-count)
        (elfeed-log 'info (format "alert %s" (elfeed-entry-title entry)))
        (alert (elfeed-entry-title entry)
               :title (format "elfeed updated %d" (- newcnt my/elfeed-unread-count))))))

  ;; FIXME: (void-function (setf (elfeed-meta)))
  (eval-when-compile (require 'elfeed))
  (defun my/elfeed-update-feed-bg (url)
    "Update a specific feed at background."
    (elfeed-with-fetch url
      (if (elfeed-is-status-error status use-curl)
          (let ((print-escape-newlines t))
            (elfeed-handle-http-error
             url (if use-curl elfeed-curl-error-message status)))
        (condition-case error
            (let ((feed (elfeed-db-get-feed url)))
              (unless use-curl
                (elfeed-move-to-first-empty-line)
                (set-buffer-multibyte t))
              (unless (eql elfeed-curl-status-code 304)
                ;; Update Last-Modified and Etag
                (setf (elfeed-meta feed :last-modified)
                      (cdr (assoc "last-modified" elfeed-curl-headers))
                      (elfeed-meta feed :etag)
                      (cdr (assoc "etag" elfeed-curl-headers)))
                (if (equal url elfeed-curl-location)
                    (setf (elfeed-meta feed :canonical-url) nil)
                  (setf (elfeed-meta feed :canonical-url) elfeed-curl-location))
                (let* ((xml (elfeed-xml-parse-region (point) (point-max)))
                       (entries (cl-case (elfeed-feed-type xml)
                                  (:atom (elfeed-entries-from-atom url xml))
                                  (:rss (elfeed-entries-from-rss url xml))
                                  (:rss1.0 (elfeed-entries-from-rss1.0 url xml))
                                  (otherwise
                                   (error (elfeed-handle-parse-error
                                           url "Unknown feed type."))))))
                  (elfeed-db-add entries))))
          (error (elfeed-handle-parse-error url error))))
      (unless use-curl
        (kill-buffer))))
  
  (defun my/elfeed-update-bg ()
    "Update all the feeds in `elfeed-feeds' at background."
    (elfeed-log 'info "Elfeed update: %s"
                (format-time-string "%B %e %Y %H:%M:%S %Z"))
    (mapc #'my/elfeed-update-feed-bg (elfeed--shuffle (elfeed-feed-list)))
    (elfeed-db-save))
  
  (defun my/elfeed-async-update ()
    "elfeed async update"
    (elfeed-log 'info "starting automatically update")
    (unless my/elfeed-update-timer
      (setq my/elfeed-unread-count (my/elfeed-query-count "+unread"))
      (my/elfeed-update-bg)
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
                 (my/elfeed-reset-update-timer)
                 (elfeed-log 'info "Automatic update has been completed")
                 (my/elfeed-alert-update-unread)))))))

  (defun my/elfeed-auto-update ()
    (interactive)
    (setq my/elfeed-auto-update-timer
          (run-at-time nil my/elfeed-auto-update-interval #'my/elfeed-async-update)))

  (my/elfeed-auto-update))


(use-package elfeed-webkit
  :ensure
  :after elfeed
  :bind (:map elfeed-show-mode-map
              ("%" . elfeed-webkit-toggle)))

(provide 'my-elfeed)

;;; my-elfeed.el ends here

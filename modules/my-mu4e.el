;;; my-mu4e.el --- mu4e -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (mu4e)


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

(defun my/mu4e-updated-notify ()
  ""
  (when mu4e-index-update-status
    (let ((updated (plist-get mu4e-index-update-status :updated)))
      (unless (zerop updated)
        (alert "From Emacs mu4e" :title (format "%d Mails updated" updated)))
      )))

(use-package mu4e
  :ensure t
  :defer t
  :straight
  (:host github
         :repo "djcb/mu"
         :branch "master"
         :files ("build/mu4e/*")
         :pre-build `(("autogen.sh" "--prefix"
                       ,(expand-file-name "build/install" (straight--repos-dir "mu")))
                      ("make")))
  :custom
  ((mail-user-agent 'mu4e-user-agent)
   (mu4e-mu-binary (expand-file-name "build/install/bin/mu" (straight--repos-dir "mu")))
   (mu4e-sent-messages-behavior 'sent)
   (mu4e-sent-folder "/Sent")
   (mu4e-drafts-folder "/Drafts")
   (mu4e-trash-folder "/Deleted")
   (mu4e-get-mail-command "offlineimap")
   (mu4e-update-interval 60)
   (mu4e-maildir-shortcuts
    '((:maildir "/Inbox" :key ?i))))
  :bind
  (("C-c m" . mu4e))
  :hook
  ((mu4e-index-updated . my/mu4e-updated-notify))
  :config
  (setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.office365.com"
   smtpmail-smtp-server "smtp.office365.com"
   smtpmail-local-domain "yydcnjjw.outlook.com")

  (when (daemonp)
    (mu4e t))
  )

(provide 'my-mu4e)

;;; my-mu4e.el ends here

;;; my-wl.el --- wanderlust -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (wl)


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

(defun test()
    (alert "Test" :title "Emacs" :style 'm))

(use-package wanderlust
  :defer t
  :init
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  (setq my/wl-config-dir (expand-file-name ".wl" user-emacs-directory))
  :custom
  ((wl-from "yydcnjjw <yydcnjjw@gmail.com>")
   (elmo-localdir-folder-path (expand-file-name "Mail" my/wl-config-dir))
   (elmo-msgdb-directory (expand-file-name ".elmo" my/wl-config-dir))
   (wl-temporary-file-directory (expand-file-name "tmp" my/wl-config-dir))
   (wl-folders-file (expand-file-name ".folders" my/wl-config-dir))
   (wl-message-ignored-field-list '("^.*"))
   (wl-message-visible-field-list
    '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:"))
   (wl-message-sort-field-list
    '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:"))
   (wl-stay-folder-window t)

   (wl-default-folder "%inbox") ;; my main inbox
   (wl-biff-check-folder-list '("%inbox")) ;; check every 180 seconds
   (wl-biff-check-interval 30)
   
   ;; Always download emails without confirmation
   (wl-prefetch-threshold nil)
   (wl-message-buffer-prefetch-threshold nil)
   (elmo-message-fetch-threshold nil)

   (wl-biff-check-folder-list '("%inbox"))
   
   )
  :hook
  ((wl-biff-notify . test)
   (wl-biff-unnotify . test))
  :config
  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))
  )

(provide 'my-wl)

;;; my-wl.el ends here

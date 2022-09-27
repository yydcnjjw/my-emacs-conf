;;; my-alert.el --- alert -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (alert)


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

(require 'my-const)

(defcustom my/alert-use-m-command nil
  "Alert use m command."
  :type 'boolean
  :group 'my)

(defun my/m-command ()
  "Find m command."
  (executable-find (cond
                    ((or my/windows-p my/wsl-p) "m.exe")
                    (my/linux-p "m")
                    (nil "m"))))

(defun my/alert-m-notify (info)
  "Alert m notify INFO."
  (let ((m-command (my/m-command))
        (title (plist-get info :title))
        (message (plist-get info :message)))
    (if m-command
        (call-process
         m-command nil nil nil "toast" "--summary" title "--body" message "--timeout" "5000"))
    ))

(use-package alert-toast
  :straight (alert-toast
             :host github
             :repo "gkowzan/alert-toast")
  :after alert
  :init
  (defun my/alert-toast--psprocess-init ()
    "Initialize powershell process."
    (setq alert-toast--psprocess
          (make-process :name "powershell-toast"
                        :buffer "*powershell-toast*"
                        :command '("powershell.exe" "-noprofile" "-NoExit" "-NonInteractive" "-WindowStyle" "Hidden"
                                   "-Command" "-")
                        :coding (alert-toast--coding-page)
                        :noquery t
                        :connection-type 'pipe))
    (process-send-string alert-toast--psprocess "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] > $null
[Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml, ContentType=WindowsRuntime] > $null\n"))

  (defun my/alert-toast-notify (info)
    "Send INFO using Windows 10 toast notification.
Handles :ICON, :SEVERITY, :PERSISTENT, :NEVER-PERSIST, :TITLE and
:MESSAGE keywords from INFO plist."
    (let ((data-plist (plist-get info :data))
          psscript)
      (if (and (plist-get data-plist :shoulder-person) (plist-get data-plist :shoulder-payload))
          (setq psscript (format alert-toast--psscript-shoulder
                                 (s-replace-all alert-toast--psquote-replacements
                                                (alert-toast--fill-shoulder
                                                 (plist-get info :title)
                                                 (plist-get info :message)
                                                 (alert-toast--icon-path
                                                  (or (plist-get info :icon)
                                                      alert-toast-default-icon))
                                                 (plist-get data-plist :shoulder-person)
                                                 (plist-get data-plist :shoulder-payload)))))
        (setq psscript
              (format alert-toast--psscript-text
                      (s-replace-all alert-toast--psquote-replacements
                                     (alert-toast--fill-template
                                      (plist-get info :title)
                                      (plist-get info :message)
                                      (alert-toast--icon-path (or (plist-get info :icon)      alert-toast-default-icon))
                                      (plist-get data-plist :audio)
                                      (plist-get data-plist :silent)
                                      (plist-get data-plist :long)
                                      (plist-get data-plist :loop)))
                      (or (cdr (assq (plist-get info :severity) alert-toast-priorities))
                          (cdr (assq 'normal alert-toast-priorities)))
                      (if (and (plist-get info :persistent)
                               (not (plist-get info :never-persist)))
                          (* 60 60 24 7)  ; a week
                        alert-fade-time))))
      (unless alert-toast--psprocess
        (alert-toast--psprocess-init))
      (condition-case nil
          (process-send-string alert-toast--psprocess psscript)
        (error 
         (kill-process alert-toast--psprocess)
         (setq alert-toast--psprocess nil)
         (alert-toast--psprocess-init)
         (process-send-string alert-toast--psprocess psscript)))))  
  :config
  (advice-add #'alert-toast--psprocess-init :override #'my/alert-toast--psprocess-init))
  (advice-add #'my/alert-toast-notify :override #'my/alert-toast-notify)

(defun my/alert-notify (info)
  "WSL notify INFO."
  (if my/alert-use-m-command
      (my/alert-m-notify info)
    (cond
     ((or my/wsl-p my/windows-p) (alert-toast-notify info))
     (my/linux-p (alert-libnotify-notify info))
     (nil (alert-message-notify info)))))

(use-package alert
  :defer t
  :config
  (alert-define-style 'm
                      :title "Notify using m"
                      :notifier #'my/alert-notify)
  (setq alert-default-style 'm))

(provide 'my-alert)

;;; my-alert.el ends here

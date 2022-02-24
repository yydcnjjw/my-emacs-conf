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
  :after alert
  :straight (alert-toast
             :host github
             :repo "gkowzan/alert-toast"))

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

;;; my-proxy.el --- proxy -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: ()


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

(defun my/wsl-proxy-address ()
  "WSL proxy address."
  (use-package f)
  (let ((text (f-read-text "/etc/resolv.conf")))
    (string-match "^nameserver[ ]+\\(.*\\)$" text)
    (match-string 1 text)))

(let ((proxy-address
       (if my/wsl-p (my/wsl-proxy-address)
         "127.0.0.1")))
  (setq url-proxy-services
        `(("http" . ,(format "%s:8118" proxy-address))
          ("https" . ,(format "%s:8118" proxy-address))
          )))

(provide 'my-proxy)

;;; my-proxy.el ends here

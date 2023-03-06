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


(defconst my/proxy-address (if my/wsl-p
                               my/wsl-host-address
                             "127.0.0.1"))

(defconst my/http-proxy-port "8118")
(defconst my/socks5-proxy-port "8888")

(when (or (not (boundp 'url-proxy-services)) (null url-proxy-services))
  (setq url-proxy-services
          `(("http" . ,(format "%s:%s" my/proxy-address my/http-proxy-port))
            ("https" . ,(format "%s:%s" my/proxy-address my/http-proxy-port))
            )))

;; (setenv "HTTP_PROXY" (format "socks5://%s:%s" my/proxy-address my/socks5-proxy-port))
;; (setenv "HTTPS_PROXY" (format "socks5://%s:%s" my/proxy-address my/socks5-proxy-port))

(provide 'my-proxy)

;;; my-proxy.el ends here

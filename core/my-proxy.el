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

;; (defconst my/proxy-address "127.0.0.1")
;; ;; (defconst my/proxy-address "172.23.224.1")
;; (defconst my/http-proxy-port "8118")
;; (defconst my/socks5-proxy-port "8888")

;; (when (or (not (boundp 'url-proxy-services)) (null url-proxy-services))
;;   (setq url-proxy-services
;;           `(("http" . ,(format "%s:%s" my/proxy-address my/http-proxy-port))
;;             ("https" . ,(format "%s:%s" my/proxy-address my/http-proxy-port))
;;             )))

;; (setenv "HTTP_PROXY" (format "http://%s:%s" my/proxy-address my/http-proxy-port))
;; (setenv "HTTPS_PROXY" (format "https://%s:%s" my/proxy-address my/http-proxy-port))
;; (setenv "http_proxy" (format "http://%s:%s" my/proxy-address my/http-proxy-port))
;; (setenv "https_proxy" (format "https://%s:%s" my/proxy-address my/http-proxy-port))

(provide 'my-proxy)

;;; my-proxy.el ends here

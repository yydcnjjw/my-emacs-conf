;;; init-os.el --- os -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

(require 'my-platform)
(require 'my-path)

(use-package exec-path-from-shell
  :if (daemonp)
  :functions
  (exec-path-from-shell-initialize)
  :config
  ;; FIXME: https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
  (setq process-connection-type nil)
  (exec-path-from-shell-initialize)
  (setenv "LANG" "en_US.UTF-8"))

(when my/wsl-p
  (setopt browse-url-browser-function 'my/browse-url-wsl-open)
  ;; https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
  (setq-default select-active-regions nil))

(use-package atomic-chrome
  :if (daemonp)
  :commands atomic-chrome-start-server
  :init
  (setopt atomic-chrome-buffer-open-style 'full)
  :config
  (atomic-chrome-start-server))

;; url
(setopt url-configuration-directory (expand-file-name "url" my/emacs-cache-dir))

(provide 'init-os)

;;; init-os.el ends here
